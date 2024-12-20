// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit tSNE;

interface

{$IFDEF FPC}{$Mode Delphi} {$modeswitch advancedrecords} {$ENDIF}


uses Matrix, Types, RandomEng, PCA, MatrixConst;

// ###################################################################
// this unit is based on the matlab implementation on
// https://lvdmaaten.github.io/tsne/
// ###################################################################

// t-Distributed Stochastic Neighbor Embedding:
type
  TTSNEDistFunc = (dfOrig, dfEuclid, dfNormEuclid, dfAbs, dfMahalanobis);
  TTSNEProgress = procedure(Sender : TObject; iter : integer; cost : double; yMap : IMatrix; var doCancel : boolean) of Object;
  TTSNEPCAInit = function : TMatrixPCA of Object;
  TtSNE = class(TMatrixClass)
  private
    const cK : double = 0.2;
          cPhi : double = 0.8;
  private
    fPerplexity : Integer;
    fInitDims : integer;
    fTol : Double;
    fDistFunc : TTSNEDistFunc;

    // internal states
    fBeta : double;
    fy_grads, fyincs : IMatrix;
    fQ : IMatrix;
    fMinGain : double;
    fNumIter : integer;

    fProgress : TTSNEProgress;
    fInitPCA : TTSNEPCAInit;

    fSeed : integer;
    fRandAlgorithm : TRandomAlgorithm;

    function DefInitPCA : TMatrixPCA;

    procedure UpdateGains(var Value : double; const data : PDouble; LineWidth : integer; x, y : integer);
    procedure HBetaExp( var value : double );
    procedure EntropyFuncQ( var Value : double; const data : PDouble; LineWidth : integer; x, y : integer );

    function NormalizeInput( X : TDoubleMatrix ) : IMatrix;
    function ApplyPCA( X : IMatrix ) : IMatrix;
    function PairwiseDist( X : TDoubleMatrix ) : IMatrix;
    function D2P( D : IMatrix ) : IMatrix;
    function HBeta( D : IMatrix; beta : double; var H : double; var P : IMatrix ) : boolean;
    function tsne_p( P : IMatrix; numDims : integer ) : IMatrix;
    function InternalTSNE(xs: TDoubleMatrix; numDims: integer): TDoubleMatrix;
    function PairDist(Xs: TDoubleMatrix): IMatrix;

    function InitRand( w, h : Integer ) : IMatrix;

    // ###########################################
    // #### Burnes Hut
  private
    // P: sparse matrix
    fN : integer;
    fCols, fRows : TIntegerDynArray;
    fP : TDoubleDynArray;
    fTheta : double;
    fCost : double;

    fGradPosf, fGradNegf : TDoubleDynArray;

    procedure CalcPerplexity(x : TDoubleMatrix; perplexity : double; K : integer );
    procedure SymSparseMatrix;
    function EvaluateError( pY : PConstDoubleArr; numDims : integer ) : double;
    function InternalTSNEBarnesHut( xs : TDoubleMatrix; numDims : integer) : TDoubleMatrix;
    procedure computeGradient( pY : PConstDoubleArr; numDims : integer;
                               var dC: TDoubleDynArray );
  public
    property OnProgress : TTSNEProgress read fProgress write fProgress;
    property OnInitPCA : TTSNEPCAInit read fInitPCA write fInitPCA;

    property Cost : double read fCost;

    // main function to map X to the number of desired dimensions
    function SymTSNE(X : TDoubleMatrix; theta : double = 0.5; numDims : integer = 2) : TDoubleMatrix; overload;

    // use this function if X has been preprocessed "outside" e.g. by applying an
    // incremental pca approach -> the algorithm starts with the pairwise distance calculation
    function SymTSNEPreprocessed( X : TDoubleMatrix; theta : double = 0.5; numDims : integer = 2) : TDoubleMatrix;

    constructor Create( initDims : integer = 30; perplexity : integer = 30; numIter : integer = 1000;
                        distFunc : TTSNEDistFunc = dfOrig; randSeed : integer = 0; randAlgorithm : TRandomAlgorithm = raMersenneTwister);

    class function SymTSNE(X : TDoubleMatrix; numDims : integer;
                           initDims : integer; perplexity : integer;
                           theta : double = 0.5;
                           numIter : integer = 1000;
                           distFunc : TTSNEDistFunc = dfOrig;
                           randSeed : integer = 0; randAlgorithm : TRandomAlgorithm = raMersenneTwister) : TDoubleMatrix; overload;
  end;

implementation

uses MathUtilFunc, SysUtils, Math, Classes, MatrixASMStubSwitch,
     Dist, BlockSizeSetup, MtxThreadPool, ThreadedMatrix,
     ThreadedMatrixOperations;

// ###########################################
// #### Helper Classes for Burnes Hut trees
// ###########################################

// basically the implementation from https://stevehanov.ca/blog/?id=130
// I think this one was used in the original code

// ###########################################
// #### Data point but without the dimension - we don't want to store that dimension
// value extrax for each point.
type
  TDataPoint = record
  private
    fInd : integer;
    fX : PConstDoubleArr;

    function GetX(index: integer): double;
  public
    property Ind : integer read fInd;
    property X[ index : integer ] : double read GetX;

    procedure Init; overload;
    procedure Init( ref : TDataPoint ); overload;
    procedure Init( (*d, *) ind : integer; x : PDouble ); overload;

    procedure TakeOver( ref : TDataPoint );
  end;
  PDataPoint = ^TDataPoint;
  TDataPointArr = Array of TDataPoint;
  PDataPointArr = Array of PDataPoint;

  TSearchPtEntry = record
    pt : PDataPoint;
    dist : double;
  end;
  TSearchPtEntryArr = Array of TSearchPtEntry;

  // ###########################################
  // #### Vantage point tree
  TVPTreeDistFunc = function ( idx11, idx2 : integer ) : double of Object;
  TVPTree = class(TObject)
  private
    type
      PVPTreeNode = ^TVPTreeNode;
      TVPTreeNode = record
        Index : integer;
        Threshold : double;
        Left, Right : PVPTreeNode;
      end;

      TVPTreeNodeArr = Array of TVPTreeNode;

      THeapItem = record
        dist : double;
        index : integer;
      end;
      PHeapItem = ^THeapItem;
      THeapItemArr = Array of THeapItem;
      PHeapItemArr = Array of PHeapItem;
  private
    fRnd : TRandomGenerator;
    fNodes : TVPTreeNodeArr;
    fNumNodes : integer;

    fItems : TDataPointArr;
    fRoot : PVPTreeNode;

    fDists : THeapItemArr;
    fPDists : PHeapItemArr;   // points to the fDists records
    fHlp : TDataPointArr;
    fTau : double;

    fD : integer;
    //fSL : TStringList;
    fDistFunc : TVPTreeDistFunc;

    function DistanceEuclid( idx1, idx2 : integer ) : double;
    function DistanceAbs( idx1, idx2 : integer ) : double;
    function BuildFromPoints( lower, upper : integer ) : PVPTreeNode;
    procedure InternalSearch( node : PVPTreeNode; idx : integer; k : integer;
                              heap : TPtrHeap; heapData : THeapItemArr);
  public
    procedure BuildTree(items : TDataPointArr);
    procedure Search( idx : integer; k : integer; var res : TSearchPtEntryArr);

    constructor Create(D : integer; distFunc : TTSNEDistFunc; randAlg : TRandomAlgorithm = raChaCha; seed : integer = 0);
  end;

  // ###########################################
  // #### SP tree data structures
  // the record actually does not make a copy but points to given data
  // structures. Data stems from the parent tree element...
  TSPTreeCell = record
    Dimension : integer;
    Corner : PConstDoubleArr;
    Width : PConstDoubleArr;

    procedure Init( inpDim : integer; inpCorner : PConstDoubleArr = nil; inpWidth : PConstDoubleArr = nil);
    function GetCorner( d : integer ) : double; inline;
    function GetWidth( d : integer ) : double; inline;
    procedure SetCorner( d : integer; value : double );
    procedure SetWidth( d : integer; value : double );

    function Maxwidth : double; inline;

    function ContainsPoint( pt : PConstDoubleArr ) : boolean;
  end;
  PSPTreeCell = ^TSPTreeCell;
  TSPTreeCellArr = Array of TSPTreeCell;


  // ###########################################
  // #### segment-page clustering tree
  // parallel part consists only of one entry...
  TSPTree = class;
  TSPTree = class(TObject)
  private
    // Fixed constants
    const QT_NODE_CAPACITY : LongInt = 1;

  private
    fData : PConstDoubleArr;

    fN : integer;
    fD : integer;

    fIsLeaf : boolean;
    fSize : NativeInt;
    fCumSize : NativeInt;

    fWidth : TDoubleDynArray;
    fCorner : TDoubleDynArray;

    fParent : TSPTree;

    // Axis-aligned bounding box stored as a center with half-dimensions to represent the boundaries of this quad tree
    fBoundary : TSPTreeCell;
    fCenterOfMass : TDoubleDynArray;
    fBuff : TDoubleDynArray;

    fIndex : Array[0..0 {QT_NODE_CAPACITY - 1}] of Integer;

    // data provided for children
    fChildren : Array of TSPTree;
    fNumChildren : integer;

    fChildrenBorders : TDoubleDynArray;
    fChildrenWidths : TDoubleDynArray;

    procedure Fill( N : integer );
    procedure ClearChildren;
    procedure Subdivide;
    procedure Init(parent : PSPTreeCell; inp_data, inp_corner, inp_width : PConstDoubleArr );
  public
    procedure ComputeEdgeForces( rows, cols : TIntegerDynArray; p : TDoubleDynArray; posF : TDoubleDynArray);
    // Compute non-edge forces using Barnes-Hut algorithm
    procedure ComputeNonEdgeForces(idx : Nativeint; Theta : double; negF : PConstDoubleArr; var sum_Q : double);

    function Insert( idx : integer ) : boolean;

    constructor Create( d : integer; inpData : PConstDoubleArr; N : integer ); overload;
    constructor Create( parent : TSPTree; d : integer; inpData : PConstDoubleArr; corner : PConstDoubleArr; widths : PConstDoubleArr); overload;
    destructor Destroy; override;
  end;

// ###########################################
// #### VP Tree
// ###########################################

{$REGION 'VP Tree'}
{ TDataPoint }

procedure TDataPoint.Init(ref: TDataPoint);
begin
     fInd := ref.fInd;
//     fD := ref.fD;
     fX := ref.fX;
end;

procedure TDataPoint.Init;
begin
     fInd := 0;
  //   fD := 0;
     fX := nil;
end;

procedure TDataPoint.Init((*d,*) ind: integer; x: PDouble);
begin
  //   fD := d;
     fInd := ind;
     fX := PConstDoubleArr( x );
end;

function TDataPoint.GetX(index: integer): double;
begin
     //assert(index < fD, 'Index out of bounds');
     Result := fX^[index];
end;

procedure TDataPoint.TakeOver(ref: TDataPoint);
begin
     //fD := ref.fD;
     fX := ref.fX;
     fInd := ref.fInd;

     //ref.fD := 0;
     ref.fX := nil;
     ref.fInd := 0;
end;

// ###########################################
// #### VP Tree
// ###########################################

{ TVPTree }

function TVPTree.BuildFromPoints(lower, upper: integer): PVPTreeNode;
var i : Integer;
    tmp : TDataPoint;
    med : integer;
    numElem : integer;
begin
     // indicates we are done here
     if lower > upper then
        exit(nil);

     if fNumNodes + 1 >= Length(fNodes) then
        SetLength(fNodes, Max(fNumNodes + 4096,  upper - lower + 1) );

     Result := @fNodes[fNumNodes];
     inc(fNumNodes);

     Result^.index := lower;

     if upper > lower then
     begin
          // Choose an arbitrary point and move it to the start
          i := fRnd.RandInt( (upper - lower + 1) ) + lower;
          //i := lower + (upper - lower) div 2;
          //i := lower + 1;

          tmp := fItems[i];
          fItems[i] := fItems[lower];
          fItems[lower] := tmp;

          // number of elements to sort from (from lower + 1 to upper)
          numElem := upper - lower;

          // Partition around the median distance
          if Length(fDists) < (upper - lower + 1) then
          begin
               SetLength(fDists, upper - lower + 1);
               SetLength(fHlp, upper - lower + 1);
               SetLength(fPDists, upper - lower + 1);
          end;
          med := numElem div 2;

          // make a copy -> put back later on, we sort on indices :)
          Move( fItems[lower + 1], fHlp[0], numElem*sizeof(fHlp[0]));
          for i := 0 to numElem - 1 do
          begin
               fDists[i].dist := fDistFunc(lower + i + 1, lower);
               fDists[i].index := i;
               fPDists[i] := @fDists[i];
          end;
          Result^.Threshold := KthLargestP( @fPDists[0], numElem, med);

          // write back the partitioned data from the helper array using the partioned
          // indices from the KthlargestP function
          for i := 0 to numElem - 1 do
              fItems[lower + 1 + i] := fHlp[ fPDists[i]^.index ];

          // ###########################################
          // #### Recursion to build left and right tree
          Result^.Left := BuildFromPoints(lower + 1, lower + med );   // original code goes up until med but makes that sense??
          Result^.Right := BuildFromPoints( lower + 1 + med, upper );
     end;
end;

procedure TVPTree.BuildTree(items: TDataPointArr);
begin
     fNodes := nil;
     fNumNodes := 0;
     fItems := items;

     fRoot := BuildFromPoints( 0, Length(fItems) - 1);
end;

constructor TVPTree.Create(D : integer; distFunc : TTSNEDistFunc; randAlg : TRandomAlgorithm; seed : integer);
begin
     fRnd := TRandomGenerator.Create(randAlg);
     fRnd.Init( seed );
     fD := D;

     case distFunc of
       dfEuclid: fDistFunc := DistanceEuclid;
       dfNormEuclid: raise ENotSupportedException.Create('Normlized Euclidian distance not supported in approximate TSNE');
       dfAbs: fDistFunc := DistanceAbs;
       dfMahalanobis: raise ENotSupportedException.Create('Mahalonobis distance not supported in approximate TSNE');
     else
         fDistFunc := DistanceEuclid;
     end;

     inherited Create;
end;

function TVPTree.DistanceAbs(idx1, idx2: integer): double;
var i : integer;
begin
     Result := 0;

     for i := 0 to fD - 1 do// fItems[idx1].Dimensionality - 1 do
         Result := Result + Abs( PConstDoubleArr(fItems[idx1].fX)^[i] - PConstDoubleArr(fItems[idx2].fX)^[i] );
end;

function TVPTree.DistanceEuclid(idx1, idx2: integer): double;
var i : integer;
begin
     Result := 0;

     for i := 0 to fD - 1 do// fItems[idx1].Dimensionality - 1 do
         Result := Result + sqr( PConstDoubleArr(fItems[idx1].fX)^[i] - PConstDoubleArr(fItems[idx2].fX)^[i] );

     Result := Sqrt(Result);
end;

procedure TVPTree.InternalSearch(node: PVPTreeNode; idx : integer; k: integer; heap: TPtrHeap;
 heapData : THeapItemArr);
var dist : double;
    elem : PHeapItem;
begin
     if node = nil then
        exit;

     // Compute distance between target and current node
     dist := fDistFunc(node^.Index, idx);

     // check if it's within the radius
     if dist < fTau then
     begin
          // remove element if we have already k elements
          if heap.Count = k
          then
              elem := heap.Pop
          else
              elem := @heapData[heap.Count];
          elem^.index := node^.Index;
          elem^.dist := dist;
          heap.Add(elem);

          if heap.Count = k then
             fTau := PHeapItem( heap.Top )^.dist;
     end;
                                   
     // Return if we arrived at a leaf
     if (node^.Left = nil) and (node^.Right = nil) then
        exit;

     // check if target is within the radius

     // does it really make a difference if we search right first?
     if dist < node^.Threshold then
     begin
          // search left node first
          if dist - fTau <= node^.Threshold then
             InternalSearch(node^.Left, idx, k, heap, heapData);

          if dist + fTau >= node^.Threshold then
             InternalSearch(node^.Right, idx, k, heap, heapData);
     end
     else
     begin
          // search right node first
          if dist + fTau >= node^.Threshold then
             InternalSearch(node^.Right, idx, k, heap, heapData);

          if dist - fTau <= node^.Threshold then
             InternalSearch(node^.Left, idx, k, heap, heapData);
     end;
end;

function DoubleDistCmp( item1, item2 : Pointer ) : integer;
begin
     Result := CompareValue( TVPTree.PHeapItem(item1)^.dist, TVPTree.PHeapItem(item2)^.dist );
     //Result := CompareValue( TVPTree.PHeapItem(item2)^.dist, TVPTree.PHeapItem(item1)^.dist );
end;

procedure TVPTree.Search(idx : integer; k : integer; var res : TSearchPtEntryArr);
var heapData : THeapItemArr;
    heap : TPtrHeap;
    i : Integer;
    pItem : PHeapItem;
begin
     // the first element is the one with the shortes distance (0)
     // we have never more than k elements
     SetLength(heapData, k);
     fTau := MaxDouble;

     heap := TPtrHeap.Create( DoubleDistCmp );
     //try
        heap.Capacity := k;

        // perform search... and reverse
        InternalSearch(fRoot, idx, k, heap, heapData);
        SetLength(res, heap.Count);
        for i := Length(res) - 1 downto 0 do
        begin
             pItem := heap.Pop;

             res[i].pt := @fItems[ pItem^.index ];
             res[i].dist := pItem^.dist;
        end;
     //finally
            heap.Free;
     //end;
end;

{$ENDREGION}

// ###########################################
// #### SPTREE
// ###########################################

{$REGION SPTREE}


{ TSPTreeCell }

function TSPTreeCell.ContainsPoint(pt: PConstDoubleArr): boolean;
var i: Integer;
begin
     Result := True;

     for i := 0 to Dimension - 1 do
     begin
          if (pt^[i] < Corner^[i] - width^[i]) or (pt^[i] > Corner^[i] + width^[i]) then
             exit(False);

          //if (Corner^[i] - width^[i] > pt^[i] then
//             exit(False);
//          if Corner^[i] + width^[i] < pt^[i] then
//             exit(False);
     end;
end;

function TSPTreeCell.GetCorner(d: integer): double;
begin
     assert( (d >= 0) and (d < Dimension), 'Index out of bounds');
     Result := Corner^[d];
end;

function TSPTreeCell.GetWidth(d: integer): double;
begin
     assert( (d >= 0) and (d < Dimension), 'Index out of bounds');
     Result := Width^[d];
end;

procedure TSPTreeCell.Init(inpDim: integer; inpCorner,
  inpWidth: PConstDoubleArr);
begin
     Dimension := inpDim;
     Corner := inpCorner;
     Width := inpWidth;
end;

function TSPTreeCell.Maxwidth: double;
begin
     Result := MatrixMax( PDouble(width), Dimension, 1, Dimension*sizeof(double));
end;

procedure TSPTreeCell.SetCorner(d: integer; value: double);
begin
     assert( (d >= 0) and (d < Dimension), 'Index out of bounds');
     Corner^[d] := value;
end;

procedure TSPTreeCell.SetWidth(d: integer; value: double);
begin
     assert( (d >= 0) and (d < Dimension), 'Index out of bounds');
     Width^[d] := value;
end;

{$ENDREGION}

// ###########################################
// #### TSPTree
// ###########################################

{$REGION 'TSPTree'}

{ TSPTree }

procedure TSPTree.ClearChildren;
var i : Integer;
begin
     for i := 0 to Length(fChildren) - 1 do
         FreeAndNil(fChildren[i]);
end;

procedure TSPTree.ComputeEdgeForces(rows, cols: TIntegerDynArray;
  p, posF: TDoubleDynArray);
var idx1, idx2 : integer;
    D : double;
    n, i : integer;
    dd : integer;
begin
     idx1 := 0;

     for n := 0 to fN - 1 do
     begin
          for i := rows[n] to rows[n + 1] - 1 do
          begin
               D := 1;

               idx2 := cols[i]*fD;
               for dd := 0 to fD - 1 do
                   fBuff[dd] := fData[idx1 + dd] - fData[idx2 + dd];
               for dd := 0 to fD - 1 do
                   D := D + sqr(fBuff[dd]);

               D := p[i]/D;

               // sum positive force
               for dd := 0 to fD - 1 do
                   posF[idx1 + dd] := posF[idx1 + dd] + D*fBuff[dd];
          end;

          inc(idx1, fD);
     end;
end;

procedure TSPTree.ComputeNonEdgeForces(idx: NativeInt; Theta: double;
  negF: PConstDoubleArr; var sum_Q: double);
var D : double;
    ind : integer;
    maxWidth : double;
    i : integer;
    mult : double;
    lineW : NativeInt;
begin
     if (fCumSize = 0) or (fIsLeaf and (fSize = 1) and (fIndex[0] = idx)) then
        exit;

     // compute distance between point and center of mass
     //D := 0;
     ind := idx*fD;

     lineW := fD*sizeof(double);
     MatrixSub(@fBuff[0], lineW, @fData[ind], @fCenterOfMass[0], fD, 1, LineW, LineW );
     //for i := 0 to fD - 1 do
//         fBuff[i] := fData[ind + i] - fCenterOfMass[i];

     D := MatrixElementwiseNorm2( @fBuff[0], lineW, fD, 1, False );
     //for i := 0 to fD - 1 do
//         D := D + sqr(fBuff[i]);

     // check if we can use this node as a "summary"
     maxWidth := fBoundary.Maxwidth;

//     if fIsLeaf or (maxWidth/sqrt(D) < theta) then
     if fIsLeaf or (sqr(maxWidth) < D*sqr(theta)) then
     begin
          // compute and add t-SNE force between pont and current node
          D := 1/(1 + D);
          mult := fCumSize*D;
          sum_Q := sum_Q + mult;
          mult := mult*D;

          for i := 0 to fD - 1 do
              negF^[i] := negF^[i] + mult*fBuff[i];
     end
     else
     begin
          // recursively apply Barnes-Hut to children
          for i := 0 to fNumChildren - 1 do
              fChildren[i].ComputeNonEdgeForces(idx, theta, negF, sum_Q);
     end;
end;

constructor TSPTree.Create(parent: TSPTree; d: integer; inpData, corner,
  widths: PConstDoubleArr);
begin
     fD := d;
     Init( @parent, inpData, corner, widths);
end;

constructor TSPTree.Create(d: integer; inpData: PConstDoubleArr; N: integer);
var minY, maxY : TDoubleDynArray;
    i, j, k: Integer;
    val : double;
begin
     fN := N;
     fD := d;

     fParent := nil;

     // Compute mean, width, and heigh of current map (boundaries of SPTree)
     SetLength(fWidth, fD);
     SetLength(fCorner, fD);
     SetLength(minY, fD);
     SetLength(maxY, fD);

     for i := 0 to Length(minY) - 1 do
     begin
          minY[i] := MaxDouble;
          maxY[i] := -MaxDouble;
     end;

     for j := 0 to N - 1 do
     begin
          for k := 0 to fD - 1 do
          begin
               val := inpData^[j*fD + k];
               fCorner[k] := fCorner[k] + val;
               if minY[k] > val then
                  minY[k] := val;
               if maxy[k] < val then
                  maxY[k] := val;

          end;
     end;

     for k := 0 to fD - 1 do
     begin
          fCorner[k] := fCorner[k]/N;
          fWidth[k] := maxY[k] - minY[k];
          fWidth[k] := fWidth[k] + eps(fWidth[k]);
     end;

     // ###########################################
     // #### now build the tree
     Init(nil, inpData, @fCorner[0], @fWidth[0]);
     Fill( fN );

     inherited Create;
end;

destructor TSPTree.Destroy;
begin
     ClearChildren;

     inherited;
end;

procedure TSPTree.Fill(N: integer);
var i: Integer;
begin
     for i := 0 to N - 1 do
         Insert( i );
end;

procedure TSPTree.Init(parent: PSPTreeCell; inp_data, inp_corner,
  inp_width: PConstDoubleArr);
begin
     fNumChildren := 1 shl fD;

     fData := inp_data;
     fIsLeaf := True;
     fSize := 0;
     fCumSize := 0;

     fBoundary.Init( fD, inp_corner, inp_width );

     ClearChildren;
     SetLength(fChildren, fNumChildren);
     SetLength(fCenterOfMass, fD);
     MatrixInit(fCenterOfMass, 0);

     SetLength(fBuff, fD);
end;


function TSPTree.Insert(idx : integer): boolean;
var pPoint : PConstDoubleArr;
    mult1, mult2 : double;
    i, n : integer;
    dupl : boolean;
begin
     pPoint := @fData^[idx*fD];

     if not fBoundary.ContainsPoint( pPoint ) then
        exit(false);

     // Online update of cumulative size and center-of-mass
     inc(fCumSize);
     mult1 := (fCumSize - 1)/fCumSize;
     mult2 := 1/fCumSize;

     for i := 0 to fD - 1 do
         fCenterOfMass[i] := fCenterOfMass[i]*mult1;
     for i := 0 to fD - 1 do
         fCenterOfMass[i] := fCenterOfMass[i] + mult2*pPoint^[i];

     // If there is space in this quad tree and it is a leaf, add the object here
     if fIsLeaf and (fSize < QT_NODE_CAPACITY) then
     begin
          fIndex[fSize] := idx;
          inc(fSize);
          exit(True);
     end;

     // don't add duplicates for now (this is not very nice)
     for n := 0 to fSize - 1 do
     begin
          dupl := True;
          for i := 0 to fD - 1 do
          begin
               if pPoint^[i] <> fData^[ fIndex[n] * fD + i ] then
               begin
                    dupl := False;
                    break;
               end;
          end;

          if dupl then
             exit(true);
     end;

     // subdivide the current cell
     if fIsLeaf then
        subdivide;

     // find the place to insert
     for i := 0 to fNumChildren - 1 do
         if fChildren[i].Insert( idx ) then
            exit(True);

     // could not insert point -> fail here (this should not happen)
     Result := False;
end;

procedure TSPTree.Subdivide;
var i : Integer;
    d : Integer;
    aDiv : integer;
    pWidths : PConstDoubleArr;
    pCorners : PConstDoubleArr;
    j : Integer;
begin
     SetLength( fChildrenBorders, fD*fNumChildren );
     SetLength( fChildrenWidths, fD*fNumChildren );

     pWidths := @fChildrenWidths[0];
     pCorners := @fChildrenBorders[0];
     for i := 0 to fNumChildren - 1 do
     begin
          aDiv := 1;
          for d := 0 to fD - 1 do
          begin
               pWidths^[d] := 0.5*fBoundary.GetWidth(d);

               if ((i div aDiv) mod 2) = 1
               then
                   pCorners^[d] := fBoundary.GetCorner(d) - 0.5*fBoundary.GetWidth(d)
               else
                   pCorners^[d] := fBoundary.GetCorner(d) + 0.5*fBoundary.GetWidth(d);

               aDiv := aDiv*2;
          end;

          fChildren[i] := TSPTree.Create(self, fD, fData, pCorners, pWidths);

          inc(PDouble(pCorners), fD);
          inc(PDouble(pWidths), fD);
     end;

     // move existing points to correct children
     for i := 0 to fSize - 1 do
     begin
          j := 0;
          while (j < fNumChildren) and not fChildren[j].Insert(findex[i]) do
                inc(j);

          fIndex[i] := -1;
     end;

     // empty parent node
     fSize := 0;
     fIsLeaf := False;
end;

{$ENDREGION}

// ###########################################
// #### TSNE
// ###########################################

{ TtSNE }

{$Region 'Create/Free/Public interface'}

constructor TtSNE.Create( initDims : integer = 30; perplexity : integer = 30;
   numIter : integer = 1000; distFunc : TTSNEDistFunc = dfOrig; randSeed : integer = 0;
   randAlgorithm : TRandomAlgorithm = raMersenneTwister);
begin
     inherited Create;

     fDistFunc := distFunc;
     fNumIter := numIter;
     fTol := 1e-5;
     fInitDims := initDims;
     fPerplexity := perplexity;
     fSeed := RandSeed;
     fRandAlgorithm := randAlgorithm;
end;

class function TtSNE.SymTSNE(X : TDoubleMatrix; numDims : integer;
  initDims : integer; perplexity : integer;
  theta : double = 0.5; numIter : integer = 1000;
  distFunc : TTSNEDistFunc = dfOrig; randSeed : integer = 0;
  randAlgorithm : TRandomAlgorithm = raMersenneTwister) : TDoubleMatrix;
begin
     with TtSNE.Create(initDims, perplexity, numIter, distFunc, randSeed, randAlgorithm) do
     try
        Result := SymTSNE( X, theta, numDims );
     finally
            Free;
     end;
end;

function TtSNE.NormalizeInput(X: TDoubleMatrix): IMatrix;
var maxVal : double;
    meanVec : IMatrix;
begin
     meanVec := X.Mean(False);
     Result := X.subVec(meanVec, True);
     maxVal := MatrixAbsMax( Result.StartElement, Result.Width, Result.Height, Result.LineWidth);

     if maxVal = 0 then
        raise Exception.Create('Error normalizing dataset - empty matrix');

     Result.ScaleInPlace(1/maxVal);
end;

function TtSNE.SymTSNEPreprocessed(X: TDoubleMatrix;
  theta : double; numDims: integer): TDoubleMatrix;
begin
     fTheta := theta;

     if theta = 0
     then
         Result := InternalTSNE(X, numDims)
     else
         Result := InternalTSNEBarnesHut(X, numDims);
end;

function TtSNE.SymTSNE(X: TDoubleMatrix; theta : double = 0.5; numDims : integer = 2): TDoubleMatrix;
var xs : IMatrix;
    //tmp : double;
    //itmp : integer;
    //buf : TDoubleDynArray;
begin
     fTheta := theta;
     xs := ApplyPCA(X);
     xs := NormalizeInput(xs.GetObjRef);

     // output to compare with the original implementation
     //buf := xs.SubMatrix;

     //with TFileStream.Create('D:\xs.bin', fmCreate or fmOpenWrite) do
//     try
//        itmp := xs.Height; WriteBuffer(iTmp, sizeof(integer));
//        itmp := xs.Width; WriteBuffer(iTmp, sizeof(integer));
//        WriteBuffer(fTheta, sizeof(double));
//        tmp := fPerplexity; WriteBuffer(tmp, sizeof(double));
//        iTmp := xs.Width; WriteBuffer(iTmp, sizeof(integer));
//        iTmp := 2000; WriteBuffer(iTmp, sizeof(integer));
//
//        WriteBuffer(buf[0], Length(buf)*sizeof(double));
//     finally
//            Free;
//     end;

     if theta = 0
     then
         Result := InternalTSNE( xs.GetObjRef, numDims )
     else
         Result := InternalTSNEBarnesHut(xs.GetObjRef, numDims);
end;

function TtSNE.InitRand(w, h: Integer): IMatrix;
begin
     Result := MatrixClass.Create(w, h, True); // prepare for better block mult
     Result.InitRandom(fRandAlgorithm, fSeed);
end;

function TtSNE.InternalTSNE( xs : TDoubleMatrix; numDims : integer) : TDoubleMatrix;
var D, P : IMatrix;
    yData : IMatrix;
begin
     D := PairDist(xs);

     // Joint probabilities
     P := D2P(D);
     D := nil;

     // Run t-sne
     yData := tsne_p(P, numDims);

     // handle the reference counting
     Result := MatrixClass.Create;
     Result.TakeOver(yData.GetObjRef);

     fQ := nil;
     fy_grads := nil;
     fyincs := nil;
end;

function TtSNE.ApplyPCA(X: IMatrix): IMatrix;
var aPca : TMatrixPCA;
begin
     // apply pca and check if we can shrink it to the designated number of eigenvectors
     // note the pca routine assumes one example in one colum - the original tsne implementation
     // assumes one example in one row -> so transpose in beforehand
     if Assigned(fInitPCA)
     then
         aPca := fInitPCA()
     else
         aPca := DefInitPCA();
     try
        Result := X.Transpose;
        aPca.PCA(Result.GetObjRef, fInitDims, False);

        Result.SubVecInPlace(aPca.Mean, False);

        Result := aPca.EigVecsT.Mult( Result );
        Result.TransposeInPlace;
     finally
            aPca.Free;
     end;
end;

{$ENDREGION}

// ###########################################
// #### Exact version (based on matlab code)
// ###########################################

{$REGION 'Exact tsne'}

function TtSNE.PairwiseDist(X: TDoubleMatrix): IMatrix;
var sum_x : IMatrix;
begin
     sum_x := X.ElementWiseMult(X);
     sum_x.SumInPlace(True);

     Result := X.MultT2(X);
     Result.ScaleInPlace(-2);
     Result.AddVecInPlace(sum_x, True);
     Result.AddVecInPlace(sum_x, False);
end;

function TtSNE.PairDist(Xs: TDoubleMatrix): IMatrix;
var pDist : IMatrix;
    w : integer;
    idx : integer;
    x, y : integer;
begin
     // ###########################################
     // #### Several distance measurements
     case fDistFunc of
       dfEuclid: pDist := TDistance.EuclidPairDist(Xs);
       dfNormEuclid: pDist := TDistance.NormEuclidPairDist(Xs);
       dfAbs: pDist := TDistance.AbsPairDist(Xs);
       dfMahalanobis: pDist := TDistance.MahalanobisPairDist(Xs);
     else
         // original implementation -> is already square form!
         Result := PairwiseDist(Xs);
         exit;
     end;

     // ########################################
     // #### Create squareform of the pairwise distance vector
     //w := Floor( (1 + sqrt (1 + 8 * pDist.VecLen)) / 2 );
     w := Xs.Height;

     Result := MatrixClass.Create( w, w );

     x := 1;
     y := 0;
     for idx := 0 to pDist.VecLen - 1 do
     begin
          Result[x, y] := pDist.Vec[idx];
          Result[y, x] := pDist.Vec[idx];

          inc(x);
          if x >= w then
          begin
               inc(y);
               x := y + 1;
          end;
     end;

     // Result := Res.*Res
     Result.ElementWiseMultInPlace(Result);
end;

function TtSNE.D2P(D: IMatrix): IMatrix;
var i : NativeInt;
    numPts : NativeInt;
    betaMin, betaMax : double;
    beta : TDoubleDynArray;
    subVec : IMatrix;
    thisP : IMatrix;
    tries : integer;
    H, logU : double;
    hDiff : double;
    P : IMatrix;
procedure FillSubVec( subVec, D : IMatrix; i : integer);
var idx : integer;
    ii : integer;
begin
     idx := 0;
     for ii := 0 to D.Width - 1 do
     begin
          if ii <> i then
          begin
               subVec.Vec[idx] := D[ii, i];
               inc(idx);
          end;
     end;
end;
procedure FillP(P, subVec : IMatrix; i : integer);
var idx : integer;
    ii : integer;
begin
     idx := 0;
     for ii := 0 to D.Width - 1 do
     begin
          if ii <> i then
          begin
               P[ii, i] := subVec.Vec[idx];
               inc(idx);
          end
          else
              P[i, i] := 1;
     end;
end;

var minVal : double;
    x, y : integer;
const cMinProb = 1e-30;
begin
     numPts := d.Height;
     SetLength(beta, numPts);
     for i := 0 to Length(beta) - 1 do
         beta[i] := 1;
     P := MatrixClass.Create(numPts, numPts, 0);
     logU := Ln(fPerplexity);

     // go through all datapoints
     subVec := MatrixClass.Create( D.Width - 1, 1 );

     for i := 0 to numPts - 1 do
     begin
          // Set minimum and maximum values for precision
          betaMin := -Infinity;
          betaMax := Infinity;

          // Compute the Gaussian kernel and entropy for the current precision

          // get vector
          FillSubVec(subVec, D, i );

          HBeta( subVec, beta[i], H, thisP );

          // Evaluate whether the perplexity is within tolerance
          Hdiff := H - logU;
          tries := 0;
          while (abs(Hdiff) > fTol) and (tries < 50) do
          begin
               // If not, increase or decrease precision
               if Hdiff > 0 then
               begin
                    betaMin := beta[i];
                    if IsInfinite(betaMax)
                    then
                        beta[i] := beta[i]*2
                    else
                        beta[i] := (beta[i] + betamax)/2;
               end
               else
               begin
                    betaMax := beta[i];
                    if isInfinite(betaMin)
                    then
                        beta[i] := beta[i]/2
                    else
                        beta[i] := (beta[i] + betamin)/2;
               end;

               // Recompute the values - returns false if thisP would be (close to) zeros
               if not HBeta( subVec, beta[i], H, thisP ) then
                  break;

               // Evaluate whether the perplexity is within tolerance
               Hdiff := H - logU;
               inc(tries);
          end;

          // Set the final row of P
          FillP(P, thisP, i);
     end;

     // this routine has the diagonal elements at 1 and in case there are numerical instabilities
     // these values are also 1
     // -> set the diagonal to 0, the unstable elements to 1/100 of the minimum, at least a very low value
     minVal := Min(cMinProb, P.Min)/100;

     // diagonal - probability to self set to 0
     for i := 0 to P.Width - 1 do
         P[i, i] := 0;

     // set elements with low numerical stability to the minimum
     for y := 0 to P.Height - 1 do
         for x := 0 to P.Width - 1 do
             if P[x, y] = 1 then
                P[x, y] := minVal;

     Result := P;
end;

// Function that computes the Gaussian kernel values given a vector of
// squared Euclidean distances, and the precision of the Gaussian kernel.
// The function also computes the perplexity of the distribution.

//function [H, P] = Hbeta(D, beta)
//    P = exp(-D * beta);
//    sumP = sum(P);
//    H = log(sumP) + beta * sum(D .* P) / sumP;
//    % why not: H = exp(-sum(P(P > 1e-5) .* log(P(P > 1e-5)))); ???
//    P = P / sumP;
//end

function TtSNE.HBeta(D: IMatrix; beta: double; var H : double; var P: IMatrix) : boolean;
var sumP : double;
    muldp : IMatrix;
begin
     fBeta := beta;
     P := D.ElementwiseFunc( HBetaExp );
     sumP := (P.Sum(True) as IMatrix).Vec[0] + MinSingle;  // barnes hut from mr maaten does this (but with mindouble)
     muldp := D.ElementWiseMult(P);

     if sumP >= 1e-13 then
     begin
          H := ln(sumP) + beta*(muldp.Sum(True) as IMatrix).Vec[0]/sumP;

          P.ScaleInPlace(1/sumP);
          Result := True;
     end
     else
     begin
          // beta seems to be high. So we define a probability that we can search for later
          P.SetValue(1);
          Result := False;
     end;
end;

procedure TtSNE.HBetaExp(var value: double);
begin
     value := exp( -value*fBeta );
end;

procedure EntropyFunc(var value : double);
begin
     value := value*ln(value);
end;

procedure MaxFunc(var value : double);
var cmpVal : double;
begin
	    // branchless max with a very small error of 1e-308
     cmpVal := Integer(value >= cMinDouble);
     value :=  cmpVal*value + cMinDouble;
end;

function TtSNE.tsne_p(P: IMatrix; numDims : integer): IMatrix;
var n : integer;                        // number of instances
    momentum : double;                  // initial momentum
    epsilon : double;
    scaleFact : double;
    i : integer;
    PT : IMatrix;
    constEntr : double;
    yData : IMatrix;
    gains : IMatrix;
    iter: Integer;
    sum_ydata : IMatrix;
    tmp : IMatrix;
    num : IMatrix;
    L, Q : IMatrix;
    doCancel : boolean;
    aSum : double;
    aMean : IMatrix;
    yMul : IMatrix;
    tmp2 : IMatrix;
    line1, line2 : PConstDoubleArr;
    y : integer;
    origNumCPUCores : integer;
    threaded : boolean;

const final_momentum : double = 0.8;    // value to which momentum is changed
      mom_switch_iter : integer = 250;  // iteration at which momentum is changed
      stop_lying_iter : integer = 100;   // iteration at which lying about P-values is stopped
      min_gain : double = 0.01;         // minimum gain for delta-bar-delta

begin
     n := P.Height;
     momentum := 0.5;
     epsilon := 500;
     fMinGain := min_gain;

     // ################################################
     // #### make sure P is what we expect it to be:
     // clear diagonal
     for i := 0 to P.Width - 1 do
         p[i, i] := 0;
     PT := P.Transpose;
     P.AddInPlace( PT );
     P.ScaleInPlace(0.5);

     PT := P.Sum(True);
     PT.SumInPlace(False, True);
     scaleFact := PT[0, 0];
     P.ScaleInPlace(1/scaleFact);
     MatrixMax(P.StartElement, P.LineWidth, P.Width, P.Height, cMinDouble);

     PT := P.AsVector(True);
     PT.ElementwiseFuncInPlace( EntropyFunc );
     PT.SumInPlace(True, True);
     constEntr := PT[0,0];
     P.ScaleInPlace(4);  // lie about the p-vals to find better local minima

     yData := InitRand(numdims, n);
     yData.ScaleInPlace(0.0001);

     fyincs := MatrixClass.Create(yData.Width, yData.Height );
     gains := MatrixClass.Create(yData.Width, yData.Height, 1);

     L := MatrixClass.Create(n, n);
     Q := MatrixClass.Create(n, n);
     fy_grads := MatrixClass.Create(numDims, n);

     sum_ydata := MatrixClass.Create(yData.Width, yData.Height);

     aMean := MatrixClass.Create(numDims, 1);
     yMul := MatrixClass.Create(n, n);
     num :=  MatrixClass.Create( yMul.Width, yMul.Height, 0);

     tmp := MatrixClass.Create(numDims, n);
     tmp2 := MatrixClass.Create(n, n);

     threaded := MatrixClass = TThreadedMatrix;

     // #################################################
     // #### now iterate
     origNumCPUCores := NumCPUCores;
     numCPUCores := Min(numcpuCores, numDims);

     for iter := 1 to fNumIter do
     begin
          // Compute joint probability that point i and j are neighbors
          sum_ydata.SetWidthHeight(yData.Width, yData.Height);
          // sum_ydata.Assign(yData);
          // sum_ydata.ElementWiseMultInPlace(sum_ydata);
          MatrixElemMult(sum_ydata.StartElement, sum_ydata.LineWidth, ydata.StartElement, ydata.StartElement, ydata.Width, ydata.Height, ydata.LineWidth, ydata.LineWidth);

          //sum_ydata := ydata.ElementWiseMult(yData);
          sum_ydata.SumInPlace(True, True);

          // num = 1 ./ (1 + bsxfun(@plus, sum_ydata, bsxfun(@plus, sum_ydata', -2 * (ydata * ydata')))); % Student-t distribution
          // yMul := yData.MultT2(yData);
          if threaded
          then
              ThrMatrixMultT2Ex(yMul.StartElement, yMul.LineWidth, yData.StartElement, yData.StartElement, yData.Width, yData.Height,
                         yData.Width, yData.Height, yData.LineWidth, yData.LineWidth, BlockMatrixCacheSize, doNone, nil)
          else
              MatrixMultT2Ex(yMul.StartElement, yMul.LineWidth, yData.StartElement, yData.StartElement, yData.Width, yData.Height,
                         yData.Width, yData.Height, yData.LineWidth, yData.LineWidth, BlockMatrixCacheSize, doNone, nil);
          yMul.ScaleAndAddInPlace(1, -2);
          yMul.AddVecInPlace(sum_ydata, True);
          yMul.AddVecInPlace(sum_ydata, False);

          num.SetValue(1);
          num.ElementWiseDivInPlace(yMul);

          // set diagonal to zero
          for i := 0 to num.Width - 1 do
              num[i, i] := 0;

          aSum := num.Sum;
          Q.Assign( num );
          Q.ScaleInPlace( 1/aSum );
          //Q.ElementwiseFuncInPlace({$ifdef FPC}@{$ENDIF}MaxFunc );
          MatrixMax(Q.StartElement, Q.LineWidth, Q.Width, Q.Height, cMinDouble);

          // Compute the gradients (faster implementation)
          MatrixSub( L.StartElement, L.LineWidth, P.StartElement, Q.StartElement, P.Width, P.Height, P.LineWidth, Q.LineWidth);
          //L.Assign(P);
          //L.SubInPlace(Q);
          L.ElementWiseMultInPlace(num);

          // fy_grads := L.Sum(False);
          tmp2.SetValue(0);
          MatrixSum(tmp2.StartElement, tmp2.LineWidth, L.StartElement, L.LineWidth, L.Width, L.Height, False);

          // create diagonal from the given matrix
          line1 := PConstDoubleArr(tmp2.StartElement);
          line2 := line1;
          inc(PByte(line2), tmp2.LineWidth);

          for y := 1 to fy_grads.Height - 1 do
          begin
               line2^[y] := line1^[y];
               line1^[y] := 0;
               inc(PByte(line2), tmp2.LineWidth);
          end;

          //fy_grads.DiagInPlace(True);
          tmp2.SubInPlace(L);
          tmp2.ScaleInPlace(4);

          if threaded
          then
              ThrMatrixMult(fy_grads.StartElement, fy_grads.LineWidth, tmp2.StartElement, ydata.StartElement, tmp2.Width, tmp2.Height,
                            yData.Width, yData.Height, tmp2.LineWidth, ydata.LineWidth)
          else
              MatrixMult(fy_grads.StartElement, fy_grads.LineWidth, tmp2.StartElement, ydata.StartElement, tmp2.Width, tmp2.Height,
                         yData.Width, yData.Height, tmp2.LineWidth, ydata.LineWidth);
          //fy_grads := tmp2.Mult(ydata);

          // Update the solution
          gains.ElementwiseFuncInPlace( UpdateGains );

          fyincs.ScaleInPlace(momentum);

          //tmp.Assign(gains);
          //tmp.ElementWiseMultInPlace(fy_grads);
          MatrixElemMult(tmp.StartElement, tmp.LineWidth, gains.StartElement, fy_grads.StartElement, gains.Width, gains.Height, gains.LineWidth, fy_grads.LineWidth);
          tmp.ScaleInPlace(epsilon);
          fyincs.SubInPlace(tmp);
          yData.AddInplace(fyincs);

          MatrixMean(aMean.StartElement, aMean.LineWidth, yData.StartElement,
                     yData.LineWidth, yData.Width, yData.Height, False);
          yData.SubVecInPlace( aMean, True );

          // Update the momentum if necessary
          if iter = mom_switch_iter then
             momentum := final_momentum;

          if iter = stop_lying_iter then
             P.ScaleInPlace(1/4);

          // progress:
          if Assigned(fProgress) and ( (iter = fNumIter) or (iter mod 10 = 0) ) then
          begin
               fQ := Q;
               PT := P.ElementwiseFunc( EntropyFuncQ );
               PT.SumInPlace(True, True);
               PT.SumInPlace(False, True);

               fCost := constEntr - PT[0,0];

               doCancel := False;
               fProgress(Self, iter, fcost, yData, doCancel);

               if doCancel then
                  break;
          end;
     end;
     numCPUCores := origNumCPUCores;

     // final cost
     fQ := Q;
     PT := P.ElementwiseFunc( EntropyFuncQ );
     PT.SumInPlace(True, True);
     PT.SumInPlace(False, True);

     fCost := constEntr - PT[0,0];


     Result := yData;
end;

procedure TtSNE.UpdateGains(var Value: double; const data: PDouble; LineWidth,
  x, y: integer);
var cmp1, cmp2 : double;
begin
     // branchless version
     cmp1 := Integer(sign( fy_grads[x, y] ) = sign( fyincs[x, y] ));
     cmp2 := Integer(cmp1 = 0);
     value := cmp1*cPhi*value + cmp2*(value + cK);
     cmp2 := Integer(value > fMinGain);
     cmp1 := Integer(cmp2 = 0);
     value := cmp2*value + cmp1*fMinGain;

     //if sign( fy_grads[x, y] ) = sign( fyincs[x, y] )
//     then
//         value := cPhi*value
//     else
//         value := value + cK;
//
//     value := Math.Max(value, fminGain)
end;

procedure TtSNE.EntropyFuncQ(var Value: double; const data: PDouble; LineWidth,
  x, y: integer);
begin
     value := Value*ln(fQ[x, y]);
end;

function TtSNE.DefInitPCA : TMatrixPCA;
begin
     Result := TMatrixPCA.Create( [pcaTransposedEigVec] );
end;

{$ENDREGION}

// ###########################################
// #### Barnes Hut
// ###########################################

{$Region 'Barnes Hut implementation'}

procedure TtSNE.SymSparseMatrix;
var rowCnt : TIntegerDynArray;
    n, m, i: Integer;
    present : boolean;
    numElem : integer;
    symRow : TIntegerDynArray;
    symCol : TIntegerDynArray;
    symP : TDoubleDynArray;
    offset : TIntegerDynArray;
begin
     SetLength(rowCnt, fN);
     if not Assigned(rowCnt) then
        raise Exception.Create('Memory allocation for row count failed');

     for n := 0 to fN - 1 do
     begin
          for i := fRows[n] to fRows[n + 1] - 1 do
          begin
               // check if element fCols[i], n is present
               present := False;
               for m := fRows[fCols[i]] to fRows[fCols[i] + 1] - 1 do
               begin
                    if fCols[m] = n then
                    begin
                         present := True;
                         break;
                    end;
               end;

               inc(rowCnt[n]);
               if not present then
                  inc(rowCnt[fCols[i]]);
          end;
     end;

     numElem := 0;
     for n := 0 to fN - 1 do
         inc(numElem, rowCnt[n]);

     // allocate memory...
     SetLength(symRow, fN + 1);
     SetLength(symCol, numElem);
     SetLength(symP, numElem);

     if symP = nil then
        raise Exception.Create('Failed to allocate memory for sym matrix');

     // construct row indices for sym matrix
     symRow[0] := 0;
     for n := 0 to fN - 1 do
         symRow[n + 1] := symRow[n] + rowCnt[n];

     // fill the result matrix
     SetLength(offset, fN);

     if offset = nil then
        raise Exception.Create('Failed to allocate memory');

     for n := 0 to fN - 1 do
     begin
          for i := fRows[n] to fRows[n + 1] - 1 do
          begin
               // check if element (fcols[i], n) is present
               present := False;

               for m := fRows[ fCols[i] ] to fRows[ fCols[i] + 1 ] - 1 do
               begin
                    if fCols[m] = n then
                    begin
                         present := True;
                         if n <= fCols[i] then
                         begin
                              symCol[symRow[n] + offset[n]] := fCols[i];
                              symCol[symRow[fCols[i]] + offset[fCols[i]]] := n;
                              symP[symRow[n] + offset[n]] := fP[i] + fP[m];
                              symP[symRow[fCols[i]] + offset[fCols[i]]] := fP[i] + fP[m];
                         end;
                    end;
               end;

               // if (fCols[i], n) is not present, there is no addition involved
               if not present then
               begin
                    symCol[symRow[n] + offset[n]] := fCols[i];
                    symCol[symRow[fCols[i]] + offset[fCols[i]]] := n;
                    symP[symRow[n] + offset[n]] := fP[i];
                    symP[symRow[fCols[i]] + offset[fCols[i]]] := fP[i];
               end;

               // update offsets
               if not present or (present and (n <= fCols[i])) then
               begin
                    inc(offset[n]);
                    if fCols[i] <> n then
                       inc(offset[ fCols[i] ]);
               end;
          end;
     end;

     // divide by two
     MatrixScaleAndAdd( @symP[0], numElem*sizeof(double), numElem, 1, 0, 0.5 );

     // return/store sym matrix
     fRows := symRow;
     fCols := symCol;
     fP := symP;
end;


function TtSNE.EvaluateError(pY: PConstDoubleArr; numDims : integer): double;
var tree : TSPTree;
    buff : TDoubleDynArray;
    sumQ : double;
    i1, i2 : integer;
    C, Q : double;
    n, i, ii : integer;
begin
     SetLength(buff, numDims);

     tree := TSPTree.Create(numDims, pY, fN);
     try
        sumQ := 0;
        for n := 0 to fN - 1 do
            tree.ComputeNonEdgeForces(n, fTheta, @buff[0], sumQ);

        // Loop over all edges to compute t-SNE error
        C := 0;

        for n := 0 to fN - 1 do
        begin
             i1 := n*numDims;
             for i := fRows[n] to fRows[n + 1] - 1 do
             begin
                  Q := 0;
                  i2 := fCols[i]*numDims;

                  for ii := 0 to numDims - 1 do
                      buff[ii] := pY^[i1 + ii];
                  for ii := 0 to numDims - 1 do
                      buff[ii] := buff[ii] - pY^[i2 + ii];
                  for ii := 0 to numDims - 1 do
                      Q := Q + sqr(buff[ii]);

                  Q := (1/(1 + Q))/sumQ;
                  C := C + fP[i]*ln((fP[i] + MinSingle)/(Q + MinSingle));
             end;
        end;
     finally
            tree.Free;
     end;

     Result := C;
end;

function TtSNE.InternalTSNEBarnesHut(xs: TDoubleMatrix;
  numDims: integer): TDoubleMatrix;
var sumP : double;
    i : Integer;
    rand : TRandomGenerator;
    dY, uY, gains : TDoubleDynArray;
    iter: Integer;
    momentum : double;
    aMean : TDoubleDynArray;
    yData : IMatrix;
    pY : PConstDoubleArr;
    doCancel : boolean;
const cStopLyingIter = 250;
      cMomSwitchIter = 250;
      cFinalMomentum = 0.8;
      cInitMomentum = 0.5;
      cEta = 200;
      cLieValue = 12;

  procedure UpdateGainsBarnesHut( var gains : TDoubleDynArray; const dy, uY : TDoubleDynArray; N : integer);
  var cmp1, cmp2 : double;
      value : double;
      i : Integer;
  begin
       for i := 0 to fN*numDims - 1 do
       begin
            // branchless version
            cmp1 := Integer( (dy[i] > 0) = (uY[i] > 0) );
            cmp2 := Integer(cmp1 = 0);
            value := gains[i];
            value := cmp1*cPhi*value + cmp2*(value + cK);
            cmp2 := Integer(value > fMinGain);
            cmp1 := Integer(cmp2 = 0);
            gains[i] := cmp2*value + cmp1*fMinGain;
            //if sign( dy[i] ) <> sign( uy[i] )
//               then
//                   gains[i] := gains[i] + 0.2
//               else
//                   gains[i] := Max(0.01, gains[i] * 0.8);
       end;
  end;
begin
     // ###########################################
     // #### Compute input similarities for approximate t-SNE
     if fPerplexity > xs.Height div 3 then
        raise Exception.Create('Perplexity to high may be max ' + IntToStr( xs.Height div 3 ));
     CalcPerplexity(xs, fPerplexity, 3*fPerplexity);
     SymSparseMatrix;

     // allocate memory
     SetLength( dY, fN*numDims );
     SetLength( uY, fN*numDims );
     SetLength( gains, fN*numDims );

     yData := TDoubleMatrix.Create(numDims, fN, True);
     pY := PConstDoubleArr( yData.StartElement );

     if (gains = nil) or (pY = nil) then
        raise Exception.Create('Failed to allocate memory');

     MatrixInit(gains, 1);

     SetLength(fGradPosf, fN*numDims );
     SetLength(fGradNegf, fN*numDims );

     if fGradNegf = nil then
        raise Exception.Create('Failed to allocate memory for gradient');

     sumP := 0;
     for i := 0 to fRows[fN] - 1 do
         sumP := sumP + fP[i];
     sumP := 1/sumP;
     for i := 0 to fRows[fN] - 1 do
         fP[i] := fP[i]*sumP;

     // lie about the p-values
     for i := 0 to fRows[fN] - 1 do
         fP[i] := cLieValue*fP[i];

     rand := TRandomGenerator.Create( fRandAlgorithm );
     try
        rand.Init(fSeed);

        for i := 0 to fN*numDims - 1 do
            pY^[i] := rand.RandGauss*0.0001;
     finally
            rand.Free;
     end;

     momentum := cInitMomentum;
     SetLength(aMean, numDims);

     // ###########################################
     // #### Perform main training loop
     for iter := 0 to fNumIter - 1 do
     begin
          // compute approximate gradient
          computeGradient( pY, numDims, dY );

          // update gains
          UpdateGainsBarnesHut( gains, dY, uY, fN*numDims );

          // perform gradient update (with momentum and gains)
          for i := 0 to fN*numDims - 1 do
              uY[i] := momentum*uY[i] - cEta*dY[i]*gains[i];

          MatrixAddVec(PDouble(pY), fN*NumDims*sizeof(double), @uY[0], sizeof(double), fN*numDims, 1, True);

          // zero mean the result
          MatrixMean( @aMean[0], numDims*sizeof(double), PDouble(pY), numDims*sizeof(double), numDims, fN, False );
          MatrixSubVec(PDouble(pY), numDims*sizeof(double), @aMean[0], sizeof(double), numDims, fN, True);

          // stop lying about the p-Values after a whie and switch momentum
          if iter = cStopLyingIter then
             MatrixScaleAndAdd( @fP[0], fRows[fN]*sizeof(double), fRows[fN], 1, 0, 1/cLieValue );

          // ###########################################
          // #### Progress
          if Assigned(fProgress) and ( (iter = fNumIter) or (iter mod 50 = 0) ) then
          begin
               fcost := EvaluateError(pY, numDims);
               doCancel := False;
               fProgress(Self, iter, fcost, yData, doCancel);

               if doCancel then
                  break;
          end;

          if iter = cMomSwitchIter then
             momentum := cFinalMomentum;
     end;

     // final cost...
     fCost := EvaluateError(pY, numDims);

     // handle the reference counting
     Result := MatrixClass.Create;
     Result.TakeOver(yData.GetObjRef);
end;

procedure TtSNE.CalcPerplexity(x: TDoubleMatrix; perplexity: double;
  K: integer);
var curP : TDoubleDynArray;
    n : integer;
    tree : TVPTree;
    objX : TDataPointArr;
    found : boolean;
    beta : double;
    minBeta, maxBeta : double;
    tol : double;
    HDiff, H, sumP : double;
    iter : integer;
    indices : TSearchPtEntryArr;
    m : Integer;
   // invSumP : double;
    ptRow : integer;
begin
     fN := X.Height;
     SetLength( fRows, 1 + X.Height);
     SetLength( fCols, X.Height*K );
     SetLength( fP, Length(fCols) );
     SetLength( CurP, X.Height - 1 );

     if (curP = nil) or (fP = nil) then
        raise Exception.Create('Failed to allocate memory');

     fRows[0] := 0;

     for n := 0 to fN - 1 do
         fRows[n + 1] := K + fRows[n];

     // ###########################################
     // #### Build ball tree on data set
     SetLength(objX, X.Height);

     for n := 0 to Length(objX) - 1 do
     begin
          X.SetSubMatrix(0, n, X.Width, 1);
          objX[n].Init( (*X.Width, *)n, X.StartElement );
     end;
     X.UseFullMatrix;
     tree := TVPTree.Create(X.Width, fDistFunc, fRandAlgorithm, fSeed);
     try
        tree.BuildTree(objX);

        // ###########################################
        // #### Loop over all points to find nearest neighbors
        for n := 0 to fN - 1 do
        begin
             // Find nearest neighbors
             tree.Search(n, K + 1, indices);

             ptRow := indices[0].pt^.Ind;

             found := False;
             beta := 1;
             minBeta := Infinity;
             maxBeta := Infinity;
             tol := 1e-5;
             sumP := 0;

             // ###########################################
             // #### Iterate until we found a good perplexity
             iter := 0;
             while not found and (iter < 200) do
             begin
                  // compute gaussian kernel row
                  for m := 0 to K - 1 do
                      curP[m] := exp(-beta*sqr(indices[m + 1].dist));

                  // compute entropy of current row
                  sumP := MinDouble;
                  for m := 0 to K - 1 do
                      sumP := sumP + curP[m];
                  H := 0;
                  for m := 0 to K - 1 do
                      H := H + beta*sqr(indices[m + 1].dist)*curP[m];

                  H := H/sumP + ln(sumP);

                  // evaluate whether the entropy is within the tolerance level
                  HDiff := H - ln(perplexity);

                  found := abs(HDiff) < tol;
                  if not found then
                  begin
                       if Hdiff > 0 then
                       begin
                            minBeta := beta;
                            if IsInfinite(maxBeta)
                            then
                                beta := 2*beta
                            else
                                beta := (beta + maxBeta)/2;
                       end
                       else
                       begin
                            maxBeta := beta;
                            if IsInfinite(minBeta)
                            then
                                beta := beta/2
                            else
                                beta := (beta + minBeta)/2;
                       end;
                  end;

                  inc(iter);
             end;

             // ###########################################
             // #### Row normalize current row of P and store in matrix
             assert(sumP <> 0, 'Divide by zero in perplexity calculation');
             MatrixScaleAndAdd( @curP[0], K*sizeof(double), K, 1, 0, 1/sumP );
             for m := 0 to K - 1 do
             begin
                  fCols[fRows[ptRow] + m ] := indices[m + 1].pt^.Ind;
                  fP[fRows[ptRow] + m] := curP[m];
             end;
        end;
     finally
            tree.Free;
     end;
end;

procedure TtSNE.computeGradient(pY : PConstDoubleArr; numDims : integer;
   var dC: TDoubleDynArray);
var tree : TSPTree;
    n : Integer;
    sumQ : double;
    i : integer;
    invSumQ : double;
begin
     MatrixInit(fGradPosF, 0);
     MatrixInit(fGradNegf, 0);

     // compute all terms required for t-SNE gradient
     sumQ := 0;
     tree := TSPTree.Create( numDims, pY, fN );
     try
        tree.ComputeEdgeForces(fRows, fCols, fP, fGradPosf);
        for n := 0 to fN - 1 do
            tree.ComputeNonEdgeForces(n, fTheta, @fGradNegf[n*numDims], sumQ);

        // compute final gradient
        invSumQ := 1/sumQ;
        for i := 0 to fN*numDims - 1 do
            dC[i] := fGradPosf[i] - (fGradNegf[i]*invSumQ);
     finally
            tree.Free;
     end;
end;

{$EndRegion}

// ###########################################
// #### 
// ###########################################

end.
