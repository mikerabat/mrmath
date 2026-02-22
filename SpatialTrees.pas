// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2026, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################
unit SpatialTrees;

interface

uses MatrixConst, SysUtils, Classes, Types, RandomEng, MathUtilFunc;

// ###########################################
// #### Implementations of spatial search trees:
// #### Vantage Point (VP) Tree: splices the area according to a randomly chosen point
// #### KD Tree: Split according to a dimension - keep the bounding box for each node
//               -> speed up nearest neighbor search
//               -> a short test revealed that a sppedup is dependent on the number of dimensions
//                  at around 20 dimensions the tree search for a simple one nearest neighbor search
//                  is faster than a stupid loop

// basically the implementation from https://stevehanov.ca/blog/?id=130
// I think this one was used in the original code

// ###########################################
// #### Data point but without the dimension - we don't want to store that dimension
// value extrax for each point.
type
  TSpatialTreeDistFuncType = (dfEuclid, dfAbs);
  TDataPoint = record
  private
    fInd : integer;
    fX : PConstDoubleArr;

    function GetX(index: integer): double; inline;
  public
    property Ind : integer read fInd;
    property X[ index : integer ] : double read GetX;

    procedure Init; overload;
    procedure Init( ref : TDataPoint ); overload;
    procedure Init( ind : integer; x : PDouble ); overload;
  end;
  PDataPoint = ^TDataPoint;
  TDataPointArr = Array of TDataPoint;

  TSearchPtEntry = record
    pt : PDataPoint;
    dist : double;
  end;
  TSearchPtEntryArr = Array of TSearchPtEntry;

  // ###########################################
  // #### Common base class - implements memory management
  // #### and defines common data structures and distance functions
  TSpatialTreeDistFunc = function ( idx1 : integer ) : double of Object;
  TSpatialTreeCoordDistFunc = function ( const coord : double; dim : integer ) : double of Object;
  TCommonSpatialTree = class(TObject)
  protected
    const cBlockSize = 4096;  // normal page size 4kB
          cMaxBlockSize = 1000*cBlockSize; // 4MB max
    type
      TBlockByteArr = Array[0..cMaxBlockSize] of Byte;
      PBlockByteArr = ^TBlockByteArr;


      // output structure -> index to the input item (and the distance to the search item)
      THeapItem = record
        dist : double;
        index : integer;
      end;
      PHeapItem = ^THeapItem;
      THeapItemArr = Array of THeapItem;
      PHeapItemArr = Array of PHeapItem;
  protected
    fNodeSize : integer;
    fBlockSize : integer;
    fNumElemPerBlock : integer;
    fNodes :  TList;
    fPActNode : PByte;         // points to current block
    fActNodeIdx : integer;
    fNumNodes : integer;

    fItems : TDataPointArr;
    fRoot : PByte;

    fDists : THeapItemArr;
    fPDists : PHeapItemArr;   // points to the fDists records
    fHlp : TDataPointArr;

    fSearchPt : TDataPoint;

    fD : integer;             // number of dimensions....
    fDistFunc : TSpatialTreeDistFunc;

    // ###########################################
    // #### Common distance functions
    function DistanceEuclid( idx1 : integer ) : double;
    function DistanceAbs( idx1 : integer ) : double;

    // ###########################################
    // #### Memory handling
    function NewNode : PByte;

    function AddNodeArr : PByte;
    procedure ClearNodes;
  protected
    function BuildFromPoints( lower, upper : integer ) : PByte; virtual; abstract;
  public
    procedure BuildTree(items : TDataPointArr);

    // return the k nearest neighbours to point pt
    procedure Search( idx : integer; k : integer; var res : TSearchPtEntryArr); overload;
    procedure Search( pt : PDouble; k : integer; var res : TSearchPtEntryArr); overload; virtual; abstract;

    constructor Create(D : integer; nodeSize : integer; distFunc : TSpatialTreeDistFuncType);
    destructor Destroy; override;
  end;

  // ###########################################
  // #### Vantage point tree
  TVPTree = class(TCommonSpatialTree)
  private
    // tree nodes definition
    type
      PVPTreeNode = ^TVPTreeNode;
      TVPTreeNode = record
        Index : integer;
        Threshold : double;
        Left, Right : PVPTreeNode;
      end;
  private
    fRnd : TRandomGenerator;

    fTau : double;
  protected
    function BuildFromPoints( lower, upper : integer ) : Pbyte; override;

    procedure InternalSearch( node : PVPTreeNode; k : integer;
                              heap : TPtrHeap; heapData : TCommonSpatialTree.THeapItemArr);

  public
    // search k nearest neighbours to the original input point at idx
    procedure Search( pt : PDouble; k : integer; var res : TSearchPtEntryArr); overload; override;

    constructor Create(D : integer; distFunc : TSpatialTreeDistFuncType; randAlg : TRandomAlgorithm = raChaCha; seed : integer = 0);
    destructor Destroy; override;
  end;

  // ###########################################
  // #### KD Tree
  TKDTree = class(TCommonSpatialTree)
  private
    type
      PKDTreeNode = ^TKDTreeNode;
      TKDTreeNode = record
        Index : integer;
        Left, Right : PKDTreeNode;
        CutDim : integer;
        LoBound : PConstDoubleArr;   // bounding box
        HiBound : PConstDoubleArr;
      end;
  private
    fLoBound, fHiBound : TDoubleDynArray;
    fCoordDistFunc : TSpatialTreeCoordDistFunc;

    function CoordDistEuclid( const coord : double; dim : integer) : double;
    function CoordDistAbs( const coord : double; dim : Integer) : double;

    function NewKDNode : PKDTreeNode;
    function InternalSearch(node : PKDTreeNode; k : integer;
                            heap : TPtrHeap; heapData : TCommonSpatialTree.THeapItemArr) : boolean;
    function boundsOverlap(dist: double; node: PKDTreeNode): boolean;
    function WithinBounds(dist: double; node: PKDTreeNode): boolean;
  protected
    function InternalBuildFromPoints( depth, lower, upper : integer) : PKDTreeNode;

    function BuildFromPoints( lower, upper : integer) : PByte; override;
  public
    procedure Search( pt : PDouble; k : integer; var res : TSearchPtEntryArr); override;

    constructor Create(D : integer; distFunc : TSpatialTreeDistFuncType);
  end;

implementation

uses Math;

// ###########################################
// #### Comment
// ###########################################

function DoubleDistCmp( item1, item2 : Pointer ) : integer;
begin
     Result := CompareValue( TCommonSpatialTree.PHeapItem(item1)^.dist, TCommonSpatialTree.PHeapItem(item2)^.dist );
end;

// ###########################################
// #### Datapoint record
// ###########################################

{ TDataPoint }

procedure TDataPoint.Init(ref: TDataPoint);
begin
     fInd := ref.fInd;
     fX := ref.fX;
end;

procedure TDataPoint.Init;
begin
     fInd := 0;
     fX := nil;
end;

procedure TDataPoint.Init( ind: integer; x: PDouble);
begin
     fInd := ind;
     fX := PConstDoubleArr( x );
end;

function TDataPoint.GetX(index: integer): double;
begin
     Result := fX^[index];
end;

{ TCommonSpatialTree }

function TCommonSpatialTree.AddNodeArr: PByte;
begin
     Result := GetMemory(fBlockSize);
     fNodes.Add(Result);
     fActNodeIdx := 0;
end;

procedure TCommonSpatialTree.BuildTree(items: TDataPointArr);
begin
     fPActNode := nil;
     ClearNodes;
     fNumNodes := 0;
     fItems := items;

     fRoot := BuildFromPoints( 0, Length(fItems) - 1);
end;

procedure TCommonSpatialTree.ClearNodes;
var i : Integer;
begin
     for i := 0 to fNodes.Count - 1 do
         FreeMem(fNodes[i]);

     fActNodeIdx := 0;
     fNodes.Clear;
end;

constructor TCommonSpatialTree.Create(D: integer; nodeSize : integer;
  distFunc: TSpatialTreeDistFuncType);
begin
     fD := D;
     fNodes := TList.Create;
     fNodeSize := nodeSize;
     // max 2MB blocks min 4kB -> at least 256  elements per line...)
     fBlockSize := Min( cMaxBlockSize,  256*nodeSize );
     // round to next 4kB Block
     fBlockSize := fBlockSize + fBlockSize mod cBlockSize;

     fNumElemPerBlock := fBlockSize div nodeSize;

     assert(fNumElemPerBlock > 0, 'Element size bigger than expected');

     case distFunc of
       dfAbs: fDistFunc := DistanceAbs;
     else
         fDistFunc := DistanceEuclid;
     end;

     inherited Create;
end;

destructor TCommonSpatialTree.Destroy;
begin
     ClearNodes;
     fNodes.Free;

     inherited;
end;

function TCommonSpatialTree.DistanceAbs(idx1: integer): double;
var i : integer;
begin
     Result := 0;

     for i := 0 to fD - 1 do
         Result := Result + Abs( PConstDoubleArr(fItems[idx1].fX)^[i] - PConstDoubleArr(fSearchPt.fX)^[i] );
end;

function TCommonSpatialTree.DistanceEuclid(idx1: integer): double;
var i : integer;
begin
     Result := 0;

     for i := 0 to fD - 1 do
         Result := Result + sqr( PConstDoubleArr(fItems[idx1].fX)^[i] - PConstDoubleArr(fSearchPt.fX)^[i] );

     Result := Sqrt(Result);
end;

function TCommonSpatialTree.NewNode: PByte;
begin
     if (fPActNode = nil) or (fActNodeIdx = fNumElemPerBlock) then
        fPActNode := AddNodeArr;

     Result := @PBlockByteArr(fPActNode)^[fActNodeIdx*fNodeSize];
     inc(fActNodeIdx);
     inc(fNumNodes);
end;

procedure TCommonSpatialTree.Search(idx, k: integer; var res: TSearchPtEntryArr);
begin
     Search(PDouble(fItems[idx].fX), k, res);
end;

// ###########################################
// #### VP Tree
// ###########################################
{ TVPTree }

function TVPTree.BuildFromPoints(lower, upper: integer): PByte;
var i : Integer;
    tmp : TDataPoint;
    med : integer;
    numElem : integer;
    res : PVPTreeNode;
begin
     // indicates we are done here
     if lower > upper then
        exit(nil);

     res := PVPTreeNode(NewNode);

     res^.index := lower;

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
          fSearchPt := fItems[lower];
          for i := 0 to numElem - 1 do
          begin
               fDists[i].dist := fDistFunc(lower + i + 1);
               fDists[i].index := i;
               fPDists[i] := @fDists[i];
          end;
          res^.Threshold := KthLargestP( @fPDists[0], numElem, med);

          // write back the partitioned data from the helper array using the partioned
          // indices from the KthlargestP function
          for i := 0 to numElem - 1 do
              fItems[lower + 1 + i] := fHlp[ fPDists[i]^.index ];

          // ###########################################
          // #### Recursion to build left and right tree
          res^.Left := PVPTreeNode(BuildFromPoints(lower + 1, lower + med));   // original code goes up until med but makes that sense??
          res^.Right := PVPTreeNode(BuildFromPoints( lower + 1 + med, upper));
     end
     else
     begin
          res.Left := nil;
          res.Right := nil;
          res.Threshold := 0;
     end;

     Result := PByte(res);
end;

constructor TVPTree.Create(D: integer; distFunc: TSpatialTreeDistFuncType;
  randAlg: TRandomAlgorithm; seed: integer);
begin
     inherited Create(D, sizeof(TVPTreeNode), distFunc);

     fRnd := TRandomGenerator.Create( randAlg );
     fRnd.Init(seed);
end;

destructor TVPTree.Destroy;
begin
     fRnd.Free;

     inherited;
end;

procedure TVPTree.InternalSearch(node: PVPTreeNode; k: integer;
  heap: TPtrHeap; heapData: TCommonSpatialTree.THeapItemArr);
var dist : double;
    elem : PHeapItem;
begin
     if node = nil then
        exit;

     // Compute distance between target and current node
     dist := fDistFunc(node^.Index);

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
             InternalSearch(node^.Left, k, heap, heapData);

          if dist + fTau >= node^.Threshold then
             InternalSearch(node^.Right, k, heap, heapData);
     end
     else
     begin
          // search right node first
          if dist + fTau >= node^.Threshold then
             InternalSearch(node^.Right, k, heap, heapData);

          if dist - fTau <= node^.Threshold then
             InternalSearch(node^.Left, k, heap, heapData);
     end;
end;

procedure TVPTree.Search(pt: PDouble; k: integer; var res: TSearchPtEntryArr);
var heapData : THeapItemArr;
    heap : TPtrHeap;
    i : Integer;
    pItem : PHeapItem;
begin
     fSearchPt.fInd := -1;
     fSearchPt.fX := PConstDoubleArr( pt );

     // the first element is the one with the shortes distance (0)
     // we have never more than k elements
     SetLength(heapData, k);
     fTau := MaxDouble;

     heap := TPtrHeap.Create( DoubleDistCmp );
     heap.Capacity := k;

     // perform search... and reverse
     InternalSearch( PVPTreeNode( fRoot ), k, heap, heapData);
     SetLength(res, heap.Count);
     for i := Length(res) - 1 downto 0 do
     begin
          pItem := heap.Pop;

          res[i].pt := @fItems[ pItem^.index ];
          res[i].dist := pItem^.dist;
     end;

     heap.Free;
end;

// ###########################################
// #### KD Tree
// ###########################################

function TKDTree.CoordDistAbs(const coord : double; dim: Integer): double;
begin
     Result := Abs( coord - PConstDoubleArr(fSearchPt.fX)^[dim] )
end;

function TKDTree.CoordDistEuclid(const coord : double; dim: integer): double;
begin
     Result := sqr( coord - PConstDoubleArr(fSearchPt.fX)^[dim] );
end;

function TKDTree.BuildFromPoints(lower, upper: integer): PByte;
var j, i: Integer;
begin
     SetLength(fLoBound, fD);
     SetLength(fHiBound, fD);


     // calculate bounding box
     for j := 0 to fD - 1 do
     begin
          fLoBound[j] := fItems[lower].fX^[j];
          fHiBound[j] := fLoBound[j];

          for i := lower + 1 to upper do
          begin
               fLoBound[j] := Min(fLoBound[j], fItems[i].fX^[j]);
               fHiBound[j] := Max(fHiBound[j], fItems[i].fX^[j]);
          end;
     end;

     // ###########################################
     // #### Build tree
     Result := PByte( InternalBuildFromPoints(0, lower, upper) );

     fLoBound := nil;
     fHiBound := nil;
end;

constructor TKDTree.Create(D: integer; distFunc: TSpatialTreeDistFuncType);
begin
     // node size is the node itself + the bounding box (hi low coordinates)
     inherited Create(D, D*sizeof(double)*2*sizeof(TKDTree), distFunc);

     case distFunc of
       dfAbs: fCoordDistFunc := CoordDistAbs;
     else
         fCoordDistFunc := CoordDistEuclid;
     end;
end;

function TKDTree.InternalBuildFromPoints(depth, lower, upper: integer): PKDTreeNode;
var numElem : integer;
    med : integer;
    i : integer;
    cutVal : double;
    tmpVal : double;
begin
     if lower > upper then
        exit(nil);

     Result := NewKDNode;
     Result^.CutDim := depth mod fD;
     Result^.index := lower;

     if upper > lower then
     begin
          // ###########################################
          // #### Partition along the cutdim
          // number of elements to sort from (from lower + 1 to upper)
          numElem := upper - lower + 1;

          // Partition around the median distance
          if Length(fDists) < (upper - lower + 1) then
          begin
               SetLength(fDists, upper - lower + 1);
               SetLength(fHlp, upper - lower + 1);
               SetLength(fPDists, upper - lower + 1);
          end;

          med := numElem div 2;

          // make a copy -> put back later on, we sort on indices :)
          Move( fItems[lower], fHlp[0], numElem*sizeof(fHlp[0]));
          fSearchPt := fItems[lower];
          for i := 0 to numElem - 1 do
          begin
               fDists[i].dist := fHlp[i].fX^[Result^.CutDim];
               fDists[i].index := i;
               fPDists[i] := @fDists[i];
          end;
          cutVal := KthLargestP( @fPDists[0], numElem, med );

          // median value always stored at the lower indices (it is later not touched any more)
          fItems[lower] := fHlp[ fPDists[med]^.index ];
          // write back the partitioned data from the helper array using the partioned
          // indices from the KthlargestP function
          for i := 0 to med - 1 do
              fItems[lower + 1 + i] := fHlp[ fPDists[i]^.index ];
          for i := med + 1 to numElem - 1 do
              fItems[lower + i] := fHlp[ fPDists[i]^.index ];

          // ###########################################
          // #### Recursion to build left and right tree
          tmpVal := fHiBound[Result^.CutDim];
          fHiBound[Result^.CutDim] := cutVal;
          Result^.Left := InternalBuildFromPoints(depth + 1, lower + 1, lower + med );
          fHiBound[Result^.CutDim] := tmpVal;

          tmpVal := fLoBound[Result^.CutDim];
          fLoBound[Result^.CutDim] := cutVal;
          Result^.Right := InternalBuildFromPoints(depth + 1, lower + 1 + med, upper );
          fLoBound[Result^.CutDim] := tmpVal;
     end
     else
     begin
          Result^.Left := nil;
          Result^.Right := nil;
     end;
end;

// returns true if the bounds of node overlap with the ball with radius dist around the search point
function TKDTree.boundsOverlap(dist : double; node : PKDTreeNode) : boolean;
var distSum : double;
     i : Integer;
begin
     Result := False;
     distSum := 0;
     for i := 0 to fD - 1 do
     begin
          if fSearchPt.fX^[i] < node^.LoBound^[i] then
          begin
               distsum := distsum + fCoordDistFunc( node^.LoBound^[i], i );
               if distsum > dist then
                  exit;
          end
          else if fSearchPt.fX^[i] > node^.HiBound^[i] then
          begin
               distSum := distSum + fCoordDistFunc( node^.HiBound^[i], i) ;
               if distSum > dist then
                  exit;
          end;
     end;
     Result := True;
end;

// return true if the bounds of node completely contain the ball with radious dist around the search point
function TKDTree.WithinBounds(dist : double; node : PKDTreeNode) : boolean;
var i : Integer;
begin
     Result := False;
     for i := 0 to fD - 1 do
     begin
          if (fCoordDistFunc( node^.LoBound^[i], i ) <= dist) or
             (fCoordDistFunc( node^.HiBound^[i], i ) <= dist)
          then
              exit;
     end;

     Result := True;
end;

function TKDTree.InternalSearch(node: PKDTreeNode; k: integer; heap: TPtrHeap;
  heapData: TCommonSpatialTree.THeapItemArr): boolean;
var curdist : double;
    dist : double;
    elem : PHeapItem;
begin
     Result := True;
     curdist := fDistFunc( node^.Index );

     // check if it's within the radius
     if heap.Count < k then
     begin
          elem := @heapData[heap.Count];
          elem^.dist := curDist;
          elem^.index := node^.Index;
          heap.Add(elem);
     end
     else
     begin
          if PHeapItem(heap.Top)^.dist > curdist then
          begin
               elem := heap.Pop;
               elem^.index := node^.Index;
               elem^.dist := curdist;
               heap.Add(elem);
          end;
     end;

     // first search on side closer to the point
     if fSearchPt.fX^[ node^.CutDim ] < fItems[node^.Index].fX^[ node^.CutDim ] then
     begin
          if (node^.Left <> nil) and InternalSearch(node^.Left, k, heap, heapData) then
             exit;
     end
     else
     begin
          if (node^.Right <> nil) and InternalSearch(node^.Right, k, heap, heapData) then
             exit;
     end;

     // second pass on the other side (if the distance is within the nodes boundaries)
     dist := MaxDouble;
     if heap.Count = k then
        dist := PHeapItem( heap.Top )^.dist;

     if fSearchPt.fX^[ node^.CutDim ] < fItems[node^.Index].fX^[ node^.CutDim ] then
     begin
          if (node^.Right <> nil) and boundsOverlap(dist, node^.Right) and InternalSearch(node^.Right, k, heap, heapData) then
             exit;
     end
     else
     begin
          if (node^.Left <> nil) and boundsOverlap(dist, node^.Left) and InternalSearch(node^.Left, k, heap, heapData) then
             exit;
     end;

     if heap.Count = k then
        dist := PHeapItem( heap.Top )^.dist;

     Result := WithinBounds( dist, node );
end;

function TKDTree.NewKDNode: PKDTreeNode;
var pBounds : PConstDoubleArr;
begin
     Result := PKDTreeNode(NewNode);
     pBounds := PConstDoubleArr(Result);
     inc(PByte(pBounds), sizeof(TKDTreeNode));

     Result^.LoBound := pBounds;
     Result^.HiBound := @pBounds^[fD];

     // init with current bounds
     Move(fLoBound[0], Result^.LoBound^[0], fD*sizeof(double));
     Move(fHiBound[0], Result^.HiBound^[0], fD*sizeof(double));
end;

procedure TKDTree.Search(pt: PDouble; k: integer; var res: TSearchPtEntryArr);
var heapData : THeapItemArr;
    heap : TPtrHeap;
    i : Integer;
    pItem : PHeapItem;
begin
     fSearchPt.fInd := -1;
     fSearchPt.fX := PConstDoubleArr(pt);

     // the first element is the one with the shortes distance (0)
     // we have never more than k elements
     SetLength(heapData, k);

     heap := TPtrHeap.Create( DoubleDistCmp );
     heap.Capacity := k;

     // perform search... and reverse
     InternalSearch(PKDTreeNode(fRoot), k, heap, heapData);
     SetLength(res, heap.Count);
     for i := Length(res) - 1 downto 0 do
     begin
          pItem := heap.Pop;

          res[i].pt := @fItems[ pItem^.index ];
          res[i].dist := pItem^.dist;
     end;

     heap.Free;
end;


end.
