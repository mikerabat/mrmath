// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit SimpleMatrixOperations;

// ############################################
// #### Base matrix operations based on functions
// ############################################

interface

uses MatrixConst, Types;

procedure GenericMtxCopy(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt); overload;
procedure GenericMtxCopy(var dest : Array of double; const Src : Array of double; width, height : NativeInt); overload;
function GenericMtxCopy(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt) : TDoubleDynArray; overload;
function GenericMtxCopy(const Src : Array of double; width, height : NativeInt) : TDoubleDynArray; overload;

procedure GenericMtxIndex( dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; colIdx, rowIdx : TIntegerDynArray);

procedure GenericMtxInit( dest : PDouble; destLineWidth : NativeInt; width, height : NativeInt; const value : double );

procedure GenericColSwap(A, B : PDouble; const LineWidthAB : NativeInt; Height : NativeInt);
procedure GenericRowSwap(A, B : PDouble; width : NativeInt);

function GenericMtxAdd(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxAdd(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;
function GenericMtxAdd(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt); overload;

procedure GenericAddVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);

function GenericMtxSub(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxSub(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;
function GenericMtxSub(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxSub(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt); overload;

procedure GenericSubVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);


function GenericMtxMult(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
function GenericMtxMult(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt); overload;

// calculatates mt1'*mt2
procedure GenericTranspMtxMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
// calculates mt1*mt2'
procedure GenericMtxMultTransp(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;


// calculates Result := sum_0_n-1( x[i]*y[i] );
function GenericVecDotMult( x : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt ) : double;

// calculates y[i] = y[i] + alpha*x[i]
procedure GenericVecAdd( X : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt; const alpha : double );

procedure GenericTranspMtxMultAdd(dest : PDouble; const destLineWidth : NativeInt;
  mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; C : PDouble; LineWidthC : NativeInt);
// calculates dest = alpha*mt1*mt2' + C
procedure GenericMtxMultTranspAdd(dest : PDouble; const destLineWidth : NativeInt; const alpha : double;
  mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; C : PDouble; LineWidthC : NativeInt);


// strassen multiplication
procedure GenericMtxDeltaUpdate(dest : PDouble; destLineWidth : NativeInt; A11, B11 : PDouble; width, height, LineWidth1 : NativeInt);
procedure GenericStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);

// performs matrix vector multiplication in the form: dest := alpha*mt1*v + beta*dest
procedure GenericMtxVecMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
// performs matrix vector multiplication in the form: dest := alpha*mt1'*v + beta*dest
procedure GenericMtxVecMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);

// performs matrix vector multiplication in the form: dest := alpha*mt1*v + beta*dest
// where the matrix mt1 is a symmetric matrix and only the upper part is touched in the multiplication
procedure GenericMtxVecMultUpperSym( dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
//procedure GenericMtxVecMultUpperSym1( dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);

// same as upper sym but uses the lower part of the matrix
procedure GenericMtxVecMultLowerSym( dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);


procedure GenericMtxElemMult(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;
function GenericMtxElemMult(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt); overload;
function GenericMtxElemMult(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;

procedure GenericMtxElemDiv(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;
function GenericMtxElemDiv(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt); overload;
function GenericMtxElemDiv(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;

// GenericMtx transposition functions. Note the there is no inplace GenericMtx transpose - this will result in an unspecified end GenericMtx.
function GenericMtxTranspose(mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxTranspose(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); overload;
function GenericMtxTranspose(const mt : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
procedure GenericMtxTranspose(var dest : Array of Double; const mt : Array of Double; width : NativeInt; height : NativeInt); overload;

// -> only square matrices are allowed:
procedure GenericMtxTransposeInplace(dest : PDouble; const destLineWidth : NativeInt; n : NativeInt);


function GenericMtxMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
function GenericMtxMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
function GenericMtxAbsMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
function GenericMtxAbsMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;

// applies dest^ := max(dest^, maxVal) over all elements
procedure GenericMtxMaxVal(dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; maxVal : double);
procedure GenericMtxMinVal(dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double);

// applies dest[i] := min(dest[i], mt[i])
procedure GenericVecMin( dest : PDouble; mt : PDouble; n : NativeInt );
procedure GenericVecMax( dest : PDouble; mt : PDouble; n : NativeInt );

// matrix max and min of an upper or lower diagonal matrix
function GenericMtxMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxAbsMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxAbsMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxAbsMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function GenericMtxAbsMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;


function GenericMtxNormalize(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function GenericMtxNormalize(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure GenericMtxNormalize(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
procedure GenericMtxNormalize(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;

procedure GenericMtxMean(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
procedure GenericMtxVar(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
procedure GenericMtxSum(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
function GenericMtxSumSum(Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt) : double;

// Calculate the mean and variance at the same time -> dest is at least a 2xheight matrix
procedure GenericMtxMeanVar(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);

procedure GenericMtxCumulativeSum(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
procedure GenericMtxDifferentiate(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);

// matrix median (row or column wise). For the calculation of the median at least width (for rowwise) or height (for columnwise) extra
// memory is needed. The memory is either allocated on the fly or can be provided in the function.
procedure GenericMtxMedian(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; tmp : PDouble = nil);

procedure GenericMtxSort(dest : PDouble; destLineWidth : NativeInt; width, height : integer; RowWise : boolean; tmp : PDouble = nil);

procedure GenericMtxElemAdd(Dest : PDouble; LineWidth, Width, Height : NativeInt; const Offset : double);
procedure GenericMtxAddAndScale(Dest : PDouble; LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
procedure GenericMtxScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : NativeInt; const Offset, Scale : double);

// element wise eukledian norm
function GenericMtxElementwiseNorm2(Src : PDouble; srcLineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;


procedure GenericMtxAbs(dest : PDouble; destLineWidth, width, height : NativeInt);
procedure GenericMtxSqrt(dest : PDouble; destLineWidth : NativeInt; width, height : NativeInt);


// Apply a function to a matrix:
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixObjFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefObjFunc); overload;

// apply a function to a part of a matrix (startx to startx + width -1, starty to starty + height - 1)
procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixFunc); overload;
procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixObjFunc); overload;
procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefFunc); overload;
procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefObjFunc); overload;

{$IFDEF FPC}
   {.$DEFINE ANONMETHODS}
{$ELSE}
   {$IF CompilerVersion >= 20.0}
      {$DEFINE ANONMETHODS}
   {$IFEND}
{$ENDIF}

{$IFDEF ANONMETHODS}
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFuncRef); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixFuncRef); overload;
procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;
{$ENDIF}

// ###########################################
// #### Matrix multiplications used in QR and Hess Decomposition
procedure GenericMtxMultTria2T1Lower(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// same as above but works on 2 columns on mt2 at the same time
procedure GenericMtxMultTria2T1_2(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt;
  mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// dtrm right upper transpose unit
procedure GenericMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);


// calculates mt1 = mt1*mt2, mt2 = upper triangular matrix with non unit diagonal elements
procedure GenericMtxMultRightUpperTriaNoUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// calculates mt1 = mt1*mt2', mt2 = upper triangular matrix with non unit diagonal elements
procedure GenericMtxMultRightUpperTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// calculates mt1 = mt1*mt2', mt2 = upper triangular matrix. diagonal elements are assumed to be 1!
procedure GenericMtxMultRightUpperTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure GenericMtxMultRightUpperTriaUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be non unit!
procedure GenericMtxMultRightLowerTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// performs mt1 = mt1*mt2 where mt2 is an lower triangular matrix with unit elements in the diagonal
procedure GenericMtxMultRightLowerTriaUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// performs mt1 = mt1*mt2 where mt2 is an lower triangular matrix with non unit elements in the diagonal
procedure GenericMtxMultRightLowerTriaNoUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure GenericMtxMultRightLowerTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

  

// calculates  x = mt1'*x where x is a vector and mt1 an upper triangular (len x len) matrix with non unit elements in the diagonal
procedure GenericMtxMultUpTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);

// calculates x = mt1'*x where x is a vector and mt1 an lower triangular (len x len) matrix with unit elements in the diagonal
procedure GenericMtxMultLowTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);

// calculates x = mt1*x where x is a vector and mt1 is a lower triangular (len x len) matrix with unit elements in the diagonal
procedure GenericMtxMultLowNoTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);

// calculates x = mt1*x where x is a vector and mt1 is a lower triangular (len x len) matrix with non unit elements in the diagonal
procedure GenericMtxMultLowNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);

// calculates x = mt1*x where x is a vector and mt1 is an upper triangular (len x len) matrix with non unit elements in the diagonal
procedure GenericMtxMultUpNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; incX : NativeInt; len : NativeInt);


// calculates A = A - B'
procedure GenericMatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt);



// performs a symmetric rank update in the form dest = alpha* A*AT + Beta*C
// Assumes a lower triangualr matrix
procedure GenericSymRankKUpd(dest : PDouble; LineWidthDest: NativeInt; A : PDouble; LineWidthA : NativeInt; k, height : NativeInt; const Alpha, Beta : double);
procedure GenericSolveLoTriMtxTranspNoUnit(A : PDouble; const LineWidthA : NativeInt; B : PDouble; const LineWidthB : NativeInt; width, height : NativeInt);
procedure GenericSymRank2UpdateUpper( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
  B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt );

procedure GenericRank1Update(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double);

procedure GenericMtxDistanceSqr(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt);
procedure GenericMtxDistanceAbs(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt);

procedure GenericInitMemAligned(A : PDouble; NumBytes : NativeInt; Value : double);

// simple convolution: the input and output parameter are assumed to be vectors!
// it's also assumed that memory before A is accessible for at least bLen elements
// -> these elements are used for the convulution calculation
// -> note the function is called by MatrixConvolve in MatrixASMStubSwitch.pas
procedure GenericConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt);

implementation

uses Math, MathUtilFunc;

procedure GenericInitMemAligned(A : PDouble; NumBytes : NativeInt; Value : double);
var numElem : NativeInt;
    pA : PConstDoubleArr;
    counter: Integer;
begin
     numElem := NumBytes div sizeof(double);
     pA := PConstDoubleArr(A);

     for counter := 0 to numElem - 1 do
         pA^[counter] := Value;
end;

function GenericMtxNormalize(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     SetLength(Result, width*height);
     GenericMtxNormalize(@Result[0], width*sizeof(double), Src, srcLineWidth, width, height, RowWise);
end;

function GenericMtxNormalize(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (length(Src) >= width*height), 'Dimension error');

     SetLength(Result, width*height);
     GenericMtxNormalize(@Result[0], width*sizeof(double), @Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure GenericMtxNormalize(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var normVal : double;
    pSrc1 : PConstDoubleArr;
    pDest1 : PConstDoubleArr;
    pSrc, pDest : PDouble;
    x, y : NativeInt;
begin
     assert((destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)) and (width > 0) and (height > 0), 'Dimension error');

     if RowWise then
     begin
          for y := 0 to height - 1 do
          begin
               pSrc1 := PConstDoubleArr(Src);
               pDest1 := PConstDoubleArr(dest);
               normVal := 0;
               for x := 0 to width - 1 do
                   normVal := normVal + sqr(pSrc1^[x]);
               normVal := 1/sqrt(normVal);

               for x := 0 to width - 1 do
                   pDest1^[x] := pSrc1^[x]*normVal;

               inc(PByte(dest), destLineWidth);
               inc(PByte(src), srcLineWidth);
          end;
     end
     else
     begin
          for x := 0 to width - 1 do
          begin
               pSrc := Src;
               pDest := dest;
               normVal := 0;
               for y := 0 to height - 1 do
               begin
                    normVal := normVal + sqr(pSrc^);
                    inc(PByte(pSrc), srcLineWidth);
               end;
               normVal := 1/sqrt(normVal);

               pSrc := Src;
               for y := 0 to height - 1 do
               begin
                    pDest^ := pSrc^*normVal;
                    inc(PByte(pDest), destLineWidth);
                    inc(PByte(pSrc), srcLineWidth);
               end;

               inc(dest);
               inc(src);
          end;
     end;
end;

procedure GenericMtxNormalize(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean);
begin
     assert((width > 0) and (height > 0) and (length(Src) >= width*height) and (length(dest) >= width*height), 'Dimension error');

     GenericMtxNormalize(@dest[0], width*sizeof(double), @Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure GenericMtxMean(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var val : double;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   val := val + pVal2^[x];

               if Width > 0 then
                  dest^ := val/Width;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := 0;

               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := val + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
                  dest^ := val/Height;

               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

procedure GenericMtxVar(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
var val : double;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
    meanVal : double;
    aVariance : double;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   val := val + pVal2^[x];

               aVariance := 0;
               if Width > 0 then
               begin
                    meanVal := val/width;

                    for x := 0 to Width - 1 do
                        aVariance := aVariance + sqr(pVal2^[x] - meanVal);
                    
                    if unbiased 
                    then
                        dest^ := aVariance/Max(1, width - 1)
                    else
                        dest^ := aVariance/width;
               end
               else
                   dest^ := 0;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := 0;

               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := val + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
               begin
                    meanVal := val/Height;

                    aVariance := 0;
                    pVal := PDouble(pVal1);
                    for y := 0 to Height - 1 do
                    begin
                         aVariance := aVariance + sqr(pVal^ - meanVal);
                         inc(PByte(pVal), srcLineWidth);
                    end;

                    if unbiased 
                    then
                        dest^ := aVariance/Max(1, Height - 1)
                    else
                        dest^ := aVariance/Height;
               end
               else
                   dest^ := 0;

               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

procedure GenericMtxMedian(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; 
  width, height : NativeInt; RowWise : boolean; tmp : PDouble = nil);
var x, y : NativeInt;
    pVal1 : PByte;
    pVal : PDouble;
    tmpMem : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if (Width = 0) or (height = 0) then
        exit;

     tmpMem := tmp;
     
     pVal1 := PByte(Src);
     if RowWise then
     begin
          if tmp = nil then
             tmpMem := GetMemory(width*sizeof(double));
          
          for y := 0 to Height - 1 do
          begin
               // copy since we destroy the array when calculating the median
               Move(pVal1^, tmpMem^, Width*sizeof(double));
               
               if Width and 1 = 1 
               then
                   dest^ := KthLargest(tmpMem, width, width div 2)
               else
                   dest^ := (KthLargest(tmpMem, width, width div 2) + KthLargest(tmpMem, width, Max(0, width div 2 - 1)))/2;
                   
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          if tmp = nil then
             tmpMem := GetMemory(height*sizeof(double));
          
          for x := 0 to Width - 1 do
          begin
               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    PConstDoubleArr(tmpMem)^[y] := pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if height and 1 = 1 
               then
                   dest^ := KthLargest(tmpMem, height, height div 2)
               else
                   dest^ := (KthLargest(tmpMem, height, height div 2) + KthLargest(tmpMem, height, Max(0, height div 2 - 1)))/2;

               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;

     if tmp = nil then
        FreeMemory(tmpMem);
end;

procedure GenericMtxSort(dest : PDouble; destLineWidth : NativeInt; width, height : integer; RowWise : boolean; tmp : PDouble = nil);
var x, y : NativeInt;
    pDest : PDouble;
    tmpMem : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if (Width = 0) or (height = 0) then
        exit;

     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               QuickSort(PConstDoubleArr(Dest), width);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          tmpMem := tmp;
          if tmp = nil then
             tmpMem := GetMemory(height*sizeof(double));
          
          for x := 0 to Width - 1 do
          begin
               pDest := dest;
               
               for y := 0 to Height - 1 do
               begin
                    PConstDoubleArr(tmpMem)^[y] := pDest^;
                    inc(PByte(pDest), destLineWidth);
               end;

               QuickSort(PConstDoubleArr(tmpMem), height);
               
               pDest := dest;
               for y := 0 to Height - 1 do
               begin
                    pDest^ := PConstDoubleArr(tmpMem)^[y];
                    inc(PByte(pDest), destLineWidth);
               end;

               inc(dest);
          end;

          if tmp = nil then
             FreeMemory(tmpMem);
     end;
end;

procedure GenericMtxMeanVar(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
var x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
    pDest1 : PDouble;
    meanVal : double;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert( not RowWise or (destLineWidth >= 2*sizeof(double)), 'Results linewidth is too short');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               meanVal := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   meanVal := meanVal + pVal2^[x];

               if Width > 0 then
               begin
                    meanVal := meanVal/Width;
                    dest^ := meanVal;
                    inc(dest);
                    
                    dest^ := 0;
                    for x := 0 to Width - 1 do
                        dest^ := dest^ + sqr( pVal2^[x] - meanVal );

                    if unbiased 
                    then
                        dest^ := dest^/Max(1, width - 1)
                    else
                        dest^ := dest^/width;

                    dec(dest);
               end
               else
               begin
                    dest^ := 0;
                    inc(dest);
                    dest^ := Infinity;
                    dec(dest);
               end;
                  
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          pDest1 := dest;
          inc(PByte(pDest1), destLineWidth);
          
          for x := 0 to Width - 1 do
          begin
               meanVal := 0;

               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    meanVal := meanVal + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
               begin
                    meanVal := meanVal/Height;
                    dest^ := meanVal;
                    
                    pVal := PDouble(pVal1);
                    pDest1^ := 0;
                    for y := 0 to Height - 1 do
                    begin
                         pDest1^ := pDest1^ + sqr( pVal^ - meanVal );
                         inc(PByte(pVal), srcLineWidth);
                    end;   

                    if unbiased 
                    then
                        pDest1^ := pDest1^/Max(1, height - 1)
                    else
                        pDest1^ := pDest1^/height;
               end
               else
               begin
                    dest^ := 0;
                    pDest1^ := Infinity;
               end;

               inc(pVal1, sizeof(double));
               inc(dest);
               inc(pDest1);
          end;
     end;
end;

procedure GenericMtxSum(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var val : double;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   val := val + pVal2^[x];

               dest^ := val;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := 0;

               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := val + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               dest^ := val;
               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

function GenericMtxSumSum(Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt) : double;
var x, y : NativeInt;
    pVal2 : PConstDoubleArr;
    lineVal : double;
begin
     assert((Width >= 0) and (Height >= 0), 'No data assigned');
     Result := 0;
     for y := 0 to Height - 1 do
     begin
          pVal2 := PConstDoubleArr(src);
          lineVal := 0;
          for x := 0 to Width - 1 do
              lineVal := lineVal + pVal2^[x];

          Result := Result + lineVal;

          inc(PByte(src), srcLineWidth);
     end;
end;

procedure GenericMtxCumulativeSum(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var val : double;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
    pDest : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
               begin
                    val := val + pVal2^[x];
                    PConstDoubleArr(dest)^[x] := val;
               end;

               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := 0;

               pVal := PDouble(pVal1);
               pDest := dest;
               for y := 0 to Height - 1 do
               begin
                    val := val + pVal^;
                    pDest^ := val;
                    inc(PByte(pVal), srcLineWidth);
                    inc(PByte(pDest), destLineWidth);
               end;

               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

procedure GenericMtxDifferentiate(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var x, y : NativeInt;
    pVal11 : PDouble;
    pVal2 : PConstDoubleArr;
    pVal1 : PDouble;
    pDest : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          pVal2 := PConstDoubleArr(src);
          for y := 0 to Height - 1 do
          begin
               for x := 0 to Width - 2 do
                   PConstDoubleArr(dest)^[x] := PConstDoubleArr(pVal2)^[x + 1] - PConstDoubleArr(pVal2)^[x];

               inc(PByte(pVal2), srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               pVal1 := PDouble(src);
               pVal11 := pVal1;
               inc(PByte(pVal11), srcLineWidth);
               pDest := dest;
               for y := 0 to Height - 2 do
               begin
                    pDest^ := pVal11^ - pVal1^;
                    pVal1 := pVal11;
                    inc(PByte(pVal11), srcLineWidth);
                    inc(PByte(pDest), destLineWidth);
               end;

               inc(src);
               inc(dest);
          end;
     end;
end;


function GenericMtxElementwiseNorm2(Src : PDouble; srcLineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;
var pSrc : PConstDoubleArr;
    x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     Result := 0;

     pSrc := PConstDoubleArr(Src);
     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              Result := Result + Sqr(pSrc^[x]);

          inc(PByte(pSrc), srcLineWidth);
     end;

     if doSqrt then
        Result := Sqrt(Result);
end;

procedure GenericMtxInit( dest : PDouble; destLineWidth : NativeInt; width, height : NativeInt; const value : double );
var pDest : PConstDoubleArr;
    x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     pDest := PConstDoubleArr( dest );

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              pDest^[x] := value;

          inc(PByte(pDest), destLineWidth);
     end;
end;

procedure GenericMtxCopy(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt);
var y : NativeInt;
    w : NativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     w := width*sizeof(double);
     for y := 0 to Height - 1 do
     begin
          Move(Src^, dest^, w);
          inc(PByte(Src), srcLineWidth);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxCopy(var dest : Array of double; const Src : Array of double; width, height : NativeInt);
begin
     assert((width > 0) and (height > 0) and (Length(dest) = Length(src)) and (length(src) = width*height), 'Dimension error');
     
     GenericMtxCopy(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height);
end;

function GenericMtxCopy(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);

     GenericMtxCopy(@Result[0], width*sizeof(double), Src, srcLineWidth, width, height);
end;

function GenericMtxCopy(const Src : Array of double; width, height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (length(src) = width*height), 'Dimension error');
     SetLength(Result, width*height);
     GenericMtxCopy(@Result[0], width*sizeof(double), @Src[0], width*sizeof(double), width, height);
end;

procedure GenericMtxIndex( dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; colIdx, rowIdx : TIntegerDynArray);
var pDest : PConstDoubleArr;
    pSrc : PConstDoubleArr;
    x, y : NativeInt;
begin
     pDest := PConstDoubleArr(dest);
     for y := 0 to Length(rowIdx) - 1 do
     begin
          pSrc := PConstDoubleArr( GenPtrArr(src, 0, rowIdx[y], srcLineWidth) );

          for x := 0 to Length(colIdx) - 1 do
              pDest^[x] := pSrc^[colIdx[x]];

          inc( PByte(pDest), destLineWidth );
     end;
end;

procedure GenericColSwap(A, B : PDouble; const LineWidthAB : NativeInt; Height : NativeInt);
var tmp : double;
    i : integer;
begin
     for i := 0 to Height - 1 do
     begin
          tmp := A^;
          A^ := B^;
          B^ := tmp;

          inc(PByte(A), LineWidthAB);
          inc(PByte(B), LineWidthAB);
     end;
end;


procedure GenericRowSwap(A, B : PDouble; width : NativeInt);
var i : NativeInt;
    tmp : double;
    pA, pB : PConstDoubleArr;
begin
     pA := PConstDoubleArr(A);
     pB := PConstDoubleArr(B);
     for i := 0 to width - 1 do
     begin
          tmp := pA^[i];
          pA^[i] := pB^[i];
          pB^[i] := tmp;
     end;
end;

function GenericMtxMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := mt;

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Max(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := y to N - 1 do
              Result := Max(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := y to N - 1 do
              Result := Min(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := 0 to y do
              Result := Max(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := 0 to y do
              Result := Min(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxAbsMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := Abs(mt^);

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := y to N - 1 do
              Result := Max(Result, Abs(PConstDoubleArr(pMt)^[x]));

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxAbsMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := abs(mt^);

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := y to N - 1 do
              Result := Min(Result, Abs(PConstDoubleArr(pMt)^[x]));

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxAbsMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := abs(mt^);

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := 0 to y do
              Result := Max(Result, Abs(PConstDoubleArr(pMt)^[x]));

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxAbsMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
var x, y : NativeInt;
    pMt : PDouble;
begin
     assert((N > 0) and (N*sizeof(double) <= LineWidth), 'Dimension error');
     Result := abs(mt^);

     pMt := mt;

     for y := 0 to N - 1 do
     begin
          for x := 0 to y do
              Result := Min(Result, Abs(PConstDoubleArr(pMt)^[x]));

          inc(PByte(pMt), LineWidth);
     end;
end;


procedure GenericMtxElemAdd(Dest : PDouble; LineWidth, Width, Height : NativeInt; const Offset : double);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstDoubleArr(dest)^[x] := PConstDoubleArr(dest)^[x] + Offset;

          inc(PByte(dest), LineWidth);
     end;
end;


procedure GenericMtxAddAndScale(Dest : PDouble; LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstDoubleArr(dest)^[x] := (PConstDoubleArr(dest)^[x] + Offset)*Scale;

          inc(PByte(dest), LineWidth);
     end;
end;

procedure GenericMtxScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstDoubleArr(dest)^[x] := (PConstDoubleArr(dest)^[x]*Scale) + Offset;

          inc(PByte(dest), LineWidth);
     end;
end;

function GenericMtxMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
var x, y : NativeInt;
    pMt : PByte;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := PByte(mt);

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Min(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxAbsMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
var x, y : NativeInt;
    pMt : PByte;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');
     Result := Abs(mt^);

     pMt := PByte(mt);

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Max(Result, Abs(PConstDoubleArr(pMt)^[x]));

          inc(PByte(pMt), LineWidth);
     end;
end;

function GenericMtxAbsMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
var x, y : NativeInt;
    pMt : PByte;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');
     Result := abs(mt^);

     pMt := PByte(mt);

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Min(Result, Abs(PConstDoubleArr(pMt)^[x]));

          inc(PByte(pMt), LineWidth);
     end;
end;

procedure GenericMtxMaxVal(dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; maxVal : double);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     pLine := PConstDoubleArr(dest);
     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              if pLine^[x] < maxVal then
                 pLine^[x] := maxVal;

          inc(PByte(pLine), LineWidth);
     end;
end;

procedure GenericMtxMinVal(dest : PDouble; const LineWidth : NativeInt; width, height : NativeInt; minVal : double);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     pLine := PConstDoubleArr(dest);
     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              if pLine^[x] > minVal then
                 pLine^[x] := minVal;

          inc(PByte(pLine), LineWidth);
     end;
end;

procedure GenericVecMin( dest : PDouble; mt : PDouble; n : NativeInt );
var i : integer;
begin
     for i := 0 to n - 1 do
         PConstDoubleArr(dest)^[i] := Min(PConstDoubleArr(dest)^[i], PConstDoubleArr(mt)^[i]);
end;

procedure GenericVecMax( dest : PDouble; mt : PDouble; n : NativeInt );
var i : integer;
begin
     for i := 0 to n - 1 do
         PConstDoubleArr(dest)^[i] := Max(PConstDoubleArr(dest)^[i], PConstDoubleArr(mt)^[i]);
end;

procedure GenericMtxAdd(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstDoubleArr(dest)^[x] := PConstDoubleArr(mt1)^[x] + PConstDoubleArr(mt2)^[x];

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;


function GenericMtxAdd(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt;
                       const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     GenericMtxAdd(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function GenericMtxAdd(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := GenericMtxAdd(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;
procedure GenericMtxAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');
     
     GenericMtxAdd(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure GenericMtxSub(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstDoubleArr(dest)^[x] := PConstDoubleArr(mt1)^[x] - PConstDoubleArr(mt2)^[x];

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;

function GenericMtxSub(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt;
                   const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     GenericMtxSub(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function GenericMtxSub(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := GenericMtxSub(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure GenericMtxSub(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     GenericMtxSub(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure GenericSubVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
var pB, pA : PConstDoubleArr;
    pB2 : PDouble;
    y : Integer;
    x : Integer;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');

     if incX = sizeof(Double) then
     begin
          pB := PConstDoubleArr(B);
          pA := PConstDoubleArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to Width - 1 do
                        pA^[x] := pA^[x] - pB^[x];

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := pA^[x] - pB^[y];

                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end
     else
     begin
          pA := PConstDoubleArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    pB2 := PDouble(B);
                    for x := 0 to Width - 1 do
                    begin
                         pA^[x] := pA^[x] - pB2^;
                         inc(PByte(pB2), incX);
                    end;

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               pB2 := PDouble(B);
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := pA^[x] - pB2^;

                    inc(PByte(pB2), incX);
                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end;
end;

procedure GenericAddVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
var pB, pA : PConstDoubleArr;
    pB2 : PDouble;
    y : Integer;
    x : Integer;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');

     if incX = sizeof(Double) then
     begin
          pB := PConstDoubleArr(B);
          pA := PConstDoubleArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to Width - 1 do
                        pA^[x] := pA^[x] + pB^[x];

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := pA^[x] + pB^[y];

                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end
     else
     begin
          pA := PConstDoubleArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    pB2 := PDouble(B);
                    for x := 0 to Width - 1 do
                    begin
                         pA^[x] := pA^[x] + pB2^;
                         inc(PByte(pB2), incX);
                    end;

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               pB2 := PDouble(B);
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := pA^[x] + pB2^;

                    inc(PByte(pB2), incX);
                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end;
end;

function GenericMtxMult(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     SetLength(Result, Height1*width2);
     GenericMtxMult(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure GenericMtxMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
var x, y, idx : NativeInt;
    destOffset : NativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     destOffset := destLineWidth - Width2*sizeof(double);
     assert((destOffset >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     for y := 0 to Height1 - 1 do
     begin
          for x := 0 to Width2 - 1 do
          begin
               dest^ := 0;
               valCounter1 := PConstDoubleArr(mt1);
               valCounter2 := mt2;
               for idx := 0 to width1 - 1 do
               begin
                    dest^ := dest^ + valCounter1^[idx]*valCounter2^;
                    inc(PByte(valCounter2), LineWidth2);
               end;

               inc(mt2);
               inc(dest);
          end;
          dec(mt2, Width2);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(dest), destOffset);
     end;
end;

procedure GenericMtxMultTransp(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
var x, y, idx : NativeInt;
    pMt2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');
     assert(destLineWidth - height2*sizeof(double) >= 0, 'Destination width error');

     for y := 0 to Height1 - 1 do
     begin
          pDest := dest;
          pMt2 := mt2;
          for x := 0 to height2 - 1 do
          begin
               pDest^ := 0;
               for idx := 0 to width1 - 1 do
                   pDest^ := pDest^ + PConstDoubleArr(mt1)^[idx]*PConstDoubleArr(pMt2)^[idx];

               inc(PByte(pMt2), LineWidth2);
               inc(pDest);
          end;
          inc(PByte(mt1), LineWidth1);
          inc(PByte(dest), destLineWidth);
     end;
end;

function GenericVecDotMult( x : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt ) : double;
var pXa, pYa : PConstDoubleArr;
    i : NativeInt;
begin
     if (incX = sizeof(double)) and (incY = sizeof(double)) then
     begin
          pXA := PConstDoubleArr(X);
          pYA := PConstDoubleArr(Y);

          Result := 0;
          for i := 0 to N - 1 do
              Result := result + pXA^[i]*pYA^[i];
     end
     else
     begin
          Result := 0;

          for i := 0 to N - 1 do
          begin
               Result := Result + x^*y^;
               inc(PByte(x), incX);
               inc(PByte(y), incY);
          end;
     end;
end;

procedure GenericVecAdd( X : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt; const alpha : double );
var pXa, pYa : PConstDoubleArr;
    i : NativeInt;
begin
     if (incX = sizeof(double)) and (incY = sizeof(double)) then
     begin
          pXA := PConstDoubleArr(X);
          pYA := PConstDoubleArr(Y);

          for i := 0 to N - 1 do
              pYA^[i] := pYA^[i] + alpha*pXA^[i];
     end
     else
     begin
          for i := 0 to N - 1 do
          begin
               y^ := y^ + alpha*x^;
               inc(PByte(x), incX);
               inc(PByte(y), incY);
          end;
     end;
end;

// performs dest = mt1'*mt2
procedure GenericTranspMtxMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (height2 = height1), 'Dimension error');

     for y := 0 to width1 - 1 do
     begin
          pDest := dest;
          for x := 0 to Width2 - 1 do
          begin
               pDest^ := 0;
               valCounter1 := mt1;
               valCounter2 := mt2;
               for idx := 0 to height2 - 1 do
               begin
                    pDest^ := pDest^ + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               inc(mt2);
               inc(pDest);
          end;

          dec(mt2, Width2);
          inc(mt1);

          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericTranspMtxMultAdd(dest : PDouble; const destLineWidth : NativeInt;
  mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; C : PDouble; LineWidthC : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    pDest : PDouble;
    pC : PDouble;
    addVal : Double;
begin
     assert((width1 > 0) and (height1 > 0) and (height2 = height1), 'Dimension error');

     for y := 0 to width1 - 1 do
     begin
          pC := C;
          pDest := dest;
          for x := 0 to Width2 - 1 do
          begin
               if C <> nil
               then
                   addVal := pC^
               else
                   addVal := 0;

               pDest^ := 0;
               valCounter1 := mt1;
               valCounter2 := mt2;
               for idx := 0 to height2 - 1 do
               begin
                    pDest^ := pDest^ + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := pDest^ + addVal;
               inc(mt2);
               inc(pDest);
               inc(pC);
          end;

          dec(mt2, Width2);
          inc(mt1);

          inc(PByte(dest), destLineWidth);
          inc(PByte(C), LineWidthC);
     end;
end;

// performs dest = alpha*A*B' + C
procedure GenericMtxMultTranspAdd(dest : PDouble; const destLineWidth : NativeInt; const alpha : double;
  mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; C : PDouble; LineWidthC : NativeInt);
var x, y, idx : NativeInt;
    pDest : PDouble;
    pC : PDouble;
    addVal : Double;
    pMT2 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');

     for y := 0 to height1 - 1 do
     begin
          pC := C;
          pDest := dest;
          pMt2 := mt2;
          for x := 0 to height2 - 1 do
          begin
               addVal := pC^;

               pDest^ := 0;
               for idx := 0 to width2 - 1 do
                   pDest^ := pDest^ + PConstDoubleArr(mt1)^[idx]*PConstDoublearr(pMt2)^[idx];

               pDest^ := alpha*pDest^ + addVal;
               inc(PByte(PMt2), LineWidth2);
               inc(pDest);
               inc(pC);
          end;

          inc(PByte(mt1), LineWidth1);

          inc(PByte(dest), destLineWidth);
          inc(PByte(C), LineWidthC);
     end;
end;

function GenericMtxMult(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, height1*width2);
     GenericMtxMult(@Result[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure GenericMtxMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');

     GenericMtxMult(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure GenericMtxVecMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
var pMt1, pMt11, pMt12, pMt13 : PConstDoubleArr;
    pV : PDouble;
    i, j : NativeInt;
    res1, res2, res3, res4 : double;
begin
     // loop unrolled 4 times
     if (width = 0) or (height = 0) or ( (alpha = 0) and (beta = 1)) then
        exit;
     for i := 0 to Height div 4 - 1 do
     begin
          res1 := 0;
          res2 := 0;
          res3 := 0;
          res4 := 0;

          pMt1 := PConstDoubleArr(mt1);
          inc(PByte(Mt1), LineWidthMT);
          pMt11 := PConstDoubleArr(mt1);
          inc(pByte(MT1), LineWidthMT);
          pMt12 := PConstDoubleArr(mt1);
          inc(pByte(MT1), LineWidthMT);
          pMt13 := PConstDoubleArr(mt1);
          inc(pByte(MT1), LineWidthMT);

          pV := v;
          for j := 0 to Width - 1 do
          begin
               res1 := res1 + pV^*pMt1^[j];
               res2 := res2 + pV^*pMt11^[j];
               res3 := res3 + pV^*pMt12^[j];
               res4 := res4 + pV^*pMt13^[j];

               inc(PByte(pV), LineWidthV);
          end;

          dest^ := beta*dest^ + alpha*res1;
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res2;
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res3;
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res4;
          inc(PByte(dest), destLineWidth);
     end;

     // do the rest
     for i := (height div 4)*4 to Height - 1 do
     begin
          pMt1 := PConstDoubleArr(mt1);
          inc(pByte(MT1), LineWidthMT);

          res1 := 0;
          pV := v;
          for j := 0 to Width - 1 do
          begin
               res1 := res1 + pV^*pMt1^[j];
               inc(PByte(pV), LineWidthV);
          end;

          dest^ := beta*dest^ + alpha*res1;
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxVecMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
var pMt1 : PConstDoubleArr;
    pV : PDouble;
    i, j : NativeInt;
    res : Array[0..15] of double;
    res1 : double;
begin
     if (width = 0) or (height = 0) or ( (alpha = 0) and (beta = 1)) then
        exit;

     for i := 0 to Width div Length(res) - 1 do
     begin
          for j := 0 to High(res) do
              res[j] := 0;

          pMt1 := PConstDoubleArr(mt1);
          inc(Mt1, Length(res));
          pV := v;

          for j := 0 to Height - 1 do
          begin
               res[0] := res[0] + pV^*pMt1^[0];
               res[1] := res[1] + pV^*pMt1^[1];
               res[2] := res[2] + pV^*pMt1^[2];
               res[3] := res[3] + pV^*pMt1^[3];
               res[4] := res[4] + pV^*pMt1^[4];
               res[5] := res[5] + pV^*pMt1^[5];
               res[6] := res[6] + pV^*pMt1^[6];
               res[7] := res[7] + pV^*pMt1^[7];
               res[8] := res[8] + pV^*pMt1^[8];
               res[9] := res[9] + pV^*pMt1^[9];
               res[10] := res[10] + pV^*pMt1^[10];
               res[11] := res[11] + pV^*pMt1^[11];
               res[12] := res[12] + pV^*pMt1^[12];
               res[13] := res[13] + pV^*pMt1^[13];
               res[14] := res[14] + pV^*pMt1^[14];
               res[15] := res[15] + pV^*pMt1^[15];

               inc(PByte(pV), LineWidthV);
               inc(PByte(pMt1), LineWidthMT);
          end;

          dest^ := beta*dest^ + alpha*res[0];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[1];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[2];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[3];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[4];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[5];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[6];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[7];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[8];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[9];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[10];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[11];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[12];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[13];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[14];
          inc(PByte(dest), destLineWidth);
          dest^ := beta*dest^ + alpha*res[15];
          inc(PByte(dest), destLineWidth);
     end;

     // remainding columns
     for i := (width div Length(res))*Length(res) to Width - 1 do
     begin
          res1 := 0;

          pMt1 := PConstDoubleArr( mt1 );
          inc(Mt1);
          pV := v;

          for j := 0 to Height - 1 do
          begin
               res1 := res1 + pV^*pMt1^[0];

               inc(PByte(pV), LineWidthV);
               inc(PByte(pMt1), LineWidthMT);
          end;

          Dest^ := beta*Dest^ + alpha*res1;
          inc(PByte(Dest), destLineWidth);
     end;
end;

// performs matrix vector multiplication in the form: dest := alpha*mt1*v + beta*dest
// where the matrix mt1 is a symmetric matrix and only the upper part is touched in the multiplication
// the procedure tries to minimize the multiplications by accumulating the results in the resulting vector
procedure GenericMtxVecMultUpperSym( dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
var y, x: NativeInt;
    res : double;
    pMt1 : PConstDoubleArr;
    pV : PDouble;
    pD : PDouble;
    tmp : double;
begin
     pMt1 := PConstDoubleArr(mt1);

     if beta <> 1 then
     begin
          pD := dest;
          if beta = 0 then
          begin
               for y := 0 to N - 1 do
               begin
                    pD^ := 0;
                    inc(PByte(pD), destLineWidth);
               end;
          end
          else
          begin
               for y := 0 to N - 1 do
               begin
                    pD^ := beta*pD^;
                    inc(PByte(pD), destLineWidth);
               end;
          end;
     end;

     for y := 0 to N - 1 do
     begin
          pV := V;

          // calculate one destination element and accumulate the on the others
          pD := dest;
          tmp := alpha*pV^;
          res := pMt1^[y]*pV^;

          inc(PByte(pD), destLineWidth);
          inc(PByte(pV), LineWidthV);

          for x := y + 1 to N - 1 do
          begin
               res := res + pV^*pMt1^[x];
               pD^ := pD^ + tmp*pMt1^[x];
               inc(PByte(pD), destLineWidth);
               inc(PByte(pV), LineWidthV);
          end;

          Dest^ := Dest^ + alpha*res;
          inc(PByte(Dest), destLineWidth);
          inc(PByte(pMt1), LineWidthMT);
          inc(PByte(V), LineWidthV);
     end;
end;

(*
procedure GenericMtxVecMultUpperSym1( dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
var y, y1, x: NativeInt;
    res : double;
    pMt1 : PConstDoubleArr;
    pM : PDouble;
    pV : PDouble;
begin
     pMt1 := PConstDoubleArr(mt1);

     // works
     pMt1 := PConstDoubleArr(mt1);
     for y := 0 to N - 1 do
     begin
          res := 0;
          pV := V;

          // simulate the lower part
          pM := GenPtr(mt1, y, 0, LineWidthMT);
          for y1 := 0 to y - 1 do
          begin
               res := res + pV^*pM^;
               inc(PByte(pM), LineWidthMT);
               inc(PByte(pV), LineWidthV);
          end;

          // standard mult
          for x := y to N - 1 do
          begin
               res := res + pV^*pMt1^[x];
               inc(PByte(pV), LineWidthV);
          end;

          Dest^ := beta*Dest^ + alpha*res;
          inc(PByte(Dest), destLineWidth);
          inc(PByte(pMt1), LineWidthMT);
     end;
end;
*)

// performs matrix vector multiplication in the form: dest := alpha*mt1*v + beta*dest
// where the matrix mt1 is a symmetric matrix and only the upper part is touched in the multiplication
procedure GenericMtxVecMultLowerSym( dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
var y, y1, x: NativeInt;
    res : double;
    pMt1 : PConstDoubleArr;
    pM : PDouble;
    pV : PDouble;
begin
     pMt1 := PConstDoubleArr(mt1);
     for y := 0 to N - 1 do
     begin
          res := 0;
          pV := V;

          // simulate symmetric part
          for x := 0 to y do
          begin
               res := res + pV^*pMt1^[x];
               inc(PByte(pV), LineWidthV);
          end;

          // simulate the lower part
          pM := GenPtr(mt1, y, y + 1, LineWidthMT);
          for y1 := y + 1 to N - 1 do
          begin
               res := res + pV^*pM^;
               inc(PByte(pM), LineWidthMT);
               inc(PByte(pV), LineWidthV);
          end;


          Dest^ := beta*Dest^ + alpha*res;
          inc(PByte(Dest), destLineWidth);
          inc(PByte(pMt1), LineWidthMT);
     end;
end;

procedure GenericMtxElemMult(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');
     assert((width*sizeof(double) <= destLineWidth), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstDoubleArr(dest)^[x] := PConstDoubleArr(mt1)^[x]*PConstDoubleArr(mt2)^[x];

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;

function GenericMtxElemMult(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');

     SetLength(Result, width*Height);
     GenericMtxElemMult(@Result[0], width*sizeof(double), mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure GenericMtxElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     GenericMtxElemMult(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxElemMult(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');

     Result := GenericMtxElemMult(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

procedure GenericMtxElemDiv(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');
     assert((width*sizeof(double) <= destLineWidth), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstDoubleArr(dest)^[x] := PConstDoubleArr(mt1)^[x]/PConstDoubleArr(mt2)^[x];

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;

function GenericMtxElemDiv(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');

     SetLength(Result, width*Height);
     GenericMtxElemMult(@Result[0], width*sizeof(double), mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure GenericMtxElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     GenericMtxElemMult(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxElemDiv(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');

     Result := GenericMtxElemMult(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxTranspose(mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');

     SetLength(Result, width*height);
     GenericMtxTranspose(@Result[0], sizeof(double)*height, mt, LineWidth, width, height);
end;

procedure GenericMtxTranspose(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
var valCounter : PDouble;
    y : NativeInt;
    x : NativeInt;
    pMt : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(destLineWidth >= height*sizeof(double), 'Line width does not match');

     for y := 0 to Height - 1 do
     begin
          valCounter := dest;
          pMt := PConstDoubleArr(mt);

          for x := 0 to Width - 1 do
          begin
               valCounter^ := pMt^[x];
               inc(PByte(valCounter), destLineWidth);
          end;
          inc(dest);
          inc(PByte(mt), LineWidth);
     end;
end;

function GenericMtxTranspose(const mt : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(Length(mt) >= width*height, 'Dimension Error');

     Result := GenericMtxTranspose(@mt[0], width*sizeof(double), width, height);
end;

procedure GenericMtxTranspose(var dest : Array of Double; const mt : Array of Double; width : NativeInt; height : NativeInt); overload;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(Length(mt) >= width*height, 'Dimension Error');
     assert(Length(dest) >= width*height, 'Dimension Error');

     GenericMtxTranspose(@dest[0], height*sizeof(double), @mt[0], width*sizeof(double), width, height );
end;

procedure GenericMtxTransposeInplace(dest : PDouble; const destLineWidth : NativeInt; n : NativeInt);
var x, y : NativeInt;
    pDest : PConstDoubleArr;
    pDest1 : PDouble;
    tmp : double;
begin
     assert((n > 0), 'Dimension Error');

     for y := 0 to n - 2 do
     begin
          pDest := PConstDoubleArr( GenPtr(dest, 0, y, destLineWidth) );
          pDest1 := GenPtr(dest, y, y + 1, destLineWidth);
          
          for x := y + 1 to n - 1 do
          begin
               tmp := pDest^[x];
               pDest^[x] := pDest1^;
               pDest1^ := tmp;
               inc(PByte(pDest1), destLineWidth);
          end;
     end;
end;

procedure GenericMtxSqrt(dest : PDouble; destLineWidth : NativeInt; width, height : NativeInt);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              pLine^[x] := sqrt(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxAbs(dest : PDouble; destLineWidth, width, height : NativeInt);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              pLine^[x] := abs(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixObjFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := 0 to width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefObjFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := 0 to width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     inc(PByte(dest), startY*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := startX to startX + width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixObjFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(double) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     inc(PByte(dest), startY*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := startX to startX + width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixMtxRefFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(double) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixMtxRefObjFunc);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(double) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

{$IFDEF ANONMETHODS}

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFuncRef);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFuncRef);
var x, y : NativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := 0 to width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(double) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     inc(PByte(dest), startY*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := startX to startX + width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PDouble; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(double) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

{$ENDIF}

// ###########################################
// #### Blocked Matrix multiplcation + Strassen algorithm

procedure GenericMtxDeltaUpdate(dest : PDouble; destLineWidth : NativeInt; A11, B11 : PDouble;
  width, height, LineWidth1 : NativeInt);
var x, y : NativeInt;
    pDest : PDouble;
    pB11 : PDouble;
begin
     for y := 0 to height - 1 do
     begin
          pDest := dest;
          pB11 := B11;

          for x := 0 to width - 1 do
          begin
               pDest^ := pDest^ + a11^*pB11^;
               inc(pB11);
               inc(pDest);
          end;

          inc(PByte(A11), LineWidth1);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure InternalGenericStrassenMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt;
  width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; mem : PDouble);
var a11, a12, a21, a22 : PDouble;
    b11, b12, b21, b22 : PDouble;
    s1, s2, s3, s4 : PDouble;
    t1, t2, t3, t4 : PDouble;
    P1, P2, P3, P4, P5, P6, P7 : PDouble;
    U1, U2, U3, U4, U5, U6, U7 : PDouble;
    c11, c12, c21, c22 : PDouble;
    k, m, n : NativeInt;
    lineK : NativeInt;
    lineN : NativeInt;
    x, y : PDouble;
begin
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (width2 <= cStrassenMinSize)
     then
         GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else
     begin
          k := width1 div 2;
          m := height1 div 2;
          n := width2 div 2;
          
          lineK := k*sizeof(double);
          lineN := n*sizeof(double);

          a11 := mt1;
          a12 := a11;
          inc(a12, k);
          a21 := mt1;
          inc(PByte(a21), m*LineWidth1);
          a22 := a21;
          inc(a22, k);

          b11 := mt2;
          b12 := b11;
          inc(b12, n);
          b21 := mt2;
          inc(PByte(b21), k*LineWidth2);
          b22 := b21;
          inc(b22, n);

          c11 := dest;
          c12 := c11;
          inc(c12, n);
          c21 := dest;
          inc(PByte(c21), m*destLineWidth);
          c22 := c21;
          inc(c22, n);

          x := mem;
          y := x;
          inc(Y, m*Max(k, n));

          S3 := X;
          T3 := Y;
          P7 := C21;
          S1 := X;
          T1 := Y;
          P5 := C22;
          S2 := X;
          T2 := Y;
          P6 := C12;
          S4 := X;
          P3 := C11;
          P1 := X;
          U2 := C12;
          U3 := C21;
          U4 := C12;
          U7 := C22;
          U5 := C12;
          T4 := Y;
          P4 := C11;
          U6 := C21;
          P2 := C11;
          U1 := C11;

          mem := Y;
          inc(mem, k*n);

          // memory efficient mult:

          // s3 = A11 - A21
          GenericMtxSub(s3, lineK, a11, a21, k, m, LineWidth1, LineWidth1);
          // t3 = B22 - B12
          GenericMtxSub(t3, lineN, B22, B12, n, k, LineWidth2, LineWidth2);
          // p7 = s3*t3
          InternalGenericStrassenMult(p7, destLineWidth, s3, t3, k, m, n, k, lineK, lineN, mem);
          // s1 = a21 + a22
          GenericMtxAdd(s1, lineK, a21, a22, k, m, LineWidth1, LineWidth1);
          // t1 = b12 - b11
          GenericMtxSub(t1, lineN, B12, B11, n, k, LineWidth2, LineWidth2);
          // p5 = s1*t1
          InternalGenericStrassenMult(p5, destLineWidth, s1, t1, k, m, n, k, lineK, lineN, mem);
          // s2 = S1 - A11
          GenericMtxSub(s2, lineK, S1, A11, k, m, lineK, LineWidth1);
          // t2 = b22 - t1
          GenericMtxSub(t2, lineN, B22, t1, n, k, LineWidth2, lineN);
          // p6 = s2*t2
          InternalGenericStrassenMult(p6, destLineWidth, s2, t2, k, m, n, k, lineK, lineN, mem);
          // s4 = A12 - S2
          GenericMtxSub(s4, lineK, A12, S2, k, m, LineWidth1, lineK);
          // p3 = s4*b22
          InternalGenericStrassenMult(p3, destLineWidth, s4, b22, k, m, n, k, lineK, LineWidth2, mem);
          // p1 = A11*B11
          InternalGenericStrassenMult(p1, lineN, A11, B11, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U2 = P1 + P6
          GenericMtxAdd(U2, destLineWidth, P1, P6, n, m, LineN, destLineWidth);
          // U3 = U2 + P7
          GenericMtxAdd(U3, destLineWidth, U2, P7, n, m, destLineWidth, destLineWidth);
          // U4 = U2 + P5
          GenericMtxAdd(U4, destLineWidth, U2, P5, n, m, destLineWidth, destLineWidth);
          // U7 = U3 + P5
          GenericMtxAdd(U7, destLineWidth, U3, P5, n, m, destLineWidth, destLineWidth);
          // U5 = U4 + P3
          GenericMtxAdd(U5, destLineWidth, U4, P3, n, m, destLineWidth, destLineWidth);
          // t4 = T2 - B21
          GenericMtxSub(t4, lineN, T2, B21, n, k, LineN, LineWidth2);
          // p4 = A22*t4
          InternalGenericStrassenMult(p4, destLineWidth, A22, t4, k, m, n, k, LineWidth1, lineN, mem);
          // U6 = U3 - P4
          GenericMtxSub(U6, destLineWidth, U3, P4, n, m, destLineWidth, destLineWidth);
          // p2 = A12*B21
          InternalGenericStrassenMult(p2, destLineWidth, A12, B21, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U1 = P1 + P2
          GenericMtxAdd(U1, destLineWidth, P1, P2, n, m, lineN, destLineWidth);

          // tidy up work for uneven columns, rows....
          if ((width1 and $01) > 0) or ((height1 and $01) > 0) or ((width2 and $01) > 0) then
          begin
               // following the algorithm if all items are odd...:
               //
               //  A*B = [A1   ac    ][B1  bc  ] = [A1*B1   0]  +  Delta
               //        [ar'  alpha ][br' beta]   [  0     0]
               //
               // Delta = [ac   ]*[br' beta]   +   [  0     A1*bc]
               //         [alpha]                  [ar'*B1  ar'*bc]

               // we already have computed A1*B1...

               if ((width1 and $01) = 0) and ((width2 and $01) = 0) then
               begin
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, 1, width2, height2, LineWidth1, LineWidth2);
               end
               else if ((width1 and $01) = 0) and ((height1 and $01) = 0) then
               begin
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, height1, 1, height2, LineWidth1, LineWidth2);
               end
               else if ((height1 and $01) = 0) and ((width2 and $01) = 0) then
               begin
                    inc(A11, width1 - 1);
                    inc(PByte(B11), LineWidth2*(height2 - 1));
                    GenericMtxDeltaUpdate(dest, destLineWidth, A11, B11, width2, height1, LineWidth1);
               end
               else if ((width1 and $01) = 0) and ((height1 and $01) > 0) and ((width2 and $01) > 0) then
               begin
                    // last column [A]*bc
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, height1 - 1, 1, height2, LineWidth1, LineWidth2);
                    dec(B11, (width2 - 1));
                    dec(dest, (width2 - 1));

                    // [ar alpha]*[B bc]  (last line)
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, 1, width2, height2, LineWidth1, LineWidth2);
               end
               else
               begin
                    // all dimensions are odd!
                    // calc A1*bc
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, height1 - 1, 1, height2 - 1, LineWidth1, LineWidth2);
                    dec(B11, (width2 - 1));
                    dec(dest, (width2 - 1));

                    // calc ar'*B1 and ar'*bc
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, 1, width2, height2 - 1, LineWidth1, LineWidth2);

                    inc(dest, width2 - 1);
                    inc(B11, width2 - 1);
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, 1, 1, height2 - 1, LineWidth1, LineWidth2);

                    dec(dest, width2 - 1);
                    dec(PByte(dest), destLineWidth*(height1 - 1));
                    dec(PByte(A11), LineWidth1*(height1 - 1));
                    dec(B11, width2 - 1);

                    // last step is to add the vector product matrix to the existing sum...
                    inc(A11, width1 - 1);
                    inc(PByte(B11), LineWidth2*(height2 - 1));
                    GenericMtxDeltaUpdate(dest, destLineWidth, A11, B11, width2, height1, LineWidth1);
               end;
          end;
     end;
end;

procedure GenericStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var mem : PDouble;
    memSize : NativeInt;
    m, k, n : NativeInt;
    lev : NativeInt;
begin
     // check the cutoff criterion:
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (height2 <= cStrassenMinSize)
     then
         GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else
     begin
          // calc the complete used additionaly memory
          // calc the complete used additionaly memory
          memSize := 0;
          m := height1;
          k := width1;
          n := width2;
          lev := Min(m, Min(k, n));

          while lev > cStrassenMinSize do
          begin
               memSize := memSize + sizeof(double)*(m*max(k, n) + k*n);
               k := k shr 1;
               m := m shr 1;
               n := n shr 1;
               lev := lev shr 1;
          end;

          mem := GetMemory(memSize);
          try
             InternalGenericStrassenMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, mem);
          finally
                 FreeMem(mem);
          end;
     end;
end;


// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure GenericMtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var y: Integer;
    x : Integer;
    i : integer;
    pDest : PConstDoubleArr;
    pMt1 : PConstDoubleArr;
    pMt2 : PConstDoubleArr;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error'); 
     assert(width1 = height2, 'Width1 <> height1');
     for y := 0 to height1 - 1 do
     begin
          pDest := PConstDoubleArr( GenPtr(dest, 0, y, LineWidthDest ) );
          pMt1 :=  PConstDoubleArr( GenPtr(mt1, 0, y, LineWidth1 ) );
          for x := 0 to height2 - 1 do
          begin
               pMt2 := PConstDoubleArr( GenPtr(mt2, 0, x, LineWidth2 ) );
               pDest^[x] := pMt1^[x];
               for i := x + 1 to width1 - 1 do
                   pDest^[x] := pDest^[x] + pMt1^[i]*pMt2^[i];
          end;
     end;
end;

// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure GenericMtxMultTria2T1Lower(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    tmp : double;
    pMt2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');

     for x := 0 to width1 - 1 do
     begin
          pMT2 := mt2;
          pDest := dest;
          for y := 0 to width2 - 1 do
          begin
               valCounter1 := mt1;
               valCounter2 := pMT2;
               inc(PByte(valCounter2), (y + 1)*LineWidth2);
               inc(PByte(valCounter1), (y)*LineWidth1);
               tmp := valCounter1^;
               inc(PByte(valCounter1), LineWidth1);
               for idx := 1 to height2 - y - 1 do
               begin
                    tmp := tmp + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := tmp;
               inc(pDest);
               inc(pMT2);
          end;
          inc(mt1);
          inc(PByte(dest), LineWidthDest);
     end;
end;

// note the result is stored in mt2 again!
// dest = mt1*mt2; where mt2 is a upper triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure GenericMtxMultTria2_1Upper(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    tmp : double;
    pMt2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');

     for x := 0 to width1 - 1 do
     begin
          pMT2 := mt2;
          pDest := dest;
          for y := 0 to width2 - 1 do
          begin
               valCounter1 := mt1;
               valCounter2 := pMT2;
               inc(PByte(valCounter2), (y + 1)*LineWidth2);
               inc(PByte(valCounter1), (y)*LineWidth1);
               tmp := valCounter1^;
               inc(PByte(valCounter1), LineWidth1);
               for idx := 1 to height2 - 1 do
               begin
                    tmp := tmp + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := tmp;
               inc(pDest);
               inc(pMT2);
          end;
          inc(mt1);
          inc(PByte(dest), LineWidthDest);
     end;
end;

// same as the above function but utilizes lines better (2 double values written in a line)
procedure GenericMtxMultTria2T1_2(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt;
  mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PConstDoubleArr;
    tmp : Array[0..1] of double;
    pMt2 : PDouble;
    pDest : PDouble;
    width2D2 : NativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');

     width2D2 := width2 div 2;

     for x := 0 to width1 - 1 do
     begin
          pMT2 := mt2;
          pDest := dest;
          for y := 0 to width2D2 - 1 do
          begin
               valCounter1 := PConstDoubleArr(mt1);
               valCounter2 := PConstDoubleArr(pMT2);
               inc(PByte(valCounter2), (2*y + 1)*LineWidth2);
               inc(PByte(valCounter1), 2*y*LineWidth1);

               tmp[0] := valCounter1^[0];
               inc(PByte(valCounter1), LineWidth1);

               // second line: second element of mt2 is 1!
               if height2 > 2*y + 1 then
               begin
                    tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
                    tmp[1] := valCounter1^[0];

                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               // rest is a double column!
               for idx := 2 to height2 - 2*y - 1 do
               begin
                    tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
                    tmp[1] := tmp[1] + valCounter1^[0]*valCounter2^[1];
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := tmp[0];
               PDouble(NativeUint(pDest) + sizeof(double))^ := tmp[1];
               inc(pDest, 2);
               inc(pMT2, 2);
          end;

          if (width2 and $01) = 1 then
          begin
               // special handling of last column (just copy the value)
               valCounter1 := PConstDoubleArr(mt1);
               inc(PByte(valCounter1), LineWidth1*(height1 - 1));
               pDest^ := valCounter1^[0];
          end;

          inc(mt1);
          inc(PByte(dest), LineWidthDest);
     end;
end;

// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure GenericMtxMultRightUpperTriaNoUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PDouble;
    tmp : double;
    pMt1 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     assert(width1 = width2, 'Widths need to be the same');
     // start from the back
     inc(mt2, width2 - 1);
     for x := 0 to width2 - 1 do
     begin
          pmt1 := mt1;
          for y := 0 to height1 - 1 do
          begin
               tmp := 0;
               valCounter1 := PConstDoubleArr( pmt1 );
               valCounter2 := MT2;

               for idx := 0 to width1 - x - 1 do
               begin
                    tmp := tmp + valCounter1^[idx]*valCounter2^;
                    inc(PByte(valCounter2), LineWidth2);
               end;

               PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
               inc(PByte(pmT1), LineWidth1);
          end;

          dec(mt2);
     end;
end;

// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure GenericMtxMultRightUpperTriaUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var y: Integer;
    x : Integer;
    i : integer;
    pMt1 : PConstDoubleArr;
    pMt2 : PDouble;
    tmp : double;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error'); 
     assert(width1 = width2, 'Widths need to be the same');
     for y := 0 to height1 - 1 do
     begin
          pMt1 :=  PConstDoubleArr( GenPtr(mt1, 0, y, LineWidth1 ) );
          for x := width2 - 1 downto 1 do
          begin
               pMt2 := GenPtr(mt2, x, 0, LineWidth2 );
               tmp := 0;
               for i := 0 to x - 1 do
               begin
                    tmp := tmp + pMt1^[i]*pMt2^;
                    inc(PByte(pMt2), LineWidth2);
               end;

               pMt1^[x] := tmp + pMt1^[x]; // pMt2 is unit at that point
          end;
     end;
end;

// performs mt1 = mt1*mt2 where mt2 is an lower triangular matrix with unit elements in the diagonal
procedure GenericMtxMultRightLowerTriaUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var y : Integer;
    y2 : integer;
    pMT1 : PConstDoubleArr;
    pMT2 : PDouble;
    val : double;
    x : Integer;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     assert(width2 = height2, 'MT2 is expected to be square');

     for x := 0 to width2 - 1 do
     begin
          for y := 0 to height1 - 1 do
          begin
               pMT1 := GenPtrArr(MT1, 0, y, LineWidth1);
               pMt2 := GenPtr(MT2, x, x + 1, LineWidth2);

               val := pMt1^[x];
               for y2 := x + 1 to Height2 - 1 do
               begin
                    val := val + pMt1^[y2]*pMt2^;
                    inc(PByte(pMt2), LineWidth2);
               end;

               pMt1^[x] := val;
          end;
     end;
end;

// performs mt1 = mt1*mt2 where mt2 is an lower triangular matrix with non unit elements in the diagonal
procedure GenericMtxMultRightLowerTriaNoUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var y : Integer;
    y2 : integer;
    pMT2 : PDouble;
    pMT1 : PConstDoubleArr;
    val : double;
    x : Integer;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     assert(width2 = height2, 'MT2 is expected to be square');

     for x := 0 to width1 - 1 do
     begin
          for y := 0 to height1 - 1 do
          begin
               pMT1 := GenPtrArr(MT1, 0, y, LineWidth1);
               pMt2 := GenPtr(MT2, x, x, LineWidth2);

               val := 0;
               for y2 := x to Height2 - 1 do
               begin
                    val := val + pMt1^[y2]*pMt2^;
                    inc(PByte(pMt2), LineWidth2);
               end;

               pMt1^[x] := val;
          end;
     end;
end;


// calculates  x = mt1'*x where x is a vector and mt1 an upper triangular (len x len) matrix with non unit elements in the diagonal
procedure GenericMtxMultUpTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
var i, j : NativeInt;
    val : double;
    pX : PDouble;
    pMT1 : PDouble;
begin
     // implicit transpose -> start at the last column of mt1 and go backwards
     for i := len - 1 downto 0 do
     begin
          val := 0;
          pMt1 := GenPtr( mt1, i, 0, LineWidth1 );
          pX := X;

          for j := 0 to i - 1 do
          begin
               val := val + pX^*pMt1^;
               inc(PByte(pMt1), LineWidth1);
               inc(PByte(pX), IncX );
          end;

          pX^ := val + pX^*pMt1^;
     end;
end;

procedure GenericMtxMultUpNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; incX : NativeInt; len : NativeInt);
var i, j : NativeInt;
    pMt1 : PConstDoubleArr;
    pX : PDouble;
    val : double;
begin
     pMt1 := PConstDoubleArr( mt1 );
     for i := 0 to len - 1 do
     begin
          pX := x;
          val := 0;
          
          for j := i to len - 1 do
          begin
               val := val + pMt1^[j]*pX^;
               inc(PByte(pX), incX);
          end;

          x^ := val;
          inc(PByte(x), incX);
          inc(PByte(pMt1), LineWidth1);
     end;
end;
// calculates x = mt1'*x where x is a vector and mt1 an lower triangular (len x len) matrix with unit elements in the diagonal
procedure GenericMtxMultLowTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
var i, j : NativeInt;
    val : double;
    pX : PDouble;
    pX1 : PDouble;
    pMT1 : PDouble;
begin
     // implicit transpose -> start at the first column of mt1 and decrease the x vector one by one
     pX1 := X;
     for i := 0 to len - 1 do
     begin
          pMt1 := GenPtr( mt1, i, i + 1, LineWidth1 );
          pX := pX1;
          inc(PByte(pX), incX );

          // unit elements of mt1
          val := pX1^;

          // we already handled the diagonal element so start at the next one
          for j := 1 to len - i - 1 do
          begin
               val := val + pX^*pMt1^;
               inc(PByte(pMt1), LineWidth1);
               inc(PByte(pX), IncX );
          end;

          pX1^ := val;
          inc(PByte(pX1), incX);
     end;
end;

// calculates x = mt1*x where x is a vector and mt1 is a lower triangular (len x len) matrix with unit elements in the diagonal
procedure GenericMtxMultLowNoTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
var pX : PDouble;
    pMT1 : PConstDoubleArr;
    i, j : Integer;
    val : double;
begin
     for i := len - 1 downto 0 do
     begin
          pMT1 := GenPtrArr(mt1, 0, i, LineWidth1);
          pX := x;
          val := 0;
          for j := 0 to i - 1 do
          begin
               val := val + pMT1^[j]*pX^;
               inc(PByte(pX), incX);
          end;

          pX^ := pX^ + val;
     end;
end;

procedure GenericMtxMultLowNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
var pX : PDouble;
    pMT1 : PConstDoubleArr;
    i, j : Integer;
    val : double;
begin
     for i := len - 1 downto 0 do
     begin
          pMT1 := GenPtrArr(mt1, 0, i, LineWidth1);
          pX := x;
          val := 0;
          for j := 0 to i - 1 do
          begin
               val := val + pMT1^[j]*pX^;
               inc(PByte(pX), incX);
          end;
          pX^ := val + pMt1^[i]*pX^;
     end;
end;

// note the result is stored in mt1 again!
// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure GenericMtxMultRightUpperTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var y: Integer;
    x : Integer;
    i : integer;
    pMt1 : PConstDoubleArr;
    pMt2 : PConstDoubleArr;
    tmp : double;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');
     assert(width1 = height2, 'width1 <> height2');
     for y := 0 to height1 - 1 do
     begin
          pMt1 :=  PConstDoubleArr( GenPtr(mt1, 0, y, LineWidth1 ) );
          for x := 0 to height2 - 1 do
          begin
               pMt2 := PConstDoubleArr( GenPtr(mt2, 0, x, LineWidth2 ) );
               tmp := 0;
               for i := x to width1 - 1 do
                   tmp := tmp + pMt1^[i]*pMt2^[i];

               pMt1^[x] := tmp;
          end;
     end;
end;

// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure GenericMtxMultRightLowerTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PConstDoubleArr;
    tmp : double;
    pMT1 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     assert(width1 = width2, 'Widths need to be the same');
     inc(PByte(mt2), (height2 - 1)*LineWidth2);

     for x := 0 to width2 - 2 do
     begin
          pMt1 := mt1;
          for y := 0 to height1 - 1 do
          begin
               tmp := 0;
               valCounter1 := PConstDoubleArr(pMt1);
               valCounter2 := PConstDoubleArr(mt2);
               for idx := 0 to width2 - x - 2 do
                   tmp := tmp + valCounter1^[idx]*valCounter2^[idx];

               PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^[width2 - x - 1];

               inc(PByte(pMt1), LineWidth1);
          end;

          dec(PByte(mt2), LineWidth2);
     end;
end;

// calculates mt1 = mt1*mt2', mt2 = upper triangular matrix. diagonal elements are assumed to be 1!
procedure GenericMtxMultRightUpperTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PConstDoubleArr;
    tmp : double;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension error');
     assert(width1 = width2, 'Widths need to be the same');

     for x := 0 to width1 - 2 do
     begin
          valCounter2 := GenPtrArr(mt2, 0, x, LineWidth2);
          valCounter1 := PConstDoubleArr(mt1);

          for y := 0 to height1 - 1 do
          begin
               // unit element
               tmp := valCounter1^[x];

               for idx := x + 1 to width2 - 1 do
                   tmp := tmp + valCounter1^[idx]*valCounter2^[idx];
               valCounter1^[x] := tmp;

               inc(PByte(valCounter1), LineWidth1);
          end;
     end;
end;


procedure GenericMtxMultRightLowerTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
var x, y, idx : NativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PConstDoubleArr;
    tmp : double;
    pMT1 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     assert(width1 = width2, 'Widths need to be the same');
     inc(PByte(mt2), (height2 - 1)*LineWidth2);

     for x := 0 to width2 - 1 do
     begin
          pMt1 := mt1;
          for y := 0 to height1 - 1 do
          begin
               tmp := 0;
               valCounter1 := PConstDoubleArr(pMt1);
               valCounter2 := PConstDoubleArr(mt2);
               for idx := 0 to width2 - x - 1 do
                   tmp := tmp + valCounter1^[idx]*valCounter2^[idx];

               PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp;

               inc(PByte(pMt1), LineWidth1);
          end;

          dec(PByte(mt2), LineWidth2);
     end;
end;

procedure GenericMatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt);
var x, y : NativeInt;
    pA : PConstDoubleArr;
    pB : PDouble;
begin
     // calculate A = A - B'
     for y := 0 to height - 1 do
     begin
          pA := PConstDoubleArr( A );
          pB := B;
          for x := 0 to width - 1 do
          begin
               pA^[x] := pA^[x] - pB^;
               inc(PByte(pB), LineWidthB);
          end;

          inc(PByte(A), LineWidthA);
          inc(B);
     end;
end;


// ###########################################
// #### Procedures used in cholesky decomposition
// ###########################################

// performs a symmetric rank k update in the form dest = alpha* A*AT + Beta*C
// A is height x height
// originaly SSYRK.f 'Lower', 'No Transpose'
// -> updated memory access for row major matrices
procedure GenericSymRankKUpd(dest : PDouble; LineWidthDest: NativeInt; A : PDouble; LineWidthA : NativeInt; k, height : NativeInt; const Alpha, Beta : double);
var i, j : NativeInt;
    pCij : PDouble;
    pAil, pAjl : PConstDoubleArr;
    temp : double;
    l : NativeInt;
begin
     if (k <= 0) or (height <= 0) then
        exit;

     for j := 0 to height - 1 do
     begin
          pCij := dest;
          inc(PByte(pCij), j*LineWidthDest);

          pAil := PConstDoubleArr(A);
          pAjl := PConstDoubleArr(A);
          inc(PByte(pAjl), j*LineWidthA);

          // perform on lower diagonal part of C
          for i := 0 to j do
          begin
               temp := 0;
               for l := 0 to k - 1 do
                   temp := temp + pAil^[l]*pAjl^[l];

               pCij^ := Beta*pCij^ + alpha*temp;

               inc(pCij);
               inc(PByte(pAil), LineWidthA);
          end;
     end;
end;

// performs a rank2 update in the form
// C = C - A*B' - B*A'
// N...order of C (N x N matrix)
// k... number of columns of A and B
// the lower triangle of C is not referenced
// dsyr2k in lapack upper no transpose with alpha = -1, beta = 1
procedure GenericSymRank2UpdateUpper( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
  B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt );
var i, j, l : NativeInt;
    pC1, pA1, pA2, pB1, pB2 : PConstDoubleArr;
begin
     for j := 0 to N - 1 do
     begin
          pA1 := GenPtrArr(A, 0, j, LineWidthA);
          pB1 := GenPtrArr(B, 0, j, LineWidthB);
          pC1 := GenPtrArr(C, 0, j, LineWidthC);

          for i := j to N - 1 do
          begin
               pA2 := GenPtrArr(A, 0, i, LineWidthA);
               pB2 := GenPtrArr(B, 0, i, LineWidthB);

               for l := 0 to k - 1 do
                   pC1^[i] := pC1^[i] - pA1^[l]*pB2^[l] - pB1^[l]*pA2^[l];
          end;
     end;
end;



// originaly: STRSM with 'Right', 'Lower', 'Transpose', 'Non-unit'
// which solves one of the matrix equations
//    op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
// alpha is set to one
//  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
//    non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
//    op( A ) = A   or   op( A ) = A**T
// operation is: B := alpha * inv( opA) )*B
procedure GenericSolveLoTriMtxTranspNoUnit(A : PDouble; const LineWidthA : NativeInt; B : PDouble; const LineWidthB : NativeInt; width, height : NativeInt);
var j, i, k : NativeInt;
    pAjk : PDouble;
    pBij, pBik : PDouble;
    pAkk : PDouble;
    temp : double;
begin
     if (width = 0) or (height = 0) then
        exit;

     pAkk := A;
     for k := 0 to width - 1 do
     begin
          temp := 1/pAkk^;
          
          pBik := B;
          inc(pBik, k);
          for i := 0 to height - 1 do
          begin
               pBik^ := temp*pBik^;
               inc(PByte(pBik), LineWidthB);
          end;

          pAjk := pAkk;
          inc(PByte(pAjk), LineWidthA);
          
          for j := k + 1 to width - 1 do
          begin
               if pAjk^ <> 0 then
               begin
                    temp := pAjk^;
 
                    pBij := B;
                    inc(pBij, j);
                    pBik := B;
                    inc(pBik, k);
                    
                    for i := 0 to height - 1 do
                    begin
                         pBij^ := pBij^ - temp*pBik^;
                         inc(PByte(pBij), LineWidthB);
                         inc(PByte(pBik), LineWidthB);
                    end;
               end;

               inc(PByte(pAjk), LineWidthA);
          end;

          inc(pAkk);
          inc(PByte(pAkk), LineWidthA);
     end;
end;

// original dger
// X, Y are vectors, incX, incY is the iteration in bytes from one to the next vector element
// A is a matrix
procedure GenericRank1Update(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  X, Y : PDouble; incX, incY : NativeInt; alpha : double); 
var i : NativeInt;
    j : NativeInt;
    tmp : double;
    pX, pY : PDouble;
    pA, pY1 : PConstDoubleArr;
begin
     // special case: sequential vector y
     if incy = sizeof(double) then
     begin
          // performs A = A + alpha*X*Y' in row major form
          pY1 := PConstDoubleArr(y);
          pX := X;

          for i := 0 to Height - 1 do
          begin
               if pX^ <> 0 then
               begin
                    tmp := alpha*pX^;

                    pA := PConstDoubleArr(GenPtr(A, 0, i, LineWidthA));

                    for j := 0 to Width - 1 do
                        pA^[j] := pA^[j] + tmp*pY1^[j];
               end;

               inc(PByte(pX), incX);
          end;
     end
     else
     begin
          // performs A = A + alpha*X*Y' in row major form
          pX := X;
          for i := 0 to Height - 1 do
          begin
               if pX^ <> 0 then
               begin
                    tmp := alpha*pX^;

                    pA := PConstDoubleArr(GenPtr(A, 0, i, LineWidthA));
                    pY := Y;
                    for j := 0 to Width - 1 do
                    begin
                         pA^[j] := pA^[j] + tmp*pY^;
                         inc(PByte(pY), incY);
                    end;
               end;

               inc(PByte(pX), incX);
          end;
     end;
end;

procedure GenericMtxDistanceSqr(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt);
var pDist : PConstDoubleArr;
    i, j : NativeInt;
begin
     pDist := PConstDoubleArr( dist );
     dec(xLen);
     dec(yLen);

     for i := 0 to xLen do
     begin
          for j := 0 to yLen do
              pDist^[j] := sqr( PConstDoubleArr(X)^[i] - PConstDoubleArr(Y)^[j] );
          inc(PByte(pDist), LineWidthDist);
     end;
end;

procedure GenericMtxDistanceAbs(dist : PDouble; LineWidthDist : NativeInt; X, Y : PDouble; xLen, yLen : NativeInt);
var pDist : PConstDoubleArr;
    i, j : NativeInt;
begin
     pDist := PConstDoubleArr( dist );
     dec(xLen);
     dec(yLen);

     for i := 0 to xLen do
     begin
          for j := 0 to yLen do
              pDist^[j] := abs( PConstDoubleArr(X)^[i] - PConstDoubleArr(Y)^[j] );
          inc(PByte(pDist), LineWidthDist);
     end;
end;

procedure GenericConvolveRevB(dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt);
var pDest : PConstDoubleArr;
    pA : PConstDoubleArr;
    pB : PConstDoubleArr;
    i, j : integer;
    aSum : double;
begin
     pDest := PConstDoubleArr(dest);
     pA := PConstDoubleArr(A);
     dec(PDouble(pA), bLen - 1);
     pB := PConstDoubleArr(B);

     for i := 0 to aLen - 1 do
     begin
          aSum := 0;
          for j := 0 to bLen - 1 do
              aSum := aSum + pA^[j]*pB^[j];

          pDest^[i] := aSum;

          inc(PDouble(pA), 1);
     end;
end;


end.

