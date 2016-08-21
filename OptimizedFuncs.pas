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


unit OptimizedFuncs;

// ##########################################################
// #### proxy for the optimizations

interface

uses MatrixConst, Types;

procedure MatrixCopy(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); overload;
procedure MatrixCopy(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt); overload;
function MatrixCopy(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt) : TDoubleDynArray; overload;
function MatrixCopy(const Src : Array of double; width, height : TASMNativeInt) : TDoubleDynArray; overload;

procedure MatrixRowSwap(A, B : PDouble; width : TASMNativeInt);

procedure MatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixAdd(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt); overload;
function MatrixAdd(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;

procedure MatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixSub(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixSub(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt); overload;
function MatrixSub(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;

// performs mt1 * mt2
function MatrixMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;
function MatrixMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;

procedure MatrixMultEx(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;

// performs mt1' * mt2
function MatrixMultT1(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT1(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;
function MatrixMultT1(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;

procedure MatrixMultT1Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;

// performs mt1 * mt2'
function MatrixMultT2(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT2(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;
function MatrixMultT2(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;

procedure MatrixMultT2Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;

procedure MatrixMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

// performs dest = alpha*dest + beta*mt1*v
// wheras dest is a vector, mt1 a width x height matrix and v again a vector
procedure MatrixMtxVecMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// performs matrix vector multiplication in the form: dest := alpha*mt1'*v + beta*dest
procedure MatrixMtxVecMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);


procedure MatrixElemMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixElemMult(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
function MatrixElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;

procedure MatrixElemDiv(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixElemDiv(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
function MatrixElemDiv(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;


// GenericMtx transposition functions. Note the there is no inplace GenericMtx transpose - this will result in an unspecified end GenericMtx.
function MatrixTranspose(mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixTranspose(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt); overload;
function MatrixTranspose(const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixTranspose(var dest : Array of Double; const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;

function MatrixMax(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function MatrixMin(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function MatrixNormalize(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixNormalize(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixNormalize(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
procedure MatrixNormalize(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

function MatrixMean(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixMean(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixMean(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
procedure MatrixMean(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

function MatrixMedian(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixMedian(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixMedian(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; hlpMem : PDouble); overload;
procedure MatrixMedian(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

procedure MatrixSort(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; hlpMem : PDouble = nil);

function MatrixVar(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
function Matrixvar(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
procedure MatrixVar(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean); overload;
procedure MatrixVar(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean); overload;


function MatrixSum(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixSum(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixSum(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

procedure MatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure MatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure MatrixAbs(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure MatrixSQRT(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

// element wise eukledian norm
function MatrixElementwiseNorm2(Src : PDouble; const srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;

procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc); overload;

procedure InitMathFunctions(UseSSEOptFuncs : boolean; useStrassenMult : boolean);
procedure InitSSEOptFunctions(UseSSEOptFuncs : boolean);

// note: there are cases where the strassen multiplication is slower than a block wise
// multiplication - e.g. if the tidy up work is too often executed.
// also the block wise multiplication has a lower additional memory consumption
procedure InitMult(useStrassenMult : boolean);

type
  TMatrixMultFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
  TMatrixBlockedMultfunc = procedure (dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
  TMatrixMultTria2T1 = procedure (dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
                                  width1, height1, width2, height2 : TASMNativeInt);
  TMatrixAddFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
  TMatrixSubFunc = TMatrixAddFunc;
  TMatrixElemWiseFunc = TMatrixAddFunc;
  TMatrixAddScaleFunc = procedure(dest : PDouble; LineWidth, width, height : TASMNativeInt; const dOffset, Scale : double);
  TMatrixSQRTFunc = procedure(dest : PDouble; LineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixAbsFunc = procedure(dest : PDouble; LineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixCopyFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixMinMaxFunc = function(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
  TMatrixTransposeFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
  TMatrixElemWiseNormFunc = function (dest : PDouble; LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
  TMatrixNormalizeFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt; RowWise : Boolean);
  TMatrixVarianceFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt; RowWise : Boolean; unbiased : boolean);
  TMatrixRowSwapFunc = procedure (A, B : PDouble; width : TASMNativeInt);
  TMatrixMedianFunc = procedure (dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; tmp : PDouble = nil);
  TMatrixSortFunc = procedure (dest : PDouble; destLineWidth : TASMNativeInt; width, height : integer; RowWise : boolean; tmp : PDouble = nil);
  TMatrixVecMultFunc = procedure (dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

implementation

uses BlockSizeSetup, SimpleMatrixOperations, ASMMatrixOperations, ASMMatrixMultOperations,
     ASMMatrixMultOperationsx64, ASMMatrixVectorMultOperations, ASMMatrixVectorMultOperationsx64,
     CPUFeatures;

var actUseSSEoptions : boolean;
    actUseStrassenMult : boolean;

var multFunc : TMatrixMultFunc;
    blockedMultFunc : TMatrixBlockedMultfunc;
    blockedMultT1Func : TMatrixBlockedMultfunc;
    blockedMultT2Func : TMatrixBlockedMultfunc;
    multTria2T1 : TMatrixMultTria2T1;
    MtxVecMultFunc : TMatrixVecMultFunc;
    MtxVecMultTFunc : TMatrixVecMultFunc;
    addFunc : TMatrixAddFunc;
    subFunc : TMatrixSubFunc;
    elemWiseFunc : TMatrixElemWiseFunc;
    elemWiseDivFunc : TMatrixElemWiseFunc;
    addScaleFunc : TMatrixAddScaleFunc;
    scaleAddFunc : TMatrixAddScaleFunc;
    sqrtFunc : TMatrixSQRTFunc;
    absFunc : TMatrixAbsFunc;
    copyFunc : TMatrixCopyFunc;
    maxFunc : TMatrixMinMaxFunc;
    minFunc : TMatrixMinMaxFunc;
    elemNormFunc : TMatrixElemWiseNormFunc;
    transposeFunc : TMatrixTransposeFunc;
    matrixNormalizeFunc : TMatrixNormalizeFunc;
    matrixMeanFunc : TMatrixNormalizeFunc;
    matrixMedianFunc : TMatrixMedianFunc;
    matrixSortFunc : TMatrixSortFunc;
    matrixVarFunc : TMatrixVarianceFunc;
    matrixSumFunc : TMatrixNormalizeFunc;
    rowSwapFunc : TMatrixRowSwapFunc;

procedure MatrixCopy(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     copyFunc(dest, destLineWidth, Src, srcLineWidth, width, height);
end;

procedure MatrixCopy(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0) and (Length(dest) = Length(src)) and (length(src) = width*height), 'Dimension error');
     copyFunc(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height);
end;

function MatrixCopy(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);

     copyFunc(@Result[0], width*sizeof(double), Src, srcLineWidth, width, height);
end;

function MatrixCopy(const Src : Array of double; width, height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (length(src) = width*height), 'Dimension error');
     SetLength(Result, width*height);
     copyfunc(@Result[0],  width*sizeof(double), @Src[0], width*sizeof(double), width, height);
end;

function MatrixAdd(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     addFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixRowSwap(A, B : PDouble; width : TASMNativeInt);
begin
     assert((width > 0), 'Dimension error');

     rowSwapFunc(A, B, width);
end;


procedure MatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
begin
     addFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixAdd(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := MatrixAdd(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure MatrixAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     addFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

function MatrixSub(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     SubFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
begin
     SubFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixSub(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := MatrixSub(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure MatrixSub(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     subFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

function MatrixMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     SetLength(Result, Height1*width2);
     MatrixMult(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     MatrixMult(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function MatrixMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, height1*width2);
     MatrixMult(@Result[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure MatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var multBlockSize : TASMNativeInt;
begin
     multBlockSize := BlockedMatrixMultSize*BlockedMatrixMultSize;
     if ((width1 >= BlockedMatrixMultSize) and (height1 >= BlockedMatrixMultSize) and (height2 >= BlockedMatrixMultSize)) or
        (width1*height1 >= multBlockSize) or (width2*Height2 >= multBlockSize)
     then
         blockedMultFunc(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, doNone, nil)
     else
         multFunc(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultEx(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultFunc(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

// mt1' * mt2
function MatrixMultT1(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');
     SetLength(Result, width1*width2);
     MatrixMultT1(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultT1(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(Length(dest) >= width1*width2, 'Dimension Error');
     MatrixMultT1(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function MatrixMultT1(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, width1*width2);
     MatrixMultT1(@Result[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure MatrixMultT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     blockedMultT1Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, doNone, nil);
end;

procedure MatrixMultT1Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultT1Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

// mt1 * mt2'
function MatrixMultT2(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');
     SetLength(Result, height1*height2);
     MatrixMultT2(@Result[0], sizeof(double)*height2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultT2(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(Length(dest) >= height1*height2, 'Dimension Error');
     MatrixMultT2(@dest[0], height2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function MatrixMultT2(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, height1*height2);
     MatrixMultT2(@Result[0], height2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure MatrixMultT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     blockedMultT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, doNone, nil)
end;

procedure MatrixMultT2Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

procedure MatrixMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multTria2T1(dest, LineWidthDest, mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MatrixMtxVecMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     MtxVecMultFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure MatrixMtxVecMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     MtxVecMultTFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure MatrixElemMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
begin
     elemWiseFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixElemMult(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     elemWiseFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     elemWiseFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, Height, width*sizeof(double), width*sizeof(double));
end;

function MatrixElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
begin
     Result := MatrixElemMult(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

procedure MatrixElemDiv(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
begin
     elemWiseDivFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixElemDiv(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     elemWiseDivFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     elemWiseDivFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function MatrixElemDiv(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
begin
     Result := MatrixElemDiv(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;



function MatrixTranspose(mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension Error');
     SetLength(Result, height*width);
     transposeFunc(@Result[0], height*sizeof(double), mt, LineWidth, width, height);
end;

procedure MatrixTranspose(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)) and (destLineWidth >= height*sizeof(double)), 'Dimension Error');
     transposeFunc(dest, destLineWidth, mt, LineWidth, width, height);
end;

function MatrixTranspose(const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (Length(mt) = width*height), 'Dimension Error');
     Result := MatrixTranspose(@mt[0], width*sizeof(double), width, height);
end;

procedure MatrixTranspose(var dest : Array of Double; const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0) and (Length(mt) = width*height) and (length(dest) = Length(mt)), 'Dimension Error');
     transposeFunc(@dest[0], height*sizeof(double), @mt[0], width, height, width*sizeof(double));
end;

function MatrixMax(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     if Width = 1
     then
         Result := GenericMtxMax(mt, width, height, LineWidth)
     else
         Result := maxFunc(mt, width, height, LineWidth);
end;

function MatrixMin(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     if Width = 1
     then
         Result := GenericMtxMin(mt, width, height, LineWidth)
     else
         Result := minFunc(mt, width, height, LineWidth);
end;

function MatrixNormalize(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixNormalize(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise);
end;

function MatrixNormalize(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixNormalize(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixNormalize(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');
     matrixNormalizeFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixNormalize(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean);
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixNormalize(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise);
end;

function MatrixMean(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixMean(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise);
end;

function MatrixMean(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixMean(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixMean(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     assert((RowWise and (destLineWidth >= sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixMeanFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixMean(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixMean(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise);
end;

function MatrixMedian(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixMedian(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise, nil);
end;

function MatrixMedian(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixMedian(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixMedian(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; hlpMem : PDouble); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     assert((RowWise and (destLineWidth >= sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixMedianFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise, hlpMem);
end;

procedure MatrixMedian(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixMedian(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise, nil);
end;

procedure MatrixSort(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; hlpMem : PDouble = nil);
begin
     if (width <= 0) or (height <= 0) then
        exit;

     matrixSortFunc(dest, destLineWidth, width, height, RowWise, hlpMem);
end;

function MatrixVar(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixVar(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise, unbiased);
end;

function MatrixVar(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixVar(@Src[0], width*sizeof(double), width, height, RowWise, unbiased);
end;

procedure MatrixVar(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     assert((RowWise and (destLineWidth >= sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixVarFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise, unbiased);
end;

procedure MatrixVar(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixVar(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise, unbiased);
end;

function MatrixSum(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixSum(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise);
end;

function MatrixSum(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixSum(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixSum(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and
            ( ((not rowWise) and (destLineWidth >= width*sizeof(double))) or
              ((rowWise) and (destLineWidth >= sizeof(double)) )) , 'Dimension error');
     matrixSumFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and
             ( ((not rowWise) and (Length(dest) >= width)) or
               ((rowWise) and (Length(dest) >= height) )) and
                (Length(src) >= width*height), 'Dimension error');
     MatrixSum(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     addScaleFunc(dest, linewidth, width, height, Offset, Scale);
end;

procedure MatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     scaleAddFunc(dest, linewidth, width, height, Offset, Scale);
end;

procedure MatrixSQRT(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     sqrtFunc(Dest, LineWidth, Width, Height);
end;

procedure MatrixAbs(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     absFunc(Dest, LineWidth, Width, Height);
end;

// element wise eukledian norm
function MatrixElementwiseNorm2(Src : PDouble; const srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(srcLineWidth >= width*sizeof(double), 'Line width error');
     Result := elemNormFunc(Src, srcLineWidth, Width, Height);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure InitSSEOptFunctions(UseSSEOptFuncs : boolean);
begin
     InitMathFunctions(UseSSEOptFuncs, actUseStrassenMult);
end;

procedure InitMult(useStrassenMult : boolean);
begin
     InitMathFunctions(actUseSSEoptions, useStrassenMult);
end;

procedure InitMathFunctions(UseSSEOptFuncs : boolean; useStrassenMult : boolean);
var fpuCtrlWord : Word;
begin
     // check features
     if IsSSE3Present and UseSSEOptFuncs then
     begin
          if useStrassenMult
          then
              multFunc := {$IFDEF FPC}@{$ENDIF}ASMStrassenMatrixMultiplication
          else
              multFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMult;
          addFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixAdd;
          subFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSub;
          elemWiseFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixElemMult;
          addScaleFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixAddAndScale;
          scaleAddFunc := {$IFDEF FPC}@{$ENDIF}ASMMAtrixScaleAndAdd;
          sqrtFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSQRT;
          blockedMultFunc := {$IFDEF FPC}@{$ENDIF}BlockedMatrixMultiplication;
          blockedMultT1Func := {$IFDEF FPC}@{$ENDIF}BlockedMatrixMultiplicationT1;
          blockedMultT2Func := {$IFDEF FPC}@{$ENDIF}BlockedMatrixMultiplicationT2;
          copyFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixCopy;
          minFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMin;
          maxFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMax;
          transposeFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixTranspose;
          elemNormFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixElementwiseNorm2;
          matrixNormalizeFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixNormalize;
          matrixMeanFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMean;
          matrixVarFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixVar;
          matrixSumFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSum;
          rowSwapFunc := {$IFDEF FPC}@{$ENDIF}ASMRowSwap;
          absFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixAbs;
          elemWiseDivFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixElemDiv;
          multTria2T1 := {$IFDEF FPC}@{$ENDIF}ASMMtxMultTria2T1;
          MtxVecMultFunc := {$IFDEF FPC}@{$ENDIF}ASMMtxVecMult;
          MtxVecMultTFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixVectMultT;
     end
     else
     begin
          if useStrassenMult
          then
              multFunc := {$IFDEF FPC}@{$ENDIF}GenericStrassenMatrixMultiplication
          else
              multFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMult;
          addFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxAdd;
          subFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSub;
          elemWiseFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxElemMult;
          addScaleFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxAddAndScale;
          scaleAddFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxScaleAndAdd;
          sqrtFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSqrt;
          blockedMultFunc := {$IFDEF FPC}@{$ENDIF}GenericBlockedMatrixMultiplication;
          blockedMultT1Func := {$IFDEF FPC}@{$ENDIF}GenericBlockedMatrixMultiplicationT1;
          blockedMultT2Func := {$IFDEF FPC}@{$ENDIF}GenericBlockedMatrixMultiplicationT2;
          copyFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxCopy;
          minFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMin;
          maxFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMax;
          transposeFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxTranspose;
          elemNormFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxElementwiseNorm2;
          matrixNormalizeFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxNormalize;
          matrixMeanFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMean;
          matrixVarFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxVar;
          matrixSumFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSum;
          rowSwapFunc := {$IFDEF FPC}@{$ENDIF}GenericRowSwap;
          absFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxAbs;
          elemWiseDivFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxElemDiv;
          multTria2T1 := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTria2T1;
          MtxVecMultFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxVecMult;
          MtxVecMultTFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxVecMultT;
     end;

     matrixMedianFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMedian;
     matrixSortFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSort;
     
     actUseSSEoptions := IsSSE3Present;
     actUseStrassenMult := useStrassenMult;

     // set only double precission for compatibility
     fpuCtrlWord := Get8087CW;
     // set bits 8 and 9 to "10" -> double precission calculation instead of extended!
     fpuCtrlWord := fpuCtrlWord or $0300;
     fpuCtrlWord := fpuCtrlWord and $FEFF;
     Set8087CW(fpuCtrlWord);
end;

initialization
  actUseSSEoptions := IsSSE3Present;
  actUseStrassenMult := False;

  InitMathFunctions(actUseSSEoptions, actUseSSEoptions);

end.
