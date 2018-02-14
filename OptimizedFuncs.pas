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

function MtxAlloc( NumBytes : TASMNativeInt ) : Pointer;
function MtxAllocAlign( NumBytes : TASMNativeInt; var mem : Pointer ) : Pointer; overload;
function MtxMallocAlign( NumBytes : TASMNativeInt; var mem : Pointer ) : Pointer; overload;
function MtxAllocAlign( width, height : TASMNativeInt; var LineWidth : TASMNativeInt; var Mem : Pointer) : Pointer; overload;
procedure MtxMemInit(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

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

// calculate A = A - B'
procedure MatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);

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

// used in QR decomp
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
procedure MatrixMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
  // mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure MtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure MtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// W = C1*V1*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure MtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure MtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure MtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);


// performs dest = alpha*dest + beta*mt1*v
// wheras dest is a vector, mt1 a width x height matrix and v again a vector
procedure MatrixMtxVecMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
// performs matrix vector multiplication in the form: dest := alpha*mt1'*v + beta*dest
procedure MatrixMtxVecMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);

procedure MatrixRank1Update(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

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

// inplace transposition of an N x N matrix
procedure MatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);

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

procedure MatrixMeanVar(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean);


function MatrixSum(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixSum(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixSum(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

procedure MatrixCumulativeSum(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
procedure MatrixDiff(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);

procedure MatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure MatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure MatrixAbs(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);
procedure MatrixSQRT(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt);

// element wise eukledian norm
function MatrixElementwiseNorm2(Src : PDouble; const srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt; doSqrt : boolean) : double;

procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc); overload;

// matrix rotation stubs
procedure ApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);

procedure ApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);

procedure MatrixRotate(N : TASMNativeInt; DX : PDouble; const LineWidthDX : TASMNativeInt; DY : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);


type
  TCPUInstrType = (itFPU, itSSE, itAVX);

procedure InitMathFunctions(instrType : TCPUInstrType; useStrassenMult : boolean);
procedure InitSSEOptFunctions(instrType : TCPUInstrType);
function GetCurCPUInstrType : TCPUInstrType;

// note: there are cases where the strassen multiplication is slower than a block wise
// multiplication - e.g. if the tidy up work is too often executed.
// also the block wise multiplication has a lower additional memory consumption
procedure InitMult(useStrassenMult : boolean);

type
  TMatrixMultFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
  TMatrixBlockedMultfunc = procedure (dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
  TMatrixMultTria2T1 = procedure (dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
                                  width1, height1, width2, height2 : TASMNativeInt);
  TMatrixMultTriaStoreT1 = procedure (mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt; width1, height1, width2, height2 : TASMNativeInt);
  TMatrixAddFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
  TMatrixSubFunc = TMatrixAddFunc;
  TMatrixSubTFunc = procedure (A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixElemWiseFunc = TMatrixAddFunc;
  TMatrixAddScaleFunc = procedure(dest : PDouble; LineWidth, width, height : TASMNativeInt; const dOffset, Scale : double);
  TMatrixSQRTFunc = procedure(dest : PDouble; LineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixAbsFunc = procedure(dest : PDouble; LineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixCopyFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixMinMaxFunc = function(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
  TMatrixTransposeFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
  TMatrixTransposeInplaceFunc = procedure(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);
  TMatrixElemWiseNormFunc = function (dest : PDouble; LineWidth : TASMNativeInt; Width, height : TASMNativeInt; doSqrt : boolean) : double;
  TMatrixNormalizeFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt; RowWise : Boolean);
  TMatrixVarianceFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt; RowWise : Boolean; unbiased : boolean);
  TMatrixRowSwapFunc = procedure (A, B : PDouble; width : TASMNativeInt);
  TMatrixMedianFunc = procedure (dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; tmp : PDouble = nil);
  TMatrixSortFunc = procedure (dest : PDouble; destLineWidth : TASMNativeInt; width, height : integer; RowWise : boolean; tmp : PDouble = nil);
  TMatrixVecMultFunc = procedure (dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
  TMatrixRank1UpdateFunc = procedure (A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
                                      const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);

  TApplyPlaneRotSeqMatrix = procedure (width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
  TMatrixRotate = procedure (N : TASMNativeInt; DX : PDouble; const LineWidthDX : TASMNativeInt; DY : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
  TMemInitFunc = procedure(A : PDouble; NumBytes : TASMNativeInt; const Value : double);

implementation

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses ASMMatrixOperations, BlockedMult,
     {$IFNDEF x64}
     ASMMatrixMultOperations,
     ASMMatrixRotations, ASMMoveOperations,
     ASMMatrixTransposeOperations, ASMMatrixAddSubOperations,
     AVXMatrixOperations, AVXMatrixMultOperations, AVXMatrixRotations,
     AVXMatrixAddSubOperations, AVXMoveOperations, AVXMatrixTransposeOperations,
     {$ELSE}
     AVXMatrixOperations, ASMMatrixMultOperationsx64, AVXMatrixMultOperationsx64,
     AVXMatrixAddSubOperationsx64, AVXMoveOperationsx64,
     ASMMatrixRotationsx64, ASMMoveOperationsx64, ASMMatrixAddSubOperationsx64,
     AVXMatrixRotationsx64, AVXMatrixTransposeOperationsx64,
     ASMMatrixTransposeOperationsx64,
     {$ENDIF}
     BlockSizeSetup, SimpleMatrixOperations, CPUFeatures, MatrixRotations, corr;

var multFunc : TMatrixMultFunc;
    blockedMultFunc : TMatrixBlockedMultfunc;
    blockedMultT1Func : TMatrixBlockedMultfunc;
    blockedMultT2Func : TMatrixBlockedMultfunc;
    multT2Func : TMatrixMultFunc;
    multTria2T1Func : TMatrixMultTria2T1;
    multTria2TUpperFunc : TMatrixMultTria2T1;
    multTria2T1StoreT1Func : TMatrixMultTriaStoreT1;
    multLowTria2T2Store1Func : TMatrixMultTriaStoreT1;
    multTria2Store1Func : TMatrixMultTriaStoreT1;
    multTria2Store1UnitFunc : TMatrixMultTriaStoreT1;
    MtxVecMultFunc : TMatrixVecMultFunc;
    MtxVecMultTFunc : TMatrixVecMultFunc;
    addFunc : TMatrixAddFunc;
    subFunc : TMatrixSubFunc;
    subTFunc : TMatrixSubTFunc;
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
    transposeInplaceFunc : TMatrixTransposeInplaceFunc;
    matrixNormalizeFunc : TMatrixNormalizeFunc;
    matrixMeanFunc : TMatrixNormalizeFunc;
    matrixMedianFunc : TMatrixMedianFunc;
    matrixSortFunc : TMatrixSortFunc;
    matrixVarFunc : TMatrixVarianceFunc;
    matrixSumFunc : TMatrixNormalizeFunc;
    matrixMeanVarFunc : TMatrixVarianceFunc;
    matrixCumulativeSumFunc : TMatrixNormalizeFunc;
    matrixDiffFunc : TMatrixNormalizeFunc;
    rowSwapFunc : TMatrixRowSwapFunc;
    Rank1UpdateFunc : TMatrixRank1UpdateFunc;
    matrixRot : TMatrixRotate;
    PlaneRotSeqRVB : TApplyPlaneRotSeqMatrix;
    PlaneRotSeqRVF : TApplyPlaneRotSeqMatrix;
    PlaneRotSeqLVB : TApplyPlaneRotSeqMatrix;
    PlaneRotSeqLVF : TApplyPlaneRotSeqMatrix;
    memInitFunc : TMemInitFunc;

 // current initialization
var curUsedCPUInstrSet : TCPUInstrType;
    curUsedStrassenMult : boolean;

function MtxAlloc( NumBytes : TASMNativeInt ) : Pointer;
begin
     assert(NumBytes and $7 = 0, 'Numbytes not multiple of 8');

     Result := nil;
     if NumBytes <= 0 then
        exit;

     // align to next multiple of 32 bytes
     if NumBytes and $1F <> 0 then
        NumBytes := NumBytes and $FFFFFFE0 + 32;

     Result := GetMemory(NumBytes);
     if Assigned(Result) then
        MtxMemInit(Result, NumBytes, 0 );
end;

function MtxAllocAlign( NumBytes : TASMNativeInt; var mem : Pointer ) : Pointer;
begin
     Result := MtxMallocAlign( NumBytes, mem );

     if Assigned(Result) then
        MtxMemInit(mem, NumBytes, 0 );
end;

function MtxMallocAlign( NumBytes : TASMNativeInt; var mem : Pointer ) : Pointer;
begin
     Result := nil;
     mem := nil;
     if NumBytes <= 0 then
        exit;

     // align to next multiple of 32 bytes
     inc(NumBytes, $20);
     if NumBytes and $1F <> 0 then
        NumBytes := NumBytes and $FFFFFFE0 + $20;

     mem := GetMemory(NumBytes);
     if Assigned(mem) then
     begin
          Result := mem;
          if TASMNativeUInt(Result) and $1F <> 0 then
             Result := Pointer( TASMNativeUInt( Result ) + $20 - TASMNativeUInt( Result ) and $1F );
     end;
end;

function MtxAllocAlign( width, height : TASMNativeInt; var LineWidth : TASMNativeInt; var Mem : Pointer) : Pointer; overload;
var numBytes : TASMNativeInt;
begin
     if width and $03 <> 0 then
        width := width + 4 - (width and $03);

     LineWidth := sizeof(double)*width;

     numBytes := $20 + Height*LineWidth;
     mem := GetMemory( numBytes );
     if Assigned(mem) then
     begin
          Result := mem;
          if TASMNativeUInt(Result) and $1F <> 0 then
             Result := Pointer( TASMNativeUInt( Result ) + $20 - TASMNativeUInt( Result ) and $1F );

          MtxMemInit(mem, NumBytes, 0 );
     end;
end;

procedure MtxMemInit( A : PDouble; NumBytes : TASMNativeInt; const value : double );
var ptr : PDouble;
begin
     assert(numBytes mod 8 = 0, 'error only multiple of sizeof(double) allowed');
     // normally getMemory should return aligned bytes ->
     // but just in case:
     ptr := A;
     while (NumBytes > 0) and ( (TASMNativeUInt(ptr) and $1F) <> 0 ) do
     begin
          ptr^ := 0;
          inc(ptr);
          dec(NumBytes, sizeof(double));
     end;

     if NumBytes > 0 then
        memInitfunc(ptr, NumBytes, 0);
end;

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

procedure MatrixSubT(A : PDouble; LineWidthA : TASMNativeInt; B : PDouble; LineWidthB : TASMNativeInt; width, height : TASMNativeInt);
begin
     subTFunc(A, LineWidthA, B, LineWidthB, width, height);
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
     //blockedMultT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, doNone, nil)
     multT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultT2Ex(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

procedure MatrixMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multTria2T1Func(dest, LineWidthDest, mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multTria2T1StoreT1Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multTria2Store1Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multTria2TUpperFunc(dest, LineWidthDest, mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multLowTria2T2Store1Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultTria2Store1Unit(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
begin
     multTria2Store1UnitFunc(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MatrixMtxVecMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     MtxVecMultFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure MatrixMtxVecMultT(dest : PDouble; destLineWidth : TASMNativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : TASMNativeInt; width, height : TASMNativeInt; alpha, beta : double);
begin
     MtxVecMultTFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure MatrixRank1Update(A : PDouble; const LineWidthA : TASMNativeInt; width, height : TASMNativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : TASMNativeInt);
begin
     if (width <= 0) or (height <= 0) then
        exit;

     Rank1UpdateFunc(A, LineWidthA, width, height, alpha, x, y, incx, incy)
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

procedure MatrixTransposeInplace(mt : PDouble; const LineWidth : TASMNativeInt; N : TASMNativeInt);
begin
     TransposeInplaceFunc(mt, LineWidth, N);
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

procedure MatrixMeanVar(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean);
begin
     assert((RowWise and (destLineWidth >= 2*sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixMeanVarFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise, unbiased);
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

procedure MatrixCumulativeSum(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
begin
     assert( (width > 0) and (height > 0), 'Dimension error');
      
     matrixCumulativeSumFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixDiff(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); 
begin
     assert( (Width > 0) and (height > 0), 'Dimension error');

     matrixDiffFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
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
function MatrixElementwiseNorm2(Src : PDouble; const srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt; doSqrt : boolean) : double;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(srcLineWidth >= width*sizeof(double), 'Line width error');
     Result := elemNormFunc(Src, srcLineWidth, Width, Height, doSqrt);
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

procedure InitSSEOptFunctions(instrType : TCPUInstrType);
begin
     InitMathFunctions(instrType, curUsedStrassenMult);
end;

procedure InitMult(useStrassenMult : boolean);
begin
     InitMathFunctions(curUsedCPUInstrSet, useStrassenMult);
end;

procedure ApplyPlaneRotSeqRVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqRVB(width, height, A, LineWidthA, C, S);
end;
procedure ApplyPlaneRotSeqRVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqRVF(width, height, A, LineWidthA, C, S);
end;

procedure ApplyPlaneRotSeqLVB(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqLVB(width, height, A, LineWidthA, C, S);
end;

procedure ApplyPlaneRotSeqLVF(width, height : TASMNativeInt; A : PDouble; const LineWidthA : TASMNativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqLVF(width, height, A, LineWidthA, C, S);
end;

procedure MatrixRotate(N : TASMNativeInt; DX : PDouble; const LineWidthDX : TASMNativeInt; DY : PDouble; LineWidthDY : TASMNativeInt; const c, s : double);
begin
     matrixRot(N, DX, LineWidthDX, DY, LineWidthDY, c, s);
end;

function GetCurCPUInstrType : TCPUInstrType;
begin
     Result := curUsedCPUInstrSet;
end;

procedure InitMathFunctions(instrType : TCPUInstrType; useStrassenMult : boolean);
var fpuCtrlWord : Word;
begin
     // check features
     {$IFNDEF FPC}
     if instrType = itAVX then
        instrType := itsse;
     {$ENDIF}

     {$IFDEF FPC}
     if IsAVXPresent and (instrType = itAVX) then
     begin
          curUsedCPUInstrSet := itAVX;
          if useStrassenMult
          then
              multFunc := {$IFDEF FPC}@{$ENDIF}AVXStrassenMatrixMultiplication
          else
              multFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixMult;
          addFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixAdd;
          subFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixSub;
          subTFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixSubT;
          elemWiseFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixElemMult;
          addScaleFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixAddAndScale;
          scaleAddFunc := {$IFDEF FPC}@{$ENDIF}AVXMAtrixScaleAndAdd;
          sqrtFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixSQRT;
          blockedMultFunc := {$IFDEF FPC}@{$ENDIF}BlockMatrixMultiplication;
          blockedMultT1Func := {$IFDEF FPC}@{$ENDIF}BlockMatrixMultiplicationT1;
          blockedMultT2Func := {$IFDEF FPC}@{$ENDIF}BlockMatrixMultiplicationT2;
          copyFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixCopy;
          minFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixMin;
          maxFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixMax;
          transposeFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixTranspose;
          transposeInplaceFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixTransposeInplace;
          elemNormFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixElementwiseNorm2;
          matrixNormalizeFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixNormalize;
          matrixMeanFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixMean;
          matrixVarFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixVar;
          matrixMeanVarFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixMeanVar;
          matrixSumFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixSum;
          matrixCumulativeSumFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixCumulativeSum;
          matrixDiffFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixDifferentiate;
          rowSwapFunc := {$IFDEF FPC}@{$ENDIF}AVXRowSwap;
          absFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixAbs;
          elemWiseDivFunc := {$IFDEF FPC}@{$ENDIF}AVXMatrixElemDiv;
          multT2Func := {$IFDEF FPC}@{$ENDIF}AVXMatrixMultTransposed;
          multTria2T1Func := {$IFDEF FPC}@{$ENDIF}AVXMtxMultTria2T1;
          multTria2T1StoreT1Func := {$IFDEF FPC}@{$ENDIF}AVXMtxMultTria2T1StoreT1;
          multTria2TUpperFunc := {$IFDEF FPC}@{$ENDIF}AVXMtxMultTria2TUpperUnit;
          multTria2Store1Func := {$IFDEF FPC}@{$ENDIF}AVXMtxMultTria2Store1;
          multTria2Store1UnitFunc := {$IFDEF FPC}@{$ENDIF}AVXMtxMultTria2Store1Unit;
          multLowTria2T2Store1Func := {$IFDEF FPC}@{$ENDIF}AVXMtxMultLowTria2T2Store1;
          MtxVecMultFunc := {$IFDEF FPC}@{$ENDIF}AVXMtxVecMult;
          MtxVecMultTFunc := {$IFDEF FPC}@{$ENDIF}AVXMtxVecMultT;
          Rank1UpdateFunc := {$IFDEF FPC}@{$ENDIF}AVXRank1Update;
          matrixRot := {$IFDEF FPC}@{$ENDIF}AVXMatrixRotate;
          PlaneRotSeqRVB := {$IFDEF FPC}@{$ENDIF}AVXApplyPlaneRotSeqRVB;
          PlaneRotSeqRVF := {$IFDEF FPC}@{$ENDIF}AVXApplyPlaneRotSeqRVF;
          PlaneRotSeqLVB := {$IFDEF FPC}@{$ENDIF}AVXApplyPlaneRotSeqLVB;
          PlaneRotSeqLVF := {$IFDEF FPC}@{$ENDIF}AVXApplyPlaneRotSeqLVF;
          memInitFunc := {$IFDEF FPC}@{$ENDIF}AVXInitMemAligned;

          TDynamicTimeWarp.UseSSE := True;
     end
     else {$ENDIF} if IsSSE3Present and (instrType = itSSE) then
     begin
          curUsedCPUInstrSet := itSSE;
          if useStrassenMult
          then
              multFunc := {$IFDEF FPC}@{$ENDIF}ASMStrassenMatrixMultiplication
          else
              multFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMult;
          addFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixAdd;
          subFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSub;
          subTFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSubT;
          elemWiseFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixElemMult;
          addScaleFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixAddAndScale;
          scaleAddFunc := {$IFDEF FPC}@{$ENDIF}ASMMAtrixScaleAndAdd;
          sqrtFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSQRT;
          blockedMultFunc := {$IFDEF FPC}@{$ENDIF}BlockMatrixMultiplication;
          blockedMultT1Func := {$IFDEF FPC}@{$ENDIF}BlockMatrixMultiplicationT1;
          blockedMultT2Func := {$IFDEF FPC}@{$ENDIF}BlockMatrixMultiplicationT2;
          multT2Func := {$IFDEF FPC}@{$ENDIF}ASMMatrixMultTransposed;
          copyFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixCopy;
          minFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMin;
          maxFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMax;
          transposeFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixTranspose;
          transposeInplaceFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixTransposeInplace;
          elemNormFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixElementwiseNorm2;
          matrixNormalizeFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixNormalize;
          matrixMeanFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMean;
          matrixVarFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixVar;
          matrixMeanVarFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixMeanVar;
          matrixSumFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixSum;
          matrixCumulativeSumFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixCumulativeSum;
          matrixDiffFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixDifferentiate;
          rowSwapFunc := {$IFDEF FPC}@{$ENDIF}ASMRowSwap;
          absFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixAbs;
          elemWiseDivFunc := {$IFDEF FPC}@{$ENDIF}ASMMatrixElemDiv;
          multTria2T1Func := {$IFDEF FPC}@{$ENDIF}ASMMtxMultTria2T1;
          multTria2T1StoreT1Func := {$IFDEF FPC}@{$ENDIF}ASMMtxMultTria2T1StoreT1;
          multTria2TUpperFunc := {$IFDEF FPC}@{$ENDIF}ASMMtxMultTria2TUpperUnit;
          multTria2Store1Func := {$IFDEF FPC}@{$ENDIF}ASMMtxMultTria2Store1;
          multTria2Store1UnitFunc := {$IFDEF FPC}@{$ENDIF}ASMMtxMultTria2Store1Unit;
          multLowTria2T2Store1Func := {$IFDEF FPC}@{$ENDIF}ASMMtxMultLowTria2T2Store1;
          MtxVecMultFunc := {$IFDEF FPC}@{$ENDIF}ASMMtxVecMult;
          MtxVecMultTFunc := {$IFDEF FPC}@{$ENDIF}ASMMtxVecMultT;
          Rank1UpdateFunc := {$IFDEF FPC}@{$ENDIF}ASMRank1Update;
          matrixRot := {$IFDEF FPC}@{$ENDIF}ASMMatrixRotate;
          PlaneRotSeqRVB := {$IFDEF FPC}@{$ENDIF}ASMApplyPlaneRotSeqRVB;
          PlaneRotSeqRVF := {$IFDEF FPC}@{$ENDIF}ASMApplyPlaneRotSeqRVF;
          PlaneRotSeqLVB := {$IFDEF FPC}@{$ENDIF}ASMApplyPlaneRotSeqLVB;
          PlaneRotSeqLVF := {$IFDEF FPC}@{$ENDIF}ASMApplyPlaneRotSeqLVF;
          memInitFunc := {$IFDEF FPC}@{$ENDIF}ASMInitMemAligned;

          TDynamicTimeWarp.UseSSE := True;
     end
     else
     begin
          curUsedCPUInstrSet := itFPU;
          if useStrassenMult
          then
              multFunc := {$IFDEF FPC}@{$ENDIF}GenericStrassenMatrixMultiplication
          else
              multFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMult;
          addFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxAdd;
          subFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSub;
          subTFunc := {$IFDEF FPC}@{$ENDIF}GenericMatrixSubT;
          elemWiseFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxElemMult;
          addScaleFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxAddAndScale;
          scaleAddFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxScaleAndAdd;
          sqrtFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSqrt;
          blockedMultFunc := {$IFDEF FPC}@{$ENDIF}GenericBlockMatrixMultiplication;
          blockedMultT1Func := {$IFDEF FPC}@{$ENDIF}GenericBlockMatrixMultiplicationT1;
          blockedMultT2Func := {$IFDEF FPC}@{$ENDIF}GenericBlockMatrixMultiplicationT2;
          copyFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxCopy;
          minFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMin;
          maxFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMax;
          transposeFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxTranspose;
          transposeInplaceFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxTransposeInplace;
          elemNormFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxElementwiseNorm2;
          matrixNormalizeFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxNormalize;
          matrixMeanFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMean;
          matrixVarFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxVar;
          matrixMeanVarFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMeanVar;
          matrixSumFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSum;
          matrixCumulativeSumFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxCumulativeSum;
          matrixDiffFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxDifferentiate;
          rowSwapFunc := {$IFDEF FPC}@{$ENDIF}GenericRowSwap;
          absFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxAbs;
          elemWiseDivFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxElemDiv;
          multTria2T1Func := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTria2T1Lower;
          multTria2TUpperFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTria2TUpperUnit;
          multTria2T1StoreT1Func := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTria2T1StoreT1;
          multLowTria2T2Store1Func := {$IFDEF FPC}@{$ENDIF}GenericMtxMultLowTria2T2Store1;
          multTria2Store1Func := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTria2Store1;
          multTria2Store1UnitFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTria2Store1Unit;
          multT2Func := {$IFDEF FPC}@{$ENDIF}GenericMtxMultTransp;
          MtxVecMultFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxVecMult;
          MtxVecMultTFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxVecMultT;
          Rank1UpdateFunc := {$IFDEF FPC}@{$ENDIF}GenericRank1Update;
          matrixRot := {$IFDEF FPC}@{$ENDIF}GenericMatrixRotate;
          PlaneRotSeqRVB := {$IFDEF FPC}@{$ENDIF}GenericApplyPlaneRotSeqRVB;
          PlaneRotSeqRVF := {$IFDEF FPC}@{$ENDIF}GenericApplyPlaneRotSeqRVF;
          PlaneRotSeqLVB := {$IFDEF FPC}@{$ENDIF}GenericApplyPlaneRotSeqLVB;
          PlaneRotSeqLVF := {$IFDEF FPC}@{$ENDIF}GenericApplyPlaneRotSeqLVF;
          memInitFunc := {$IFDEF FPC}@{$ENDIF}GenericInitMemAligned;

          TDynamicTimeWarp.UseSSE := False
     end;

     matrixMedianFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxMedian;
     matrixSortFunc := {$IFDEF FPC}@{$ENDIF}GenericMtxSort;

     curUsedStrassenMult := useStrassenMult;

     // set only double precission for compatibility
     fpuCtrlWord := Get8087CW;
     // set bits 8 and 9 to "10" -> double precission calculation instead of extended!
     fpuCtrlWord := fpuCtrlWord or $0300;
     fpuCtrlWord := fpuCtrlWord and $FEFF;
     Set8087CW(fpuCtrlWord);
end;

initialization
  curUsedCPUInstrSet := itAVX;
  curUsedStrassenMult := False;

  InitMathFunctions(curUsedCPUInstrSet, curUsedStrassenMult);

end.
