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


unit MatrixASMStubSwitch;

// ##########################################################
// #### proxy for the optimizations

interface

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

uses MatrixConst, Types;

function MtxAlloc( NumBytes : NativeInt ) : Pointer;
function MtxAllocAlign( NumBytes : NativeInt; var mem : Pointer ) : Pointer; overload;
function MtxMallocAlign( NumBytes : NativeInt; var mem : Pointer ) : Pointer; overload;
function MtxMallocAlign( width, height : NativeInt; var LineWidth : NativeInt; var mem : Pointer ) : Pointer; overload;
function MtxAllocAlign( width, height : NativeInt; var LineWidth : NativeInt; var Mem : Pointer) : Pointer; overload;
procedure MtxMemInit(A : PDouble; NumBytes : NativeInt; const Value : double);

procedure MatrixCopy(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt); overload;
procedure MatrixCopy(var dest : Array of double; const Src : Array of double; width, height : NativeInt); overload;
function MatrixCopy(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt) : TDoubleDynArray; overload;
function MatrixCopy(const Src : Array of double; width, height : NativeInt) : TDoubleDynArray; overload;
procedure MatrixInit(dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; const value : Double); overload;
procedure MatrixInit(var dest : Array of double; const Value : double); overload;

procedure MatrixIndex(dest : PDouble; const destLineWidth : NativeInt; src : PDouble; const srcLineWidth : NativeInt; colIdx, rowIdx : TIntegerDynArray);

procedure MatrixRowSwap(A, B : PDouble; width : NativeInt);
procedure MatrixColSwap(A, B : PDouble; const LineWidthAB : NativeInt; Height : NativeInt);

procedure MatrixAdd(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
function MatrixAdd(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray; overload;
procedure MatrixAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt); overload;
function MatrixAdd(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;

procedure MatrixAddVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);

procedure MatrixSub(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
function MatrixSub(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray; overload;
procedure MatrixSub(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt); overload;
function MatrixSub(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;

// calculate A = A - B'
procedure MatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt);
// calculate A = A - Repmat(B, 1, height) if rowwise, B is a vector
procedure MatrixSubVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);

// performs mt1 * mt2
function MatrixMult(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt); overload;
function MatrixMult(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;

procedure MatrixMultEx(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;

// performs mt1' * mt2
function MatrixMultT1(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT1(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt); overload;
function MatrixMultT1(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT1(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;

procedure MatrixMultT1Ex(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;

// performs mt1 * mt2'
function MatrixMultT2(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT2(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt); overload;
function MatrixMultT2(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixMultT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;

procedure MatrixMultT2Ex(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;

// used in QR decomp and hess
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
procedure MatrixMultTria2T1(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
  // mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
// W = V1'*T -> V1 is an upper triangular matrix with assumed unit diagonal entries. Operation on V1 transposition
procedure MtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);

// mt1 = mt1*mt2' where mt2 is an upper triangular matrix with non unit diagonal elements
procedure MtxMultRightUpperTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix with non unit diagonal elements
procedure MtxMultRightUpperTriaNoUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix - diagonal elements are unit
procedure MtxMultRightUpperTriaUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// mt1 = mt1*mt2' where mt2 is an upper triangular matrix with unit diagonal elements
procedure MtxMultRightUpperTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);


// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure MtxMultRightLowerTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are non unit!
procedure MtxMultRightLowerTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// performs mt1 = mt1*mt2 where mt2 is an lower triangular matrix with non unit elements in the diagonal
procedure MtxMultRightLowerTriaNoUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
// performs mt1 = mt1*mt2 where mt2 is an lower triangular matrix with unit elements in the diagonal
procedure MtxMultRightLowerTriaUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);


// calculates  x = mt1'*x where x is a vector and mt1 an upper triangular (len x len) matrix with non unit elements in the diagonal
procedure MtxMultUpTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
// calculates x = mt1'*x where x is a vector and mt1 an lower triangular (len x len) matrix with unit elements in the diagonal
procedure MtxMultLowTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
// calculates x = mt1*x where x is a vector and mt1 is a lower triangular (len x len) matrix with unit elements in the diagonal
procedure MtxMultLowNoTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
// calculates x = mt1*x where x is a vector and mt1 is an upper triangular (len x len) matrix with non unit elements in the diagonal
procedure MtxMultUpNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; incX : NativeInt; len : NativeInt);
// calculates x = mt1*x where x is a vector and mt1 is a lower triangular (len x len) matrix with unit elements in the diagonal
procedure MtxMultLowNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);



// performs dest = alpha*mt1*v + beta*dest
// wheras dest is a vector, mt1 a width x height matrix and v again a vector
procedure MatrixMtxVecMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
// performs matrix vector multiplication in the form: dest := alpha*mt1'*v + beta*dest
procedure MatrixMtxVecMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
// performs matrix vector multiplication in the form: dest := alpha*mt1*v + beta*dest
// where the matrix mt1 is a symmetric matrix and only the upper part is touched in the multiplication
procedure MatrixMtxVecMultUpperSym(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
// same as above but using the lower matrix part
procedure MatrixMtxVecMultLowerSym(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);

// calculates the dot product of x and y: Result := sum_i=0_n-1 ( x[i]*y[i] )
function MatrixVecDotMult(  x : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt ) : double;
// calculates y[i] = y[i] + alpha*x[i]
procedure MatrixVecAdd( X : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt; const alpha : double );

procedure MatrixRank1Update(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : NativeInt);

// performs C := beta*C - alpha*A*B**T - alpha*B*A**T
// N...order of C (N x N matrix)
// k... number of columns of A and B
// the lower triangle of C is not referenced
// this function uses as beta = 1, alpha = -1
// dsyr2k in lapack upper no transpose with alpha = -1, beta = 1
procedure MatrixUpperSymRank2Update( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
  B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt );

// ###########################################
// #### Element by element operations
// ###########################################

procedure MatrixElemMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
function MatrixElemMult(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt); overload;
function MatrixElemMult(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;

procedure MatrixElemDiv(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
function MatrixElemDiv(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
procedure MatrixElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt); overload;
function MatrixElemDiv(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;

procedure MatrixElemAdd( Dest : PDouble; const LineWidth, Width, Height : NativeInt; const Offset : double );
procedure MatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
procedure MatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
procedure MatrixAbs(Dest : PDouble; const LineWidth, Width, Height : NativeInt);
procedure MatrixSQRT(Dest : PDouble; const LineWidth, Width, Height : NativeInt);


// ###########################################
// #### Matrix transposition
// ###########################################

// GenericMtx transposition functions. Note the there is no inplace GenericMtx transpose - this will result in an unspecified end GenericMtx.
function MatrixTranspose(mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
procedure MatrixTranspose(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); overload;
function MatrixTranspose(const mt : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
procedure MatrixTranspose(var dest : Array of Double; const mt : Array of Double; width : NativeInt; height : NativeInt); overload;

// inplace transposition of an N x N matrix
procedure MatrixTransposeInplace(mt : PDouble; const LineWidth : NativeInt; N : NativeInt);

// ###########################################
// #### Max/sum mean variance
// ###########################################

function MatrixMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
function MatrixMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
function MatrixAbsMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
function MatrixAbsMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;

// min/max of an square upper/lower matrix
function MatrixMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixAbsMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixAbsMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixAbsMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
function MatrixAbsMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;


function MatrixNormalize(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixNormalize(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixNormalize(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
procedure MatrixNormalize(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;

function MatrixMean(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixMean(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixMean(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
procedure MatrixMean(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;

function MatrixMedian(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixMedian(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixMedian(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; hlpMem : PDouble); overload;
procedure MatrixMedian(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;

procedure MatrixRollMedian( dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; order : integer; rowWise : boolean);

procedure MatrixSort(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; hlpMem : PDouble = nil);

function MatrixVar(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
function Matrixvar(const Src : Array of double; width, height : NativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
procedure MatrixVar(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean); overload;
procedure MatrixVar(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean; unbiased : boolean); overload;

procedure VecMeanVar( var dest : TMeanVarRec; vec : PDouble; width : NativeInt; unbiased : boolean);  
procedure MatrixMeanVar(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
procedure MatrixNormalizeMeanVar(dest : PDouble; destLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
procedure MatrixNormalizeMeanVar(var aVec : Array of double); overload;

function MatrixSum(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixSum(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixSum(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;

procedure MatrixCumulativeSum(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
procedure MatrixDiff(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);


// element wise eukledian norm
function MatrixElementwiseNorm2(Src : PDouble; const srcLineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;

// ###########################################
// #### Apply functions on elements in a matrix A(i, j) = f( A(i, j) )
// ###########################################

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixObjFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFunc); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefObjFunc); overload;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixFunc); overload;
procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixObjFunc); overload;
procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefFunc); overload;
procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefObjFunc); overload;

{$IFDEF FPC}
   {:$DEFINE ANONMETHODS}
{$ELSE}
   {$IF CompilerVersion >= 20.0}
      {$DEFINE ANONMETHODS}
   {$IFEND}
{$ENDIF}

{$IFDEF ANONMETHODS}
procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFuncRef); overload;
procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixFuncRef); overload;
procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;

{$ENDIF}

// matrix rotation stubs
procedure ApplyPlaneRotSeqRVB(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqRVF(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);

procedure ApplyPlaneRotSeqLVB(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
procedure ApplyPlaneRotSeqLVF(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);

procedure MatrixRotate(N : NativeInt; DX : PDouble; const LineWidthDX : NativeInt; DY : PDouble; LineWidthDY : NativeInt; const c, s : double);

// convolution
// performs the convolution on a matrix rowwise!
// A is either a vector or a matrix, B is always a vector (length is veclen)!
// if memory is provided the memory needs to be at least as long as length(b) + 8 (for memory alignment and cpu alignment)
procedure MatrixConvolve(dest : PDouble; const LineWidthDest : NativeInt; A, B : PDouble; const LineWidthA : NativeInt; width, height, veclen : NativeInt; mem : Pointer = nil);

type
  TCPUInstrType = (itFPU, itSSE, itAVX, itFMA);

procedure InitMathFunctions(instrType : TCPUInstrType; useStrassenMult : boolean);
procedure InitSSEOptFunctions(instrType : TCPUInstrType);
function GetCurCPUInstrType : TCPUInstrType;

// note: there are cases where the strassen multiplication is slower than a block wise
// multiplication - e.g. if the tidy up work is too often executed.
// also the block wise multiplication has a lower additional memory consumption
procedure InitMult(useStrassenMult : boolean);

type
  TMatrixMultFunc = procedure(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
  TMatrixBlockedMultfunc = procedure (dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble);
  TMatrixMultTria2T1 = procedure (dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
                                  width1, height1, width2, height2 : NativeInt);
  TMatrixMultTriaStoreT1 = procedure (mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt; width1, height1, width2, height2 : NativeInt);
  TMatrixMultVecTriaStoreX = procedure( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
  TMatrixAddFunc = procedure(dest : PDouble; destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
  TMatrixSubFunc = TMatrixAddFunc;
  TMatrixSubTFunc = procedure (A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt);
  TMatrixSubVecFunc = procedure(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
  TMatrixElemWiseFunc = TMatrixAddFunc;
  TMatrixAddScaleFunc = procedure(dest : PDouble; LineWidth, width, height : NativeInt; const dOffset, Scale : double);
  TMatrixElemAddFunc = procedure(dest : PDouble; LineWidth, width, height : NativeInt; const dOffset : double);
  TMatrixSQRTFunc = procedure(dest : PDouble; LineWidth : NativeInt; width, height : NativeInt);
  TMatrixAbsFunc = procedure(dest : PDouble; LineWidth : NativeInt; width, height : NativeInt);
  TMatrixCopyFunc = procedure(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt);
  TMatrixInitFunc = procedure(dest : PDouble; destLIneWidth : NativeInt; Width, Height : NativeInt; const value : double );
  TMatrixMinMaxFunc = function(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
  TMatrixMinMaxTriaFunc = function(mt : PDouble; N : NativeInt; const LineWidth : NativeInt) : double;
  TMatrixTransposeFunc = procedure(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
  TMatrixTransposeInplaceFunc = procedure(mt : PDouble; const LineWidth : NativeInt; N : NativeInt);
  TMatrixElemWiseNormFunc = function (dest : PDouble; LineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;
  TMatrixNormalizeFunc = procedure(dest : PDouble; destLineWidth : NativeInt; src : PDouble; srcLineWidth : NativeInt; Width, Height : NativeInt; RowWise : Boolean);
  TMatrixVarianceFunc = procedure(dest : PDouble; destLineWidth : NativeInt; src : PDouble; srcLineWidth : NativeInt; Width, Height : NativeInt; RowWise : Boolean; unbiased : boolean);
  TMatrixRowSwapFunc = procedure (A, B : PDouble; width : NativeInt);
  TMatrixColSwapFunc = procedure(A, B : PDouble; const LineWidthAB : NativeInt; Height : NativeInt);
  TMatrixMedianFunc = procedure (dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; tmp : PDouble = nil);
  TMatrixSymRank2Func = procedure( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
                                   B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt );
  TMatrixSortFunc = procedure (dest : PDouble; destLineWidth : NativeInt; width, height : integer; RowWise : boolean; tmp : PDouble = nil);
  TMatrixVecMultFunc = procedure (dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
  TMatrixVecMultSymFunc = procedure (dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
  TMatrixDotMultFunc = function( x : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt ) : double;
  TMatrixVecAddFunc = procedure( X : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt; const alpha : double );
  TMatrixRank1UpdateFunc = procedure (A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
                                      X, Y : PDouble; incX, incY : NativeInt; alpha : double);

  TApplyPlaneRotSeqMatrix = procedure (width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
  TMatrixRotate = procedure (N : NativeInt; DX : PDouble; const LineWidthDX : NativeInt; DY : PDouble; LineWidthDY : NativeInt; const c, s : double);
  TMemInitFunc = procedure(A : PDouble; NumBytes : NativeInt; Value : double);
  TVecConvolve = procedure (dest : PDouble; A, B : PDouble; aLen, bLen : NativeInt);


implementation

{$I 'mrMath_CPU.inc'}

{$IFDEF FPC} {$S-} {$ENDIF}

uses {$IFNDEF MRMATH_NOASM}
     ASMMatrixOperations,
     {$IFNDEF x64}
     ASMMatrixMultOperations,
     ASMMatrixRotations, ASMMoveOperations, ASMVecConvolve, AVXVecConvolve,
     ASMMatrixTransposeOperations, ASMMatrixAddSubOperations,
     AVXMatrixOperations, AVXMatrixMultOperations, AVXMatrixRotations,
     AVXMatrixAddSubOperations, AVXMoveOperations, AVXMatrixTransposeOperations,
     FMAMatrixOperations, FMAMatrixMultOperations, FMAVecConvolve,
     {$ELSE}
     AVXMatrixOperations, ASMMatrixMultOperationsx64, AVXMatrixMultOperationsx64,
     AVXMatrixAddSubOperationsx64, AVXMoveOperationsx64,
     ASMMatrixRotationsx64, ASMMoveOperationsx64, ASMMatrixAddSubOperationsx64,
     AVXMatrixRotationsx64, AVXMatrixTransposeOperationsx64,
     ASMMatrixTransposeOperationsx64, FMAMatrixOperations, FMAMatrixMultOperationsx64,
     ASMVecConvolvex64, AVXVecConvolvex64, FMAVecConvolvex64,
     {$ENDIF}
     {$ENDIF}
     BlockedMult, BlockSizeSetup, SimpleMatrixOperations, CPUFeatures, MatrixRotations, Corr,
     RandomEng, RollingMedMean;

var multFunc : TMatrixMultFunc;
    blockedMultFunc : TMatrixBlockedMultfunc;
    blockedMultT1Func : TMatrixBlockedMultfunc;
    blockedMultT2Func : TMatrixBlockedMultfunc;
    multT2Func : TMatrixMultFunc;
    multTria2T1Func : TMatrixMultTria2T1;
    multTria2TUpperUnitFunc : TMatrixMultTria2T1;
    multRightUpperTriaNoUnitT2Func : TMatrixMultTriaStoreT1;
    multRightLowerTriaUnitT2Func : TMatrixMultTriaStoreT1;
    multRightUpperTriaUnitT2Func : TMatrixMultTriaStoreT1;
    multRightLowerTriaNoUnitT2Func : TMatrixMultTriaStoreT1;
    multRightUpperTriaNoUnitFunc : TMatrixMultTriaStoreT1;
    multRightUpperTriaUnitFunc : TMatrixMultTriaStoreT1;
    multRightLowerTriaUnitFunc : TMatrixMultTriaStoreT1;
    multRightLowerTriaNoUnitFunc : TMatrixMultTriaStoreT1;
    multUpTranspNoUnitVecFunc : TMatrixMultVecTriaStoreX;
    multLowTranspUnitVecFunc : TMatrixMultVecTriaStoreX;
    multLowNoTranspUnitVecFunc : TMatrixMultVecTriaStoreX;
    multLowNoTranspNoUnitVecFunc : TMatrixMultVecTriaStoreX;
    multUpNoTranspNoUnitVecFunc : TMatrixMultVecTriaStoreX;
    MtxVecMultFunc : TMatrixVecMultFunc;
    MtxVecMultTFunc : TMatrixVecMultFunc;
    MtxVecMultUpperSymFunc : TMatrixVecMultSymFunc;
    MtxVecMultLowerSymFunc : TMatrixVecMultSymFunc;
    MatrixVecDotMultFunc : TMatrixDotMultFunc;
    MatrixVecAddFunc : TMatrixVecAddFunc;
    elemAddFunc : TMatrixElemAddFunc;
    addFunc : TMatrixAddFunc;
    subFunc : TMatrixSubFunc;
    subTFunc : TMatrixSubTFunc;
    subVecFunc : TMatrixSubVecFunc;
    addVecFunc : TMatrixSubVecFunc;
    elemWiseFunc : TMatrixElemWiseFunc;
    elemWiseDivFunc : TMatrixElemWiseFunc;
    addScaleFunc : TMatrixAddScaleFunc;
    scaleAddFunc : TMatrixAddScaleFunc;
    sqrtFunc : TMatrixSQRTFunc;
    absFunc : TMatrixAbsFunc;
    copyFunc : TMatrixCopyFunc;
    initfunc : TMatrixInitFunc;
    maxFunc : TMatrixMinMaxFunc;
    minFunc : TMatrixMinMaxFunc;
    absMinFunc : TMatrixMinMaxFunc;
    absMaxFunc : TMatrixMinMaxFunc;
    minLowerFunc : TMatrixMinMaxTriaFunc;
    maxLowerFunc : TMatrixMinMaxTriaFunc;
    minUpperFunc : TMatrixMinMaxTriaFunc;
    maxUpperFunc : TMatrixMinMaxTriaFunc;
    absMinLowerFunc : TMatrixMinMaxTriaFunc;
    absMaxLowerFunc : TMatrixMinMaxTriaFunc;
    absMinUpperFunc : TMatrixMinMaxTriaFunc;
    absMaxUpperFunc : TMatrixMinMaxTriaFunc;
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
    colSwapFunc : TMatrixColSwapFunc;
    Rank1UpdateFunc : TMatrixRank1UpdateFunc;
    SymRank2UpdateFunc : TMatrixSymRank2Func;
    matrixRot : TMatrixRotate;
    PlaneRotSeqRVB : TApplyPlaneRotSeqMatrix;
    PlaneRotSeqRVF : TApplyPlaneRotSeqMatrix;
    PlaneRotSeqLVB : TApplyPlaneRotSeqMatrix;
    PlaneRotSeqLVF : TApplyPlaneRotSeqMatrix;
    memInitFunc : TMemInitFunc;
    vecConvolve : TVecConvolve;

 // current initialization
var curUsedCPUInstrSet : TCPUInstrType;
    curUsedStrassenMult : boolean;

function MtxAlloc( NumBytes : NativeInt ) : Pointer;
begin
     assert(NumBytes and $7 = 0, 'Numbytes not multiple of 8');

     Result := nil;
     if NumBytes <= 0 then
        exit;

     // align to next multiple of 32 bytes
     if NumBytes and $1F <> 0 then
        NumBytes := NumBytes and $FFFFFFE0 + $20;

     Result := GetMemory(NumBytes);
     if Assigned(Result) then
        MtxMemInit(Result, NumBytes, 0 );
end;

function MtxAllocAlign( NumBytes : NativeInt; var mem : Pointer ) : Pointer;
begin
     Result := MtxMallocAlign( NumBytes, mem );

     if Assigned(Result) then
        MtxMemInit(mem, NumBytes, 0 );
end;

function MtxMallocAlign( NumBytes : NativeInt; var mem : Pointer ) : Pointer;
begin
     Result := nil;
     mem := nil;
     if NumBytes <= 0 then
        exit;

     // align to next multiple of 32 bytes
     inc(NumBytes, $40);
     if NumBytes and $1F <> 0 then
        NumBytes := NumBytes and $FFFFFFE0 + $20;

     mem := GetMemory(NumBytes);
     if Assigned(mem) then
        Result := AlignPtr32( mem );
end;

function MtxMallocAlign( width, height : NativeInt; var LineWidth : NativeInt; var mem : Pointer ) : Pointer; overload;
var numBytes : NativeInt;
begin
     if width and $03 <> 0 then
        width := width + 4 - (width and $03);

     Result := nil;
     LineWidth := sizeof(double)*width;

     numBytes := $20 + Height*LineWidth;
     mem := GetMemory( numBytes );
     if Assigned(mem) then
        Result := AlignPtr32( mem );
end;

function MtxAllocAlign( width, height : NativeInt; var LineWidth : NativeInt; var Mem : Pointer) : Pointer; overload;
var numBytes : NativeInt;
begin
     if width and $03 <> 0 then
        width := width + 4 - (width and $03);

     Result := nil;
     LineWidth := sizeof(double)*width;

     numBytes := $20 + Height*LineWidth;
     mem := GetMemory( numBytes );
     if Assigned(mem) then
     begin
          Result := AlignPtr32( mem );
          MtxMemInit(mem, NumBytes, 0 );
     end;
end;

procedure MtxMemInit( A : PDouble; NumBytes : NativeInt; const value : double );
var ptr : PDouble;
begin
     assert(numBytes mod 8 = 0, 'error only multiple of sizeof(double) allowed');
     // normally getMemory should return aligned bytes ->
     // but just in case:
     ptr := A;
     while (NumBytes > 0) and ( (NativeUint(ptr) and $1F) <> 0 ) do
     begin
          ptr^ := Value;
          inc(ptr);
          dec(NumBytes, sizeof(double));
     end;

     if NumBytes > 0 then
        memInitfunc(ptr, NumBytes, Value);
end;

procedure MatrixCopy(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt);
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     copyFunc(dest, destLineWidth, Src, srcLineWidth, width, height);
end;

procedure MatrixCopy(var dest : Array of double; const Src : Array of double; width, height : NativeInt);
begin
     assert((width > 0) and (height > 0) and (Length(dest) = Length(src)) and (length(src) = width*height), 'Dimension error');
     copyFunc(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height);
end;

function MatrixCopy(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);

     copyFunc(@Result[0], width*sizeof(double), Src, srcLineWidth, width, height);
end;

function MatrixCopy(const Src : Array of double; width, height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (length(src) = width*height), 'Dimension error');
     SetLength(Result, width*height);
     copyfunc(@Result[0],  width*sizeof(double), @Src[0], width*sizeof(double), width, height);
end;

procedure MatrixIndex(dest : PDouble; const destLineWidth : NativeInt; src : PDouble; const srcLineWidth : NativeInt; colIdx, rowIdx : TIntegerDynArray);
begin
     GenericMtxIndex( dest, destLineWidth, src, srcLineWidth, colIdx, rowIdx );
end;

procedure MatrixInit(var dest : Array of double; const Value : double);
begin
     if Length(Dest) > 0 then
        MatrixInit(@dest[0], Length(dest)*sizeof(double), Length(dest), 1, value);
end;

procedure MatrixInit(dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; const value : Double);
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     initfunc( dest, destLineWidth, width, height, Value);
end;

function MatrixAdd(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     addFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixAddVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
begin
     addVecFunc(A, LineWidthA, B, incX, width, Height, rowWise);
end;

procedure MatrixRowSwap(A, B : PDouble; width : NativeInt);
begin
     assert((width > 0), 'Dimension error');

     rowSwapFunc(A, B, width);
end;

procedure MatrixColSwap(A, B : PDouble; const LineWidthAB : NativeInt; Height : NativeInt);
begin
     colswapFunc(A, B, LineWidthAB, Height);
end;

procedure MatrixAdd(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
begin
     addFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixAdd(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := MatrixAdd(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure MatrixAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     addFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

function MatrixSub(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     SubFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixSubT(A : PDouble; LineWidthA : NativeInt; B : PDouble; LineWidthB : NativeInt; width, height : NativeInt);
begin
     subTFunc(A, LineWidthA, B, LineWidthB, width, height);
end;

procedure MatrixSubVec(A : PDouble; LineWidthA : NativeInt; B : PDouble; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
begin
     subVecFunc(A, LineWidthA, B, incX, width, height, RowWise);
end;

procedure MatrixSub(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
begin
     SubFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixSub(const mt1, mt2 : array of double; width : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := MatrixSub(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure MatrixSub(var dest : Array of double; const mt1, mt2 : Array of double; width : NativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     subFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

function MatrixMult(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     SetLength(Result, Height1*width2);
     MatrixMult(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     MatrixMult(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function MatrixMult(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, height1*width2);
     MatrixMult(@Result[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure MatrixMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var multBlockSize : NativeInt;
begin
     multBlockSize := BlockedMatrixMultSize*BlockedMatrixMultSize;
     if ((width1 >= BlockedMatrixMultSize) and (height1 >= BlockedMatrixMultSize) and (height2 >= BlockedMatrixMultSize)) or
        (width1*height1 >= multBlockSize) or (width2*Height2 >= multBlockSize)
     then
         blockedMultFunc(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, doNone, nil)
     else
         multFunc(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultEx(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultFunc(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

// mt1' * mt2
function MatrixMultT1(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');
     SetLength(Result, width1*width2);
     MatrixMultT1(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultT1(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(Length(dest) >= width1*width2, 'Dimension Error');
     MatrixMultT1(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function MatrixMultT1(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, width1*width2);
     MatrixMultT1(@Result[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure MatrixMultT1(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     blockedMultT1Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, doNone, nil);
end;

procedure MatrixMultT1Ex(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultT1Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

// mt1 * mt2'
function MatrixMultT2(mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');
     SetLength(Result, height1*height2);
     MatrixMultT2(@Result[0], sizeof(double)*height2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultT2(var dest : Array of Double; mt1, mt2 : Array of double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(Length(dest) >= height1*height2, 'Dimension Error');
     MatrixMultT2(@dest[0], height2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function MatrixMultT2(const mt1, mt2 : Array of Double; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, height1*height2);
     MatrixMultT2(@Result[0], height2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure MatrixMultT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     multT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure MatrixMultT2Ex(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : PDouble); overload;
begin
     blockedMultT2Func(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, blockSize, op, mem);
end;

procedure MatrixMultTria2T1(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multTria2T1Func(dest, LineWidthDest, mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightUpperTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightUpperTriaNoUnitT2Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightUpperTriaNoUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightUpperTriaNoUnitFunc(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultTria2TUpperUnit(dest : PDouble; LineWidthDest : NativeInt; mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multTria2TUpperUnitFunc(dest, LineWidthDest, mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightLowerTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightLowerTriaUnitT2Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightUpperTriaUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightUpperTriaUnitT2Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightLowerTriaNoUnitT2(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightLowerTriaNoUnitT2Func(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightUpperTriaUnit(mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightUpperTriaUnitFunc(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightLowerTriaUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightLowerTriaUnitFunc(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultRightLowerTriaNoUnit( mt1 : PDouble; LineWidth1 : NativeInt; mt2 : PDouble; LineWidth2 : NativeInt;
  width1, height1, width2, height2 : NativeInt);
begin
     multRightLowerTriaNoUnitFunc(mt1, LineWidth1, mt2, LineWidth2, width1, height1, width2, height2);
end;

procedure MtxMultUpTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
begin
     multUpTranspNoUnitVecFunc(mt1, LineWidth1, x, incX, len);
end;

procedure MtxMultLowTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
begin
     multLowTranspUnitVecFunc(mt1, LineWidth1, x, incX, len);
end;

procedure MtxMultLowNoTranspUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
begin
     multLowNoTranspUnitVecFunc(mt1, LineWidth1, x, incX, len);
end;

procedure MtxMultLowNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; IncX : NativeInt; len : NativeInt);
begin
     multLowNoTranspNoUnitVecFunc(mt1, LineWidth1, x, incX, len);
end;


procedure MtxMultUpNoTranspNoUnitVec( mt1 : PDouble; LineWidth1 : NativeInt; x : PDouble; incX : NativeInt; len : NativeInt);
begin
     multUpNoTranspNoUnitVecFunc(mt1, LineWidth1, x, incX, len);
end;

procedure MatrixMtxVecMult(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
begin
     MtxVecMultFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure MatrixMtxVecMultT(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; width, height : NativeInt; alpha, beta : double);
begin
     MtxVecMultTFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, width, height, alpha, beta);
end;

procedure MatrixMtxVecMultUpperSym(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
begin
     if N > 0 then
        MtxVecMultUpperSymFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, N, alpha, beta);
end;

function MatrixVecDotMult(  x : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt ) : double;
begin
     if N > 0 then
     begin
          Result := MatrixVecDotMultFunc( x, incX, y, incY, N);
          
          //if incX = sizeof(double) 
//          then
//              MatrixMtxVecMult(@result, sizeof(double), x, y, N*sizeof(double), incY, N, 1, 1, 0 )
//          else
//              MatrixMtxVecMultT(@result, sizeof(double), x, y, incX, incY, 1, N, 1, 0);
     end
     else
         Result := 0;
end;

procedure MatrixVecAdd( X : PDouble; incX : NativeInt; y : PDouble; incY : NativeInt; N : NativeInt; const alpha : double );
begin
     if N > 0 then
        MatrixVecAddFunc( X, incX, Y, incY, N, alpha );
end;

procedure MatrixMtxVecMultLowerSym(dest : PDouble; destLineWidth : NativeInt; mt1, v : PDouble; LineWidthMT, LineWidthV : NativeInt; N : NativeInt; alpha, beta : double);
begin
     MtxVecMultLowerSymFunc(dest, destLineWidth, mt1, V, LineWidthMT, LineWidthV, N, alpha, beta);
end;

procedure MatrixRank1Update(A : PDouble; const LineWidthA : NativeInt; width, height : NativeInt;
  const alpha : double; X, Y : PDouble; incX, incY : NativeInt);
begin
     if (width <= 0) or (height <= 0) then
        exit;

     Rank1UpdateFunc(A, LineWidthA, width, height, x, y, incx, incy, alpha)
end;

procedure MatrixUpperSymRank2Update( C : PDouble; LineWidthC : NativeInt; A : PDouble; LineWidthA : NativeInt;
  B : PDouble; LineWidthB : NativeInt; N : NativeInt; k : NativeInt );
begin
     if (N = 0) or (k = 0) then
        exit;

     SymRank2UpdateFunc( C, LineWidthC, A, LineWidthA, B, LineWidthB, N, k );
end;

procedure MatrixElemMult(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
begin
     elemWiseFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixElemMult(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     elemWiseFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt); overload;
begin
     assert((width > 0), 'Dimension Error');
     assert(Length(mt1) >= width, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     elemWiseFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, Height, width*sizeof(double), width*sizeof(double));
end;

function MatrixElemMult(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
begin
     Result := MatrixElemMult(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

procedure MatrixElemDiv(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
begin
     elemWiseDivFunc(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function MatrixElemDiv(mt1, mt2 : PDouble; width : NativeInt; height : NativeInt; const LineWidth1, LineWidth2 : NativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     elemWiseDivFunc(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure MatrixElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt); overload;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     elemWiseDivFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function MatrixElemDiv(const mt1, mt2 : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
begin
     Result := MatrixElemDiv(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;



function MatrixTranspose(mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension Error');
     SetLength(Result, height*width);
     transposeFunc(@Result[0], height*sizeof(double), mt, LineWidth, width, height);
end;

procedure MatrixTranspose(dest : PDouble; const destLineWidth : NativeInt; mt : PDouble; const LineWidth : NativeInt; width : NativeInt; height : NativeInt);
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)) and (destLineWidth >= height*sizeof(double)), 'Dimension Error');
     transposeFunc(dest, destLineWidth, mt, LineWidth, width, height);
end;

function MatrixTranspose(const mt : Array of Double; width : NativeInt; height : NativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (Length(mt) = width*height), 'Dimension Error');
     Result := MatrixTranspose(@mt[0], width*sizeof(double), width, height);
end;

procedure MatrixTranspose(var dest : Array of Double; const mt : Array of Double; width : NativeInt; height : NativeInt);
begin
     assert((width > 0) and (height > 0) and (Length(mt) = width*height) and (length(dest) = Length(mt)), 'Dimension Error');
     transposeFunc(@dest[0], height*sizeof(double), @mt[0], width, height, width*sizeof(double));
end;

procedure MatrixTransposeInplace(mt : PDouble; const LineWidth : NativeInt; N : NativeInt);
begin
     TransposeInplaceFunc(mt, LineWidth, N);
end;

function MatrixMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     if Width = 1
     then
         Result := GenericMtxMax(mt, width, height, LineWidth)
     else
         Result := maxFunc(mt, width, height, LineWidth);
end;

function MatrixMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     if Width = 1
     then
         Result := GenericMtxMin(mt, width, height, LineWidth)
     else
         Result := minFunc(mt, width, height, LineWidth);
end;

function MatrixAbsMax(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     if Width = 1
     then
         Result := GenericMtxAbsMin(mt, width, height, LineWidth)
     else
         Result := absMinFunc(mt, width, height, LineWidth);
end;

function MatrixAbsMin(mt : PDouble; width, height : NativeInt; const LineWidth : NativeInt) : double;
begin
     assert((width > 0) and (height > 0) and (LineWidth >= width*sizeof(double)), 'Dimension error');

     if Width = 1
     then
         Result := GenericMtxMax(mt, width, height, LineWidth)
     else
         Result := absMaxFunc(mt, width, height, LineWidth);
end;


function MatrixMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := mt^
     else
         Result := maxUpperFunc(mt, N, LineWidth );
end;

function MatrixMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := mt^
     else
         Result := minUpperFunc(mt, N, LineWidth );
end;

function MatrixMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := mt^
     else
         Result := maxLowerFunc(mt, N, LineWidth );
end;

function MatrixMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := mt^
     else
         Result := minLowerFunc(mt, N, LineWidth );
end;

function MatrixAbsMaxUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := Abs(mt^)
     else
         Result := absMaxUpperFunc(mt, N, LineWidth );
end;

function MatrixAbsMinUpper( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := Abs(mt^)
     else
         Result := absMinUpperFunc(mt, N, LineWidth );
end;

function MatrixAbsMaxLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := Abs(mt^)
     else
         Result := absMaxLowerFunc(mt, N, LineWidth );
end;

function MatrixAbsMinLower( mt : PDouble; N : NativeInt; const LineWidth : NativeInt ) : double;
begin
     assert((N > 0) and (LineWidth >= N*sizeof(double)), 'Dimension error');

     if N = 1
     then
         Result := Abs(mt^)
     else
         Result := absMinLowerFunc(mt, N, LineWidth );
end;


function MatrixNormalize(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixNormalize(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise);
end;

function MatrixNormalize(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixNormalize(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixNormalize(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');
     matrixNormalizeFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixNormalize(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean);
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixNormalize(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise);
end;

function MatrixMean(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixMean(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise);
end;

function MatrixMean(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixMean(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixMean(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     assert((RowWise and (destLineWidth >= sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixMeanFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixMean(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixMean(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise);
end;

function MatrixMedian(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixMedian(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise, nil);
end;

function MatrixMedian(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixMedian(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixMedian(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; hlpMem : PDouble); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     assert((RowWise and (destLineWidth >= sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixMedianFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise, hlpMem);
end;

procedure MatrixMedian(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixMedian(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise, nil);
end;

procedure MatrixRollMedian( dest : PDouble; const destLineWidth : NativeInt; Width, Height : NativeInt; order : integer; rowWise : boolean);
var rollMed : TRollingMedian;
    i : Integer;
begin
     if (width <= 0) or (height <= 0) then
        exit;
     rollMed := TRollingMedian.Create(order);
     try
        if RowWise then
        begin
             for i := 0 to Height - 1 do
             begin
                  rollMed.FilterVals(dest, sizeof(double), Width);
                  rollMed.Clear;
                  inc(PByte(dest), destLineWidth);
             end;
        end
        else
        begin
             for i := 0 to Width - 1 do
             begin
                  rollMed.FilterVals(dest, destLineWidth, Height);
                  rollMed.Clear;
                  inc(dest);
             end;
        end;
     finally
            rollMed.Free;
     end;
end;

procedure MatrixSort(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; hlpMem : PDouble = nil);
begin
     if (width <= 0) or (height <= 0) then
        exit;

     matrixSortFunc(dest, destLineWidth, width, height, RowWise, hlpMem);
end;

function MatrixVar(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixVar(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise, unbiased);
end;

function MatrixVar(const Src : Array of double; width, height : NativeInt; RowWise : boolean; unbiased : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixVar(@Src[0], width*sizeof(double), width, height, RowWise, unbiased);
end;

procedure MatrixVar(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     assert((RowWise and (destLineWidth >= sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixVarFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise, unbiased);
end;

procedure MatrixVar(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean; unbiased : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
     MatrixVar(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise, unbiased);
end;

function MatrixSum(Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);
     MatrixSum(@Result[0], width*sizeof(double), src, srcLineWidth, width, height, RowWise);
end;

procedure VecMeanVar( var dest : TMeanVarRec; vec : PDouble; width : NativeInt; unbiased : boolean);  
begin
     MatrixMeanVar( @dest, sizeof(dest), vec, width*sizeof(double), width, 1, True, unbiased);
end;

procedure MatrixMeanVar(dest : PDouble; destLineWidth : NativeInt; Src : PDouble; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
begin
     assert((RowWise and (destLineWidth >= 2*sizeof(double))) or (not RowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');
     matrixMeanVarFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise, unbiased);
end;

procedure MatrixNormalizeMeanVar(var aVec : Array of double);
begin
     if Length(aVec) <> 0 then
        MatrixNormalizeMeanVar(@aVec[0], Length(aVec)*sizeof(double), Length(aVec), 1, True);
end;

procedure MatrixNormalizeMeanVar(dest : PDouble; destLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var meanVar : TDoubleDynArray;
    x, y : NativeInt;
    invstdDev : double;
const cMinVar = 1e-300;
begin
     if RowWise then
     begin
          SetLength(meanVar, 2*height);
          MatrixMeanVar( @meanVar[0], 2*sizeof(double), dest, destLineWidth, width, height, True, True);

          for y := 0 to Height - 1 do
          begin
               invstdDev := 0;
               if meanVar[1 + y*2] > cMinVar then
                  invstdDev := 1/sqrt(meanVar[1 + y*2]);

               MatrixAddAndScale(Dest, destLineWidth, Width, 1, -meanVar[0 + y*2], invstdDev);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          SetLength(meanVar, 2*Width);
          MatrixMeanVar( @meanVar[0], width*sizeof(double), dest, destLineWidth, width, height, False, True);

          for x := 0 to Width - 1 do
          begin
               invstdDev := 0;
               if meanVar[x + width] > cMinVar then
                  invstdDev := 1/sqrt(meanVar[x + width]);

               MatrixAddAndScale(Dest, destLineWidth, 1, Height, -meanVar[x], invstdDev);
               inc(dest);
          end;
     end;
end;

function MatrixSum(const Src : Array of double; width, height : NativeInt; RowWise : boolean) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0) and (Length(src) >= width*height), 'Dimension error');
     Result := MatrixSum(@Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixSum(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and
            ( ((not rowWise) and (destLineWidth >= width*sizeof(double))) or
              ((rowWise) and (destLineWidth >= sizeof(double)) )) , 'Dimension error');
     matrixSumFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : NativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and
             ( ((not rowWise) and (Length(dest) >= width)) or
               ((rowWise) and (Length(dest) >= height) )) and
                (Length(src) >= width*height), 'Dimension error');
     MatrixSum(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height, RowWise);
end;

procedure MatrixCumulativeSum(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
begin
     assert( (width > 0) and (height > 0), 'Dimension error');
      
     matrixCumulativeSumFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixDiff(dest : PDouble; const destLineWidth : NativeInt; Src : PDouble; const srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); 
begin
     assert( (Width > 0) and (height > 0), 'Dimension error');

     matrixDiffFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixElemAdd( Dest : PDouble; const LineWidth, Width, Height : NativeInt; const Offset : double );
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     elemAddFunc(dest, linewidth, width, height, Offset);
end;

procedure MatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     addScaleFunc(dest, linewidth, width, height, Offset, Scale);
end;

procedure MatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : NativeInt; const Offset, Scale : double);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     scaleAddFunc(dest, linewidth, width, height, Offset, Scale);
end;

procedure MatrixSQRT(Dest : PDouble; const LineWidth, Width, Height : NativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     sqrtFunc(Dest, LineWidth, Width, Height);
end;

procedure MatrixAbs(Dest : PDouble; const LineWidth, Width, Height : NativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(LineWidth >= width*sizeof(double), 'Line width error');

     absFunc(Dest, LineWidth, Width, Height);
end;

// element wise eukledian norm
function MatrixElementwiseNorm2(Src : PDouble; const srcLineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert(srcLineWidth >= width*sizeof(double), 'Line width error');
     Result := elemNormFunc(Src, srcLineWidth, Width, Height, doSqrt);
end;


procedure MatrixConvolve(dest : PDouble; const LineWidthDest : NativeInt; A, B : PDouble; const LineWidthA : NativeInt; width, height, veclen : NativeInt; mem : Pointer = nil);
var y : NativeInt;
    revB : PConstDoubleArr;
    aMem : Pointer;
    cpuAlign : integer;
    origVecLen : integer;
    i: Integer;
    offset : integer;
    x : integer;
    swinginWidth : integer;
    z: Integer;
    aSum : double;
    pA, pDest : PConstDoubleArr;
begin
     // reverse B -> faster access
     // round up to 4 and fill with zeros so we can streamline the function better
     cpuAlign := 0;
     origVecLen := veclen;
     if curUsedCPUInstrSet = itSSE then
        cpuAlign := 2;
     if curUsedCPUInstrSet = itAVX then
        cpuAlign := 4;
     if curUsedCPUInstrSet = itFMA then
        cpuAlign := 4;

     aMem := nil;

     if Assigned(mem)
     then
         revB := AlignPtr32(mem)
     else
         revB := PConstDoubleArr(MtxMallocAlign( (veclen + 2*cpuAlign)*sizeof(double), aMem));
     if cpuAlign <> 0 then
     begin
          if veclen and (cpuAlign - 1) <> 0 then
             veclen := veclen + cpuAlign - veclen and (cpuAlign - 1);
     end;

     swinginWidth := veclen;
     if vecLen > width then
        swinginWidth := width;

     // zeropad
     offset := (veclen - origVecLen);
     for i := 0 to offset - 1 do
         revB^[i] := 0;
     // reverse
     for i := 0 to origVecLen - 1 do
         revB^[i + offset] := PConstDoubleArr(B)^[origVecLen - i - 1];

     for y := 0 to height - 1 do
     begin
          pDest := GenPtrArr(dest, 0, y, LineWidthDest);
          pA := GenPtrArr( A, 0, y, LineWidthA );
          // swing in -> zero pad
          for x := 0 to swinginWidth - 1 do
          begin
               aSum := 0;
               for z := 0 to x do
                   aSum := aSum + revB^[vecLen - x + z - 1]*pA^[z];

               pDest^[x] := aSum;
          end;

          // vector convolution without swing in
          if width > vecLen then
             vecConvolve(@pDest^[veclen], @pA^[veclen], PDouble(revB), width - veclen, veclen);
     end;

     if Assigned(aMem) then
        FreeMem(aMem);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixObjFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefObjFunc);
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixFunc); overload;
begin
     GenericSubMtxFunc(dest, destLineWidth, startX, starty, width, height, func);
end;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixObjFunc); overload;
begin
     GenericSubMtxFunc(dest, destLineWidth, startX, starty, width, height, func);
end;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefFunc); overload;
begin
     GenericSubMtxFunc(dest, destLineWidth, startX, starty, width, height, func);
end;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefObjFunc); overload;
begin
     GenericSubMtxFunc(dest, destLineWidth, startX, starty, width, height, func);
end;


{$IFDEF ANONMETHODS}

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixFuncRef); overload;
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure MatrixFunc(dest : PDouble; const destLineWidth : NativeInt; width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;
begin
     GenericMtxFunc(dest, destLineWidth, width, height, func);
end;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixFuncRef); overload;
begin
     GenericSubMtxFunc(dest, destLineWidth, startx, starty, width, height, func);
end;

procedure SubMatrixFunc(dest : PDouble; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TMatrixMtxRefFuncRef); overload;
begin
     GenericSubMtxFunc(dest, destLineWidth, startx, starty, width, height, func);
end;

{$ENDIF}

procedure InitSSEOptFunctions(instrType : TCPUInstrType);
begin
     InitMathFunctions(instrType, curUsedStrassenMult);
end;

procedure InitMult(useStrassenMult : boolean);
begin
     InitMathFunctions(curUsedCPUInstrSet, useStrassenMult);
end;

procedure ApplyPlaneRotSeqRVB(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqRVB(width, height, A, LineWidthA, C, S);
end;
procedure ApplyPlaneRotSeqRVF(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqRVF(width, height, A, LineWidthA, C, S);
end;

procedure ApplyPlaneRotSeqLVB(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqLVB(width, height, A, LineWidthA, C, S);
end;

procedure ApplyPlaneRotSeqLVF(width, height : NativeInt; A : PDouble; const LineWidthA : NativeInt; C, S : PConstDoubleArr);
begin
     PlaneRotSeqLVF(width, height, A, LineWidthA, C, S);
end;

procedure MatrixRotate(N : NativeInt; DX : PDouble; const LineWidthDX : NativeInt; DY : PDouble; LineWidthDY : NativeInt; const c, s : double);
begin
     matrixRot(N, DX, LineWidthDX, DY, LineWidthDY, c, s);
end;

function GetCurCPUInstrType : TCPUInstrType;
begin
     Result := curUsedCPUInstrSet;
end;

procedure InitMathFunctions(instrType : TCPUInstrType; useStrassenMult : boolean);
{$IF defined(x86) or defined(x64)}
var fpuCtrlWord : Word;
{$IFEND}
begin
     // decrease instruction set until we find one suitable
     {$IFNDEF MRMATH_NOASM}
     if (instrType = itFMA) and not IsFMAPresent then
        instrType := itAVX;
     if (instrType = itAVX) and not IsAVXPresent then
        instrType := itSSE;
     {$ENDIF}

     // generic functions that have no specialized implementation
     initfunc := GenericMtxInit;
     elemAddFunc := GenericMtxElemAdd;
     multRightUpperTriaUnitFunc := GenericMtxMultRightUpperTriaUnit;
     multRightLowerTriaUnitFunc := GenericMtxMultRightLowerTriaUnit;
     multRightLowerTriaNoUnitFunc := GenericMtxMultRightLowerTriaNoUnit;
     multUpTranspNoUnitVecFunc := GenericMtxMultUpTranspNoUnitVec;
     multRightUpperTriaUnitT2Func := GenericMtxMultRightUpperTriaUnitT2;
     multRightLowerTriaUnitT2Func := GenericMtxMultRightLowerTriaUnitT2;
     multRightLowerTriaNoUnitT2Func := GenericMtxMultRightLowerTriaNoUnitT2;
     multLowTranspUnitVecFunc := GenericMtxMultLowTranspUnitVec;
     multLowNoTranspUnitVecFunc := GenericMtxMultLowNoTranspUnitVec;
     multLowNoTranspNoUnitVecFunc := GenericMtxMultLowNoTranspNoUnitVec;
     multUpNoTranspNoUnitVecFunc := GenericMtxMultUpNoTranspNoUnitVec;
     minLowerFunc := GenericMtxMinLower;
     maxLowerFunc := GenericMtxMaxLower;
     minUpperFunc := GenericMtxMinUpper;
     maxUpperFunc := GenericMtxMaxUpper;
     absMinFunc := GenericMtxAbsMin;
     absMaxFunc := GenericMtxAbsMax;
     absMinLowerFunc := GenericMtxAbsMinLower;
     absMaxLowerFunc := GenericMtxAbsMaxLower;
     absMinUpperFunc := GenericMtxAbsMinUpper;
     absMaxUpperFunc := GenericMtxAbsMaxUpper;
     MtxVecMultUpperSymFunc := GenericMtxVecMultUpperSym;
     MtxVecMultLowerSymFunc := GenericMtxVecMultLowerSym;
     MatrixVecAddFunc := GenericVecAdd;
     matrixMedianFunc := GenericMtxMedian;
     matrixSortFunc := GenericMtxSort;
     colSwapFunc := GenericColSwap;

     {$IFDEF MRMATH_NOASM}
     TDynamicTimeWarp.UseSSE := False;
     TRandomGenerator.cpuSet := TChaChaCPUInstrType.itFPU;
     {$ENDIF}
     {$IFNDEF MRMATH_NOASM}
     TDynamicTimeWarp.UseSSE := True;
     TRandomGenerator.cpuSet := TChaChaCPUInstrType.itSSE;

     // check features - IsAVXPresent actually checks for avx2
     if (IsAVXPresent and (instrType = itAVX)) or (IsFMAPresent and (instrType = itFMA)) then
     begin
          curUsedCPUInstrSet := itAVX;
          if useStrassenMult
          then
              multFunc := AVXStrassenMatrixMultiplication
          else
              multFunc := AVXMatrixMult;
          addFunc := AVXMatrixAdd;
          subFunc := AVXMatrixSub;
          subTFunc := AVXMatrixSubT;
          subVecFunc := AVXMatrixSubVec;
          addVecFunc := AVXMatrixAddVec;
          elemWiseFunc := AVXMatrixElemMult;
          addScaleFunc := AVXMatrixAddAndScale;
          scaleAddFunc := AVXMAtrixScaleAndAdd;
          sqrtFunc := AVXMatrixSQRT;
          blockedMultFunc := BlockMatrixMultiplication;
          blockedMultT1Func := BlockMatrixMultiplicationT1;
          blockedMultT2Func := BlockMatrixMultiplicationT2;
          copyFunc := AVXMatrixCopy;
          minFunc := AVXMatrixMin;
          maxFunc := AVXMatrixMax;
          transposeFunc := AVXMatrixTranspose;
          transposeInplaceFunc := AVXMatrixTransposeInplace;
          elemNormFunc := AVXMatrixElementwiseNorm2;
          matrixNormalizeFunc := AVXMatrixNormalize;
          matrixMeanFunc := AVXMatrixMean;
          matrixVarFunc := AVXMatrixVar;
          matrixMeanVarFunc := AVXMatrixMeanVar;
          matrixSumFunc := AVXMatrixSum;
          matrixCumulativeSumFunc := AVXMatrixCumulativeSum;
          matrixDiffFunc := AVXMatrixDifferentiate;
          rowSwapFunc := AVXRowSwap;
          absFunc := AVXMatrixAbs;
          elemWiseDivFunc := AVXMatrixElemDiv;
          multT2Func := AVXMatrixMultTransposed;
          multTria2T1Func := AVXMtxMultTria2T1;
          multRightUpperTriaNoUnitT2Func := AVXMtxMultTria2T1StoreT1;
          multTria2TUpperUnitFunc := AVXMtxMultTria2TUpperUnit;
          multRightUpperTriaNoUnitFunc := AVXMtxMultTria2Store1;
          multRightUpperTriaUnitFunc := AVXMtxMultTria2Store1Unit;
          multRightLowerTriaUnitT2Func := AVXMtxMultLowTria2T2Store1;
          MtxVecMultFunc := AVXMtxVecMult;
          MtxVecMultTFunc := AVXMtxVecMultT;
          Rank1UpdateFunc := AVXRank1Update;
          matrixRot := AVXMatrixRotate;
          PlaneRotSeqRVB := AVXApplyPlaneRotSeqRVB;
          PlaneRotSeqRVF := AVXApplyPlaneRotSeqRVF;
          PlaneRotSeqLVB := AVXApplyPlaneRotSeqLVB;
          PlaneRotSeqLVF := AVXApplyPlaneRotSeqLVF;
          memInitFunc := AVXInitMemAligned;
          vecConvolve := AVXVecConvolveRevB;
          elemAddFunc := AVXMatrixElemAdd;
          MatrixVecDotMultFunc := AVXMatrixVecDotMult;
          SymRank2UpdateFunc := AVXSymRank2UpdateUpper;


          // ##############################################
          // #### override if fma is requested
          if instrType = itFMA then
          begin
               multT2Func := FMAMatrixMultTransposed;
               multTria2T1Func := FMAMtxMultTria2T1;
               multRightUpperTriaNoUnitT2Func := FMAMtxMultTria2T1StoreT1;
               multTria2TUpperUnitFunc := FMAMtxMultTria2TUpperUnit;
               multRightLowerTriaUnitT2Func := FMAMtxMultLowTria2T2Store1;

               MtxVecMultFunc := FMAMtxVecMult;
               MtxVecMultTFunc := FMAMtxVecMultT;
               vecConvolve := FMAVecConvolveRevB;

               if useStrassenMult
               then
                   multFunc := FMAStrassenMatrixMultiplication
               else
                   multFunc := FMAMatrixMult;

               curUsedCPUInstrSet := itFMA;
          end;
     end
     else if IsSSE3Present and (instrType = itSSE) then
     begin
          curUsedCPUInstrSet := itSSE;
          if useStrassenMult
          then
              multFunc := ASMStrassenMatrixMultiplication
          else
              multFunc := ASMMatrixMult;
          addFunc := ASMMatrixAdd;
          subFunc := ASMMatrixSub;
          subTFunc := ASMMatrixSubT;
          subVecFunc := ASMMatrixSubVec;
          addVecFunc := ASMMatrixAddVec;
          elemWiseFunc := ASMMatrixElemMult;
          addScaleFunc := ASMMatrixAddAndScale;
          scaleAddFunc := ASMMAtrixScaleAndAdd;
          sqrtFunc := ASMMatrixSQRT;
          blockedMultFunc := BlockMatrixMultiplication;
          blockedMultT1Func := BlockMatrixMultiplicationT1;
          blockedMultT2Func := BlockMatrixMultiplicationT2;
          multT2Func := ASMMatrixMultTransposed;
          copyFunc := ASMMatrixCopy;
          minFunc := ASMMatrixMin;
          maxFunc := ASMMatrixMax;
          absMinFunc := ASMMatrixAbsMin;
          absMaxFunc := ASMMatrixAbsMax;
          transposeFunc := ASMMatrixTranspose;
          transposeInplaceFunc := ASMMatrixTransposeInplace;
          elemNormFunc := ASMMatrixElementwiseNorm2;
          matrixNormalizeFunc := ASMMatrixNormalize;
          matrixMeanFunc := ASMMatrixMean;
          matrixVarFunc := ASMMatrixVar;
          matrixMeanVarFunc := ASMMatrixMeanVar;
          matrixSumFunc := ASMMatrixSum;
          matrixCumulativeSumFunc := ASMMatrixCumulativeSum;
          matrixDiffFunc := ASMMatrixDifferentiate;
          rowSwapFunc := ASMRowSwap;
          absFunc := ASMMatrixAbs;
          elemWiseDivFunc := ASMMatrixElemDiv;
          multTria2T1Func := ASMMtxMultTria2T1;
          multRightUpperTriaNoUnitT2Func := ASMMtxMultTria2T1StoreT1;
          multTria2TUpperUnitFunc := ASMMtxMultTria2TUpperUnit;
          multRightUpperTriaNoUnitFunc := ASMMtxMultTria2Store1;
          multRightUpperTriaUnitFunc := ASMMtxMultTria2Store1Unit;
          multRightLowerTriaUnitT2Func := ASMMtxMultLowTria2T2Store1;
          MtxVecMultFunc := ASMMtxVecMult;
          MtxVecMultTFunc := ASMMtxVecMultT;
          Rank1UpdateFunc := ASMRank1Update;
          matrixRot := ASMMatrixRotate;
          PlaneRotSeqRVB := ASMApplyPlaneRotSeqRVB;
          PlaneRotSeqRVF := ASMApplyPlaneRotSeqRVF;
          PlaneRotSeqLVB := ASMApplyPlaneRotSeqLVB;
          PlaneRotSeqLVF := ASMApplyPlaneRotSeqLVF;
          memInitFunc := ASMInitMemAligned;
          initfunc := ASMMatrixInit;
          vecConvolve := ASMVecConvolveRevB;
          MatrixVecDotMultFunc := ASMMatrixVecDotMult;
          SymRank2UpdateFunc := ASMSymRank2UpdateUpper;

          TDynamicTimeWarp.UseSSE := True;
     end
     else
     {$ENDIF} // MRMATH_NOASM
     begin
          curUsedCPUInstrSet := itFPU;
          if useStrassenMult
          then
              multFunc := GenericStrassenMatrixMultiplication
          else
              multFunc := GenericMtxMult;
          addFunc := GenericMtxAdd;
          subFunc := GenericMtxSub;
          subTFunc := GenericMatrixSubT;
          subVecFunc := GenericSubVec;
          addVecFunc := GenericAddVec;
          elemWiseFunc := GenericMtxElemMult;
          addScaleFunc := GenericMtxAddAndScale;
          scaleAddFunc := GenericMtxScaleAndAdd;
          sqrtFunc := GenericMtxSqrt;
          blockedMultFunc := GenericBlockMatrixMultiplication;
          blockedMultT1Func := GenericBlockMatrixMultiplicationT1;
          blockedMultT2Func := GenericBlockMatrixMultiplicationT2;
          copyFunc := GenericMtxCopy;
          initfunc := GenericMtxInit;
          minFunc := GenericMtxMin;
          maxFunc := GenericMtxMax;
          transposeFunc := GenericMtxTranspose;
          transposeInplaceFunc := GenericMtxTransposeInplace;
          elemNormFunc := GenericMtxElementwiseNorm2;
          matrixNormalizeFunc := GenericMtxNormalize;
          matrixMeanFunc := GenericMtxMean;
          matrixVarFunc := GenericMtxVar;
          matrixMeanVarFunc := GenericMtxMeanVar;
          matrixSumFunc := GenericMtxSum;
          matrixCumulativeSumFunc := GenericMtxCumulativeSum;
          matrixDiffFunc := GenericMtxDifferentiate;
          rowSwapFunc := GenericRowSwap;
          absFunc := GenericMtxAbs;
          elemWiseDivFunc := GenericMtxElemDiv;
          multTria2T1Func := GenericMtxMultTria2T1Lower;
          multTria2TUpperUnitFunc := GenericMtxMultTria2TUpperUnit;
          multRightUpperTriaNoUnitT2Func := GenericMtxMultRightUpperTriaNoUnitT2;
          multRightLowerTriaUnitT2Func := GenericMtxMultRightLowerTriaUnitT2;
          multRightUpperTriaNoUnitFunc := GenericMtxMultRightUpperTriaNoUnit;
          multRightUpperTriaUnitFunc := GenericMtxMultRightUpperTriaUnit;
          multT2Func := GenericMtxMultTransp;
          MtxVecMultFunc := GenericMtxVecMult;
          MtxVecMultTFunc := GenericMtxVecMultT;
          Rank1UpdateFunc := GenericRank1Update;
          matrixRot := GenericMatrixRotate;
          PlaneRotSeqRVB := GenericApplyPlaneRotSeqRVB;
          PlaneRotSeqRVF := GenericApplyPlaneRotSeqRVF;
          PlaneRotSeqLVB := GenericApplyPlaneRotSeqLVB;
          PlaneRotSeqLVF := GenericApplyPlaneRotSeqLVF;
          memInitFunc := GenericInitMemAligned;
          vecConvolve := GenericConvolveRevB;
          elemAddFunc := GenericMtxElemAdd;
          MatrixVecDotMultFunc := GenericVecDotMult;
          SymRank2UpdateFunc := GenericSymRank2UpdateUpper;

          TDynamicTimeWarp.UseSSE := False;
          TRandomGenerator.cpuSet := TChaChaCPUInstrType.itFPU;
     end;

     curUsedStrassenMult := useStrassenMult;


     {$IF defined(x86) or defined(x64)}

     // set only double precission for compatibility
     fpuCtrlWord := Get8087CW;
     // set bits 8 and 9 to "10" -> double precission calculation instead of extended!
     fpuCtrlWord := fpuCtrlWord or $0300;
     fpuCtrlWord := fpuCtrlWord and $FEFF;
     Set8087CW(fpuCtrlWord);
     // todo: there must be something like this for ARM...
     {$IFEND}
end;

initialization
  curUsedCPUInstrSet := itFMA;
  curUsedStrassenMult := False;

  InitMathFunctions(curUsedCPUInstrSet, curUsedStrassenMult);

end.
