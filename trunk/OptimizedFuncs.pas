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

function MatrixAdd(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixAdd(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixAdd(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt); overload;

function MatrixSub(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixSub(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixSub(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixSub(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt); overload;

function MatrixMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;
function MatrixMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;

procedure MatrixElemMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function MatrixElemMult(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure MatrixElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
function MatrixElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;

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

function MatrixSum(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function MatrixSum(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure MatrixSum(dest : PDouble; const destLineWidth : TASMNativeInt; Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

procedure MatrixAddAndScale(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure MatrixScaleAndAdd(Dest : PDouble; const LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
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

implementation

uses BlockSizeSetup, SimpleMatrixOperations, ASMMatrixOperations, CPUFeatures;

var actUseSSEoptions : boolean;
    actUseStrassenMult : boolean;

type
  TMatrixMultFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
  TMatrixBlockedMultfunc = procedure (dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : PDouble);
  TMatrixAddFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
  TMatrixSubFunc = TMatrixAddFunc;
  TMatrixElemWiseFunc = TMatrixAddFunc;
  TMatrixAddScaleFunc = procedure(dest : PDouble; LineWidth, width, height : TASMNativeInt; const dOffset, Scale : double);
  TMatrixSQRTFunc = procedure(dest : PDouble; LineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixCopyFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
  TMatrixMinMaxFunc = function(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
  TMatrixTransposeFunc = procedure(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
  TMatrixElemWiseNormFunc = function (dest : PDouble; LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
  TMatrixNormalizeFunc = procedure(dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt; RowWise : Boolean);
  TMatrixRowSwapFunc = procedure (A, B : PDouble; width : TASMNativeInt);

var multFunc : TMatrixMultFunc;
    blockedMultFunc : TMatrixBlockedMultfunc;
    addFunc : TMatrixAddFunc;
    subFunc : TMatrixSubFunc;
    elemWiseFunc : TMatrixElemWiseFunc;
    addScaleFunc : TMatrixAddScaleFunc;
    scaleAddFunc : TMatrixAddScaleFunc;
    sqrtFunc : TMatrixSQRTFunc;
    copyFunc : TMatrixCopyFunc;
    maxFunc : TMatrixMinMaxFunc;
    minFunc : TMatrixMinMaxFunc;
    elemNormFunc : TMatrixElemWiseNormFunc;
    transposeFunc : TMatrixTransposeFunc;
    matrixNormalizeFunc : TMatrixNormalizeFunc;
    matrixMeanFunc : TMatrixNormalizeFunc;
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

     elemWiseFunc(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

function MatrixElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
begin
     Result := MatrixElemMult(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
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
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)) and (destLineWidth >= width*sizeof(double)), 'Dimension error');
     matrixSumFunc(dest, destLineWidth, Src, srcLineWidth, width, height, RowWise);
end;

procedure MatrixSum(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;
begin
     assert((width > 0) and (height > 0) and (Length(dest) >= width*height) and (Length(src) >= width*height), 'Dimension error');
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
              multFunc := ASMStrassenMatrixMultiplication
          else
              multFunc := ASMMatrixMult;
          addFunc := ASMMatrixAdd;
          subFunc := ASMMatrixSub;
          elemWiseFunc := ASMMatrixElemMult;
          addScaleFunc := ASMMatrixAddAndScale;
          scaleAddFunc := ASMMAtrixScaleAndAdd;
          sqrtFunc := ASMMatrixSQRT;
          blockedMultFunc := BlockedMatrixMultiplication;
          copyFunc := ASMMatrixCopy;
          minFunc := ASMMatrixMin;
          maxFunc := ASMMatrixMax;
          transposeFunc := ASMMatrixTranspose;
          elemNormFunc := ASMMatrixElementwiseNorm2;
          matrixNormalizeFunc := ASMMatrixNormalize;
          matrixMeanFunc := ASMMatrixMean;
          matrixSumFunc := ASMMatrixSum;
          rowSwapFunc := ASMRowSwap;
     end
     else
     begin
          if useStrassenMult
          then
              multFunc := GenericStrassenMatrixMultiplication
          else
              multFunc := GenericMtxMult;
          addFunc := GenericMtxAdd;
          subFunc := GenericMtxSub;
          elemWiseFunc := GenericMtxElemMult;
          addScaleFunc := GenericMtxAddAndScale;
          scaleAddFunc := GenericMtxScaleAndAdd;
          sqrtFunc := GenericMtxSqrt;
          blockedMultFunc := GenericBlockedMatrixMultiplication;
          copyFunc := GenericMtxCopy;
          minFunc := GenericMtxMin;
          maxFunc := GenericMtxMax;
          transposeFunc := GenericMtxTranspose;
          elemNormFunc := GenericMtxElementwiseNorm2;
          matrixNormalizeFunc := GenericMtxNormalize;
          matrixMeanFunc := GenericMtxMean;
          matrixSumFunc := GenericMtxSum;
          rowSwapFunc := GenericRowSwap;
     end;

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
  actUseStrassenMult := True;

  InitMathFunctions(actUseSSEoptions, actUseSSEoptions);

end.
