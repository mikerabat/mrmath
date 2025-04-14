// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2017, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit LinAlgLU;

interface

{$IFDEF FPC} {$MODESWITCH ADVANCEDRECORDS} {$ENDIF}

uses MatrixConst;

// interface functions (used in different parts - don't call them directly)
procedure LUSwap(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; k1, k2 : NativeInt; indx : PIntegerArray; var parity : NativeInt);
procedure LUBacksup(A : PDouble; width, height : NativeInt; B : PDouble; const LineWidth : NativeInt);

// inplace LU decomposition of the matrix A. Diagonal elements of the lower triangular matrix are set to one
// thus the diagonal elements of the resulting matrix A are composed from the upper diagonal elements only.
// The index records the row permutation effected by the partial pivoting.
function MatrixLUDecompInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult;
function MatrixLUDecomp(A : PDouble; const LineWidthA : NativeInt; LUDecomp : PDouble; const LineWidthLU : NativeInt; width : NativeInt; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult; overload;
function MatrixLUDecomp(A : PDouble; const LineWidthA : NativeInt; LUDecomp : PDouble; const LineWidthLU : NativeInt; width : NativeInt; progress : TLinEquProgress = nil) : TLinEquResult; overload;
procedure MatrixLUBackSubst(LUDecomp : PDouble; const LineWidthLU : NativeInt; width : NativeInt; const  indx : PIntegerArray; B : PDouble; const LineWidthB : NativeInt; progress : TLinEquProgress = nil);


// inverse of a matrix by using the LU decomposition
function MatrixInverseInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; progress : TLinEquProgress = nil) : TLinEquResult;

// Matrix determinant calculated from the LU decomposition. Returns zero in case of a singular matrix. Drawback is a double
// memory usage since the LU decomposition must be stored in a temporary matrix.
function MatrixDeterminant(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; progress : TLinEquProgress = nil) : double;


// Matrix Line Equation Solver routines which are based on LU decomposition.
// note these functions use temporarily double the size of A memory.
// The result is stored in X. B and X must have the same size, also B may have
// more than one column.
function MatrixLinEQSolve(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; B : PDouble; const LineWidthB : NativeInt; X : PDouble; const LineWidthX : NativeInt;
   Width2 : NativeInt; const NumRefinments : NativeInt = 0; progress : TLinEquProgress = nil) : TLinEquResult;

// threaded function for linear equations
function ThrMatrixLUDecomp(A : PDouble; const LineWidthA : NativeInt; width : integer; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult;
// inverse of a matrix by using the LU decomposition
function ThrMatrixInverse(A : PDouble; const LineWidthA : NativeInt; width : integer; progress : TLinEquProgress = nil) : TLinEquResult;

// ###########################################
// #### Threaded versions
// ###########################################

// Matrix determinant calculated from the LU decomposition. Returns zero in case of a singular matrix. Drawback is a double
// memory usage since the LU decomposition must be stored in a temporary matrix.
function ThrMatrixDeterminant(A : PDouble; const LineWidthA : NativeInt; width : integer; progress : TLinEquProgress = nil) : double;

// uses the threaded version of the LU decomposition and a threaded back substition.
function ThrMatrixLinEQSolve(A : PDouble; const LineWidthA : NativeInt; width : integer; B : PDouble; const LineWidthB : NativeInt; X : PDouble;
 const LineWidthX : NativeInt;  width2 : integer; const NumRefinments : integer = 0; progress : TLinEquProgress = nil) : TLinEquResult;


implementation

uses MatrixASMStubSwitch, Math, SysUtils, Types, MtxThreadPool,
     ThreadedMatrixOperations, BlockSizeSetup;

// ######################################################
// #### internaly used objects and definitions
type
  TLinearEQProgress = class(TObject)
  public
    refProgress : TLinEquProgress;
    numRefinenmentSteps : NativeInt;

    procedure LUDecompSolveProgress(Progress : integer);
    procedure RefinementProgress(Progress : integer);
  end;

{ TLinearEQProgress }

procedure TLinearEQProgress.LUDecompSolveProgress(Progress: integer);
begin
     if numRefinenmentSteps > 0 
     then
         refProgress(progress*8 div 10)
     else
         refProgress(progress);
end;

procedure TLinearEQProgress.RefinementProgress(Progress: integer);
begin
     refProgress(80 + 2*progress div 10);
end;

// LUSWAP performs a series of row interchanges on the matrix A.
// One row interchange is initiated for each of rows K1 through K2 of A.
procedure LUSwap(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; k1, k2 : NativeInt; indx : PIntegerArray; var parity : NativeInt);
var i : NativeInt;
    pA1, pA2 : PDouble;
begin
     for i := k1 to k2 do
     begin
          if indx^[i] <> i then
          begin
               // interchange rows
               pA1 := GenPtr(A, 0, i, LineWidthA);
               pA2 := GenPtr(A, 0, indx^[i], LineWidthA);

               // swap a complete row at once
               MatrixRowSwap(pA1, pA2, width);

               parity := -parity;
          end;
     end;
end;

procedure LUBacksup(A : PDouble; width, height : NativeInt; B : PDouble; const LineWidth : NativeInt);
var j, i, k : NativeInt;
    pA, pAi : PDouble;
    pB : PDouble;
    pBj : PConstDoubleArr;
    pBDestj : PConstDoubleArr;
    pBDest, pBDesti : PDouble;
begin
     pA := GenPtr(A, 0, 1, LineWidth);
     pB := B;
     pBDest := GenPtr( B, 0, 1, LineWidth);
     for k := 0 to height - 1 do
     begin
          pAi := pA;
          pBDesti := pBDest;

          for i := k + 1 to height - 1 do
          begin
               pBj := PConstDoubleArr( pB );
               pBDestj := PConstDoubleArr( pBDesti );
               for j := 0 to width - 1 do
                   pBDestj^[j] := pBDestj^[j] - pBj^[j]*pAi^;

               inc(PByte(pAi), LineWidth);
               inc(PByte(pBDesti), LineWidth);
          end;
          inc(pA);
          inc(PByte(pA), LineWidth);
          inc(PByte(pB), LineWidth);
          inc(PByte(pBDest), LineWidth);
     end;
end;

const cBlkMultSize = 48;
type
  TRecMtxLUDecompData = record
    progress : TLinEquProgress;
    numCols,
    numCalc : NativeInt;
    blkMultMem : Pdouble;
    LineWidth : NativeInt;
  end;

function InternalRecursiveMatrixLUDecompInPlace(A : PDouble;  width, height : NativeInt;
 indx : PIntegerArray; var parity : NativeInt; var data : TRecMtxLUDecompData) : TLinEquResult;
var mn : NativeInt;
    pA : PDouble;
    idx : NativeInt;
    maxVal : double;
    nleft, nright : NativeInt;
    i : NativeInt;
    pB, a12, a21 : PDouble;
    absMaxVal : double;
begin
     mn := min(width, height);

     if mn > 1 then
     begin
          nleft := mn div 2;
          nright := width - nleft;

          Result := InternalRecursiveMatrixLUDecompInPlace(A, nLeft, height, indx, parity, data);

          if Result <> leOk then
             exit;

          pA := GenPtr(A, nLeft, 0, data.LineWidth);
          LUSwap(pA, data.LineWidth, nright, 0, nleft - 1, indx, parity);

          // lu backsup A12 = L - one*A12
          if nRight > 1 then
             LUBacksup(A, nRight, nLeft, pA, data.LineWidth);

          // matrix mult sub
          // A22 = A22 - A21*A12
          a12 := GenPtr(A, nLeft, 0, data.LineWidth);
          pB := GenPtr(A, nLeft, nLeft, data.LineWidth);
          a21 := GenPtr(A, 0, nleft, data.LineWidth);

          // in this case it's faster to have a small block size!
          if (nright > cBlkMultSize) or (height - nleft > cBlkMultSize)
          then
              //BlockMatrixMultiplication(pB, data.LineWidth, a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth, cBlkMultSize, doSub, data.blkMultMem)
              MatrixMultEx(pB, data.LineWidth, a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth, cBlkMultSize, doSub, data.blkMultMem)
          else
          begin
               MatrixMult(data.blkMultMem, (nright + nright and $01)*sizeof(double), a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth);
               MatrixSub(pB, data.LineWidth, pB, data.blkMultMem, nright, height - nleft, data.LineWidth, (nright + nright and $01)*sizeof(double));
          end;

          // apply recursive LU to A(nleft + 1, nleft + 1);
          Result := InternalRecursiveMatrixLUDecompInPlace(pB, nright, height - nleft, @(indx^[nleft]), parity, data);
          if Result <> leok then
             exit;

          for i := nLeft to width - 1 do
              indx^[i] := indx^[i] + nLeft;

          // dlswap
          LUSwap(A, data.LineWidth, nleft, nleft, mn - 1, indx, parity);
     end
     else
     begin
          // find maximum element of this column
          maxVal := 0;
          absMaxVal := 0;
          idx := -1;
          pA := A;
          for i := 0 to Height - 1 do
          begin
               if Abs(pA^) > absMaxVal then
               begin
                    idx := i;
                    maxVal := pA^;
                    absMaxVal := abs(maxVal);
               end;

               inc(PByte(pA), data.LineWidth);
          end;

          // now it's time to apply the gauss elimination
          indx^[0] := idx;

          // check for save invertion of maxVal
          if Abs(maxVal) > 10/MaxDouble then
          begin
               MatrixScaleAndAdd(A, data.LineWidth, 1, Height, 0, 1/maxVal);

               pA := GenPtr(A, 0, idx, data.LineWidth);
               pA^ := A^;
               A^ := maxVal;

               Result := leOk;

               if Assigned(data.progress) then
               begin
                    inc(data.numCalc);
                    data.progress(data.numCalc*100 div data.numCols);
               end;
          end
          else
              Result := leSingular;
     end;
end;

function MatrixLUDecompInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; indx : PIntegerArray; progress : TLinEquProgress) : TLinEquResult;
var parity : NativeInt;
    mem : Pointer;
    rc : TRecMtxLUDecompData;
begin
     FillChar(indx^, width*sizeof(integer), 0);
     parity := 1;
     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.blkMultMem := MtxAllocAlign( BlockMultMemSize(cBlkMultSize), mem );
     rc.LineWidth := LineWidthA;
     Result := InternalRecursiveMatrixLUDecompInPlace(A, width, width, indx, parity, rc);
     FreeMem(mem);
end;

function MatrixLUDecomp(A : PDouble; const LineWidthA : NativeInt; LUDecomp : PDouble; const LineWidthLU : NativeInt; width : NativeInt; indx : PIntegerArray; progress : TLinEquProgress) : TLinEquResult;
begin
     Assert(width > 0, 'Dimension Error');

     // copy data -> now we can perform an inline LU decomposition
     MatrixCopy(LUDecomp, LineWidthLU, A, LineWidthA, width, width);
     Result := MatrixLUDecompInPlace(LUDecomp, lineWidthLU, width, indx, progress);
end;

function MatrixLUDecomp(A : PDouble; const LineWidthA : NativeInt; LUDecomp : PDouble; const LineWidthLU : NativeInt; width : NativeInt; progress : TLinEquProgress) : TLinEquResult;
var indx : array of integer;
begin
     Assert(width > 0, 'Dimension Error');
     setLength(indx, width);

     Result := MatrixLUDecomp(A, LineWidthA, LUDecomp, LineWidthLU, width, @indx[0], progress);
end;

procedure MatrixLUBackSubst(LUDecomp : PDouble; const LineWidthLU : NativeInt; width : NativeInt; const  indx : PIntegerArray;
  B : PDouble; const LineWidthB : NativeInt; progress : TLinEquProgress);
var i : NativeInt;
    ii : NativeInt;
    ip : NativeInt;
    j : NativeInt;
    sum : double;
    pB : PDouble;
    pB2 : PDouble;
    pVal : PDouble;
    pVal2 : PConstDoubleArr;
begin
     assert(width*sizeof(double) <= LineWidthLU, 'Dimension Error');

     if Assigned(progress) then
        progress(0);

     ii := -1;
     pB2 := B;
     for i := 0 to width - 1 do
     begin
          ip := indx^[i];
          pB := GenPtr(B, 0, ip, LineWidthB);
          sum := pB^;
          pB^ := pB2^;

          if ii >= 0 then
          begin
               pVal2 := PConstDoubleArr( GenPtr(LUDecomp, 0, i, LineWidthLU ) );
               pB := GenPtr(B, 0, ii, LineWidthB);
               for j := ii to i - 1 do
               begin
                    sum := sum - pVal2^[j]*pB^;
                    inc(PByte(pB), LineWidthB);
               end;
          end
          else if sum <> 0
          then
              ii := i;

          pB2^ := sum;
          inc(PByte(pB2), LineWidthB);
     end;

     if Assigned(progress) then
        progress(50);

     pB := GenPtr(B, 0, width - 1, LineWidthB);
     pVal := GenPtr( LUDecomp, width - 1, width -1 , LineWidthLU);
     for i := width - 1 downto 0 do
     begin
          sum := pB^;

          pB2 := GenPtr(pB, 0, 1, LineWidthB);
          pVal2 := PConstDoubleArr( GenPtr(pVal, 1, 0, LineWidthLU) );
          for j := 0 to width - i - 2 do
          begin
               sum := sum - pVal2^[j]*pB2^;
               inc(PByte(pB2), LineWidthB);
          end;

          pB^ := sum/pVal^;

          dec(pVal);
          dec(PByte(pVal), LineWidthLU);
          dec(PByte(pB), LineWidthB);
     end;

     if Assigned(progress) then
        progress(100);
end;


function MatrixInverseInPlace(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; progress : TLinEquProgress) : TLinEquResult;
var Y : PDouble;
    ptrMem : Pointer;
    indx : array of integer;
    i, j : NativeInt;
    pVal : PDouble;
    col : TDoubleDynArray;
    w : NativeInt;
begin
     Assert(lineWidthA >= width*sizeof(double), 'Dimension Error');
     Assert(width > 0, 'Dimension error');

     w := width + width and $01;
     Y := MtxMallocAlign(w*w*sizeof(double), ptrMem);
     SetLength(indx, width);
     SetLength(col, width);

     MatrixCopy(Y, sizeof(double)*w, A, LineWidthA, width, width);
     Result := MatrixLUDecompInPlace(Y, w*sizeof(double), width, @indx[0], progress);

     if Result = leSingular then
     begin
          FreeMem(ptrMem);
          exit;
     end;

     for j := 0 to width - 1 do
     begin
          pVal := GenPtr( A, j, 0, LineWidthA );

          for i := 0 to width - 1 do
              col[i] := 0;
          col[j] := 1;
          MatrixLUBackSubst(Y, w*sizeof(double), width, @indx[0], @col[0], sizeof(double));

          for i := 0 to width - 1 do
          begin
               pVal^ := col[i];
               inc(PByte(pVal), LineWidthA);
          end;
     end;

     FreeMem(ptrMem);
end;

function MatrixDeterminant(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; progress : TLinEquProgress) : double;
var LUDecomp : PDouble;
    indx : Array of Integer;
    i : NativeInt;
    pVal : PDouble;
    parity : NativeInt;
    rc : TRecMtxLUDecompData;
    w : NativeInt;
    ptrMem : Pointer;
    mem : Array[0..(8+4*cBlkMultSize*cBlkMultSize)] of double;
begin
     assert(width > 0, 'Dimension error');
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');

     w := width + width and $01;
     LUDecomp := MtxMallocAlign(w*w*sizeof(double), ptrMem);
     SetLength(indx, width);
     MatrixCopy(LUDecomp, w*sizeof(double), A, LineWidthA, width, width);

     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.blkMultMem := AlignPtr64( @mem[0] );
     rc.LineWidth := w*sizeof(double);

     parity := 1;
     if InternalRecursiveMatrixLUDecompInPlace(LUDecomp, width, width, @indx[0], parity, rc) = leSingular then
     begin
          Result := 0;
          FreeMem(ptrMem);
          exit;
     end;
     pVal := LUDecomp;
     Result := parity;
     for i := 0 to width - 1 do
     begin
          Result := Result * pVal^;
          inc(pVal);
          inc(PByte(pVal), w*sizeof(double));
     end;

     FreeMem(ptrMem);
end;

function MatrixLinEQSolve(A : PDouble; const LineWidthA : NativeInt; width : NativeInt; B : PDouble; const LineWidthB : NativeInt; X : PDouble;
 const LineWidthX : NativeInt;  Width2 : NativeInt; const NumRefinments : NativeInt; progress : TLinEquProgress) : TLinEquResult;
var indx : Array of Integer;
    LUDecomp : TDoubleDynArray;
    sdp : double;
    row : TDoubleDynArray;
    pB : PDouble;
    i : NativeInt;
    pA : PDouble;
    j, k : NativeInt;
    pX : PDouble;
    pVal : PDouble;
    refinementCounter : NativeInt;
    progObj : TLinearEQProgress;
    progRef : TLinEquProgress;
begin
     progRef := nil;
     progObj := nil;
     if Assigned(progress) then
     begin
          progObj := TLinearEQProgress.Create;
          progObj.refProgress := progress;
          progObj.numRefinenmentSteps := NumRefinments;
          progRef := {$IFDEF FPC}@{$ENDIF}progObj.LUDecompSolveProgress;
     end;


     // ###########################################
     // #### Standard LU Decomposition
     SetLength(LUDecomp, width*width);
     SetLength(indx, width);
     Result := MatrixLUDecomp(A, LineWidthA, @LUDecomp[0], width*sizeof(double), width, @indx[0], progRef);

     if Result = leSingular then
     begin
          progObj.Free;
          exit;
     end;

     for i := 0 to width2 - 1 do
     begin
          // copy one column
          pX := X;
          inc(pX, i);
          pVal := B;
          inc(pVal, i);
          for j := 0 to width - 1 do
          begin
               pX^ := pVal^;
               inc(PByte(pX), LineWidthX);
               inc(PByte(pVal), LineWidthB);
          end;
          pX := X;
          inc(pX, i);

          // calculate vector X
          MatrixLUBackSubst(@LUDecomp[0], width*sizeof(double), width, @indx[0], pX, LineWidthX);
     end;

     // ###########################################
     // #### Iterative refinements
     if NumRefinments > 0 then
     begin
          SetLength(row, width);
          // for each solution do a separate refinement:
          for k := 0 to width2 - 1 do
          begin
               if Assigned(progobj) then
                  progObj.RefinementProgress(Int64(k)*100 div Int64(width2));

               for refinementCounter := 0 to NumRefinments - 1 do
               begin
                    pb := B;

                    pA := A;
                    for i := 0 to width - 1 do
                    begin
                         pVal := pA;

                         sdp := -pB^;
                         inc(PByte(pB), LineWidthB);
                         pX := X;
                         for j := 0 to width - 1 do
                         begin
                              sdp := sdp + pX^*pVal^;
                              inc(pVal);
                              inc(pX);
                         end;

                         inc(PByte(pA), LineWidthA);
                         row[i] := sdp;
                    end;

                    MatrixLUBackSubst(@LUDecomp[0], sizeof(double)*width, width, @indx[0], @row[0], sizeof(double));
                    pX := X;
                    for i := 0 to width - 1 do
                    begin
                         pX^ := pX^ - row[i];
                         inc(PByte(pX), LineWidthX);
                    end;
               end;

               inc(B);
               inc(X);
          end;
     end;

     if Assigned(progObj) then
        progObj.Free;
     if Assigned(progress) then
        progress(100);
end;

// ###########################################
// #### Threaded versions
// ###########################################

type
  TAsyncMatrixUSubst = record
  public
    A, B : PDouble;
    lineWidth : NativeInt;
    width, height : NativeInt;

    procedure Create(aA : PDouble; awidth, aheight : NativeInt; aB : PDouble; const aLineWidth : NativeInt);
  end;
  PAsyncMatrixUSubst = ^TAsyncMatrixUSubst;

type
  TAsyncMatrixLUBacksup = record
    A, B : PDouble;
    lineWidthA : NativeInt;
    lineWidthB : NativeInt;
    width, height : NativeInt;
    indx : PIntegerArray;
    offset : integer;
  end;
  PAsyncMatrixLUBacksup = ^TAsyncMatrixLUBacksup;

procedure MatrixLUBacksupFunc(obj : Pointer);
begin
     LUBacksup(PAsyncMatrixUSubst(obj)^.A, PAsyncMatrixUSubst(obj)^.width,
               PAsyncMatrixUSubst(obj)^.height, PAsyncMatrixUSubst(obj)^.B,
               PAsyncMatrixUSubst(obj)^.lineWidth);
end;

procedure ThrMatrixUSubst(A : PDouble; width, height : integer; B : PDouble; const LineWidth : NativeInt);
var i: NativeInt;
    calls : IMtxAsyncCallGroup;
    wSize : integer;
    thrSize : Integer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixUSubst;
    numUsed : integer;
begin
     // ######################################################
     // #### prepare objs
     numUsed := 0;

     thrSize := Max(32, width div numUseCPUCores + Integer((width mod numUseCPUCores) <> 0));

     for i := 0 to numUseCPUCores - 1 do
     begin
          wSize := thrSize;
          if wSize*(i + 1) >= width then
             wSize := width - i*thrSize;

          objs[numUsed].Create(A, wSize, height, B, LineWidth);
          inc(numUsed);

          if thrSize*(i + 1) >= width then
             break;

          inc(B, thrSize);
     end;

     if numUsed = 0 then
        exit;

     // #######################################################
     // #### execute tasks
     calls := nil;
     if numUsed > 1 then
        calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixLUBacksupFunc, @objs[i]);

     MatrixLUBacksupFunc(@objs[numUsed - 1]);

     if numUsed > 1 then
        calls.SyncAll;
end;

function InternalThrMatrixLUDecomp(A : PDouble; width, height : integer;
 indx : PIntegerArray; var parity : NativeInt; var data : TRecMtxLUDecompData) : TLinEquResult;
// this is basically a copy of the unthreaded LU decomposition but with threaded parts (LU Backsup and Multiplication)!
const cMinThrMultSize = 64;
var mn : NativeInt;
    pA : PDouble;
    idx : NativeInt;
    maxVal : double;
    nleft, nright : NativeInt;
    i : NativeInt;
    pB, a12, a21 : PDouble;
    absMaxVal : double;
begin
     mn := min(width, height);

     if mn > 1 then
     begin
          nleft := mn div 2;
          nright := width - nleft;

          Result := InternalThrMatrixLUDecomp(A, nLeft, height, indx, parity, data);

          if Result <> leOk then
             exit;

          pA := A;
          inc(pA, nLeft);
          LUSwap(pA, data.LineWidth, nright, 0, nleft - 1, indx, parity);

          // lu backsup A12 = L - one*A12
          if nRight > 1 then
          begin
               if (nRight > cMinThrMultSize)
               then
                   ThrMatrixUSubst(A, nright, nleft, pA, data.LineWidth)
               else
                   LUBacksup(A, nRight, nLeft, pA, data.LineWidth);
          end;

          // matrix mult sub
          // A22 = A22 - A21*A12
          pB := A;
          inc(pB, nleft);
          a12 := pB;
          inc(PByte(pB), nLeft*data.LineWidth);

          a21 := A;
          inc(PByte(a21), nleft*data.LineWidth);
          // in this case it's faster to have a small block size!
          if (nright > cBlkMultSize) or (height - nleft > cBlkMultSize)
          then
              //BlockMatrixMultiplication(pB, data.LineWidth, a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth, cBlkMultSize, doSub, data.blkMultMem)
              ThrMatrixMultEx(pB, data.LineWidth, a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth, cBlkMultSize, doSub, data.blkMultMem)
          else
          begin
               MatrixMult(data.blkMultMem, (nright + nright and $01)*sizeof(double), a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth);
               MatrixSub(pB, data.LineWidth, pB, data.blkMultMem, nright, height - nleft, data.LineWidth, (nright + nright and $01)*sizeof(double));
          end;

          // apply recursive LU to A(nleft + 1, nleft + 1);
          Result := InternalThrMatrixLUDecomp(pB, nright, height - nleft, @(indx^[nleft]), parity, data);
          if Result <> leok then
             exit;

          for i := nLeft to width - 1 do
              indx^[i] := indx^[i] + nLeft;

          // dlswap
          LUSwap(A, data.LineWidth, nleft, nleft, mn - 1, indx, parity);
     end
     else
     begin
          // find maximum element of this column
          maxVal := 0;
          absMaxVal := 0;
          idx := -1;
          pA := A;
          for i := 0 to Height - 1 do
          begin
               if Abs(pA^) > absMaxVal then
               begin
                    idx := i;
                    maxVal := pA^;
                    absMaxVal := abs(maxVal);
               end;

               inc(PByte(pA), data.LineWidth);
          end;

          // now it's time to apply the gauss elimination
          indx^[0] := idx;

          // check for save invertion of maxVal
          if Abs(maxVal) > 10/MaxDouble then
          begin
               MatrixScaleAndAdd(A, data.LineWidth, 1, Height, 0, 1/maxVal);
               pA := A;
               inc(PByte(pA), data.LineWidth*idx);
               pA^ := A^;
               A^ := maxVal;

               Result := leOk;

               if Assigned(data.progress) then
               begin
                    inc(data.numCalc);
                    data.progress(data.numCalc*100 div data.numCols);
               end;
          end
          else
              Result := leSingular;
     end;
end;

function ThrMatrixLUDecomp(A : PDouble; const LineWidthA : NativeInt; width : integer; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult;
var parity : NativeInt;
    rc : TRecMtxLUDecompData;
    mem : Pointer;
begin
     mem := MtxAlloc(4*numUseCPUCores*(cBlkMultSize + numUseCPUCores + 2)*cBlkMultSize*sizeof(double) + $20);
     FillChar(indx^, width*sizeof(integer), 0);

     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.LineWidth := LineWidthA;
     rc.blkMultMem := AlignPtr32(mem);
     parity := 1;
     Result := InternalThrMatrixLUDecomp(A, width, width, indx, parity, rc);

     FreeMem(mem);
end;

procedure MatrixLUInvertCall(obj : Pointer);
var i, j : integer;
    pVal : PDouble;
    width, height : integer;
    col : PConstDoubleArr;
    ptrMem : Pointer;
begin
     width := PAsyncMatrixLUBacksup(obj)^.width;
     height := PAsyncMatrixLUBacksup(obj)^.height;
     col := MtxMallocAlign(sizeof(double)*height, ptrMem);

     for j := 0 to width - 1 do
     begin
          FillChar(col^[0], height*sizeof(double), 0);
          col^[j + PAsyncMatrixLUBacksup(obj)^.offset] := 1;
          MatrixLUBackSubst(PAsyncMatrixLUBacksup(obj)^.A, PAsyncMatrixLUBacksup(obj)^.lineWidthA, height,
                            PAsyncMatrixLUBacksup(obj)^.indx, PDouble(col), sizeof(double));

          pVal := PAsyncMatrixLUBacksup(obj)^.B;
          inc(pVal, PAsyncMatrixLUBacksup(obj)^.offset + j);

          for i := 0 to height - 1 do
          begin
               pVal^ := col^[i];
               inc(PByte(pVal), PAsyncMatrixLUBacksup(obj)^.LineWidthB);
          end;
     end;
     FreeMem(ptrMem);
end;

procedure MatrixLUBacksupCall(obj : Pointer);
var j : integer;
    pVal : PDouble;
    width, height : integer;
begin
     width := PAsyncMatrixLUBacksup(obj)^.width;
     height := PAsyncMatrixLUBacksup(obj)^.height;

     pVal := PAsyncMatrixLUBacksup(obj)^.B;
     inc(pVal, PAsyncMatrixLUBacksup(obj)^.offset);
     for j := 0 to width - 1 do
     begin
          MatrixLUBackSubst(PAsyncMatrixLUBacksup(obj)^.A, PAsyncMatrixLUBacksup(obj)^.lineWidthA, height,
                            PAsyncMatrixLUBacksup(obj)^.indx, pVal, PAsyncMatrixLUBacksup(obj)^.LineWidthB);
          inc(pVal);
     end;
end;

function ThrMatrixInverse(A : PDouble; const LineWidthA : NativeInt; width : integer; progress : TLinEquProgress = nil) : TLinEquResult;
var Y : PDouble;
    indx : array of integer;
    i : Integer;
    w : integer;
    thrSize : integer;
    wSize : integer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixLUBacksup;
    numUsed : integer;
    calls : IMtxAsyncCallGroup;
    ptrMem : Pointer;
    YLineWidth : NativeInt;
begin
     Assert(lineWidthA >= width*sizeof(double), 'Dimension Error');
     Assert(width > 0, 'Dimension error');

     w := width;
     if w and $03 <> 0 then
        w := w + 4 - width and $03;
     YLineWidth := w*sizeof(double);
     Y := MtxMallocAlign(w*width*sizeof(double), ptrMem);
     SetLength(indx, 2*width);

     MatrixCopy(Y, YLineWidth, A, LineWidthA, width, width);
     Result := ThrMatrixLUDecomp(Y, YLineWidth, width, @(indx[0]), progress);

     if Result = leSingular then
     begin
          FreeMem(ptrMem);

          exit;
     end;

     numUsed := 0;

     // at least 32 blocks
     thrSize := Max(32, width div numUseCPUCores + Integer((width mod numUseCPUCores) <> 0));

     // ############################################################
     // #### prepare tasks
     for i := 0 to numUseCPUCores - 1 do
     begin
          wSize := thrSize;
          if wSize*(i+1) > width then
             wSize := width - i*thrSize;

          objs[numUsed].A := Y;
          objs[numUsed].lineWidthA := YLineWidth;
          objs[numUsed].width := Min(wSize, width);
          objs[numUsed].height := width;
          objs[numUsed].offset := i*thrSize;
          objs[numUsed].B := A;
          objs[numUsed].LineWidthB := LineWidthA;
          objs[numUsed].indx := @indx[0];

          inc(numUsed);

          if (i + 1)*thrSize >= width then
             break;
     end;

     // ################################################
     // #### Execute tasks
     calls := nil;
     if numUsed > 1 then
        calls := MtxInitTaskGroup;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixLUInvertCall, @objs[i]);

     MatrixLUInvertCall(@objs[numUsed - 1]);

     if numUsed > 1 then
        calls.SyncAll;

     FreeMem(ptrMem);
end;

function ThrMatrixDeterminant(A : PDouble; const LineWidthA : NativeInt; width : integer; progress : TLinEquProgress = nil) : double;
var LUDecomp : PDouble;
    indx : Array of Integer;
    i : integer;
    pVal : PDouble;
    parity : NativeInt;
    rc : TRecMtxLUDecompData;
    w : integer;
    mem : PDouble;
    ptrMem1 : Pointer;
    ptrMem : Pointer;
    LULineWidth : NativeInt;
begin
     assert(width > 0, 'Dimension error');
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');

     w := width;
     if w and $03 <> 0 then
        w := w + 4 - width and $03;
     LULineWidth := w*SizeOf(double);
     LUDecomp := MtxMallocAlign(w*width*sizeof(double), ptrMem);

     mem := MtxAllocAlign(4*numUseCPUCores*(cBlkMultSize + numUseCPUCores + 2)*cBlkMultSize*sizeof(double) + $20, ptrMem1);
     SetLength(indx, width);
     MatrixCopy(LUDecomp, LULineWidth, A, LineWidthA, width, width);

     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.blkMultMem := mem;
     rc.LineWidth := w*sizeof(double);

     parity := 1;
     if InternalThrMatrixLUDecomp(LUDecomp, width, width, @indx[0], parity, rc) = leSingular then
     begin
          Result := 0;
          FreeMem(ptrMem);
          FreeMem(ptrMem1);
          exit;
     end;
     pVal := LUDecomp;
     Result := parity;
     for i := 0 to width - 1 do
     begin
          Result := Result * pVal^;
          inc(pVal);
          inc(PByte(pVal), LULineWidth);
     end;

     FreeMem(ptrMem);
     FreeMem(ptrMem1);
end;

function ThrMatrixLinEQSolve(A : PDouble; const LineWidthA : NativeInt; width : integer; B : PDouble; const LineWidthB : NativeInt; X : PDouble;
 const LineWidthX : NativeInt;  Width2 : integer; const NumRefinments : integer; progress : TLinEquProgress) : TLinEquResult;
var indx : Array of Integer;
    LUDecomp : PDouble;
    sdp : double;
    row : Array of double;
    pB : PDouble;
    i : Integer;
    pA : PDouble;
    j, k : Integer;
    pX : PDouble;
    pVal : PDouble;
    refinementCounter : integer;
    progObj : TLinearEQProgress;
    progRef : TLinEquProgress;
    w : NativeInt;
    thrSize : integer;
    wSize : integer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMatrixLUBacksup;
    numUsed : integer;
    calls : IMtxAsyncCallGroup;
    ptrMem : Pointer;
    LULineWidth : integer;
begin
     progRef := nil;
     progObj := nil;
     if Assigned(progress) then
     begin
          progObj := TLinearEQProgress.Create;
          progObj.refProgress := progress;
          progObj.numRefinenmentSteps := NumRefinments;
          progRef := {$IFDEF FPC}@{$ENDIF}progObj.LUDecompSolveProgress;
     end;

     w := width;
     if w and $03 <> 0 then
        w := w + 4 - width and $03;
     LULineWidth := w*SizeOf(double);

     LUDecomp := MtxMallocAlign(w*width*sizeof(double), ptrMem);
     MatrixCopy(LUDecomp, LULineWidth, A, LineWidthA, width, width);

     SetLength(indx, width);
     Result := ThrMatrixLUDecomp(LUDecomp, LULineWidth, width, @indx[0], progRef);

     if Result = leSingular then
     begin
          progObj.Free;
          FreeMem(ptrMem);
          exit;
     end;

     thrSize := Max(1, width2 div numUseCPUCores + Integer((width2 mod numUseCPUCores) <> 0));

     // first copy the B matrix to X -> the result is overwritten
     MatrixCopy(X, LineWidthX, B, LineWidthB, width2, width);

     // now distribute the computaions accross all cpu's
     calls := nil;
     if Min(thrSize, numUseCPUCores) > 1 then
        calls := MtxInitTaskGroup;
     numUsed := 0;
     
     for i := 0 to Min(thrSize, numUseCPUCores) - 1 do
     begin
          wSize := thrSize;
          if i = Min(thrSize, numUseCPUCores) - 1 then
             wSize := width2 - i*thrSize;

          objs[numUsed].A := LUDecomp;
          objs[numUsed].lineWidthA := LULineWidth;
          objs[numUsed].width := wSize;
          objs[numUsed].height := width;
          objs[numUsed].offset := i*thrSize;
          objs[numUsed].B := X;
          objs[numUsed].LineWidthB := LineWidthX;
          objs[numUsed].indx := @indx[0];
          inc(numUsed);
     end;

     for i := 0 to numUsed - 2 do
         calls.AddTaskRec(@MatrixLUBacksupCall, @objs[i]);

     MatrixLUBacksupCall( @objs[numUsed - 1] );

     if calls <> nil then
        calls.SyncAll;
     calls := nil;
     
     // todo: thread this part too?
     if NumRefinments > 0 then
     begin
          SetLength(row, width);
          // for each solution do a separate refinement:
          for k := 0 to width2 - 1 do
          begin
               if Assigned(progobj) then
                  progObj.RefinementProgress(Int64(k)*100 div Int64(width2));

               for refinementCounter := 0 to NumRefinments - 1 do
               begin
                    pb := B;

                    pA := A;
                    for i := 0 to width - 1 do
                    begin
                         pVal := pA;

                         sdp := -pB^;
                         inc(PByte(pB), LineWidthB);
                         pX := X;
                         for j := 0 to width - 1 do
                         begin
                              sdp := sdp + pX^*pVal^;
                              inc(pVal);
                              inc(pX);
                         end;

                         inc(PByte(pA), LineWidthA);
                         row[i] := sdp;
                    end;

                    MatrixLUBackSubst(LUDecomp, LULineWidth, width, @indx[0], @row[0], sizeof(double));
                    pX := X;
                    for i := 0 to width - 1 do
                    begin
                         pX^ := pX^ - row[i];
                         inc(PByte(pX), LineWidthX);
                    end;
               end;

               inc(B);
               inc(X);
          end;
     end;

     FreeMem(ptrMem);

     if Assigned(progObj) then
        progObj.Free;
     if Assigned(progress) then
        progress(100);
end;

{ TAsyncMatrixLUBacksupobj }

procedure TAsyncMatrixUSubst.Create(aA: PDouble;
  awidth, aheight: NativeInt; aB: PDouble;
  const aLineWidth: NativeInt);
begin
     A := aA;
     B := aB;
     lineWidth := aLineWidth;
     width := aWidth;
     height := aheight;
end;

end.
