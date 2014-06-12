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

unit ThreadedLinAlg;

// ############################################################
// #### Multi threaded versions of most of the functions from LinearAlgebraicEquations.pas
// ############################################################

interface

uses MatrixConst;

// threaded function for linear equations
function ThrMatrixLUDecomp(A : PDouble; const LineWidthA : Integer; width : integer; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult;
// inverse of a matrix by using the LU decomposition
function ThrMatrixInverse(A : PDouble; const LineWidthA : integer; width : integer; progress : TLinEquProgress = nil) : TLinEquResult;

// Matrix determinant calculated from the LU decomposition. Returns zero in case of a singular matrix. Drawback is a double
// memory usage since the LU decomposition must be stored in a temporary matrix.
function ThrMatrixDeterminant(A : PDouble; const LineWidthA : integer; width : integer; progress : TLinEquProgress = nil) : double;

// uses the threaded version of the LU decomposition and a threaded back substition.
function ThrMatrixLinEQSolve(A : PDouble; const LineWidthA : integer; width : integer; B : PDouble; const LineWidthB : integer; X : PDouble;
 const LineWidthX : integer;  width2 : integer; const NumRefinments : integer = 0; progress : TLinEquProgress = nil) : TLinEquResult;

implementation

uses MtxThreadPool, LinearAlgebraicEquations, Math, ThreadedMatrixOperations, OptimizedFuncs;

type
  TAsyncMatrixUSubst = class(TObject)
    A, B : PDouble;
    lineWidth : TASMNativeInt;
    width, height : TASMNativeInt;

    constructor Create(aA : PDouble; awidth, aheight : TASMNativeInt; aB : PDouble; const aLineWidth : TASMNativeInt);
  end;

type
  TAsyncMatrixLUBacksup = class(TObject)
    A, B : PDouble;
    lineWidthA : TASMNativeInt;
    lineWidthB : TASMNativeInt;
    width, height : TASMNativeInt;
    indx : PIntegerArray;
    offset : integer;
  end;

function MatrixLUBacksupFunc(obj : TObject) : integer;
begin
     LUBacksup(TAsyncMatrixUSubst(obj).A, TAsyncMatrixUSubst(obj).width,
               TAsyncMatrixUSubst(obj).height, TAsyncMatrixUSubst(obj).B,
               TAsyncMatrixUSubst(obj).lineWidth);

     Result := 0;
end;

procedure ThrMatrixUSubst(A : PDouble; width, height : integer; B : PDouble; const LineWidth : TASMNativeInt);
var i: TASMNativeInt;
    obj : TAsyncMatrixUSubst;
    calls : IMtxAsyncCallGroup;
    wSize : integer;
    thrSize : Integer;
begin
     calls := MtxInitTaskGroup;
     
     thrSize := width div numCPUCores + Integer((width mod numCPUCores) <> 0);

     for i := 0 to numCPUCores - 1 do
     begin
          wSize := thrSize;
          if i = numCPUCores - 1 then
             wSize := width - i*thrSize;

          obj := TAsyncMatrixUSubst.Create(A, wSize, height, B, LineWidth);
          calls.AddTask(@MatrixLUBacksupFunc, obj);

          inc(B, thrSize);
     end;

     calls.SyncAll;
end;

const cBlkMultSize = 48;
type
  TRecMtxLUDecompData = record
    progress : TLinEquProgress;
    numCols,
    numCalc : integer;
    blkMultMem : Pdouble;
    LineWidth : TASMNativeInt;
  end;

function InternalThrMatrixLUDecomp(A : PDouble; width, height : integer;
 indx : PIntegerArray; parity : integer; var data : TRecMtxLUDecompData) : TLinEquResult;
// this is basically a copy of the unthreaded LU decomposition but with threaded parts (LU Backsup and Multiplication)!
const cMinThrMultSize = 64;
var mn : integer;
    pA : PDouble;
    idx : integer;
    maxVal : double;
    nleft, nright : integer;
    i : integer;
    pB, a12, a21 : PDouble;
    absMaxVal : double;
begin
     mn := min(width, height);

     if mn > 1 then
     begin
          // the lu backsubstitution function likes it best if the width is bigger than the height!)
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
               if (nRight > 64)
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

          if (height - nleft > cMinThrMultSize) or (nleft > cMinThrMultSize)
          then
              ThrMatrixMultEx(pB, data.LineWidth, a21, a12, nleft, height - nleft, nright, nleft, data.LineWidth, data.LineWidth, doSub, data.blkMultMem, cBlkMultSize)
          else
          begin
               // avoid multiple getmems accuring in the blocked version
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

          if Abs(maxVal) > MinDouble then
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

function ThrMatrixLUDecomp(A : PDouble; const LineWidthA : Integer; width : integer; indx : PIntegerArray; progress : TLinEquProgress = nil) : TLinEquResult;
var parity : integer;
    rc : TRecMtxLUDecompData;
    mem : Pointer;
begin
     mem := AllocMem(4*numCPUCores*(cBlkMultSize + numCPUCores + 2)*cBlkMultSize*sizeof(double) + 32);
     FillChar(indx^, width*sizeof(integer), 0);

     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.LineWidth := LineWidthA;
     rc.blkMultMem := PDouble(TASMNativeUInt(mem) + 16 - TASMNativeUInt(mem) and $0F);
     parity := 1;
     Result := InternalThrMatrixLUDecomp(A, width, width, indx, parity, rc);

     FreeMem(mem);
end;

function MatrixLUInvertCall(obj : TObject) : integer;
var i, j : integer;
    pVal : PDouble;
    width, height : integer;
    col : PConstDoubleArr;
begin
     width := TAsyncMatrixLUBacksup(obj).width;
     height := TAsyncMatrixLUBacksup(obj).height;
     col := GetMemory(sizeof(double)*height);

     for j := 0 to width - 1 do
     begin
          FillChar(col^[0], height*sizeof(double), 0);
          col^[j + TAsyncMatrixLUBacksup(obj).offset] := 1;
          MatrixLUBackSubst(TAsyncMatrixLUBacksup(obj).A, TAsyncMatrixLUBacksup(obj).lineWidthA, height,
                            TAsyncMatrixLUBacksup(obj).indx, PDouble(col), sizeof(double));

          pVal := TAsyncMatrixLUBacksup(obj).B;
          inc(pVal, TAsyncMatrixLUBacksup(obj).offset + j);

          for i := 0 to height - 1 do
          begin
               pVal^ := col^[i];
               inc(PByte(pVal), TAsyncMatrixLUBacksup(obj).LineWidthB);
          end;
     end;
     FreeMem(col);

     Result := 0;
end;

function MatrixLUBacksupCall(obj : TObject) : integer;
var j : integer;
    pVal : PDouble;
    width, height : integer;
begin
     width := TAsyncMatrixLUBacksup(obj).width;
     height := TAsyncMatrixLUBacksup(obj).height;

     pVal := TAsyncMatrixLUBacksup(obj).B;
     inc(pVal, TAsyncMatrixLUBacksup(obj).offset);
     for j := 0 to width - 1 do
     begin
          MatrixLUBackSubst(TAsyncMatrixLUBacksup(obj).A, TAsyncMatrixLUBacksup(obj).lineWidthA, height,
                            TAsyncMatrixLUBacksup(obj).indx, pVal, TAsyncMatrixLUBacksup(obj).LineWidthB);
          inc(pVal);
     end;

     Result := 0;
end;

function ThrMatrixInverse(A : PDouble; const LineWidthA : integer; width : integer; progress : TLinEquProgress = nil) : TLinEquResult;
var Y : PDouble;
    indx : array of integer;
    i : Integer;
    w : integer;
    thrSize : integer;
    wSize : integer;
    obj : TAsyncMatrixLUBacksup;
    calls : IMtxAsyncCallGroup;
begin
     Assert(lineWidthA >= width*sizeof(double), 'Dimension Error');
     Assert(width > 0, 'Dimension error');

     w := width + width and $01;
     Y := GetMemory(w*w*sizeof(double));
     SetLength(indx, 2*width);

     MatrixCopy(Y, sizeof(double)*w, A, LineWidthA, width, width);
     Result := ThrMatrixLUDecomp(Y, w*sizeof(double), width, @(indx[0]), progress);

     if Result = leSingular then
     begin
          FreeMem(Y);

          exit;
     end;

     calls := MtxInitTaskGroup;

     thrSize := width div numCPUCores + Integer((width mod numCPUCores) <> 0);

     for i := 0 to numCPUCores - 1 do
     begin
          wSize := thrSize;
          if i = numCPUCores - 1 then
             wSize := width - i*thrSize;

          obj := TAsyncMatrixLUBacksup.Create;
          obj.A := Y;
          obj.lineWidthA := w*sizeof(double);
          obj.width := wSize;
          obj.height := width;
          obj.offset := i*thrSize;
          obj.B := A;
          obj.LineWidthB := LineWidthA;
          obj.indx := @indx[0];

          calls.AddTask(@MatrixLUInvertCall, obj);
     end;

     calls.SyncAll;

     FreeMem(Y);
end;

function ThrMatrixDeterminant(A : PDouble; const LineWidthA : integer; width : integer; progress : TLinEquProgress = nil) : double;
var LUDecomp : PDouble;
    indx : Array of Integer;
    i : integer;
    pVal : PDouble;
    parity : integer;
    rc : TRecMtxLUDecompData;
    w : integer;
    mem : PDouble;
begin
     assert(width > 0, 'Dimension error');
     assert(LineWidthA >= width*sizeof(double), 'Dimension error');

     w := width + width and $01;
     LUDecomp := GetMemory(w*w*sizeof(double));
     mem := GetMemory(4*numCPUCores*(cBlkMultSize + numCPUCores + 2)*cBlkMultSize*sizeof(double) + 32);
     SetLength(indx, width);
     MatrixCopy(LUDecomp, w*sizeof(double), A, LineWidthA, width, width);

     rc.progress := progress;
     rc.numCols := width;
     rc.numCalc := 0;
     rc.blkMultMem := PDouble(TASMNativeUInt(mem) + 16 - TASMNativeUInt(mem) and $0F);
     rc.LineWidth := w*sizeof(double);

     parity := 1;
     if InternalThrMatrixLUDecomp(LUDecomp, width, width, @indx[0], parity, rc) = leSingular then
     begin
          Result := 0;
          FreeMem(LUDecomp);
          FreeMem(mem);
          exit;
     end;
     pVal := LUDecomp;
     Result := parity;
     for i := 0 to width - 1 do
     begin
          Result := Result * pVal^;
          inc(pVal);
          inc(PByte(pVal), width*sizeof(double));
     end;

     FreeMem(LUDecomp);
     FreeMem(mem);
end;

function ThrMatrixLinEQSolve(A : PDouble; const LineWidthA : integer; width : integer; B : PDouble; const LineWidthB : integer; X : PDouble;
 const LineWidthX : integer;  Width2 : integer; const NumRefinments : integer; progress : TLinEquProgress) : TLinEquResult;
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
    w : TASMNativeInt;
    thrSize : integer;
    wSize : integer;
    obj : TAsyncMatrixLUBacksup;
    calls : IMtxAsyncCallGroup;
begin
     progRef := nil;
     progObj := nil;
     if Assigned(progress) then
     begin
          progObj := TLinearEQProgress.Create;
          progObj.refProgress := progress;
          progObj.numRefinenmentSteps := NumRefinments;
          progRef := @(progObj.LUDecompSolveProgress);
     end;

     w := width + width and $01;

     LUDecomp := GetMemory(w*w*sizeof(double));
     MatrixCopy(LUDecomp, w*sizeof(double), A, LineWidthA, width, width);

     SetLength(indx, width);
     Result := ThrMatrixLUDecomp(LUDecomp, w*sizeof(double), width, @indx[0], progRef);

     if Result = leSingular then
     begin
          progObj.Free;
          FreeMem(LUDecomp);
          exit;
     end;

     thrSize := Max(1, width2 div numCPUCores + Integer((width2 mod numCPUCores) <> 0));

     // first copy the B matrix to X -> the result is overwritten
     MatrixCopy(X, LineWidthX, B, LineWidthB, width2, width);

     // now distribute the computaions accross all cpu's
     calls := MtxInitTaskGroup;
     
     for i := 0 to Min(thrSize, numCPUCores) - 1 do
     begin
          wSize := thrSize;
          if i = Min(thrSize, numCPUCores) - 1 then
             wSize := width2 - i*thrSize;

          obj := TAsyncMatrixLUBacksup.Create;
          obj.A := LUDecomp;
          obj.lineWidthA := w*sizeof(double);
          obj.width := wSize;
          obj.height := width;
          obj.offset := i*thrSize;
          obj.B := X;
          obj.LineWidthB := LineWidthX;
          obj.indx := @indx[0];

          calls.AddTask(@MatrixLUBacksupCall, obj);
     end;

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

                    MatrixLUBackSubst(LUDecomp, sizeof(double)*w, width, @indx[0], @row[0], sizeof(double));
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

     FreeMem(LUDecomp);

     if Assigned(progObj) then
        progObj.Free;
     if Assigned(progress) then
        progress(100);
end;

{ TAsyncMatrixLUBacksupobj }

constructor TAsyncMatrixUSubst.Create(aA: PDouble;
  awidth, aheight: TASMNativeInt; aB: PDouble;
  const aLineWidth: TASMNativeInt);
begin
     A := aA;
     B := aB;
     lineWidth := aLineWidth;
     width := aWidth;
     height := aheight;
end;

end.
