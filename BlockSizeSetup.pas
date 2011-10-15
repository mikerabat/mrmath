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


unit BlockSizeSetup;

// ####################################################
// #### functions to initialized/evaluate the optimal blocksizes
// #### for the processors
// ####################################################

interface

const cDefCacheBlkSize = 256;

var BlockMatrixCacheSize : integer = cDefCacheBlkSize;
    BlockMatrixVectorCacheSize : integer = cDefCacheBlkSize*cDefCacheBlkSize;

var BlockedMatrixMultSize : integer = 2048;
    BlockedVectorMatrixMultSize : integer = cDefCacheBlkSize*cDefCacheBlkSize;
    TransposeMatrixMultSize : integer = 32;

    ThreadMatrixMultSize : integer = 2*cDefCacheBlkSize;

// checks when first applying a transpose operation is better for the multiplication
procedure SetupOptTransposeMultMatrixSize;
procedure SetupOptBlockMatrixSize;
procedure SetupOptBlockMatrixVectorSize;
procedure SetupBlockedMatrixMultSize;
procedure SetupBlockedVectorMatrixMultSize;
//procedure SetupThreadMatrixMultSize;

implementation

uses Classes, ASMMatrixOperations, Types, Windows, Math, SimpleMatrixOperations,
     ASMMatrixVectorMultOperations, OptimizedFuncs;

const cMatrixMaxTestSize = 2048;
      cVectorMatrixTestSize = cMatrixMaxTestSize*24;
      cNumIter = 5;

procedure SetupInitVars(var dest, a, b : PDouble; awidth, aheight, bwidth : integer); overload;
var pa, pb, pDest : PDouble;
    x : integer;
begin
     GetMem(a, aWidth*aHeight*sizeof(double));
     GetMem(b, bWidth*aWidth*sizeof(double));
     GetMem(dest, aHeight*bWidth*sizeof(double));

     pa := a;
     pb := b;
     pDest := dest;

     for x := 0 to aWidth*aHeight - 1 do
     begin
          pa^ := random;
          inc(pa);
     end;

     for x := 0 to aWidth*bwidth - 1 do
     begin
          pb^ := random;
          inc(pb);
     end;

     for x := 0 to aHeight*bwidth - 1 do
     begin
          pDest^ := random;
          inc(pDest);
     end;
end;

procedure SetupInitVars(var dest, a, b : PDouble; setupSize : integer);  overload;
begin
     SetupInitVars(dest, a, b, setupSize, setupSize, setupSize);
end;

function CheckMatrixMult(dest, a, b : PDouble; size : integer; iter : integer = cNumIter; destLineWidth : integer = cMatrixMaxTestSize*sizeof(double); LineWidth1 : integer = cMatrixMaxTestSize*sizeof(double); LineWidth2 : integer = cMatrixMaxTestSize*sizeof(double)) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     QueryPerformanceCounter(startTime);
     for i := 0 to iter - 1 do
         ASMMatrixMult(dest, destLineWidth, a, b, size, size, size, size, LineWidth1, LineWidth2);
     QueryPerformanceCounter(endTime);

     Result := endTime - startTime;
end;

function CheckTransposeMatrixMult(dest, a, b : PDouble; size : integer; iter : integer = cNumIter; destLineWidth : integer = cMatrixMaxTestSize*sizeof(double); LineWidth1 : integer = cMatrixMaxTestSize*sizeof(double); LineWidth2 : integer = cMatrixMaxTestSize*sizeof(double)) : int64;
var startTime, endTime : Int64;
    i : integer;
    transMtx : PDouble;
begin
     QueryPerformanceCounter(startTime);
     for i := 0 to iter - 1 do
     begin
          GetMem(transMtx, size*size*sizeof(double));
          MatrixTranspose(transMtx, size*sizeof(double), b, LineWidth2, size, size);
          ASMMatrixMultTransposed(dest, destLineWidth, a, transMtx, size, size, size, size, LineWidth1, size*SizeOf(double));
          FreeMem(transMtx);
     end;
     QueryPerformanceCounter(endTime);

     Result := endTime - startTime;
end;


function CheckBlockedMatrixMult(dest, a, b : PDouble; size : integer; blockSize : integer) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     QueryPerformanceCounter(startTime);
     for i := 0 to cNumIter - 1 do
         BlockedMatrixMultiplication(dest, size*sizeof(double), a, b, size, size, size, size, size*sizeof(double), size*sizeof(double), blockSize);
     QueryPerformanceCounter(endTime);

     Result := endTime - startTime;
end;

function CheckBlockedMatrixVectorMult(dest, a, b : PDouble; aWidth, aHeight : integer; LineWidth1 : integer; blockSize : integer) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     QueryPerformanceCounter(startTime);
     for i := 0 to cNumIter - 1 do
         BlockedMatrixVectorMultiplication(dest, sizeof(double), a, b, aWidth, aHeight, aWidth, LineWidth1, blockSize);
     QueryPerformanceCounter(endTime);

     Result := endTime - startTime;
end;

function CheckMatrixVectorMult(dest, a, b : PDouble; aWidth, aHeight  : integer; lineWidth1 : integer) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     QueryPerformanceCounter(startTime);
     for i := 0 to cNumIter - 1 do
         ASMMatrixMult(dest, sizeof(double), a, b, aWidth, aHeight, 1, aWidth, LineWidth1, sizeof(double));
     QueryPerformanceCounter(endTime);

     Result := endTime - startTime;
end;

procedure SetupBlockedVectorMatrixMultSize;
var a, b : PDouble;
    dest : PDouble;
    actSize : integer;
    minSize, maxSize : integer;
    t1, t2 : Int64;
begin
     SetupInitVars(dest, a, b, cVectorMatrixTestSize, cMatrixMaxTestSize, 1);

     // Calculate the break even point where the blocked operation is faster than
     // the simple one
     minSize := 0;
     maxSize := 2048;

     while True do
     begin
          actSize := (maxSize + minSize) div 2;

          t2 := CheckMatrixVectorMult(dest, a, b, cVectorMatrixTestSize, actSize, cVectorMatrixTestSize*sizeof(double));
          t1 := CheckBlockedMatrixVectorMult(dest, a, b, cVectorMatrixTestSize, actSize, cVectorMatrixTestSize*sizeof(Double), BlockMatrixVectorCacheSize);

          if t2*1.05 > t1
          then
              maxSize := actSize
          else
              minSize := actSize;

          if maxSize - minSize <= 4 then
             break;
     end;

     BlockedVectorMatrixMultSize := (maxSize + minSize) div 2;

     FreeMem(a);
     FreeMem(b);
     FreeMem(dest);
end;

procedure SetupBlockedMatrixMultSize;
var a, b : PDouble;
    dest : PDouble;
    actSize : integer;
    minSize, maxSize : integer;
    t1, t2 : Int64;
begin
     SetupInitVars(dest, a, b, cMatrixMaxTestSize);

     // Calculate the break even point where the blocked operation is faster than
     // the simple one
     minSize := 0;
     maxSize := 2048;

     while True do
     begin
          actSize := (maxSize + minSize) div 2;

          t2 := CheckMatrixMult(dest, a, b, actSize);
          t1 := CheckBlockedMatrixMult(dest, a, b, actSize, BlockMatrixCacheSize);

          if t2*1.05 > t1
          then
              maxSize := actSize
          else
              minSize := actSize;

          if maxSize - minSize <= 4 then
             break;
     end;

     BlockedMatrixMultSize := (maxSize + minSize) div 2;
     BlockedMatrixMultSize := BlockedMatrixMultSize - BlockedMatrixMultSize mod 16;

     FreeMem(a);
     FreeMem(b);
     FreeMem(dest);
end;

type
  TMultBlockIter = record
    t : int64;
    size : integer;
  end;
  PMultBlockIter = ^TMultBlockIter;

function t12Compare(p1, p2 : Pointer) : integer;
begin
     Result := CompareValue(PMultBlockIter(p1)^.t, PMultBlockIter(p2)^.t);
end;

procedure SetupOptBlockMatrixSize;
var a, b : PDouble;
    dest : PDouble;
    actSize : integer;
    tmin : int64;
    tmax : int64;
    actElem : PMultBlockIter;
    iterList : TList;
    i : Integer;
const cCoarseIter = 256;
      cFineIter = 16;
begin
     SetupInitVars(dest, a, b, cMatrixMaxTestSize div 2);

     // Calculate the break even point where the blocked operation is faster than
     // the simple one

     actSize := 32;

     iterList := TList.Create;

     while actSize < cMatrixMaxTestSize div 2 do
     begin
          new(actElem);

          actElem^.size := actSize;

          actElem^.t := CheckBlockedMatrixMult(dest, a, b, cMatrixMaxTestSize div 2, actSize);

          iterList.Add(actElem);

          inc(actSize, cCoarseIter);
     end;

     iterList.Sort(t12Compare);

     tmin := Min( Min(PMultBlockIter(iterList[0])^.size, PMultBlockIter(iterList[1])^.size), PMultBlockIter(iterList[2])^.size);
     tmax := Max( Max(PMultBlockIter(iterList[0])^.size, PMultBlockIter(iterList[1])^.size), PMultBlockIter(iterList[2])^.size);

     for i := 0 to iterList.Count - 1 do
         dispose(PMultBlockIter(iterList[i]));

     iterList.Clear;

     actSize := tmin;
     while actSize <= tmax do
     begin
          new(actElem);

          actElem^.size := actSize;

          actElem^.t := CheckBlockedMatrixMult(dest, a, b, cMatrixMaxTestSize div 2, actSize);

          iterList.Add(actElem);

          inc(actSize, cFineIter);
     end;

     iterList.Sort(t12Compare);
     BlockMatrixCacheSize := PMultBlockIter(iterList[0])^.size;
     BlockMatrixCacheSize := BlockMatrixCacheSize  - BlockMatrixCacheSize mod 16;

     for i := 0 to iterList.Count - 1 do
         dispose(PMultBlockIter(iterList[i]));

     FreeMem(a);
     FreeMem(b);
     FreeMem(dest);

     iterList.Free;
end;

procedure SetupOptBlockMatrixVectorSize;
var a, b : PDouble;
    dest : PDouble;
    actSize : integer;
    tmin : int64;
    tmax :int64;
    actElem : PMultBlockIter;
    iterList : TList;
    i : Integer;
const cCoarseIter = 256;
      cFineIter = 16;
begin
     SetupInitVars(dest, a, b, cVectorMatrixTestSize, cMatrixMaxTestSize div 2, 1);

     // Calculate the break even point where the blocked operation is faster than
     // the simple one

     actSize := 32;

     iterList := TList.Create;

     while actSize < cMatrixMaxTestSize div 2 do
     begin
          new(actElem);

          actElem^.size := actSize;

          actElem^.t := CheckBlockedMatrixVectorMult(dest, a, b, cVectorMatrixTestSize, cMatrixMaxTestSize div 2, cVectorMatrixTestSize*sizeof(double), actSize);

          iterList.Add(actElem);
          inc(actSize, cCoarseIter);
     end;

     iterList.Sort(t12Compare);

     tmin := Min( Min(PMultBlockIter(iterList[0])^.size, PMultBlockIter(iterList[1])^.size), PMultBlockIter(iterList[2])^.size);
     tmax := Max( Max(PMultBlockIter(iterList[0])^.size, PMultBlockIter(iterList[1])^.size), PMultBlockIter(iterList[2])^.size);

     for i := 0 to iterList.Count - 1 do
         dispose(PMultBlockIter(iterList[i]));

     iterList.Clear;

     actSize := tmin;
     while actSize <= tmax do
     begin
          new(actElem);

          actElem^.size := actSize;

          actElem^.t := CheckBlockedMatrixVectorMult(dest, a, b, cVectorMatrixTestSize, cMatrixMaxTestSize div 2, cVectorMatrixTestSize*sizeof(double), actSize);

          iterList.Add(actElem);

          inc(actSize, cFineIter);
     end;

     iterList.Sort(t12Compare);
     BlockMatrixVectorCacheSize := PMultBlockIter(iterList[0])^.size;

     for i := 0 to iterList.Count - 1 do
         dispose(PMultBlockIter(iterList[i]));

     FreeMem(a);
     FreeMem(b);
     FreeMem(dest);

     iterList.Free;
end;

procedure SetupOptTransposeMultMatrixSize;
var a, b, dest : PDouble;
    i : integer;
    t1 : int64;
    t2 : int64;
    mtxSizes : Array[0..50] of integer;
    j : Integer;
begin
     SetupInitVars(dest, a, b, 512);

     for j := 0 to High(mtxSizes) do
     begin
          mtxSizes[j] := 32;
          for i := 4 to 512 - 1 do
          begin
               t2 := CheckMatrixMult(dest, a, b, i, 20, 512*sizeof(double), 512*sizeof(double), 512*sizeof(double));
               t1 := CheckTransposeMatrixMult(dest, a, b, i, 20, 512*sizeof(double), 512*sizeof(double), 512*sizeof(double));

               if t1*1.05 < t2 then
               begin
                    mtxSizes[j] := i;
                    break;
               end;
          end;
     end;

     // result is the mean:
     TransposeMatrixMultSize := 0;
     for j := 0 to High(mtxSizes) - 1 do
         inc(TransposeMatrixMultSize, mtxSizes[j]);

     TransposeMatrixMultSize := TransposeMatrixMultSize div Length(mtxSizes);
     TransposeMatrixMultSize := TransposeMatrixMultSize - TransposeMatrixMultSize mod 16;
end;

end.
