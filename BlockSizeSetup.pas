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

// nice values for processors:
// core2 ~ 256
// AMD A8 ~ 128
const cDefCacheBlkSize = 128;

var BlockMatrixCacheSize : integer = cDefCacheBlkSize;    // use this to determine the blocked multiplication cache size


var BlockedMatrixMultSize : integer = 512;   // used to get a threshold on how to switch between mult procedures (e.g. use inner loop)
    BlockedVectorMatrixMultSize : integer = cDefCacheBlkSize*cDefCacheBlkSize;
    QRBlockSize : integer = 32;
    QRMultBlockSize : integer = 128;
    CholBlockSize : integer = 32;
    SVDBlockSize : integer = 32;
    HessBlockSize : integer = 32;
    HessMultBlockSize : integer = 128;

// checks when first applying a transpose operation is better for the multiplication
procedure SetupOptBlockMatrixSize;
procedure SetupBlockedMatrixMultSize;

// returns the needed additional size when doing a blocked multiplication
function BlockMultMemSize(blkSize : integer) : integer;

implementation

uses Classes, ASMMatrixOperations, Types, MtxTimer, Math, MatrixASMStubSwitch, BlockedMult;

const cMatrixMaxTestSize = 2048;
      cNumIter = 5;

procedure SetupInitVars(out dest, a, b : PDouble; awidth, aheight, bwidth : integer); overload;
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

procedure SetupInitVars(out dest, a, b : PDouble; setupSize : integer);  overload;
begin
     SetupInitVars(dest, a, b, setupSize, setupSize, setupSize);
end;

function CheckMatrixMult(dest, a, b : PDouble; size : integer; iter : integer = cNumIter; destLineWidth : integer = cMatrixMaxTestSize*sizeof(double); LineWidth1 : integer = cMatrixMaxTestSize*sizeof(double); LineWidth2 : integer = cMatrixMaxTestSize*sizeof(double)) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     startTime := MtxGetTime;
     for i := 0 to iter - 1 do
         ASMMatrixMult(dest, destLineWidth, a, b, size, size, size, size, LineWidth1, LineWidth2);
     endTime := MtxGetTime;

     Result := endTime - startTime;
end;

function CheckTransposeMatrixMult(dest, a, b : PDouble; size : integer; iter : integer = cNumIter; destLineWidth : integer = cMatrixMaxTestSize*sizeof(double); LineWidth1 : integer = cMatrixMaxTestSize*sizeof(double); LineWidth2 : integer = cMatrixMaxTestSize*sizeof(double)) : int64;
var startTime, endTime : Int64;
    i : integer;
    transMtx : PDouble;
begin
     startTime := MtxGetTime;
     for i := 0 to iter - 1 do
     begin
          GetMem(transMtx, size*size*sizeof(double));
          MatrixTranspose(transMtx, size*sizeof(double), b, LineWidth2, size, size);
          ASMMatrixMultTransposed(dest, destLineWidth, a, transMtx, size, size, size, size, LineWidth1, size*SizeOf(double));
          FreeMem(transMtx);
     end;
     endTime := MtxGetTime;

     Result := endTime - startTime;
end;


function CheckBlockedMatrixMult(dest, a, b : PDouble; size : integer; blockSize : integer) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     startTime := MtxGetTime;
     for i := 0 to cNumIter - 1 do
         BlockMatrixMultiplication(dest, size*sizeof(double), a, b, size, size, size, size, size*sizeof(double), size*sizeof(double), blockSize);
     endTime := MtxGetTime;

     Result := endTime - startTime;
end;

function CheckBlockedMatrixVectorMult(dest, a, b : PDouble; aWidth, aHeight : integer; LineWidth1 : integer; blockSize : integer) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     startTime := MtxGetTime;
     for i := 0 to cNumIter - 1 do
         BlockMatrixVectorMultiplication(dest, sizeof(double), a, b, aWidth, aHeight, aWidth, LineWidth1, blockSize);
     endTime := MtxGetTime;

     Result := endTime - startTime;
end;

function CheckMatrixVectorMult(dest, a, b : PDouble; aWidth, aHeight  : integer; lineWidth1 : integer) : int64;
var startTime, endTime : Int64;
    i : integer;
begin
     startTime := MtxGetTime;
     for i := 0 to cNumIter - 1 do
         ASMMatrixMult(dest, sizeof(double), a, b, aWidth, aHeight, 1, aWidth, LineWidth1, sizeof(double));
     endTime := MtxGetTime;

     Result := endTime - startTime;
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
     BlockedMatrixMultSize := Max(32, BlockedMatrixMultSize - BlockedMatrixMultSize mod 16);

     BlockedMatrixMultSize := Max( QRBlockSize, Max( QRMultBlockSize, Max(SVDBlockSize, Max(CholBlockSize, BlockedMatrixMultSize ) ) ) );
     BlockedMatrixMultSize := Max( QRBlockSize, Max( QRMultBlockSize, Max(SVDBlockSize, Max(CholBlockSize, BlockedMatrixMultSize ) ) ) );


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

     actSize := 64;

     iterList := TList.Create;

     while actSize < cMatrixMaxTestSize div 2 do
     begin
          new(actElem);

          actElem^.size := actSize;

          actElem^.t := CheckBlockedMatrixMult(dest, a, b, cMatrixMaxTestSize div 2, actSize);

          iterList.Add(actElem);

          inc(actSize, cCoarseIter);
     end;

     iterList.Sort(@t12Compare);

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

     iterList.Sort(@t12Compare);
     BlockMatrixCacheSize := PMultBlockIter(iterList[0])^.size;
     BlockMatrixCacheSize := BlockMatrixCacheSize  - BlockMatrixCacheSize mod 16;

     for i := 0 to iterList.Count - 1 do
         dispose(PMultBlockIter(iterList[i]));

     FreeMem(a);
     FreeMem(b);
     FreeMem(dest);

     iterList.Free;
end;

function BlockMultMemSize(blkSize : integer) : integer;
var blockByteSize : integer;
begin
     blockByteSize := blkSize*blkSize*sizeof(double);

     Result := 4*blockByteSize;
end;

end.
