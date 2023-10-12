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

unit BlockedMult;

interface

{$IFDEF FPC} {$MODESWITCH ADVANCEDRECORDS} {$ENDIF}

uses MatrixConst;

// ###################################################################
// #### Generic blocked multiplication routines

// blocked matrix mult + strassen multiplication functions

procedure GenericBlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble;
  width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil);

// calculates dest = mt1'*mt2
procedure GenericBlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
// calculates dest = mt1*mt2'
procedure GenericBlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);


// this routine first performs a transposition on the second matrix before the multiplication is executed. This results
// normaly in quite a boost.

procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil); overload;

// calculates dest = mt1'*mt2
procedure BlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
// calculates dest = mt1*mt2'
procedure BlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);



// this is a blockwise matrix multiplication routine which takes a limited cache into account.
// The routine tries to tile the matrix into 256x256 blocks (which seems to be a good approximation
// for a Core2 processor) which fits into the Level1 cache thus reduces
// cache misses.
procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation = doNone); overload;

procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; height2 : NativeInt; const LineWidth1 : NativeInt); overload;
procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; height2 : NativeInt; const LineWidth1 : NativeInt; blockSize : NativeInt); overload;

// ################################################################
// #### Threaded versions
function UseInnerBlockMult( w, h : NativeInt) : boolean;

// calculates dest = mt1*mt2'
procedure ThrBlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);

procedure ThrBlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure ThrBlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil); overload;


implementation

uses BlockSizeSetup, SimpleMatrixOperations, MatrixASMStubSwitch, Math,
     {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     MtxThreadPool, ThreadedMatrixOperations, MathUtilFunc;

{$I 'mrMath_CPU.inc'}

{$IFNDEF MRMATH_NOASM}

procedure YieldProcessor; {$IFDEF FPC} assembler; {$ENDIF}
asm
   PAUSE;
end;

procedure MemoryBarrier; {$IFDEF FPC} assembler; {$ENDIF}
{$IFDEF x64}
asm
   push rax;
   XCHG [rsp],rax;
   POP  rax;
end;
{$ELSE}
asm
   push eax;
   XCHG [esp],eax;
   POP  eax;
end;
{$ENDIF}

{$ELSE}

// MRMATH_NOASM for FPC - need to be adjusted for non x86 cores
procedure YieldProcessor;
begin
     {$IFDEF MSWINDOWS}
     SwitchToThread;
     {$ENDIF}
     // todo other platforms
end;

procedure MemoryBarrier;
begin
     // todo: MemoryBarrier is only availabel as macro in winnt.h not in windows.pas
end;

{$ENDIF}

procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; height2 : NativeInt; const LineWidth1 : NativeInt); overload;
begin
     BlockMatrixVectorMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1, BlockedVectorMatrixMultSize);
end;

procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; height2 : NativeInt; const LineWidth1 : NativeInt; blockSize : NativeInt); overload;
var h : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pActBlk : PDouble;
    pHelp : PDouble;
    w1FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    gammaWidth : NativeInt;
    ptrMem : Pointer;
begin
     if (width1 = 0) or (height1 = 0) or (height2 = 0) then
        exit;

     assert((width1 = height2), 'Dimension error');
     assert((LineWidth1 >= width1*sizeof(double)), 'Line widths do not match');
     assert(BlockSize > 1, 'Error block size must be at least 2');

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     actBlk := MtxMallocAlign(2*blockSize*sizeof(double), ptrMem );
     multBlk := actBlk;
     inc(multBlk, blockSize);

     blkHeight := blockSize;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          FillChar(actBlk^, blockSize*sizeof(double), 0);
          pa := mt1;
          pb := mt2;

          gammaWidth := blockSize;
          for idx := 0 to gamma do
          begin
               if (idx = gamma) and not w1FitCacheSize then
                  gammaWidth := width1 mod blockSize;

               if gammaWidth > 1
               then
                   MatrixMult(multBlk, sizeof(double), pa, pb, gammaWidth, blkHeight, 1, gammaWidth, LineWidth1, sizeof(double))
               else
                   GenericMtxMult(multBlk, sizeof(double), pa, pb, gammaWidth, blkHeight, 1, gammaWidth, LineWidth1, sizeof(double));

               // treat the addition as vector add:
               if blkHeight > 1
               then
                   MatrixAdd(actBlk, sizeof(double), actBlk, multBlk, blkHeight, 1, sizeof(double), sizeof(double))
               else
                   // intersting: the normal matrix addition is a bit faster (or just a tiny bit slower) than the asm version in this case!
                   GenericMtxAdd(actBlk, sizeof(double), actBlk, multBlk, 1, blkHeight, sizeof(double), sizeof(double));

               inc(pa, gammaWidth);
               inc(pb, gammaWidth);
          end;

          pHelp := Dest;
          pActBlk := actBlk;
          for idx := 0 to blkHeight - 1 do
          begin
               pHelp^ := pActBlk^;
               inc(PByte(pHelp), destLineWidth);
               inc(pActBlk);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
          inc(PByte(Dest), blkHeight*destLineWidth);
     end;

     FreeMem(ptrMem);
end;

procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; op : TMatrixMultDestOperation);
begin
     BlockMatrixMultiplicationDirect(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op);
end;

procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation);
var w, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkWidth : NativeInt;
    gammaWidth : NativeInt;
    sizeVal : NativeInt;
    m1, m2 : Pointer;
    blockByteSize : NativeInt;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     // estimate a good blocksize - one with a small number of zero columns (but as near as possible to cCacheMtxSize
     if blockSize <= 0 then
     begin
          sizeVal := Max(width1, width2);

          while sizeVal > cCacheMtxSize do
                sizeVal := sizeVal shr 1;

          blockSize := Min(Max(width1, width2), Max(64, sizeVal));
     end;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + NativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);
     actBlk := MtxMallocAlign(blockByteSize, m1);
     multBlk := MtxMallocAlign(blockByteSize, m2);

     blkHeight := blockSize;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkWidth := blockSize;

          for blkIdxX := 0 to w do
          begin
               if (blkIdxX = w) and not w2FitCacheSize then
                  blkWidth := (width2 mod blockSize);

               MtxMemInit(actBlk, blockByteSize, 0 );
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    if blkWidth > 1 then
                    begin
                         MatrixMult(multBlk, blockSize*sizeof(double), pa, pb, gammaWidth, blkHeight, blkWidth, gammaWidth, LineWidth1, LineWidth2);
                         MatrixAdd(actBlk, blockSize*sizeof(double), actBlk, multBlk, blkWidth, blkHeight, blockSize*sizeof(double), blockSize*sizeof(double));
                    end
                    else
                    begin
                         GenericMtxMult(multBlk, blockSize*sizeof(double), pa, pb, gammaWidth, blkHeight, blkWidth, gammaWidth, LineWidth1, LineWidth2);
                         GenericMtxAdd(actBlk, blockSize*sizeof(double), actBlk, multBlk, blkWidth, blkHeight, blockSize*sizeof(double), blockSize*sizeof(double));
                    end;

                    inc(pa, gammaWidth);
                    inc(PByte(pb), gammaWidth*LineWidth2);
               end;

               // apply final operation such that we got the final result: Dest := Dest +- A*B ;
               case op of
                 doNone: MatrixCopy(pDest, destLineWidth, actBlk, blockSize*sizeof(double), blkWidth, blkHeight);
                 doAdd: MatrixAdd(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockSize*sizeof(double));
                 doSub: MatrixSub(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockSize*sizeof(double));
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     FreeMem(m1);
     FreeMem(m2);
end;

procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; op : TMatrixMultDestOperation);
begin
     BlockMatrixMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil);
end;

procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var w, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk, copyBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkWidth : NativeInt;
    gammaWidth : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
    isAligned : boolean;
    ptrMem : Pointer;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     if (blockSize > width1) and (not assigned(mem)) then
        blockSize := Next2Pwr( width1, blockSize );

     isAligned := (NativeUint(dest) and $00000001F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + NativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);
     transBlk := PDouble(NativeUint(actBlk) + 2*blockByteSize);
     copyBlk := PDouble(NativeUint(actBlk) + 3*blockByteSize);

     blockLineSize := blockSize*sizeof(double);

     blkHeight := blockSize;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkWidth := blockSize;

          for blkIdxX := 0 to w do
          begin
               if (blkIdxX = w) and not w2FitCacheSize then
                  blkWidth := (width2 mod blockSize);

               MtxMemInit(actBlk, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    if (blkWidth > 3) and (blkHeight > 3) then
                    begin
                         MatrixTranspose(transBlk, blockLineSize, pb, LineWidth2, blkWidth, gammaWidth);

                         // it is faster to copy the block rather then multply it unaligned!
                         if (not isAligned) or ((LineWidth1 and $00000001F) <> 0) then
                         begin
                              MatrixCopy(copyBlk, blockLineSize, pa, LineWidth1, gammaWidth, blkHeight);
                              MatrixMultT2(multblk, blockLineSize, copyBlk, transBlk, gammaWidth, blkHeight, gammaWidth, blkWidth, blockLineSize, blockLineSize);
                         end
                         else
                             MatrixMultT2(multBlk, blockLineSize, pa, transBlk, gammaWidth, blkHeight, gammaWidth, blkWidth, LineWidth1, blockLineSize);

                         MatrixAdd(actBlk, blockLineSize, actBlk, multBlk, blkWidth, blkHeight, blockLineSize, blockLineSize);
                    end
                    else
                    begin
                         GenericMtxMult(multBlk, blockLineSize, pa, pb, gammaWidth, blkHeight, blkWidth, gammaWidth, LineWidth1, LineWidth2);
                         GenericMtxAdd(actBlk, blockLineSize, actBlk, multBlk, blkWidth, blkHeight, blockLineSize, blockLineSize);
                    end;

                    inc(pa, gammaWidth);
                    inc(PByte(pb), gammaWidth*LineWidth2);
               end;

               // apply final operation such that we got the final result: Dest := Dest +- A*B ;
               case op of
                 doNone: MatrixCopy(pDest, destLineWidth, actBlk, blockLineSize, blkWidth, blkHeight);
                 doAdd: MatrixAdd(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockLineSize);
                 doSub: MatrixSub(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockLineSize);
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     if Assigned(ptrMem) then
        FreeMem(ptrMem);
end;

// calculates mt1'*mt2
procedure BlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var w, w1 : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk1, transBlk2 : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkWidth1 : NativeInt;
    blkWidth : NativeInt;
    gammaHeight : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
    ptrMem : Pointer;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((height1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     if (blockSize > width1) and (not Assigned(mem)) then
        blockSize := Next2Pwr( height1, blockSize );

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     w1 := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;
     w := width2 div blockSize + NativeInt(not w2FitCacheSize) - 1;
     gamma := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );

     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);
     transBlk1 := PDouble(NativeUint(actBlk) + 2*blockByteSize);
     transBlk2 := PDouble(NativeUint(actBlk) + 3*blockByteSize);

     blockLineSize := blockSize*sizeof(double);

     blkWidth1 := blockSize;

     for blkIdxY := 0 to w1 do
     begin
          if (blkIdxY = w1) and not w1FitCacheSize then
             blkWidth1 := (width1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkWidth := blockSize;

          for blkIdxX := 0 to w do
          begin
               if (blkIdxX = w) and not w2FitCacheSize then
                  blkWidth := (width2 mod blockSize);

               MtxMemInit(actBlk, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaHeight := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not h1FitCacheSize then
                       gammaHeight := height1 mod blockSize;

                    if (blkWidth > 3) and (blkWidth1 > 3) then
                    begin
                         MatrixTranspose(transBlk1, blockLineSize, pa, LineWidth1, blkWidth1, gammaHeight);
                         MatrixTranspose(transBlk2, blockLineSize, pb, LineWidth2, blkWidth, gammaHeight);

                         MatrixMultT2(multblk, blockLineSize, transBlk1, transBlk2, gammaHeight, blkWidth1, gammaHeight, blkWidth, blockLineSize, blockLineSize);
                         MatrixAdd(actBlk, blockLineSize, actBlk, multBlk, blkWidth, blkWidth1, blockLineSize, blockLineSize);
                    end
                    else
                    begin
                         GenericMtxTranspose(transBlk1, blockLineSize, pa, LineWidth1, blkWidth1, gammaHeight);
                         GenericMtxMult(multBlk, blockLineSize, transBlk1, pb, gammaHeight, blkWidth1, blkWidth, gammaHeight, blockLineSize, LineWidth2);
                         GenericMtxAdd(actBlk, blockLineSize, actBlk, multBlk, blkWidth, blkWidth1, blockLineSize, blockLineSize);
                    end;

                    inc(PByte(pa), gammaHeight*LineWidth1);
                    inc(PByte(pb), gammaHeight*LineWidth2);
               end;

               // apply final operation such that we got the final result: Dest := Dest +- A*B ;
               case op of
                 doNone: MatrixCopy(pDest, destLineWidth, actBlk, blockLineSize, blkWidth, blkWidth1);
                 doAdd: MatrixAdd(pDest, destLineWidth, pDest, actBlk, blkWidth, blkWidth1, destLineWidth, blockLineSize);
                 doSub: MatrixSub(pDest, destLineWidth, pDest, actBlk, blkWidth, blkWidth1, destLineWidth, blockLineSize);
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(mt1, blkWidth1);
     end;

     if Assigned(ptrMem) then
        FreeMem(ptrMem);
end;

// calculates mt1*mt2'
procedure BlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var h1, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    copyBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    h2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkHeight1 : NativeInt;
    gammaWidth : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
    isAligned : boolean;
    ptrMem : Pointer;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = width2), 'Dimension error');
     assert((destLineWidth - height2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     // try to take a shortcut where blocked matrix mult is hindering performance
     if (op = doNone) and (width1 <= blockSize) then
     begin
          MatrixMultT2( dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2 );
          exit;
     end;

     if (blockSize > width1) and (not Assigned(mem)) then
        blockSize := Next2Pwr( width1, blockSize );
     
     isAligned := (NativeUint(dest) and $0000000F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     h2FitCacheSize := (height2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     h1 := height2 div blockSize + NativeInt(not h2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);
     copyBlk := PDouble(NativeUint(actBlk) + 2*blockByteSize);

     blockLineSize := blockSize*sizeof(double);

     blkHeight := blockSize;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkHeight1 := blockSize;

          for blkIdxX := 0 to h1 do
          begin
               if (blkIdxX = h1) and not h2FitCacheSize then
                  blkHeight1 := (height2 mod blockSize);

               MtxMemInit(actBlk, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    if (blkHeight1 > 3) and (blkHeight > 3) then
                    begin
                         // it is faster to copy the block rather then multply it unaligned!
                         if (not isAligned) or ((LineWidth1 and $0000001F) <> 0) then
                         begin
                              MatrixCopy(copyBlk, blockLineSize, pa, LineWidth1, gammaWidth, blkHeight);
                              MatrixMultT2(multblk, blockLineSize, copyBlk, pb, gammaWidth, blkHeight, gammaWidth, blkHeight1, blockLineSize, LineWidth2);
                         end
                         else
                             MatrixMultT2(multBlk, blockLineSize, pa, pb, gammaWidth, blkHeight, gammaWidth, blkHeight1, LineWidth1, LineWidth2);

                         MatrixAdd(actBlk, blockLineSize, actBlk, multBlk, blkHeight1, blkHeight, blockLineSize, blockLineSize);
                    end
                    else
                    begin
                         GenericMtxMultTransp(multBlk, blockLineSize, pa, pb, gammaWidth, blkHeight, gammaWidth, blkHeight1, LineWidth1, LineWidth2);
                         GenericMtxAdd(actBlk, blockLineSize, actBlk, multBlk, blkHeight1, blkHeight, blockLineSize, blockLineSize);
                    end;

                    inc(pa, gammaWidth);
                    inc(pb, gammaWidth);
               end;

               // apply final operation such that we got the final result: Dest := Dest +- A*B ;
               case op of
                 doNone: MatrixCopy(pDest, destLineWidth, actBlk, blockLineSize, blkHeight1, blkHeight);
                 doAdd: MatrixAdd(pDest, destLineWidth, pDest, actBlk, blkHeight1, blkHeight, destLineWidth, blockLineSize);
                 doSub: MatrixSub(pDest, destLineWidth, pDest, actBlk, blkHeight1, blkHeight, destLineWidth, blockLineSize);
               end;

               inc(pDest, blockSize);
               inc(PByte(pMt2), blockSize*LineWidth2);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     if Assigned(ptrMem) then
        FreeMem(ptrMem);
end;


// ########################################################
// #### Generic blocked routines
// ########################################################

procedure GenericBlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble;
  width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil);
var w, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkWidth : NativeInt;
    gammaWidth : NativeInt;
    sizeVal : NativeInt;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
     	exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     // estimate a good blocksize - one with a small number of zero columns (but as near as possible to cCacheMtxSize
     if blockSize <= 0 then
     begin
          sizeVal := Max(width1, width2);

          while sizeVal > cCacheMtxSize do
                sizeVal := sizeVal shr 1;

          blockSize := Min(Max(width1, width2), Max(64, sizeVal));
     end;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + NativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     if Assigned(mem)
     then
         actBlk := mem
     else
         GetMem(actBlk, BlockMultMemSize(blockSize));

     multBlk := actBlk;
     inc(multBlk, blockSize*blockSize);

     blkHeight := blockSize;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkWidth := blockSize;

          for blkIdxX := 0 to w do
          begin
               if (blkIdxX = w) and not w2FitCacheSize then
                  blkWidth := (width2 mod blockSize);

               FillChar(actBlk^, blockSize*blockSize*sizeof(double), 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    GenericMtxMult(multBlk, blockSize*sizeof(double), pa, pb, gammaWidth, blkHeight, blkWidth, gammaWidth, LineWidth1, LineWidth2);
                    GenericMtxAdd(actBlk, blockSize*sizeof(double), actBlk, multBlk, blkWidth, blkHeight, blockSize*sizeof(double), blockSize*sizeof(double));

                    inc(pa, gammaWidth);
                    inc(PByte(pb), gammaWidth*LineWidth2);
               end;

               // build the result: dest = Dest +- A*B  (according to the operator)
               case op of
                 doNone: GenericMtxCopy(pDest, destLineWidth, ActBlk, blockSize*sizeof(double), blkWidth, blkHeight);
                 doAdd:  GenericMtxAdd(pDest, destLineWidth, ActBlk, pDest, blkWidth, blkHeight, blockSize*sizeof(double), destLineWidth);
                 doSub:  GenericMtxSub(pDest, destLineWidth, pDest, ActBlk, blkWidth, blkHeight, destLineWidth, blockSize*sizeof(double));
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     if not Assigned(mem) then
        FreeMem(actBlk);
end;

// calculates mt1'*mt2
procedure GenericBlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var w, w1 : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk1 : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkWidth1 : NativeInt;
    blkWidth : NativeInt;
    gammaHeight : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
         exit;
     assert((height1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     w1 := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;
     w := width2 div blockSize + NativeInt(not w2FitCacheSize) - 1;
     gamma := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     if not Assigned(mem) then
        GetMem(actBlk, BlockMultMemSize(blockSize));
     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);
     transBlk1 := PDouble(NativeUint(actBlk) + 2*blockByteSize);

     blockLineSize := blockSize*sizeof(double);

     blkWidth1 := blockSize;

     for blkIdxY := 0 to w1 do
     begin
          if (blkIdxY = w1) and not w1FitCacheSize then
             blkWidth1 := (width1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkWidth := blockSize;

          for blkIdxX := 0 to w do
          begin
               if (blkIdxX = w) and not w2FitCacheSize then
                  blkWidth := (width2 mod blockSize);

               FillChar(actBlk^, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaHeight := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not h1FitCacheSize then
                       gammaHeight := height1 mod blockSize;

                    GenericMtxTranspose(transBlk1, blockLineSize, pa, LineWidth1, blkWidth1, gammaHeight);
                    GenericMtxMult(multBlk, blockLineSize, transBlk1, pb, gammaHeight, blkWidth1, blkWidth, gammaHeight, blockLineSize, LineWidth2);
                    GenericMtxAdd(actBlk, blockLineSize, actBlk, multBlk, blkWidth, blkWidth1, blockLineSize, blockLineSize);

                    inc(PByte(pa), gammaHeight*LineWidth1);
                    inc(PByte(pb), gammaHeight*LineWidth2);
               end;

               // apply final operation such that we got the final result: Dest := Dest +- A*B ;
               case op of
                 doNone: GenericMtxCopy(pDest, destLineWidth, actBlk, blockLineSize, blkWidth, blkWidth1);
                 doAdd: GenericMtxAdd(pDest, destLineWidth, pDest, actBlk, blkWidth, blkWidth1, destLineWidth, blockLineSize);
                 doSub: GenericMtxSub(pDest, destLineWidth, pDest, actBlk, blkWidth, blkWidth1, destLineWidth, blockLineSize);
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(mt1, blkWidth1);
     end;

     if not Assigned(mem) then
        FreeMem(actBlk);
end;

// calculates mt1*mt2'
procedure GenericBlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var h1, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    h2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkHeight1 : NativeInt;
    gammaWidth : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = width2), 'Dimension error');
     assert((destLineWidth - height2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     h1FitCacheSize := (height1 mod blockSize) = 0;
     h2FitCacheSize := (height2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     h1 := height2 div blockSize + NativeInt(not h2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     if not Assigned(mem) then
        GetMem(actBlk, BlockMultMemSize(blockSize));
     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);

     blockLineSize := blockSize*sizeof(double);

     blkHeight := blockSize;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkHeight1 := blockSize;

          for blkIdxX := 0 to h1 do
          begin
               if (blkIdxX = h1) and not h2FitCacheSize then
                  blkHeight1 := (height2 mod blockSize);

               FillChar(actBlk^, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    GenericMtxMultTransp(multBlk, blockLineSize, pa, pb, gammaWidth, blkHeight, gammaWidth, blkHeight1, LineWidth1, LineWidth2);
                    GenericMtxAdd(actBlk, blockLineSize, actBlk, multBlk, blkHeight1, blkHeight, blockLineSize, blockLineSize);

                    inc(pa, gammaWidth);
                    inc(pb, gammaWidth);
               end;

               // apply final operation such that we got the final result: Dest := Dest +- A*B ;
               case op of
                 doNone: GenericMtxCopy(pDest, destLineWidth, actBlk, blockLineSize, blkHeight1, blkHeight);
                 doAdd: GenericMtxAdd(pDest, destLineWidth, pDest, actBlk, blkHeight1, blkHeight, destLineWidth, blockLineSize);
                 doSub: GenericMtxSub(pDest, destLineWidth, pDest, actBlk, blkHeight1, blkHeight, destLineWidth, blockLineSize);
               end;

               inc(pDest, blockSize);
               inc(PByte(pMt2), blockSize*LineWidth2);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     if not Assigned(mem) then
        FreeMem(actBlk);
end;


// #######################################################
// #### Blocked threaded matrix multiplication
// #######################################################

type
  TFlagRec = record
     finished : LongBool;    // flags for the threading stuff
     terminated : LongBool;
  end;
  PFlagRec = ^TFlagRec;
  TFlagRecArr = Array[0..cMaxNumCores - 1] of PFlagRec;

procedure SpinWait( const waitObjs : TFlagRecArr; numElem : integer);  
var i : integer;
    cnt : integer;
    spinWaitCnt : integer;
begin
     MemoryBarrier;
     spinWaitCnt := 4000;
     if numCPUCores > numRealCores then
        spinWaitCnt := 100;
     for i := 0 to numElem - 1 do
     begin
          while (waitObjs[i]^.finished = False) do
          begin
               cnt := 0;
               while (cnt < spinWaitCnt) and (waitObjs[i]^.finished = False) do
                     inc(cnt);

               if waitObjs[i]^.finished = false then
                  YieldProcessor;
          end;
     end;
end;

type
  TAsyncMultBlkRec = record
  public
    thrIdx : integer;
    numThr : Integer;

    op : TMatrixMultDestOperation;

    actBlk : PDouble;
    pa, pb : PDouble;

    multBlk : PDouble;
    copyBlk : PDouble;
    transBlk : PDouble;
    pDest : PDouble;
    destLineWidth : NativeInt;

    gammaWidth : integer;
    blkHeight : integer;
    blkHeight1 : integer;
    blkWidth : integer;

    blockLineSize : NativeInt;
    LineWidth1, LineWidth2 : NativeInt;

    isAligned : boolean;
    stage : integer;

    flags : TFlagRec;

    procedure ApplyOp;
    procedure ApplyOpT;
    procedure MatrixBlkMultT2Func;
    procedure MatrixBlkMultFunc;
    procedure MatrixBlkTranspose;

    procedure Create(aActBlk : PDouble; aPa, aPb : PDouble;
                     aMultBlk, aCopyBlk : PDouble;
                     aGammaWidth : integer; aBlkHeight, aBlkHeight1 : integer;
                     aBlockLineSize : NativeInt;
                     aLineWidth1, aLineWidth2 : NativeInt;
                     aDest : PDouble;
                     aDestLineWidth : NativeInt;
                     aIsAligned : boolean;
                     aOp : TMatrixMultDestOperation);
  end;
  PAsyncMultBlkRec = ^TAsyncMultBlkRec;


{ TAsyncMultBlkRec }

procedure TAsyncMultBlkRec.Create(aActBlk : PDouble; aPa, aPb : PDouble;
                     aMultBlk, aCopyBlk : PDouble;
                     aGammaWidth : integer; aBlkHeight, aBlkHeight1 : integer;
                     aBlockLineSize : NativeInt;
                     aLineWidth1, aLineWidth2 : NativeInt;
                     aDest : PDouble;
                     aDestLineWidth : NativeInt;
                     aIsAligned : boolean;
                     aOp : TMatrixMultDestOperation);
begin
     actBlk := aActBlk;
     pA := aPA;
     pB := aPb;
     multBlk := aMultBlk;
     GammaWidth := aGammaWidth;
     copyBlk := aCopyBlk;
     blkHeight := aBlkHeight;
     blkHeight1 := aBlkHeight1;
     blkWidth := aBlkHeight1;
     BlockLineSize := aBlockLineSize;
     LineWidth1 := aLineWidth1;
     LineWidth2 := aLineWidth2;
     isAligned := aIsAligned;
     op := aOp;
     pDest := aDest;
     DestLineWidth := adestLineWidth;

     flags.finished := True;
     flags.Terminated := False;
end;

procedure TAsyncMultBlkRec.MatrixBlkMultT2Func;
var offset : integer;
    thrHeight : integer;
    thrCopyBlk, thrMultBlock, thrActBlk : PDouble;
    thrPA : PDouble;
begin
     thrHeight := Max(1, blkHeight div numThr);
     offset := thrIdx*thrHeight;
     if offset >= blkHeight then
     begin
          flags.finished := True;
          exit;
     end;

     if (thrIdx = numThr - 1) or (offset + thrHeight > blkHeight) then
        thrHeight := blkHeight - offset;

     thrCopyBlk := GenPtr(copyBlk, 0, offset, blockLineSize);
     thrMultBlock := GenPtr(multBlk, 0, offset, blockLineSize);
     thrActBlk := GenPtr(actBlk, 0, offset, blockLineSize);
     thrPA := GenPtr(pa, 0, offset, LineWidth1);

     // now do the multiplication part
     if (blkHeight1 > 3) and (blkHeight > 3) then
     begin
          // it is faster to copy the block rather then multply it unaligned!
          if (not isAligned) or ((LineWidth1 and $0000001F) <> 0) then
          begin
               MatrixCopy(thrCopyBlk, blockLineSize, thrPA, LineWidth1, gammaWidth, thrHeight);
               MatrixMultT2(thrMultBlock, blockLineSize, thrCopyBlk, pb, gammaWidth, thrHeight, gammaWidth, blkHeight1, blockLineSize, LineWidth2);
          end
          else
              MatrixMultT2(thrMultBlock, blockLineSize, thrPA, pb, gammaWidth, thrHeight, gammaWidth, blkHeight1, LineWidth1, LineWidth2);

          MatrixAdd(thrActBlk, blockLineSize, thrActBlk, thrMultBlock, blkHeight1, thrHeight, blockLineSize, blockLineSize);
     end
     else
     begin
          GenericMtxMultTransp(thrMultBlock, blockLineSize, thrPA, pb, gammaWidth, thrHeight, gammaWidth, blkHeight1, LineWidth1, LineWidth2);
          GenericMtxAdd(thractBlk, blockLineSize, thractBlk, thrMultBlock, blkHeight1, thrHeight, blockLineSize, blockLineSize);
     end;

     flags.finished := True;
end;

procedure TAsyncMultBlkRec.MatrixBlkMultFunc;
var offset : integer;
    thrHeight : integer;
    thrCopyBlk, thrMultBlk, thrActBlk : PDouble;
    thrPA : PDouble;
begin
     thrHeight := Max(1, blkHeight div numThr);
     offset := thrIdx*thrHeight;
     if offset >= blkHeight then
     begin
          flags.finished := True;
          exit;
     end;

     if (thrIdx = numThr - 1) or (offset + thrHeight > blkHeight) then
        thrHeight := blkHeight - offset;

     thrCopyBlk := GenPtr(copyBlk, 0, offset, blockLineSize);
     thrMultBlk := GenPtr(multBlk, 0, offset, blockLineSize);
     thrActBlk := GenPtr(actBlk, 0, offset, blockLineSize);
     thrPA := GenPtr(pa, 0, offset, LineWidth1);

     // it is faster to copy the block rather then multiply it unaligned!
     if (not isAligned) or ((LineWidth1 and $00000001F) <> 0) then
     begin
          MatrixCopy(thrCopyBlk, blockLineSize, thrPA, LineWidth1, gammaWidth, thrHeight);
          MatrixMultT2(thrMultBlk, blockLineSize, thrCopyBlk, transBlk, gammaWidth, thrHeight, gammaWidth, blkWidth, blockLineSize, blockLineSize);
     end
     else
         MatrixMultT2(thrMultBlk, blockLineSize, thrPA, transBlk, gammaWidth, thrHeight, gammaWidth, blkWidth, LineWidth1, blockLineSize);

     MatrixAdd(thrActBlk, blockLineSize, thrActBlk, thrMultBlk, blkWidth, thrHeight, blockLineSize, blockLineSize);

     flags.finished := True;
end;


procedure TAsyncMultBlkRec.ApplyOpT;
var thrDest : PDouble;
    offset : NativeInt;
    thrblkHeight : NativeInt;
    thrActBlk : PDouble;
begin
     thrblkHeight := Max(1, blkHeight div numThr);
     offset := thrIdx*thrblkHeight;

     if offset >= blkHeight then
     begin
          flags.finished := True;
          exit;
     end;

     if (thrIdx = numThr - 1) or (offset + thrblkHeight > blkHeight) then
        thrblkHeight := blkHeight - offset;

     thrDest := GenPtr(pDest, 0, offset, destLineWidth);
     thrActBlk := GenPtr(actBlk, 0, offset, blockLineSize);

     case op of
       doNone: MatrixCopy(thrDest, destLineWidth, thrActBlk, blockLineSize, blkHeight1, thrblkHeight);
       doAdd: MatrixAdd(thrDest, destLineWidth, thrDest, thrActBlk, blkHeight1, thrblkHeight, destLineWidth, blockLineSize);
       doSub: MatrixSub(thrDest, destLineWidth, thrDest, thrActBlk, blkHeight1, thrblkHeight, destLineWidth, blockLineSize);
     end;

     flags.finished := True;
end;

procedure TAsyncMultBlkRec.ApplyOp;
var thrDest : PDouble;
    offset : NativeInt;
    thrblkHeight : NativeInt;
    thrActBlk : PDouble;
begin
     thrblkHeight := Max(1, blkHeight div numThr);
     offset := thrIdx*thrblkHeight;

     if offset >= blkHeight then
     begin
          flags.finished := True;
          exit;
     end;

     if (thrIdx = numThr - 1) or (offset + thrblkHeight > blkHeight) then
        thrblkHeight := blkHeight - offset;

     thrDest := GenPtr(pDest, 0, offset, destLineWidth);
     thrActBlk := GenPtr(actBlk, 0, offset, blockLineSize);

     case op of
       doNone: MatrixCopy(thrDest, destLineWidth, thrActBlk, blockLineSize, blkWidth, thrblkHeight);
       doAdd: MatrixAdd(thrDest, destLineWidth, thrDest, thrActBlk, blkWidth, thrblkHeight, destLineWidth, blockLineSize);
       doSub: MatrixSub(thrDest, destLineWidth, thrDest, thrActBlk, blkWidth, thrblkHeight, destLineWidth, blockLineSize);
     end;

     flags.finished := True;
end;


procedure MatrixBlkMultT2FuncThr(obj : Pointer);
var pRec : PAsyncMultBlkRec;
begin
     pRec := PAsyncMultBlkRec(obj);

     while not pRec^.flags.Terminated do
     begin
          while pRec^.flags.finished do
                if pRec^.flags.Terminated then
                   exit;

          if pRec^.stage = 0
          then
              pRec^.MatrixBlkMultT2Func
          else
              pRec^.ApplyOpT;
     end;
end;

procedure MatrixBlkMultFuncThr(obj : Pointer);
var pRec : PAsyncMultBlkRec;
begin
     pRec := PAsyncMultBlkRec(obj);

     while not pRec^.flags.Terminated do
     begin
          while pRec^.flags.finished do
                if pRec^.flags.Terminated then
                   exit;

          if pRec^.stage = 0
          then
              pRec^.MatrixBlkMultFunc
          else if pRec^.stage = 1
          then
              pRec^.ApplyOp
          else
              pRec^.MatrixBlkTranspose;
     end;
end;

// empirical:
function UseInnerBlockMult( w, h : NativeInt) : boolean;
begin
     // not always in hyperthreading cpu's
     Result := ( (w < 1024) and (h < 1024) and (w > BlockedMatrixMultSize) and (h > BlockedMatrixMultSize) );
end;

procedure ThrBlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt;
  const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var h1, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    copyBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    h2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkHeight1 : NativeInt;
    gammaWidth : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
    isAligned : boolean;
    ptrMem : Pointer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMultBlkRec;
    usedCores : integer;
    i: Integer;
    calls : IMtxAsyncCallGroup;
    waitObjs : TFlagRecArr;
begin
     usedCores := NumCoresToUseForMult(width1, height1, height2, width2, blockSize);

     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = width2), 'Dimension error');
     assert((destLineWidth - height2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     if (blockSize > width1) and (not Assigned(mem)) then
        blockSize := Next2Pwr( width1, blockSize );
     
     isAligned := (NativeUint(dest) and $0000000F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     h2FitCacheSize := (height2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     h1 := height2 div blockSize + NativeInt(not h2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);
     copyBlk := PDouble(NativeUint(actBlk) + 2*blockByteSize);

     blkHeight := blockSize;
     blockLineSize := blockSize*sizeof(double);
     blkHeight1 := blockSize;
     gammaWidth := blockSize;
     pA := nil;
     pB := nil;

     for i := 0 to usedCores - 1 do
     begin
          objs[i].Create(actBlk, pA, pB, multBlk, copyBlk, gammaWidth,
                         blkHeight, blkHeight1, blockLineSize, LineWidth1, LineWidth2,
                         dest, destLineWidth,
                         isAligned, op);

          objs[i].numThr := usedCores;
          objs[i].thrIdx := i;
     end;

     // start the threads
     calls := MtxInitTaskGroup;

     // start threads
     for i := 1 to usedCores - 1 do
     begin
          calls.AddTaskRec(@MatrixBlkMultT2FuncThr, @objs[i]);
          waitObjs[i - 1] := @objs[i].flags;
     end;

     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          for i := 0 to usedCores - 1 do
              objs[i].blkHeight := blkHeight;

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkHeight1 := blockSize;

          for blkIdxX := 0 to h1 do
          begin
               if (blkIdxX = h1) and not h2FitCacheSize then
                  blkHeight1 := (height2 mod blockSize);

               for i := 0 to usedCores - 1 do
                   objs[i].blkHeight1 := blkHeight1;

               MtxMemInit(actBlk, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    for i := 0 to usedCores - 1 do
                    begin
                         objs[i].gammaWidth := gammaWidth;
                         objs[i].pa := pa;
                         objs[i].pb := pb;
                         objs[i].stage := 0;

                         objs[i].flags.finished := False; // set flag to indicate start of multiplication
                    end;

                    // use the current thread for multiplication too
                    objs[0].MatrixBlkMultT2Func;

                    // spin wait for all other treads
                    SpinWait(waitObjs, usedCores - 1);

                    inc(pa, gammaWidth);
                    inc(pb, gammaWidth);
               end;

               for i := 0 to usedCores - 1 do
               begin
                    objs[i].stage := 1;
                    objs[i].pDest := pDest;

                    objs[i].flags.finished := False; // set flag to indicate start end operation
               end;

               objs[0].ApplyOpT;

               // spin wait for all other treads
               SpinWait(waitObjs, usedCores - 1);

               inc(pDest, blockSize);
               inc(PByte(pMt2), blockSize*LineWidth2);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     // ####################################################
     // #### Terminate the threads
     for i := 0 to usedCores - 1 do
         objs[i].flags.Terminated := True;

     if usedCores > 1 then
        calls.SyncAll;

     if Assigned(ptrMem) then
        FreeMem(ptrMem);
end;

procedure ThrBlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; op : TMatrixMultDestOperation = doNone); overload;
begin
     ThrBlockMatrixMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2,
                                  LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil );
end;

procedure ThrBlockMatrixMultiplication(dest : PDouble; const destLineWidth : NativeInt; mt1, mt2 : PDouble; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt; blockSize : NativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil); overload;
var w, h : NativeInt;
    blkIdxX : NativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk, copyBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : NativeInt;
    idx : NativeInt;
    gamma : NativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : NativeInt;
    blkWidth : NativeInt;
    gammaWidth : NativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
    isAligned : boolean;
    ptrMem : Pointer;
    objs : Array[0..cMaxNumCores - 1] of TAsyncMultBlkRec;
    usedCores : integer;
    i: Integer;
    calls : IMtxAsyncCallGroup;
    waitObj : TFlagRecArr;
begin
     usedCores := NumCoresToUseForMult(width1, height1, height2, width2, blockSize);

     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     if (blockSize > width1) and (not Assigned(mem)) then
        blockSize := Next2Pwr( width1, blockSize );
     
     isAligned := (NativeUint(dest) and $00000001F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + NativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + NativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + NativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(NativeUint(actBlk) + blockByteSize);
     transBlk := PDouble(NativeUint(actBlk) + 2*blockByteSize);
     copyBlk := PDouble(NativeUint(actBlk) + 3*blockByteSize);

     blockLineSize := blockSize*sizeof(double);

     blkHeight := blockSize;

     blockLineSize := blockSize*sizeof(double);
     gammaWidth := blockSize;
     pA := nil;
     pB := nil;

     blkWidth := blockSize;
     for i := 0 to usedCores - 1 do
     begin
          objs[i].Create(actBlk, pA, pB, multBlk, copyBlk, gammaWidth,
                         blkHeight, blkWidth, blockLineSize, LineWidth1, LineWidth2,
                         dest, destLineWidth,
                         isAligned, op);

          objs[i].transBlk := transBlk;
          objs[i].numThr := usedCores;
          objs[i].thrIdx := i;
     end;

     // start the threads
     calls := MtxInitTaskGroup;

     // start threads
     for i := 1 to usedCores - 1 do
     begin
          calls.AddTaskRec(@MatrixBlkMultFuncThr, @objs[i]);
          waitObj[i - 1] := @objs[i].flags;
     end;


     for blkIdxY := 0 to h do
     begin
          if (blkIdxY = h) and not h1FitCacheSize then
             blkHeight := (height1 mod blockSize);

          for i := 0 to usedCores - 1 do
          begin
               objs[i].blkHeight := blkHeight;
               objs[i].blkHeight1 := blkHeight;
          end;

          pDest := dest;
          inc(PByte(pDest), blkIdxY*blockSize*destLineWidth);
          pMt2 := mt2;
          blkWidth := blockSize;

          for blkIdxX := 0 to w do
          begin
               if (blkIdxX = w) and not w2FitCacheSize then
                  blkWidth := (width2 mod blockSize);

               for i := 0 to usedCores - 1 do
                   objs[i].blkWidth := blkWidth;

               MtxMemInit(actBlk, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    for i := 0 to usedCores - 1 do
                    begin
                         objs[i].gammaWidth := gammaWidth;
                         objs[i].pa := pa;
                         objs[i].pb := pb;

                         objs[i].stage := 2;
                         objs[i].flags.finished := False;
                    end;

                    objs[0].MatrixBlkTranspose;

                    // spin wait for all other treads
                    SpinWait(waitObj, usedCores - 1);

                    for i := 0 to usedCores - 1 do
                    begin
                         objs[i].gammaWidth := gammaWidth;
                         objs[i].pa := pa;
                         objs[i].pb := pb;

                         objs[i].stage := 0;
                         objs[i].flags.finished := False;
                    end;

                    objs[0].MatrixBlkMultFunc;

                    // spin wait for all other treads
                    SpinWait(waitObj, usedCores - 1);

                    inc(pa, gammaWidth);
                    inc(PByte(pb), gammaWidth*LineWidth2);
               end;

               for i := 0 to usedCores - 1 do
               begin
                    objs[i].stage := 1;
                    objs[i].pDest := pDest;
                    objs[i].blkHeight1 := objs[i].blkWidth;

                    objs[i].flags.finished := False; // set flag to indicate start end operation
               end;

               objs[0].ApplyOp;

               // spin wait for all other treads
               SpinWait(waitObj, usedCores - 1);

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     // #######################################################
     // #### Terminate the threads
     for i := 0 to usedCores - 1 do
         objs[i].flags.Terminated := true;

     if usedCores > 1 then
        calls.SyncAll;

     if Assigned(ptrMem) then
        FreeMem(ptrMem);
end;

procedure TAsyncMultBlkRec.MatrixBlkTranspose;
var offset : integer;
    thrWidth : integer;
    thrTransBlk  : PDouble;
    thrPB : PDouble;
begin
     thrWidth := Max(1, blkWidth div numThr);
     offset := thrIdx*thrWidth;
     if offset >= blkWidth then
     begin
          flags.finished := True;
          exit;
     end;

     if (thrIdx = numThr - 1) or  (offset + thrWidth > blkWidth) then
        thrWidth := blkWidth - offset;

     thrTransBlk := GenPtr(transBlk, 0, offset, blockLineSize);
     thrPB := GenPtr(pB, offset, 0, LineWidth2);

     if (thrWidth > 3) and (gammaWidth > 3)
     then
         MatrixTranspose(thrTransBlk, blockLineSize, thrPB, LineWidth2, thrWidth, gammaWidth)
     else
         GenericMtxTranspose(thrTransBlk, blockLineSize, thrPB, LineWidth2, thrWidth, gammaWidth);

     flags.finished := True;
end;

end.

