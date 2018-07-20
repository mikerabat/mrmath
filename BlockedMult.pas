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

uses MatrixConst;

// ###################################################################
// #### Generic blocked multiplication routines

// blocked matrix mult + strassen multiplication functions

procedure GenericBlockMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
  width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil);

// calculates dest = mt1'*mt2
procedure GenericBlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
// calculates dest = mt1*mt2'
procedure GenericBlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);


// this routine first performs a transposition on the second matrix before the multiplication is executed. This results
// normaly in quite a boost.

procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil); overload;

// calculates dest = mt1'*mt2
procedure BlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
// calculates dest = mt1*mt2'
procedure BlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);


// this is a blockwise matrix multiplication routine which takes a limited cache into account.
// The routine tries to tile the matrix into 256x256 blocks (which seems to be a good approximation
// for a Core2 processor) which fits into the Level1 cache thus reduces
// cache misses.
procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone); overload;

procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt); overload;
procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt; blockSize : TASMNativeInt); overload;


implementation

uses Math, BlockSizeSetup, SimpleMatrixOperations, OptimizedFuncs;

procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt); overload;
begin
     BlockMatrixVectorMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1, BlockedVectorMatrixMultSize);
end;

procedure BlockMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt; blockSize : TASMNativeInt); overload;
var h : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pActBlk : PDouble;
    pHelp : PDouble;
    w1FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : TASMNativeInt;
    gammaWidth : TASMNativeInt;
    ptrMem : Pointer;
begin
     if (width1 = 0) or (height1 = 0) or (height2 = 0) then
        exit;

     assert((width1 = height2), 'Dimension error');
     assert((LineWidth1 >= width1*sizeof(double)), 'Line widths do not match');
     assert(BlockSize > 1, 'Error block size must be at least 2');

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

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

procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation);
begin
     BlockMatrixMultiplicationDirect(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op);
end;

procedure BlockMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation);
var w, h : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : TASMNativeInt;
    blkWidth : TASMNativeInt;
    gammaWidth : TASMNativeInt;
    sizeVal : TASMNativeInt;
    m1, m2 : Pointer;
    blockByteSize : TASMNativeInt;
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

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + TASMNativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

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

procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation);
begin
     BlockMatrixMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil);
end;

procedure BlockMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var w, h : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk, copyBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : TASMNativeInt;
    blkWidth : TASMNativeInt;
    gammaWidth : TASMNativeInt;
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

     isAligned := (TASMNativeUInt(dest) and $00000001F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + TASMNativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(TASMNativeUInt(actBlk) + blockByteSize);
     transBlk := PDouble(TASMNativeUInt(actBlk) + 2*blockByteSize);
     copyBlk := PDouble(TASMNativeUInt(actBlk) + 3*blockByteSize);

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
procedure BlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var w, w1 : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk1, transBlk2 : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkWidth1 : TASMNativeInt;
    blkWidth : TASMNativeInt;
    gammaHeight : TASMNativeInt;
    blockByteSize : Cardinal;
    blockLineSize : Cardinal;
    ptrMem : Pointer;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
        exit;
     assert((height1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     w1 := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;
     w := width2 div blockSize + TASMNativeInt(not w2FitCacheSize) - 1;
     gamma := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(TASMNativeUInt(actBlk) + blockByteSize);
     transBlk1 := PDouble(TASMNativeUInt(actBlk) + 2*blockByteSize);
     transBlk2 := PDouble(TASMNativeUInt(actBlk) + 3*blockByteSize);

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
procedure BlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var h1, h : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    copyBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    h2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : TASMNativeInt;
    blkHeight1 : TASMNativeInt;
    gammaWidth : TASMNativeInt;
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

     isAligned := (TASMNativeUInt(dest) and $0000000F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     h2FitCacheSize := (height2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     h1 := height2 div blockSize + TASMNativeInt(not h2FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     ptrMem := nil;
     if not Assigned(mem) then
        actBlk := MtxMallocAlign(BlockMultMemSize(blockSize), ptrMem );
     multBlk := PDouble(TASMNativeUInt(actBlk) + blockByteSize);
     copyBlk := PDouble(TASMNativeUInt(actBlk) + 2*blockByteSize);

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

procedure GenericBlockMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
  width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil);
var w, h : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : TASMNativeInt;
    blkWidth : TASMNativeInt;
    gammaWidth : TASMNativeInt;
    sizeVal : TASMNativeInt;
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

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + TASMNativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

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
procedure GenericBlockMatrixMultiplicationT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var w, w1 : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    transBlk1 : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    w2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkWidth1 : TASMNativeInt;
    blkWidth : TASMNativeInt;
    gammaHeight : TASMNativeInt;
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

     w1 := width1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + TASMNativeInt(not w2FitCacheSize) - 1;
     gamma := height1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     if not Assigned(mem) then
        GetMem(actBlk, BlockMultMemSize(blockSize));
     multBlk := PDouble(TASMNativeUInt(actBlk) + blockByteSize);
     transBlk1 := PDouble(TASMNativeUInt(actBlk) + 2*blockByteSize);

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
procedure GenericBlockMatrixMultiplicationT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
var h1, h : TASMNativeInt;
    blkIdxX : TASMNativeInt;
    actBlk : PDouble;
    multBlk : PDouble;
    pA, pB : PDouble;
    blkIdxY : TASMNativeInt;
    idx : TASMNativeInt;
    gamma : TASMNativeInt;
    pDest : PDouble;
    pMt2 : PDouble;
    w1FitCacheSize : boolean;
    h2FitCacheSize : boolean;
    h1FitCacheSize : boolean;
    blkHeight : TASMNativeInt;
    blkHeight1 : TASMNativeInt;
    gammaWidth : TASMNativeInt;
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

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     h1 := height2 div blockSize + TASMNativeInt(not h2FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     if not Assigned(mem) then
        GetMem(actBlk, BlockMultMemSize(blockSize));
     multBlk := PDouble(TASMNativeUInt(actBlk) + blockByteSize);

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

end.

