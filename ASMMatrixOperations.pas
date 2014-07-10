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


unit ASMMatrixOperations;

// #################################################
// #### Assembler versions of some of the matrix operations
// #################################################

interface

uses MatrixConst, Types;

procedure ASMMatrixCopy(Dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure ASMRowSwap(A, B : PDouble; width : TASMNativeInt);

function ASMMatrixMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure ASMMatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;
function ASMMatrixMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
// note: The asm routines always carry out 2x2 matrix multiplications thus there must be an additional zero line/column in the
// input matrices if the width/height is uneven. The routine also performs better if the matrices are aligned to 16 byte boundaries!
procedure ASMMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
procedure ASMMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; mem : PDouble); overload;
procedure ASMMatrixMultTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
procedure ASMMatrixMultDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;

// note: the matrix add routine tries to add two values at once and does not carry out any range checks thus the line widhts must
// be multiple of 16.
procedure ASMMatrixAdd(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixSub(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixElemDiv(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
procedure ASMMatrixAddAndScale(Dest : PDouble;  LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
procedure ASMMatrixSQRT(Dest : PDouble; LineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
procedure ASMMatrixAbs(Dest : PDouble; LineWidth : TASMNativeInt; Width, Height : TASMNativeInt);

function ASMMatrixMax(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function ASMMatrixMin(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

procedure ASMMatrixTranspose(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
function ASMMatrixElementwiseNorm2(dest : PDouble; LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
procedure ASMMatrixNormalize(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
procedure ASMMatrixMean(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
procedure ASMMatrixVar(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean);
procedure ASMMatrixSum(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);

// this routine first performs a transposition on the second matrix before the multiplication is executed. This results
// normaly in quite a boost.

procedure BlockedMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure BlockedMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil); overload;

// strassen algorithm for matrix multiplication
procedure ASMStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);

// this is a blockwise matrix multiplication routine which takes a limited cache into account.
// The routine tries to tile the matrix into 256x256 blocks (which seems to be a good approximation
// for a Core2 processor) which fits into the Level1 cache thus reduces
// cache misses.
procedure BlockedMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation = doNone); overload;
procedure BlockedMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone); overload;

procedure BlockedMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt); overload;
procedure BlockedMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt; blockSize : TASMNativeInt); overload;

implementation

{$IFDEF CPUX64}
{$DEFINE x64}
{$ENDIF}
{$IFDEF cpux86_64}
{$DEFINE x64}
{$ENDIF}

uses Math, BlockSizeSetup, SimpleMatrixOperations,
     {$IFDEF x64}
     ASMMatrixMultOperationsx64, ASMMatrixVectorMultOperationsx64, ASMMatrixAbsOperationsx64,
     ASMMatrixMultTransposedOperationsx64, ASMMatrixAddSubOperationsx64,
     ASMMatrixElementwiseMultOperationsx64, ASMMatrixScaleOperationsx64, ASMMatrixSQRTOperationsx64,
     ASMMoveOperationsx64, ASMMatrixMinMaxOperationsx64, ASMMatrixTransposeOperationsx64,
     ASMMatrixNormOperationsx64, ASMMatrixMeanOperationsx64, ASMMatrixSumOperationsx64
     {$ELSE}
     ASMMatrixMultOperations, ASMMatrixVectorMultOperations, ASMMatrixAbsOperations,
     ASMMatrixMultTransposedOperations, ASMMatrixAddSubOperations,
     ASMMatrixElementwiseMultOperations, ASMMatrixScaleOperations, ASMMatrixSQRTOperations,
     ASMMoveOperations, ASMMatrixMinMaxOperations, ASMMatrixTransposeOperations,
     ASMMatrixNormOperations, ASMMatrixMeanOperations, ASMMatrixSumOperations
     {$ENDIF}
     ;


function ASMMatrixMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
begin
					if (width1 = 0) or (height1 = 0) or (width2 = 0) or (height2 = 0) then
        exit;

     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');

     SetLength(Result, Height1*width2);
     ASMMatrixMult(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure ASMMatrixMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;
begin
     if (width1 = 0) or (height1 = 0) or (width2 = 0) or (height2 = 0) then
        exit;

     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');

     ASMMatrixMult(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

function ASMMatrixMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
begin
     if (width1 = 0) or (height1 = 0) or (width2 = 0) or (height2 = 0) then
        exit;

     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     Result := ASMMatrixMult(@mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure BlockedMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt); overload;
begin
     BlockedMatrixVectorMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1, BlockedVectorMatrixMultSize);
end;

procedure BlockedMatrixVectorMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1 : TASMNativeInt; blockSize : TASMNativeInt); overload;
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

     GetMem(actBlk, 2*blockSize*sizeof(double));
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
                   ASMMatrixMult(multBlk, sizeof(double), pa, pb, gammaWidth, blkHeight, 1, gammaWidth, LineWidth1, sizeof(double))
               else
                   GenericMtxMult(multBlk, sizeof(double), pa, pb, gammaWidth, blkHeight, 1, gammaWidth, LineWidth1, sizeof(double));

               // treat the addition as vector add:
               if blkHeight > 1
               then
                   ASMMatrixAdd(actBlk, blkHeight*sizeof(double), actBlk, multBlk, blkHeight, 1, blkHeight*sizeof(double), blkheight*sizeof(double))
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

     FreeMem(actBlk);
end;

procedure ASMMatrixAddAndScale(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (width = 1) and (LineWidth = sizeof(double)) then
     begin
          width := height;
          height := 1;
          LineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
     end;

     if (TASMNativeUInt(dest) and $0000000F = 0) and (LineWidth and $0000000F = 0) then
     begin
          if (width and 1) = 0
          then
              ASMMatrixAddScaleAlignedEvenW(dest, LineWidth, width, height, dOffset, Scale)
          else
              ASMMatrixAddScaleAlignedOddW(dest, LineWidth, width, height, dOffset, Scale);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixAddScaleUnAlignedEvenW(dest, LineWidth, width, height, dOffset, Scale)
          else
              ASMMatrixAddScaleUnAlignedOddW(dest, LineWidth, width, height, dOffset, Scale);
     end;
end;

procedure ASMMatrixScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const dOffset, Scale : double);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (width = 1) and (LineWidth = sizeof(double)) then
     begin
          width := height;
          height := 1;
          LineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
     end;

     if (TASMNativeUInt(dest) and $0000000F = 0) and (LineWidth and $0000000F = 0) then
     begin
          if (width and 1) = 0
          then
              ASMMatrixScaleAddAlignedEvenW(dest, LineWidth, width, height, dOffset, Scale)
          else
              ASMMatrixScaleAddAlignedOddW(dest, LineWidth, width, height, dOffset, Scale);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixScaleAddUnAlignedEvenW(dest, LineWidth, width, height, dOffset, Scale)
          else
              ASMMatrixScaleAddUnAlignedOddW(dest, LineWidth, width, height, dOffset, Scale);
     end;
end;

procedure ASMMatrixSQRT(Dest : PDouble; LineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (width = 1) and (LineWidth = sizeof(double)) then
     begin
          width := height;
          height := 1;
          LineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
     end;

     if (TASMNativeUInt(dest) and $0000000F = 0) and (LineWidth and $0000000F = 0) then
     begin
          if (width and 1) = 0
          then
              ASMMatrixSQRTAlignedEvenW(dest, LineWidth, width, height)
          else
              ASMMatrixSQRTAlignedOddW(dest, LineWidth, width, height);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixSQRTUnAlignedEvenW(dest, LineWidth, width, height)
          else
              ASMMatrixSQRTUnAlignedOddW(dest, LineWidth, width, height);
     end;
end;

procedure ASMMatrixAbs(Dest : PDouble; LineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (width = 1) and (LineWidth = sizeof(double)) then
     begin
          width := height;
          height := 1;
          LineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
     end;

     if (TASMNativeUInt(dest) and $0000000F = 0) and (LineWidth and $0000000F = 0) then
     begin
          if (width and 1) = 0
          then
              ASMMatrixAbsAlignedEvenW(dest, LineWidth, width, height)
          else
              ASMMatrixAbsAlignedOddW(dest, LineWidth, width, height);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixAbsUnAlignedEvenW(dest, LineWidth, width, height)
          else
              ASMMatrixAbsUnAlignedOddW(dest, LineWidth, width, height);
     end;
end;

procedure ASMMatrixElemMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (width = 1) and (LineWidth1 = sizeof(double)) and (LineWidth2 = sizeof(double)) and (destLineWidth = sizeof(double)) then
     begin
          width := Height;
          height := 1;
          LineWidth1 := width*sizeof(double) + (width and 1)*sizeof(double);
          LineWidth2 := LineWidth1;
          destLineWidth := LineWidth1;
     end;

     if (TASMNativeUInt(mt1) and $0000000F = 0) and (TASMNativeUInt(mt2) and $0000000F = 0) and (TASMNativeUInt(dest) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (LineWidth1 and $0000000F = 0) and (LineWidth2 and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              ASMMatrixElemMultAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixElemMultAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixElemMultUnAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixElemMultUnAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end;
end;

procedure ASMMatrixElemDiv(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (width = 1) and (LineWidth1 = sizeof(double)) and (LineWidth2 = sizeof(double)) and (destLineWidth = sizeof(double)) then
     begin
          width := Height;
          height := 1;
          LineWidth1 := width*sizeof(double) + (width and 1)*sizeof(double);
          LineWidth2 := LineWidth1;
          destLineWidth := LineWidth1;
     end;

     if (TASMNativeUInt(mt1) and $0000000F = 0) and (TASMNativeUInt(mt2) and $0000000F = 0) and (TASMNativeUInt(dest) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (LineWidth1 and $0000000F = 0) and (LineWidth2 and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              ASMMatrixElemDivAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixElemDivAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixElemDivUnAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixElemDivUnAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end;
end;

function ASMMatrixElementwiseNorm2(dest : PDouble; LineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
begin
     Result := 0;

     if (width = 0) or (height = 0) then
        exit;

     // check if they are vector operations:
     if (width = 1) and (LineWidth = sizeof(double)) then
     begin
          width := Height;
          height := 1;
          LineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
     end;

     if (TASMNativeUInt(dest) and $0000000F = 0) and (LineWidth and $0000000F = 0) then
     begin
          if (width and 1) = 0
          then
              Result := ASMMatrixElementwiseNorm2AlignedEvenW(dest, LineWidth, width, height)
          else
              Result := ASMMatrixElementwiseNorm2AlignedOddW(dest, LineWidth, width, height);
     end
     else
     begin
          if (width and 1) = 0
          then
              Result := ASMMatrixElementwiseNorm2UnAlignedEvenW(dest, LineWidth, width, height)
          else
              Result := ASMMatrixElementwiseNorm2UnAlignedOddW(dest, LineWidth, width, height);
     end;
end;


procedure ASMMatrixAdd(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (TASMNativeUInt(mt1) and $0000000F = 0) and (TASMNativeUInt(mt2) and $0000000F = 0) and (TASMNativeUInt(dest) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (LineWidth1 and $0000000F = 0) and (LineWidth2 and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              ASMMatrixAddAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixAddAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixAddUnAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixAddUnAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end;
end;

procedure ASMMatrixSub(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     if (TASMNativeUInt(mt1) and $0000000F = 0) and (TASMNativeUInt(mt2) and $0000000F = 0) and (TASMNativeUInt(dest) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (LineWidth1 and $0000000F = 0) and (LineWidth2 and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              ASMMatrixSubAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixSubAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixSubUnAlignedEvenW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2)
          else
              ASMMatrixSubUnAlignedOddW(dest, destLineWidth, mt1, mt2, width, height, LineWidth1, LineWidth2);
     end;
end;

function ASMMatrixMax(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
     Result := -MaxDouble;
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= LineWidth), 'Dimension error');

     if (TASMNativeUInt(mt) and $0000000F = 0) and (LineWidth and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              Result := ASMMatrixMaxAlignedEvenW(mt, width, height, LineWidth)
          else
              Result := ASMMatrixMaxAlignedOddW(mt, width, height, LineWidth);
     end
     else
     begin
          if (width and 1) = 0
          then
              Result := ASMMatrixMaxUnAlignedEvenW(mt, width, height, LineWidth)
          else
              Result := ASMMatrixMaxUnAlignedOddW(mt, width, height, LineWidth);
     end;
end;

function ASMMatrixMin(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
begin
     Result := MaxDouble;
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= LineWidth), 'Dimension error');

     if (TASMNativeUInt(mt) and $0000000F = 0) and (LineWidth and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              Result := ASMMatrixMinAlignedEvenW(mt, width, height, LineWidth)
          else
              Result := ASMMatrixMinAlignedOddW(mt, width, height, LineWidth);
     end
     else
     begin
          if (width and 1) = 0
          then
              Result := ASMMatrixMinUnAlignedEvenW(mt, width, height, LineWidth)
          else
              Result := ASMMatrixMinUnAlignedOddW(mt, width, height, LineWidth);
     end;
end;

procedure ASMMatrixTranspose(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= LineWidth), 'Dimension error');

     if (width > 1) and (height > 1) then
     begin
          if (TASMNativeUInt(Dest) and $0000000F = 0) and (TASMNativeUInt(mt) and $0000000F = 0) and
             (destLineWidth and $0000000F = 0) and (LineWidth and $0000000F = 0)
          then
          begin
               if width and 1 = 0 then
               begin
                    if height and 1 = 0
                    then
                        ASMMatrixTransposeAlignedEvenWEvenH(dest, destLineWidth, mt, LineWidth, width, height)
                    else
                        ASMMatrixTransposeAlignedEvenWOddH(dest, destLineWidth, mt, LineWidth, width, height);
               end
               else
               begin
                    if height and 1 = 0
                    then
                        ASMMatrixTransposeAlignedOddWEvenH(dest, destLineWidth, mt, LineWidth, width, height)
                    else
                        ASMMatrixTransposeAlignedOddWOddH(dest, destLineWidth, mt, LineWidth, width, height);
               end;
          end
          else
          begin
               if width and 1 = 0 then
               begin
                    if height and 1 = 0
                    then
                        ASMMatrixTransposeUnAlignedEvenWEvenH(dest, destLineWidth, mt, LineWidth, width, height)
                    else
                        ASMMatrixTransposeUnAlignedEvenWOddH(dest, destLineWidth, mt, LineWidth, width, height);
               end
               else
               begin
                    if height and 1 = 0
                    then
                        ASMMatrixTransposeUnAlignedOddWEvenH(dest, destLineWidth, mt, LineWidth, width, height)
                    else
                        ASMMatrixTransposeUnAlignedOddWOddH(dest, destLineWidth, mt, LineWidth, width, height);
               end;
          end;
     end
     else
         GenericMtxTranspose(dest, destLineWidth, mt, LineWidth, width, height);
end;

procedure ASMMatrixSum(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
begin
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= srcLineWidth), 'Dimension error');
     assert((rowWise and (destLineWidth >= sizeof(double))) or (not rowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');

     // check if they are vector operations:
     if (width = 1) and (srcLineWidth = sizeof(double)) and not RowWise then
     begin
          width := Height;
          height := 1;
          srcLineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
          destLineWidth := srcLineWidth;
          RowWise := True;
     end;

     if (TASMNativeUInt(Dest) and $0000000F = 0) and (TASMNativeUInt(src) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (srcLineWidth and $0000000F = 0)
     then
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixSumRowAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixSumColumnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixSumRowAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixSumColumnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height);
          end;
     end
     else
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixSumRowUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixSumColumnUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixSumRowUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixSumColumnUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height);
          end;
     end;
end;

procedure ASMMatrixMean(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
begin
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= srcLineWidth), 'Dimension error');
     assert((rowWise and (destLineWidth >= sizeof(double))) or (not rowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');

     // check if they are vector operations:
     if (width = 1) and (srcLineWidth = sizeof(double)) and not RowWise then
     begin
          width := Height;
          height := 1;
          srcLineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
          destLineWidth := srcLineWidth;
          RowWise := True;
     end;

     if (TASMNativeUInt(Dest) and $0000000F = 0) and (TASMNativeUInt(src) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (srcLineWidth and $0000000F = 0)
     then
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixMeanRowAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixMeanColumnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixMeanRowAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixMeanColumnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height);
          end;
     end
     else
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixMeanRowUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixMeanColumnUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixMeanRowUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixMeanColumnUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height);
          end;
     end;
end;

procedure ASMMatrixVar(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean);
begin
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= srcLineWidth), 'Dimension error');
     assert((rowWise and (destLineWidth >= sizeof(double))) or (not rowWise and (destLineWidth >= width*sizeof(double))), 'Dimension error');

     // check if they are vector operations:
     if (width = 1) and (srcLineWidth = sizeof(double)) and not RowWise then
     begin
          width := Height;
          height := 1;
          srcLineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
          destLineWidth := srcLineWidth;
          RowWise := True;
     end;

     if (TASMNativeUInt(Dest) and $0000000F = 0) and (TASMNativeUInt(src) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (srcLineWidth and $0000000F = 0)
     then
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixVarRowAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased)
               else
                   ASMMatrixVarColumnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixVarRowAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased)
               else
                   ASMMatrixVarColumnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased);
          end;
     end
     else
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixVarRowUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased)
               else
                   ASMMatrixVarColumnUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixVarRowUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased)
               else
                   ASMMatrixVarColumnUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height, unbiased);
          end;
     end;
end;

procedure ASMMatrixNormalize(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
begin
     if (width = 0) or (height = 0) then
        exit;
     assert((width*sizeof(double) <= srcLineWidth) and (width*sizeof(double) <= destLineWidth), 'Dimension error');

     // check if they are vector operations:
     if (width = 1) and (srcLineWidth = sizeof(double)) and not RowWise then
     begin
          width := Height;
          height := 1;
          srcLineWidth := width*sizeof(double) + (width and 1)*sizeof(double);
          destLineWidth := srcLineWidth;
          RowWise := True;
     end;

     if (TASMNativeUInt(Dest) and $0000000F = 0) and (TASMNativeUInt(src) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (srcLineWidth and $0000000F = 0)
     then
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixNormalizeRowAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixNormalizeColumnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixNormalizeRowAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixNormalizeColumnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height);
          end;
     end
     else
     begin
          if width and 1 = 0 then
          begin
               if RowWise
               then
                   ASMMatrixNormalizeRowUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixNormalizeColumnUnAlignedEvenW(dest, destLineWidth, Src, srcLineWidth, width, height)
          end
          else
          begin
               if RowWise
               then
                   ASMMatrixNormalizeRowUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height)
               else
                   ASMMatrixNormalizeColumnUnAlignedOddW(dest, destLineWidth, Src, srcLineWidth, width, height);
          end;
     end;
end;

procedure ASMMatrixCopy(Dest : PDouble; destLineWidth : TASMNativeInt; src : PDouble; srcLineWidth : TASMNativeInt; Width, Height : TASMNativeInt);
begin
     if (width = 0) or (height = 0) then
        exit;

     // check if they are vector operations:
     if (width = 1) and (srcLineWidth = sizeof(double)) and (destLineWidth = sizeof(double)) then
     begin
          width := Height;
          height := 1;
          srcLineWidth := (width + (width and 1))*sizeof(double);
          destLineWidth := srcLineWidth;
     end;

     if (TASMNativeUInt(Dest) and $0000000F = 0) and (TASMNativeUInt(src) and $0000000F = 0) and
        (destLineWidth and $0000000F = 0) and (srcLineWidth and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              ASMMatrixCopyAlignedEvenW(dest, destLineWidth, src, srcLineWidth, width, height)
          else
              ASMMatrixCopyAlignedOddW(dest, destLineWidth, src, srcLineWidth, width, height);
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMMatrixCopyUnAlignedEvenW(dest, destLineWidth, src, srcLineWidth, width, height)
          else
              ASMMatrixCopyUnAlignedOddW(dest, destLineWidth, src, srcLineWidth, width, height);
     end;
end;

procedure ASMRowSwap(A, B : PDouble; width : TASMNativeInt);
begin
     if (width = 0) then
        exit;

     if (TASMNativeUInt(A) and $0000000F = 0) and (TASMNativeUInt(B) and $0000000F = 0)
     then
     begin
          if (width and 1) = 0
          then
              ASMRowSwapAlignedEvenW(A, B, Width)
          else
              ASMRowSwapAlignedOddW(A, B, Width)
     end
     else
     begin
          if (width and 1) = 0
          then
              ASMRowSwapUnAlignedEvenW(A, B, Width)
          else
              ASMRowSwapUnAlignedOddW(A, B, Width)
     end;
end;

procedure BlockedMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation);
begin
     BlockedMatrixMultiplicationDirect(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op);
end;

procedure BlockedMatrixMultiplicationDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation);
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

     GetMem(actBlk, blockSize*blockSize*sizeof(double));
     GetMem(multBlk, blockSize*blockSize*sizeof(double));

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

                    if blkWidth > 1 then
                    begin
                         ASMMatrixMult(multBlk, blockSize*sizeof(double), pa, pb, gammaWidth, blkHeight, blkWidth, gammaWidth, LineWidth1, LineWidth2);
                         ASMMatrixAdd(actBlk, blockSize*sizeof(double), actBlk, multBlk, blkWidth, blkHeight, blockSize*sizeof(double), blockSize*sizeof(double));
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
                 doNone: ASMMatrixCopy(pDest, destLineWidth, actBlk, blockSize*sizeof(double), blkWidth, blkHeight);
                 doAdd: ASMMatrixAdd(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockSize*sizeof(double));
                 doSub: ASMMatrixSub(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockSize*sizeof(double));
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     FreeMem(actBlk);
     FreeMem(multBlk);
end;

procedure ASMMatrixMultTransposed(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
var mtx : TDoubleDynArray;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
     	  exit;
     assert((width1 = width2), 'Dimension error');
     assert((destLineWidth - height2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     if (width1 < 3) and (width2 < 3) then
     begin
          // undo transposition -> it's not good in this case:
          mtx := GenericMtxTranspose(mt2, LineWidth2, width2, height2);
          GenericMtxMult(dest, destLineWidth, mt1, @mtx[0], width1, height1, height2, width2, LineWidth1, height2*sizeof(double));
     end
     else if (width1 < 2) or (width2 < 2) then
     begin
          // matrix/vector multiplication
          if width2 = 1 then
          begin
               if LineWidth2 = sizeof(double) then
               begin
                    if ((TASMNativeUInt(dest) and $0000000F) = 0) and ((TASMNativeUInt(mt1) and $0000000F) = 0) and ((TASMNativeUInt(mt2) and $0000000F) = 0) and
                       ((LineWidth1 and $0000000F) = 0) then
                    begin
                         if (width1 and $00000001 = 0)
                         then
                             ASMMatrixVectorMultAlignedEvenW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                         else
                             ASMMatrixVectorMultAlignedOddW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                    end
                    else
                    begin
                         if (width1 and $00000001 = 0)
                         then
                             ASMMatrixVectorMultUnAlignedEvenW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                         else
                             ASMMatrixVectorMultUnAlignedOddW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                    end;
               end
               else
                   GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
          end
          else // todo: create special routines for that case too!
              GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else
     begin
          // ########################################################################
          // ####  In this case mt2 is already transposed -> direct multiplication

          // check for alignment:
          if ((TASMNativeUInt(dest) and $0000000F) = 0) and ((TASMNativeUInt(mt1) and $0000000F) = 0) and ((TASMNativeUInt(mt2) and $0000000F) = 0) and
             ((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0) then
          begin
               if (width1 and $00000001) = 0 then
               begin
                    if (height2 and $00000001) = 0 then
                    begin
                         if (width1 and $0000000F) = 0
                         then
                             ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                         else
                             ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
                    end
                    else
                        ASMMatrixMultAlignedEvenW1OddH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end
               else
               begin
                    if (height2 and $00000001) = 0
                    then
                        ASMMatrixMultAlignedOddW1EvenH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultAlignedOddW1OddH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end;
          end
          else
          begin
               if (width1 and $00000001) = 0 then
               begin
                    if (height2 and $00000001) = 0
                    then
                        ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end
               else
               begin
                    if (height2 and $00000001) = 0
                    then
                        ASMMatrixMultUnAlignedOddW1EvenH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end;
          end;
     end;
end;

procedure ASMMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; mem : PDouble); overload;
var mtx : PDouble;
    mtxLineWidth : TASMNativeInt;
    help : TASMNativeInt;
begin
     if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
     	  exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     if (width1 < 2) and (width2 < 2) then
     begin
          GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else if (width1 < 2) or (width2 < 2) then
     begin
          // matrix/vector multiplication
          if (width2 > 1) and (width1 >= 3) then
          begin
               if LineWidth2 = sizeof(double) then
               begin
                    if ((TASMNativeUInt(dest) and $0000000F) = 0) and ((TASMNativeUInt(mt1) and $0000000F) = 0) and ((TASMNativeUInt(mt2) and $0000000F) = 0) and
                       ((LineWidth1 and $0000000F) = 0) then
                    begin
                         if (width1 and $00000001 = 0)
                         then
                             ASMMatrixVectorMultAlignedEvenW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                         else
                             ASMMatrixVectorMultAlignedOddW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                    end
                    else
                    begin
                         if (width1 and $00000001 = 0)
                         then
                             ASMMatrixVectorMultUnAlignedEvenW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                         else
                             ASMMatrixVectorMultUnAlignedOddW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                    end;
               end
               else
                   GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
          end
          else // todo: create special routines for that case too!
              GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else
     begin
          // ########################################################################
          // ####  For all "bigger" matrices transpose first then multiply. It's always faster
          mtxLineWidth := (height2 + height2 and $00000001)*sizeof(double);
          if Assigned(mem)
          then
              mtx := mem
          else
              GetMem(mtx, width2*mtxLineWidth);
          assert(assigned(mtx), 'Error could not reserver transpose memory');
          ASMMatrixTranspose(mtx, mtxLineWidth, mt2, LineWidth2, width2, height2);
          help := width2;
          width2 := height2;
          height2 := help;

          // check for alignment:
          if ((TASMNativeUInt(dest) and $0000000F) = 0) and ((TASMNativeUInt(mt1) and $0000000F) = 0) and ((TASMNativeUInt(mtx) and $0000000F) = 0) and
             ((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((mtxLineWidth and $0000000F) = 0) then
          begin
               if (width1 and $00000001) = 0 then
               begin
                    if (height2 and $00000001) = 0 then
                    begin
                         if (width1 and $0000000F) = 0
                         then
                             ASMMatrixMultAlignedEvenW1EvenH2TransposedMod16(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
                         else
                             ASMMatrixMultAlignedEvenW1EvenH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth);
                    end
                    else
                        ASMMatrixMultAlignedEvenW1OddH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
               end
               else
               begin
                    if (height2 and $00000001) = 0
                    then
                        ASMMatrixMultAlignedOddW1EvenH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
                    else
                        ASMMatrixMultAlignedOddW1OddH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
               end;
          end
          else
          begin
               if (width1 and $00000001) = 0 then
               begin
                    if (height2 and $00000001) = 0
                    then
                        ASMMatrixMultUnAlignedEvenW1EvenH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
                    else
                        ASMMatrixMultUnAlignedEvenW1OddH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
               end
               else
               begin
                    if (height2 and $00000001) = 0
                    then
                        ASMMatrixMultUnAlignedOddW1EvenH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
                    else
                        ASMMatrixMultUnAlignedOddW1OddH2Transposed(dest, destLineWidth, mt1, mtx, width1, height1, width2, height2, LineWidth1, mtxLineWidth)
               end;
          end;

          if not Assigned(mem) then
             FreeMem(mtx);
     end;
end;

procedure ASMMatrixMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
begin
     ASMMatrixMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, nil);
end;

procedure BlockedMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; op : TMatrixMultDestOperation);
begin
     BlockedMatrixMultiplication(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, BlockMatrixCacheSize, op, nil);
end;

procedure BlockedMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
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
begin
					if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
     	  exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     assert(blockSize > 1, 'Error blocksize must be at least 2');

     isAligned := (TASMNativeUInt(dest) and $0000000F) = 0;

     h1FitCacheSize := (height1 mod blockSize) = 0;
     w2FitCacheSize := (width2 mod blockSize) = 0;
     w1FitCacheSize := (width1 mod blockSize) = 0;

     h := height1 div blockSize + TASMNativeInt(not h1FitCacheSize) - 1;
     w := width2 div blockSize + TASMNativeInt(not w2FitCacheSize) - 1;
     gamma := width1 div blockSize + TASMNativeInt(not w1FitCacheSize) - 1;

     blockByteSize := blockSize*blockSize*sizeof(double);

     actBlk := mem;
     if not Assigned(mem) then
        GetMem(actBlk, 4*blockByteSize);
     multBlk := PDouble(PAnsiChar(actBlk) + blockByteSize);
     transBlk := PDouble(PAnsiChar(actBlk) + 2*blockByteSize);
     copyBlk := PDouble(PAnsiChar(actBlk) + 3*blockByteSize);

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

               FillChar(actBlk^, blockByteSize, 0);
               pa := mt1;
               pb := pMt2;

               gammaWidth := blockSize;
               for idx := 0 to gamma do
               begin
                    if (idx = gamma) and not w1FitCacheSize then
                       gammaWidth := width1 mod blockSize;

                    if (blkWidth > 3) and (blkHeight > 3) then
                    begin
                         ASMMatrixTranspose(transBlk, blockLineSize, pb, LineWidth2, blkWidth, gammaWidth);

                         // it is faster to copy the block rather then multply it unaligned!
                         if (not isAligned) or ((LineWidth1 and $0000000F) <> 0) then
                         begin
                              ASMMatrixCopy(copyBlk, blockLineSize, pa, LineWidth1, gammaWidth, blkHeight);
                              ASMMatrixMultTransposed(multblk, blockLineSize, copyBlk, transBlk, gammaWidth, blkHeight, gammaWidth, blkWidth, blockLineSize, blockLineSize);
                         end
                         else
                             ASMMatrixMultTransposed(multblk, blockLineSize, pa, transBlk, gammaWidth, blkHeight, gammaWidth, blkWidth, LineWidth1, blockLineSize);

                         ASMMatrixAdd(actBlk, blockLineSize, actBlk, multBlk, blkWidth, blkHeight, blockLineSize, blockLineSize);
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
                 doNone: ASMMatrixCopy(pDest, destLineWidth, actBlk, blockLineSize, blkWidth, blkHeight);
                 doAdd: ASMMatrixAdd(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockLineSize);
                 doSub: ASMMatrixSub(pDest, destLineWidth, pDest, actBlk, blkWidth, blkHeight, destLineWidth, blockLineSize);
               end;

               inc(pDest, blockSize);
               inc(pMt2, blockSize);
          end;

          inc(PByte(mt1), blkHeight*LineWidth1);
     end;

     if not Assigned(mem) then
        FreeMem(actBlk);
end;

procedure InternalASMStrassenMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt;
  width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; mem : PDouble);
var a11, a12, a21, a22 : PDouble;
    b11, b12, b21, b22 : PDouble;
    s1, s2, s3, s4 : PDouble;
    t1, t2, t3, t4 : PDouble;
    P1, P2, P3, P4, P5, P6, P7 : PDouble;
    U1, U2, U3, U4, U5, U6, U7 : PDouble;
    c11, c12, c21, c22 : PDouble;
    k, m, n : integer;
    lineK : integer;
    lineN : integer;
    x, y : PDouble;
    multLineW : integer;
begin
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (width2 <= cStrassenMinSize) then
     begin
          multLineW := Max(cStrassenMinSize, height2)*sizeof(double);
          ASMMatrixTranspose(mem, multLineW, mt2, LineWidth2, width2, height2);
          ASMMatrixMultTransposed(dest, destLineWidth, mt1, mem, width1, height1, height2, width2, LineWidth1, multLineW);
     end
     else
     begin
          k := width1 div 2;
          m := height1 div 2;
          n := width2 div 2;
          
          lineK := k*sizeof(double);
          lineN := n*sizeof(double);

          a11 := mt1;
          a12 := a11;
          inc(a12, k);
          a21 := mt1;
          inc(PByte(a21), m*LineWidth1);
          a22 := a21;
          inc(a22, k);

          b11 := mt2;
          b12 := b11;
          inc(b12, n);
          b21 := mt2;
          inc(PByte(b21), k*LineWidth2);
          b22 := b21;
          inc(b22, n);

          c11 := dest;
          c12 := c11;
          inc(c12, n);
          c21 := dest;
          inc(PByte(c21), m*destLineWidth);
          c22 := c21;
          inc(c22, n);

          x := mem;
          y := x;
          inc(Y, m*Max(k, n));

          S3 := X;
          T3 := Y;
          P7 := C21;
          S1 := X;
          T1 := Y;
          P5 := C22;
          S2 := X;
          T2 := Y;
          P6 := C12;
          S4 := X;
          P3 := C11;
          P1 := X;
          U2 := C12;
          U3 := C21;
          U4 := C12;
          U7 := C22;
          U5 := C12;
          T4 := Y;
          P4 := C11;
          U6 := C21;
          P2 := C11;
          U1 := C11;

          mem := Y;
          inc(mem, k*n);

          // memory efficient mult:

          // s3 = A11 - A21
          ASMMatrixSub(s3, lineK, a11, a21, k, m, LineWidth1, LineWidth1);
          // t3 = B22 - B12
          ASMMatrixSub(t3, lineN, B22, B12, n, k, LineWidth2, LineWidth2);
          // p7 = s3*t3
          InternalASMStrassenMult(p7, destLineWidth, s3, t3, k, m, n, k, lineK, lineN, mem);
          // s1 = a21 + a22
          ASMMatrixAdd(s1, lineK, a21, a22, k, m, LineWidth1, LineWidth1);
          // t1 = b12 - b11
          ASMMatrixSub(t1, lineN, B12, B11, n, k, LineWidth2, LineWidth2);
          // p5 = s1*t1
          InternalASMStrassenMult(p5, destLineWidth, s1, t1, k, m, n, k, lineK, lineN, mem);
          // s2 = S1 - A11
          ASMMatrixSub(s2, lineK, S1, A11, k, m, lineK, LineWidth1);
          // t2 = b22 - t1
          ASMMatrixSub(t2, lineN, B22, t1, n, k, LineWidth2, lineN);
          // p6 = s2*t2
          InternalASMStrassenMult(p6, destLineWidth, s2, t2, k, m, n, k, lineK, lineN, mem);
          // s4 = A12 - S2
          ASMMatrixSub(s4, lineK, A12, S2, k, m, LineWidth1, lineK);
          // p3 = s4*b22
          InternalASMStrassenMult(p3, destLineWidth, s4, b22, k, m, n, k, lineK, LineWidth2, mem);
          // p1 = A11*B11
          InternalASMStrassenMult(p1, lineN, A11, B11, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U2 = P1 + P6
          ASMMatrixAdd(U2, destLineWidth, P1, P6, n, m, LineN, destLineWidth);
          // U3 = U2 + P7
          ASMMatrixAdd(U3, destLineWidth, U2, P7, n, m, destLineWidth, destLineWidth);
          // U4 = U2 + P5
          ASMMatrixAdd(U4, destLineWidth, U2, P5, n, m, destLineWidth, destLineWidth);
          // U7 = U3 + P5
          ASMMatrixAdd(U7, destLineWidth, U3, P5, n, m, destLineWidth, destLineWidth);
          // U5 = U4 + P3
          ASMMatrixAdd(U5, destLineWidth, U4, P3, n, m, destLineWidth, destLineWidth);
          // t4 = T2 - B21
          ASMMatrixSub(t4, lineN, T2, B21, n, k, LineN, LineWidth2);
          // p4 = A22*t4
          InternalASMStrassenMult(p4, destLineWidth, A22, t4, k, m, n, k, LineWidth1, lineN, mem);
          // U6 = U3 - P4
          ASMMatrixSub(U6, destLineWidth, U3, P4, n, m, destLineWidth, destLineWidth);
          // p2 = A12*B21
          InternalASMStrassenMult(p2, destLineWidth, A12, B21, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U1 = P1 + P2
          ASMMatrixAdd(U1, destLineWidth, P1, P2, n, m, lineN, destLineWidth);

          // tidy up work for uneven columns, rows....
          if ((width1 and $01) > 0) or ((height1 and $01) > 0) or ((width2 and $01) > 0) then
          begin
               // following the algorithm if all items are odd...:
               //
               //  A*B = [A1   ac    ][B1  bc  ] = [A1*B1   0]  +  Delta
               //        [ar'  alpha ][br' beta]   [  0     0]
               //
               // Delta = [ac   ]*[br' beta]   +   [  0     A1*bc]
               //         [alpha]                  [ar'*B1  ar'*bc]

               // we already have computed A1*B1...

               if ((width1 and $01) = 0) and ((width2 and $01) = 0) then
               begin
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, 1, width2, height2, LineWidth1, LineWidth2);
               end
               else if ((width1 and $01) = 0) and ((height1 and $01) = 0) then
               begin
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, height1, 1, height2, LineWidth1, LineWidth2);
               end
               else if ((height1 and $01) = 0) and ((width2 and $01) = 0) then
               begin
                    inc(A11, width1 - 1);
                    inc(PByte(B11), LineWidth2*(height2 - 1));
                    GenericMtxDeltaUpdate(dest, destLineWidth, A11, B11, width2, height1, LineWidth1);
               end
               else if ((width1 and $01) = 0) and ((height1 and $01) > 0) and ((width2 and $01) > 0) then
               begin
                    // last column [A]*bc
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, height1 - 1, 1, height2, LineWidth1, LineWidth2);
                    dec(B11, (width2 - 1));
                    dec(dest, (width2 - 1));

                    // [ar alpha]*[B bc]  (last line)
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1, 1, width2, height2, LineWidth1, LineWidth2);
               end
               else
               begin
                    // all dimensions are odd!
                    // calc A1*bc
                    inc(dest, (width2 - 1));
                    inc(B11, (width2 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, height1 - 1, 1, height2 - 1, LineWidth1, LineWidth2);
                    dec(B11, (width2 - 1));
                    dec(dest, (width2 - 1));

                    // calc ar'*B1 and ar'*bc
                    inc(PByte(dest), destLineWidth*(height1 - 1));
                    inc(PByte(A11), LineWidth1*(height1 - 1));
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, 1, width2, height2 - 1, LineWidth1, LineWidth2);

                    inc(dest, width2 - 1);
                    inc(B11, width2 - 1);
                    GenericMtxMult(dest, destLineWidth, A11, B11, width1 - 1, 1, 1, height2 - 1, LineWidth1, LineWidth2);

                    dec(dest, width2 - 1);
                    dec(PByte(dest), destLineWidth*(height1 - 1));
                    dec(PByte(A11), LineWidth1*(height1 - 1));
                    dec(B11, width2 - 1);

                    // last step is to add the vector product matrix to the existing sum...
                    inc(A11, width1 - 1);
                    inc(PByte(B11), LineWidth2*(height2 - 1));
                    GenericMtxDeltaUpdate(dest, destLineWidth, A11, B11, width2, height1, LineWidth1);
               end;
          end;
     end;
end;

procedure ASMStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var mem : PDouble;
    memSize : integer;
    m, k, n : integer;
    lev : integer;
begin
     // check the cutoff criterion:
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (height2 <= cStrassenMinSize)
     then
         ASMMatrixMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else
     begin
          // calc the complete used additionaly memory
          memSize := 0;
          m := height1;
          k := width1;
          n := width2;
          lev := Min(m, Min(k, n));

          while lev > cStrassenMinSize do
          begin
               memSize := memSize + sizeof(double)*(m*max(k, n) + k*n);
               k := k shr 1;
               m := m shr 1;
               n := n shr 1;
               lev := lev shr 1;
          end;
          // additional memory used for the transposition
          memSize := memSize + Max(cStrassenMinSize*cStrassenMinSize, n*k)*sizeof(double);

          mem := GetMemory(memSize);
          try
             InternalASMStrassenMult(Dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, mem);
          finally
                 FreeMem(mem);
          end;
     end;
end;

procedure ASMMatrixMultDirect(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
begin
					if (width1 = 0) or (width2 = 0) or (height1 = 0) or (height2 = 0) then
     	  exit;
     assert((width1 = height2), 'Dimension error');
     assert((destLineWidth - Width2*sizeof(double) >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     if (width1 < 3) and (width2 < 3)
     then
         GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else if (width1 < 2) or (width2 < 2) then
     begin
          // matrix/vector multiplication
          if width2 = 1 then
          begin
               if LineWidth2 = sizeof(double) then
               begin
                    if ((TASMNativeUInt(dest) and $0000000F) = 0) and ((TASMNativeUInt(mt1) and $0000000F) = 0) and ((TASMNativeUInt(mt2) and $0000000F) = 0) and
                       ((LineWidth1 and $0000000F) = 0) then
                    begin
                         if (width1 and $00000001 = 0)
                         then
                             ASMMatrixVectorMultAlignedEvenW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                         else
                             ASMMatrixVectorMultAlignedOddW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                    end
                    else
                    begin
                         if (width1 and $00000001 = 0)
                         then
                             ASMMatrixVectorMultUnAlignedEvenW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                         else
                             ASMMatrixVectorMultUnAlignedOddW1(dest, destLineWidth, mt1, mt2, width1, height1, height2, LineWidth1)
                    end;
               end
               else
                   GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
          end
          else // todo: create special routines for that case too!
              GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
     end
     else
     begin
          // check for alignment:
          if ((TASMNativeUInt(dest) and $0000000F) = 0) and ((TASMNativeUInt(mt1) and $0000000F) = 0) and ((TASMNativeUInt(mt2) and $0000000F) = 0) and
             ((destLineWidth and $0000000F) = 0) and ((LineWidth1 and $0000000F) = 0) and ((LineWidth2 and $0000000F) = 0) then
          begin
               if (width1 and $00000001) = 0 then
               begin
                    if (width2 and $00000001) = 0
                    then
                        ASMMatrixMultAlignedEvenW1EvenW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultAlignedEvenW1OddW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end
               else
               begin
                    if (width2 and $00000001) = 0
                    then
                        ASMMatrixMultAlignedOddW1EvenW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultAlignedOddW1OddW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end;
          end
          else
          begin
               if (width1 and $00000001) = 0 then
               begin
                    if (width2 and $00000001) = 0
                    then
                        ASMMatrixMultUnAlignedEvenW1EvenW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultUnAlignedEvenW1OddW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end
               else
               begin
                    if (width2 and $00000001) = 0
                    then
                        ASMMatrixMultUnAlignedOddW1EvenW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
                    else
                        ASMMatrixMultUnAlignedOddW1OddW2(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
               end;
          end;
     end;
end;

end.

