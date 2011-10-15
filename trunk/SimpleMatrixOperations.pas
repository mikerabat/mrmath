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


unit SimpleMatrixOperations;

// ############################################
// #### Base matrix operations based on functions
// ############################################

interface

uses ASMConsts, SysUtils, Types, MatrixConst;

procedure GenericMtxCopy(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); overload;
procedure GenericMtxCopy(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt); overload;
function GenericMtxCopy(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt) : TDoubleDynArray; overload;
function GenericMtxCopy(const Src : Array of double; width, height : TASMNativeInt) : TDoubleDynArray; overload;

function GenericMtxAdd(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxAdd(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt); overload;
function GenericMtxAdd(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt); overload;

function GenericMtxSub(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxSub(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt); overload;
function GenericMtxSub(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxSub(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt); overload;

function GenericMtxMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
function GenericMtxMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt); overload;

procedure GenericMtxElemMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt); overload;
function GenericMtxElemMult(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
function GenericMtxElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;

// GenericMtx transposition functions. Note the there is no inplace GenericMtx transpose - this will result in an unspecified end GenericMtx.
function GenericMtxTranspose(mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxTranspose(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt); overload;
function GenericMtxTranspose(const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxTranspose(var dest : Array of Double; const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;

function GenericMtxMax(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
function GenericMtxMin(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;

function GenericMtxNormalize(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
function GenericMtxNormalize(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray; overload;
procedure GenericMtxNormalize(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean); overload;
procedure GenericMtxNormalize(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean); overload;

procedure GenericMtxMean(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
procedure GenericMtxSum(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);

procedure GenericMtxAddAndScale(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure GenericMtxScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);

// element wise eukledian norm
function GenericMtxElementwiseNorm2(Src : PDouble; srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;


procedure GenericMtxSqrt(dest : PDouble; destLineWidth : TASMNativeInt; width, height : TASMNativeInt);
//

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc); overload;

implementation

uses Math, MathUtilFunc;

function GenericMtxNormalize(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     SetLength(Result, width*height);
     GenericMtxNormalize(@Result[0], width*sizeof(double), Src, srcLineWidth, width, height, RowWise);
end;

function GenericMtxNormalize(const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (length(Src) >= width*height), 'Dimension error');

     SetLength(Result, width*height);
     GenericMtxNormalize(@Result[0], width*sizeof(double), @Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure GenericMtxNormalize(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
var normVal : double;
    pSrc : PDouble;
    pDest : PDouble;
    x, y : TASMNativeInt;
begin
     assert((destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)) and (width > 0) and (height > 0), 'Dimension error');

     if RowWise then
     begin
          for y := 0 to height - 1 do
          begin
               pSrc := Src;
               pDest := dest;
               normVal := 0;
               for x := 0 to width - 1 do
               begin
                    normVal := normVal + sqr(pSrc^);
                    inc(pSrc);
               end;
               normVal := 1/sqrt(normVal);

               pSrc := Src;
               for x := 0 to width - 1 do
               begin
                    pDest^ := pSrc^*normVal;
                    inc(pDest);
                    inc(pSrc);
               end;

               inc(PByte(dest), destLineWidth);
               inc(PByte(src), srcLineWidth);
          end;
     end
     else
     begin
          for x := 0 to width - 1 do
          begin
               pSrc := Src;
               pDest := dest;
               normVal := 0;
               for y := 0 to height - 1 do
               begin
                    normVal := normVal + sqr(pSrc^);
                    inc(PByte(pSrc), srcLineWidth);
               end;
               normVal := 1/sqrt(normVal);

               pSrc := Src;
               for y := 0 to height - 1 do
               begin
                    pDest^ := pSrc^*normVal;
                    inc(PByte(pDest), destLineWidth);
                    inc(PByte(pSrc), srcLineWidth);
               end;

               inc(dest);
               inc(src);
          end;
     end;
end;

procedure GenericMtxNormalize(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt; RowWise : boolean);
begin
     assert((width > 0) and (height > 0) and (length(Src) >= width*height) and (length(dest) >= width*height), 'Dimension error');

     GenericMtxNormalize(@dest[0], width*sizeof(double), @Src[0], width*sizeof(double), width, height, RowWise);
end;

procedure GenericMtxMean(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
var val : double;
    x, y : TASMNativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   val := val + pVal2^[x];

               if Width > 0 then
                  dest^ := val/Width;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := 0;

               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := val + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
                  dest^ := val/Height;

               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

procedure GenericMtxSum(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);
var val : double;
    x, y : TASMNativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := 0;

               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   val := val + pVal2^[x];

               dest^ := val;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := 0;

               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := val + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               dest^ := val;
               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

function GenericMtxElementwiseNorm2(Src : PDouble; srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;
var pSrc : PDouble;
    x, y : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     Result := 0;

     for y := 0 to Height - 1 do
     begin
          pSrc := Src;

          for x := 0 to Width - 1 do
          begin
              	Result := Result + Sqr(pSrc^);
               inc(pSrc);
          end;

          inc(PByte(Src), srcLineWidth);
     end;

     Result := Sqrt(Result);
end;

procedure GenericMtxCopy(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var y : TASMNativeInt;
    w : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     w := width*sizeof(double);
     for y := 0 to Height - 1 do
     begin
          Move(Src^, dest^, w);
          inc(PByte(Src), srcLineWidth);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxCopy(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0) and (Length(dest) = Length(src)) and (length(src) = width*height), 'Dimension error');
     
     GenericMtxCopy(@dest[0], width*sizeof(double), @src[0], width*sizeof(double), width, height);
end;

function GenericMtxCopy(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');
     SetLength(Result, width*height);

     GenericMtxCopy(@Result[0], width*sizeof(double), Src, srcLineWidth, width, height);
end;

function GenericMtxCopy(const Src : Array of double; width, height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0) and (length(src) = width*height), 'Dimension error');
     SetLength(Result, width*height);
     GenericMtxCopy(@Result[0], width*sizeof(double), @Src[0], width*sizeof(double), width, height);
end;

function GenericMtxMax(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var x, y : TASMNativeInt;
    pMt : PDouble;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');
     Result := -MaxDouble;

     pMt := mt;

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Max(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;

procedure GenericMtxAddAndScale(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var x, y : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstDoubleArr(dest)^[x] := (PConstDoubleArr(dest)^[x] + Offset)*Scale;

          inc(PByte(dest), LineWidth);
     end;
end;

procedure GenericMtxScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
var x, y : TASMNativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstDoubleArr(dest)^[x] := (PConstDoubleArr(dest)^[x]*Scale) + Offset;

          inc(PByte(dest), LineWidth);
     end;
end;

function GenericMtxMin(mt : PDouble; width, height : TASMNativeInt; const LineWidth : TASMNativeInt) : double;
var x, y : TASMNativeInt;
    pMt : PByte;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');
     Result := MaxDouble;

     pMt := PByte(mt);

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Min(Result, PConstDoubleArr(pMt)^[x]);

          inc(PByte(pMt), LineWidth);
     end;
end;


procedure GenericMtxAdd(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
var x, y : TASMNativeInt;
    resOffset : TASMNativeInt;
    mt1Offset : TASMNativeInt;
    mt2Offset : TASMNativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     resOffset := destLineWidth - sizeof(double)*width;
     mt1Offset := LineWidth1 - sizeof(double)*width;
     mt2Offset := LineWidth2 - sizeof(double)*width;
     assert((resOffset >= 0) and (mt1Offset >= 0) and (mt2Offset >= 0), 'Result dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
          begin
               dest^ := mt1^ + mt2^;
               inc(dest);
               inc(mt1);
               inc(mt2);
          end;

          inc(PByte(dest), resOffset);
          inc(PByte(mt1), mt1Offset);
          inc(PByte(mt2), mt2Offset);
     end;
end;


function GenericMtxAdd(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt;
                       const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     GenericMtxAdd(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function GenericMtxAdd(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := GenericMtxAdd(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;
procedure GenericMtxAdd(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');
     
     GenericMtxAdd(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure GenericMtxSub(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
var x, y : TASMNativeInt;
    destOffset : TASMNativeInt;
    mt1Offset : TASMNativeInt;
    mt2Offset : TASMNativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     destOffset := destLineWidth - sizeof(double)*width;
     mt1Offset := LineWidth1 - sizeof(double)*width;
     mt2Offset := LineWidth2 - sizeof(double)*width;
     assert((destOffset >= 0) and (mt1Offset >= 0) and (mt2Offset >= 0), 'Result dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
          begin
               dest^ := mt1^ - mt2^;
               inc(dest);
               inc(mt1);
               inc(mt2);
          end;

          inc(PByte(dest), destOffset);
          inc(PByte(mt1), mt1Offset);
          inc(PByte(mt2), mt2Offset);
     end;
end;

function GenericMtxSub(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt;
                   const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     SetLength(Result, Width*Height);
     GenericMtxSub(@Result[0], sizeof(double)*Width, mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

function GenericMtxSub(const mt1, mt2 : array of double; width : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');

     Result := GenericMtxSub(@mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

procedure GenericMtxSub(var dest : Array of double; const mt1, mt2 : Array of double; width : TASMNativeInt);
begin
     assert((width > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt2) = High(mt1), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     GenericMtxSub(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, (High(mt1) + 1) div width, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxMult(mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     SetLength(Result, Height1*width2);
     GenericMtxMult(@Result[0], sizeof(double)*Width2, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2);
end;

procedure GenericMtxMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
var x, y, idx : TASMNativeInt;
    destOffset : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     destOffset := destLineWidth - Width2*sizeof(double);
     assert((destOffset >= 0) and (LineWidth1 >= width1*sizeof(double)) and (LineWidth2 >= width2*sizeof(double)), 'Line widths do not match');

     for y := 0 to Height1 - 1 do
     begin
          for x := 0 to Width2 - 1 do
          begin
               dest^ := 0;
               valCounter1 := mt1;
               valCounter2 := mt2;
               for idx := 0 to width1 - 1 do
               begin
                    dest^ := dest^ + valCounter1^*valCounter2^;
                    inc(valCounter1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               inc(mt2);
               inc(dest);
          end;
          dec(mt2, Width2);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(dest), destOffset);
     end;
end;

function GenericMtxMult(const mt1, mt2 : Array of Double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(Length(mt1) >= width1*height1, 'Dimension Error');
     assert(Length(mt2) = width2*height2, 'Dimension Error');

     SetLength(Result, height1*width2);
     GenericMtxMult(@Result[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure GenericMtxMult(var dest : Array of Double; mt1, mt2 : Array of double; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt);
begin
     assert((width1 > 0) and (height1 > 0), 'Dimension Error');
     assert((width2 > 0) and (height2 > 0), 'Dimension Error');
     assert(High(mt1) >= width1 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');
     assert(High(mt2) = width2 + 1, 'Dimension Error');

     GenericMtxMult(@dest[0], width2*sizeof(double), @mt1[0], @mt2[0], width1, height1, width2, height2, width1*sizeof(double), width2*sizeof(double));
end;

procedure GenericMtxElemMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt);
var incDest : TASMNativeInt;
    incMt1 : TASMNativeInt;
    incMt2 : TASMNativeInt;
    x, y : TASMNativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');
     assert((width*sizeof(double) <= destLineWidth), 'Dimension error');

     incDest := destLineWidth - width*sizeof(double);
     incMt1 := LineWidth1 - width*sizeof(double);
     incMt2 := LineWidth2 - width*sizeof(double);

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
          begin
               dest^ := mt1^*mt2^;
               inc(mt1);
               inc(mt2);
               inc(dest);
          end;

          inc(PByte(dest), incDest);
          inc(PByte(mt1), incMt1);
          inc(PByte(mt2), incMt2);
     end;
end;

function GenericMtxElemMult(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');

     SetLength(Result, width*Height);
     GenericMtxElemMult(@Result[0], width*sizeof(double), mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure GenericMtxElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     GenericMtxElemMult(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');

     Result := GenericMtxElemMult(@mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxTranspose(mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');

     SetLength(Result, width*height);
     GenericMtxTranspose(@Result[0], sizeof(double)*height, mt, LineWidth, width, height);
end;

procedure GenericMtxTranspose(dest : PDouble; const destLineWidth : TASMNativeInt; mt : PDouble; const LineWidth : TASMNativeInt; width : TASMNativeInt; height : TASMNativeInt);
var valCounter : PDouble;
    y : TASMNativeInt;
    x : TASMNativeInt;
    pMt : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(destLineWidth >= height*sizeof(double), 'Line width does not match');

     for y := 0 to Height - 1 do
     begin
          valCounter := dest;
          pMt := mt;

          for x := 0 to Width - 1 do
          begin
               valCounter^ := pMt^;
               inc(PByte(valCounter), destLineWidth);
               inc(pMt);
          end;

          inc(dest);
          inc(PByte(mt), LineWidth);
     end;
end;

function GenericMtxTranspose(const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(Length(mt) >= width, 'Dimension Error');

     Result := GenericMtxTranspose(@mt[0], width*sizeof(double), width, height);
end;

procedure GenericMtxTranspose(var dest : Array of Double; const mt : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt) >= width + 1, 'Dimension Error');
     assert(High(dest) >= width + 1, 'Dimension Error');

     GenericMtxTranspose(@dest[0], width*sizeof(double), @mt[0], width, height, width*sizeof(double));
end;

procedure GenericMtxSqrt(dest : PDouble; destLineWidth : TASMNativeInt; width, height : TASMNativeInt);
var x, y : TASMNativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              pLine[x] := sqrt(pLine[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc);
var x, y : TASMNativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              func(pLine[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc);
var x, y : TASMNativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              func(pLine[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc);
var x, y : TASMNativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := 0 to width - 1 do
              func(pLine[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc);
var x, y : TASMNativeInt;
    pLine : PConstDoubleArr;
    pDest : PDouble;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(pdest);
          for x := 0 to width - 1 do
              func(pLine[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

end.
