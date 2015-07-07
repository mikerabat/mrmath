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

uses MatrixConst, Types;

procedure GenericMtxCopy(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt); overload;
procedure GenericMtxCopy(var dest : Array of double; const Src : Array of double; width, height : TASMNativeInt); overload;
function GenericMtxCopy(Src : PDouble; const srcLineWidth : TASMNativeInt; width, height : TASMNativeInt) : TDoubleDynArray; overload;
function GenericMtxCopy(const Src : Array of double; width, height : TASMNativeInt) : TDoubleDynArray; overload;

procedure GenericRowSwap(A, B : PDouble; width : TASMNativeInt);

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

// calculatates mt1'*mt2
procedure GenericTranspMtxMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
// calculates mt1*mt2'
procedure GenericMtxMultTransp(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;

procedure GenericTranspMtxMultAdd(dest : PDouble; const destLineWidth : TASMNativeInt;
  mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; C : PDouble; LineWidthC : TASMNativeInt);
// calculates dest = alpha*mt1*mt2' + C
procedure GenericMtxMultTranspAdd(dest : PDouble; const destLineWidth : TASMNativeInt; const alpha : double;
  mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; C : PDouble; LineWidthC : TASMNativeInt);


procedure GenericMtxElemMult(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt); overload;
function GenericMtxElemMult(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxElemMult(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
function GenericMtxElemMult(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;

procedure GenericMtxElemDiv(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt); overload;
function GenericMtxElemDiv(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
procedure GenericMtxElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt); overload;
function GenericMtxElemDiv(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray; overload;

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
procedure GenericMtxVar(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean);
procedure GenericMtxSum(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean);

// matrix median (row or column wise). For the calculation of the median at least width (for rowwise) or height (for columnwise) extra
// memory is needed. The memory is either allocated on the fly or can be provided in the function.
procedure GenericMtxMedian(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; tmp : PDouble = nil);

procedure GenericMtxAddAndScale(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);
procedure GenericMtxScaleAndAdd(Dest : PDouble; LineWidth, Width, Height : TASMNativeInt; const Offset, Scale : double);

// element wise eukledian norm
function GenericMtxElementwiseNorm2(Src : PDouble; srcLineWidth : TASMNativeInt; Width, height : TASMNativeInt) : double;


procedure GenericMtxAbs(dest : PDouble; destLineWidth, width, height : TASMNativeInt);
procedure GenericMtxSqrt(dest : PDouble; destLineWidth : TASMNativeInt; width, height : TASMNativeInt);

// blocked matrix mult + strassen multiplication functions
procedure GenericMtxDeltaUpdate(dest : PDouble; destLineWidth : TASMNativeInt; A11, B11 : PDouble; width, height, LineWidth1 : TASMNativeInt);
procedure GenericStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
procedure GenericBlockedMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
  width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation = doNone; mem : PDouble = nil);

// calculates dest = mt1'*mt2
procedure GenericBlockedMatrixMultiplicationT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);
// calculates dest = mt1*mt2'
procedure GenericBlockedMatrixMultiplicationT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; blockSize : TASMNativeInt; op : TMatrixMultDestOperation; mem : Pdouble);

// Apply a function to a matrix:
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixObjFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefFunc); overload;
procedure GenericMtxFunc(dest : PDouble; const destLineWidth : TASMNativeInt; width, height : TASMNativeInt; func : TMatrixMtxRefObjFunc); overload;

// ###########################################
// #### Matrix multiplications used in QR Decomposition
procedure GenericMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
// same as above but works on 2 columns on mt2 at the same time
procedure GenericMtxMultTria2T1_2(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt;
  mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

procedure GenericMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
procedure GenericMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
procedure GenericMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);

implementation

uses Math, BlockSizeSetup, MathUtilFunc;

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

procedure GenericMtxMedian(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; 
  width, height : TASMNativeInt; RowWise : boolean; tmp : PDouble = nil);
var x, y : TASMNativeInt;
    pVal1 : PByte;
    pVal : PDouble;
    tmpMem : PDouble;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if (Width = 0) or (height = 0) then
        exit;

     tmpMem := tmp;
     
     pVal1 := PByte(Src);
     if RowWise then
     begin
          if tmp = nil then
             tmpMem := GetMemory(width*sizeof(double));
          
          for y := 0 to Height - 1 do
          begin
               // copy since we destroy the array when calculating the median
               Move(pVal1^, tmpMem^, Width*sizeof(double));
               
               if Width and 1 = 1 
               then
                   dest^ := KthLargest(tmpMem, width, width div 2)
               else
                   dest^ := (KthLargest(tmpMem, width, width div 2) + KthLargest(tmpMem, width, Max(0, width div 2 - 1)))/2;
                   
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          if tmp = nil then
             tmpMem := GetMemory(height*sizeof(double));
          
          for x := 0 to Width - 1 do
          begin
               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    PConstDoubleArr(tmpMem)^[y] := pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if height and 1 = 1 
               then
                   dest^ := KthLargest(tmpMem, height, height div 2)
               else
                   dest^ := (KthLargest(tmpMem, height, height div 2) + KthLargest(tmpMem, height, Max(0, height div 2 - 1)))/2;

               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;

     if tmp = nil then
        FreeMemory(tmpMem);
end;

// note: this function calculates the unbiased version of the matrix variance!
procedure GenericMtxVar(dest : PDouble; destLineWidth : TASMNativeInt; Src : PDouble; srcLineWidth : TASMNativeInt; width, height : TASMNativeInt; RowWise : boolean; unbiased : boolean);
var x, y : TASMNativeInt;
    pVal1 : PByte;
    pVal2 : PConstDoubleArr;
    pVal : PDouble;
    meanVal : double;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               meanVal := 0;

               dest^ := Infinity;
               pVal2 := PConstDoubleArr(pVal1);
               for x := 0 to Width - 1 do
                   meanVal := meanVal + pVal2^[x];

               if Width > 0 then
                  meanVal := meanVal/Width;

               if meanVal <> 0 then
               begin
                    dest^ := 0;
                    for x := 0 to Width - 1 do
                        dest^ := dest^ + sqr( pVal2^[x] - meanVal );

                    if unbiased 
                    then
                        dest^ := dest^/Max(1, width - 1)
                    else
                        dest^ := dest^/width;
               end;
                  
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               meanVal := 0;

               dest^ := Infinity;
               
               pVal := PDouble(pVal1);
               for y := 0 to Height - 1 do
               begin
                    meanVal := meanVal + pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
                  meanVal := meanVal/Height;

               if meanVal <> 0 then
               begin
                    pVal := PDouble(pVal1);
                    dest^ := 0;
                    for y := 0 to Height - 1 do
                    begin
                         dest^ := dest^ + sqr( pVal^ - meanVal );
                         inc(PByte(pVal), srcLineWidth);
                    end;   

                    if unbiased 
                    then
                        dest^ := dest^/Max(1, height - 1)
                    else
                        dest^ := dest^/height;
               end;

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

procedure GenericRowSwap(A, B : PDouble; width : TASMNativeInt);
var i : TASMNativeInt;
    tmp : double;
begin
     for i := 0 to width - 1 do
     begin
          tmp := A^;
          A^ := B^;
          B^ := tmp;

          inc(A);
          inc(B);
     end;
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

procedure GenericMtxMultTransp(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt); overload;
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    pMt2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');
     assert(destLineWidth - height2*sizeof(double) >= 0, 'Destination width error');

     for y := 0 to Height1 - 1 do
     begin
          pDest := dest;
          pMt2 := mt2;
          for x := 0 to height2 - 1 do
          begin
               pDest^ := 0;
               valCounter1 := mt1;
               valCounter2 := pMt2;
               for idx := 0 to width1 - 1 do
               begin
                    pDest^ := pDest^ + valCounter1^*valCounter2^;
                    inc(valCounter1);
                    inc(valCounter2);
               end;

               inc(PByte(pMt2), LineWidth2);
               inc(pDest);
          end;
          inc(PByte(mt1), LineWidth1);
          inc(PByte(dest), destLineWidth);
     end;
end;

// performs dest = mt1'*mt2
procedure GenericTranspMtxMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (height2 = height1), 'Dimension error');

     for y := 0 to width1 - 1 do
     begin
          pDest := dest;
          for x := 0 to Width2 - 1 do
          begin
               pDest^ := 0;
               valCounter1 := mt1;
               valCounter2 := mt2;
               for idx := 0 to height2 - 1 do
               begin
                    pDest^ := pDest^ + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               inc(mt2);
               inc(pDest);
          end;

          dec(mt2, Width2);
          inc(mt1);

          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericTranspMtxMultAdd(dest : PDouble; const destLineWidth : TASMNativeInt;
  mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; C : PDouble; LineWidthC : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    pDest : PDouble;
    pC : PDouble;
    addVal : Double;
begin
     assert((width1 > 0) and (height1 > 0) and (height2 = height1), 'Dimension error');

     for y := 0 to width1 - 1 do
     begin
          pC := C;
          pDest := dest;
          for x := 0 to Width2 - 1 do
          begin
               if C <> nil
               then
                   addVal := pC^
               else
                   addVal := 0;

               pDest^ := 0;
               valCounter1 := mt1;
               valCounter2 := mt2;
               for idx := 0 to height2 - 1 do
               begin
                    pDest^ := pDest^ + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := pDest^ + addVal;
               inc(mt2);
               inc(pDest);
               inc(pC);
          end;

          dec(mt2, Width2);
          inc(mt1);

          inc(PByte(dest), destLineWidth);
          inc(PByte(C), LineWidthC);
     end;
end;

// performs dest = alpha*A*B' + C
procedure GenericMtxMultTranspAdd(dest : PDouble; const destLineWidth : TASMNativeInt; const alpha : double;
  mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
  const LineWidth1, LineWidth2 : TASMNativeInt; C : PDouble; LineWidthC : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    pDest : PDouble;
    pC : PDouble;
    addVal : Double;
    pMT2 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');

     for y := 0 to height1 - 1 do
     begin
          pC := C;
          pDest := dest;
          pMt2 := mt2;
          for x := 0 to height2 - 1 do
          begin
               addVal := pC^;

               pDest^ := 0;
               valCounter1 := mt1;
               valCounter2 := pMt2;
               for idx := 0 to width2 - 1 do
               begin
                    pDest^ := pDest^ + valCounter1^*valCounter2^;
                    inc(valCounter1);
                    inc(valCounter2);
               end;

               pDest^ := alpha*pDest^ + addVal;
               inc(PByte(PMt2), LineWidth2);
               inc(pDest);
               inc(pC);
          end;

          //inc(mt2, Width2);
          inc(PByte(mt1), LineWidth1);

          inc(PByte(dest), destLineWidth);
          inc(PByte(C), LineWidthC);
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

procedure GenericMtxElemDiv(dest : PDouble; destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; LineWidth1, LineWidth2 : TASMNativeInt); overload;
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
               dest^ := mt1^/mt2^;
               inc(mt1);
               inc(mt2);
               inc(dest);
          end;

          inc(PByte(dest), incDest);
          inc(PByte(mt1), incMt1);
          inc(PByte(mt2), incMt2);
     end;
end;

function GenericMtxElemDiv(mt1, mt2 : PDouble; width : TASMNativeInt; height : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt) : TDoubleDynArray; overload;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(double) <= LineWidth1) and (width*sizeof(double) <= LineWidth2), 'Dimension error');

     SetLength(Result, width*Height);
     GenericMtxElemMult(@Result[0], width*sizeof(double), mt1, mt2, width, height, LineWidth1, LineWidth2);
end;

procedure GenericMtxElemDiv(var dest : Array of Double; const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt);
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(High(mt1) >= width + 1, 'Dimension Error');
     assert(High(mt1) = High(mt2), 'Dimension Error');
     assert(High(dest) = High(mt1), 'Dimension Error');

     GenericMtxElemMult(@dest[0], width*sizeof(double), @mt1[0], @mt2[0], width, height, width*sizeof(double), width*sizeof(double));
end;

function GenericMtxElemDiv(const mt1, mt2 : Array of Double; width : TASMNativeInt; height : TASMNativeInt) : TDoubleDynArray;
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
              pLine^[x] := sqrt(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericMtxAbs(dest : PDouble; destLineWidth, width, height : TASMNativeInt);
var x, y : TASMNativeInt;
    pLine : PConstDoubleArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstDoubleArr(dest);
          for x := 0 to width - 1 do
              pLine^[x] := abs(pLine^[x]);
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
              func(pLine^[x]);
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
              func(pLine^[x]);
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
              func(pLine^[x], dest, destLineWidth, x, y);
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
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

// ###########################################
// #### Blocked Matrix multiplcation + Strassen algorithm

procedure GenericMtxDeltaUpdate(dest : PDouble; destLineWidth : TASMNativeInt; A11, B11 : PDouble;
  width, height, LineWidth1 : TASMNativeInt);
var x, y : TASMNativeInt;
    pDest : PDouble;
    pB11 : PDouble;
begin
     for y := 0 to height - 1 do
     begin
          pDest := dest;
          pB11 := B11;

          for x := 0 to width - 1 do
          begin
               pDest^ := pDest^ + a11^*pB11^;
               inc(pB11);
               inc(pDest);
          end;

          inc(PByte(A11), LineWidth1);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure InternalGenericStrassenMult(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt;
  width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt; mem : PDouble);
var a11, a12, a21, a22 : PDouble;
    b11, b12, b21, b22 : PDouble;
    s1, s2, s3, s4 : PDouble;
    t1, t2, t3, t4 : PDouble;
    P1, P2, P3, P4, P5, P6, P7 : PDouble;
    U1, U2, U3, U4, U5, U6, U7 : PDouble;
    c11, c12, c21, c22 : PDouble;
    k, m, n : TASMNativeInt;
    lineK : TASMNativeInt;
    lineN : TASMNativeInt;
    x, y : PDouble;
begin
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (width2 <= cStrassenMinSize)
     then
         GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
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
          GenericMtxSub(s3, lineK, a11, a21, k, m, LineWidth1, LineWidth1);
          // t3 = B22 - B12
          GenericMtxSub(t3, lineN, B22, B12, n, k, LineWidth2, LineWidth2);
          // p7 = s3*t3
          InternalGenericStrassenMult(p7, destLineWidth, s3, t3, k, m, n, k, lineK, lineN, mem);
          // s1 = a21 + a22
          GenericMtxAdd(s1, lineK, a21, a22, k, m, LineWidth1, LineWidth1);
          // t1 = b12 - b11
          GenericMtxSub(t1, lineN, B12, B11, n, k, LineWidth2, LineWidth2);
          // p5 = s1*t1
          InternalGenericStrassenMult(p5, destLineWidth, s1, t1, k, m, n, k, lineK, lineN, mem);
          // s2 = S1 - A11
          GenericMtxSub(s2, lineK, S1, A11, k, m, lineK, LineWidth1);
          // t2 = b22 - t1
          GenericMtxSub(t2, lineN, B22, t1, n, k, LineWidth2, lineN);
          // p6 = s2*t2
          InternalGenericStrassenMult(p6, destLineWidth, s2, t2, k, m, n, k, lineK, lineN, mem);
          // s4 = A12 - S2
          GenericMtxSub(s4, lineK, A12, S2, k, m, LineWidth1, lineK);
          // p3 = s4*b22
          InternalGenericStrassenMult(p3, destLineWidth, s4, b22, k, m, n, k, lineK, LineWidth2, mem);
          // p1 = A11*B11
          InternalGenericStrassenMult(p1, lineN, A11, B11, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U2 = P1 + P6
          GenericMtxAdd(U2, destLineWidth, P1, P6, n, m, LineN, destLineWidth);
          // U3 = U2 + P7
          GenericMtxAdd(U3, destLineWidth, U2, P7, n, m, destLineWidth, destLineWidth);
          // U4 = U2 + P5
          GenericMtxAdd(U4, destLineWidth, U2, P5, n, m, destLineWidth, destLineWidth);
          // U7 = U3 + P5
          GenericMtxAdd(U7, destLineWidth, U3, P5, n, m, destLineWidth, destLineWidth);
          // U5 = U4 + P3
          GenericMtxAdd(U5, destLineWidth, U4, P3, n, m, destLineWidth, destLineWidth);
          // t4 = T2 - B21
          GenericMtxSub(t4, lineN, T2, B21, n, k, LineN, LineWidth2);
          // p4 = A22*t4
          InternalGenericStrassenMult(p4, destLineWidth, A22, t4, k, m, n, k, LineWidth1, lineN, mem);
          // U6 = U3 - P4
          GenericMtxSub(U6, destLineWidth, U3, P4, n, m, destLineWidth, destLineWidth);
          // p2 = A12*B21
          InternalGenericStrassenMult(p2, destLineWidth, A12, B21, k, m, n, k, LineWidth1, LineWidth2, mem);
          // U1 = P1 + P2
          GenericMtxAdd(U1, destLineWidth, P1, P2, n, m, lineN, destLineWidth);

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

procedure GenericStrassenMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt; const LineWidth1, LineWidth2 : TASMNativeInt);
var mem : PDouble;
    memSize : TASMNativeInt;
    m, k, n : TASMNativeInt;
    lev : TASMNativeInt;
begin
     // check the cutoff criterion:
     if (width1 <= cStrassenMinSize) or (height1 <= cStrassenMinSize) or (height2 <= cStrassenMinSize)
     then
         GenericMtxMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2)
     else
     begin
          // calc the complete used additionaly memory
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

          mem := GetMemory(memSize);
          try
             InternalGenericStrassenMult(dest, destLineWidth, mt1, mt2, width1, height1, width2, height2, LineWidth1, LineWidth2, mem);
          finally
                 FreeMem(mem);
          end;
     end;
end;

procedure GenericBlockedMatrixMultiplication(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble;
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

               // build the result: dest = A*B +- Dest (according to the operator)
               case op of
                 doNone: GenericMtxCopy(pDest, destLineWidth, ActBlk, blockSize*sizeof(double), blkWidth, blkHeight);
                 doAdd:  GenericMtxAdd(pDest, destLineWidth, ActBlk, pDest, blkWidth, blkHeight, blockSize*sizeof(double), destLineWidth);
                 doSub:  GenericMtxSub(pDest, destLineWidth, ActBlk, pDest, blkWidth, blkHeight, blockSize*sizeof(double), destLineWidth);
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
procedure GenericBlockedMatrixMultiplicationT1(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
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
     multBlk := PDouble(PAnsiChar(actBlk) + blockByteSize);
     transBlk1 := PDouble(PAnsiChar(actBlk) + 2*blockByteSize);

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
procedure GenericBlockedMatrixMultiplicationT2(dest : PDouble; const destLineWidth : TASMNativeInt; mt1, mt2 : PDouble; width1 : TASMNativeInt; height1 : TASMNativeInt; width2 : TASMNativeInt; height2 : TASMNativeInt;
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
     multBlk := PDouble(PAnsiChar(actBlk) + blockByteSize);

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


// ###########################################
// #### Special multiplication routines (for now only used in QR Decomposition)
// ###########################################

// note the result is stored in mt2 again!
// dest = mt1'*mt2; where mt2 is a lower triangular matrix and the operation is transposition
// the function assumes a unit diagonal (does not touch the real middle elements)
// width and height values are assumed to be the "original" (non transposed) ones
procedure GenericMtxMultTria2T1(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    tmp : double;
    pMt2 : PDouble;
    pDest : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');

     for x := 0 to width1 - 1 do
     begin
          pMT2 := mt2;
          pDest := dest;
          for y := 0 to width2 - 1 do
          begin
               valCounter1 := mt1;
               valCounter2 := pMT2;
               inc(PByte(valCounter2), (y + 1)*LineWidth2);
               inc(PByte(valCounter1), (y)*LineWidth1);
               tmp := valCounter1^;
               inc(PByte(valCounter1), LineWidth1);
               for idx := 1 to height2 - y - 1 do
               begin
                    tmp := tmp + valCounter1^*valCounter2^;
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := tmp;
               inc(pDest);
               inc(pMT2);
          end;
          inc(mt1);
          inc(PByte(dest), LineWidthDest);
     end;
end;

// same as the above function but utilizes lines better (2 double values written in a line)
procedure GenericMtxMultTria2T1_2(dest : PDouble; LineWidthDest : TASMNativeInt; mt1 : PDouble; LineWidth1 : TASMNativeInt;
  mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PConstDoubleArr;
    valCounter2 : PConstDoubleArr;
    tmp : Array[0..1] of double;
    pMt2 : PDouble;
    pDest : PDouble;
    width2D2 : TASMNativeInt;
begin
     assert((width1 > 0) and (height1 > 0) and (height1 = height2), 'Dimension error');

     width2D2 := width2 div 2;

     for x := 0 to width1 - 1 do
     begin
          pMT2 := mt2;
          pDest := dest;
          for y := 0 to width2D2 - 1 do
          begin
               valCounter1 := PConstDoubleArr(mt1);
               valCounter2 := PConstDoubleArr(pMT2);
               inc(PByte(valCounter2), (2*y + 1)*LineWidth2);
               inc(PByte(valCounter1), 2*y*LineWidth1);

               tmp[0] := valCounter1^[0];
               inc(PByte(valCounter1), LineWidth1);

               // second line: second element of mt2 is 1!
               if height2 > 2*y + 1 then
               begin
                    tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
                    tmp[1] := valCounter1^[0];

                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               // rest is a double column!
               for idx := 2 to height2 - 2*y - 1 do
               begin
                    tmp[0] := tmp[0] + valCounter1^[0]*valCounter2^[0];
                    tmp[1] := tmp[1] + valCounter1^[0]*valCounter2^[1];
                    inc(PByte(valCounter1), LineWidth1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               pDest^ := tmp[0];
               PDouble(PAnsiChar(pDest) + sizeof(double))^ := tmp[1];
               inc(pDest, 2);
               inc(pMT2, 2);
          end;

          if (width2 and $01) = 1 then
          begin
               // special handling of last column (just copy the value)
               valCounter1 := PConstDoubleArr(mt1);
               inc(PByte(valCounter1), LineWidth1*(height1 - 1));
               pDest^ := valCounter1^[0];
          end;

          inc(mt1);
          inc(PByte(dest), LineWidthDest);
     end;
end;

// note the result is stored in mt1 again!
// mt1 = mt1*mt2; where mt2 is an upper triangular matrix
procedure GenericMtxMultTria2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    tmp : double;
    pMt1 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');

     // start from the back
     inc(mt2, width2 - 1);
     for x := 0 to width2 - 1 do
     begin
          pmt1 := mt1;
          for y := 0 to height1 - 1 do
          begin
               tmp := 0;
               valCounter1 := pmt1;
               valCounter2 := MT2;

               for idx := 0 to width1 - x - 1 do
               begin
                    tmp := tmp + valCounter1^*valCounter2^;
                    inc(valCounter1);
                    inc(PByte(valCounter2), LineWidth2);
               end;

               PConstDoubleArr(pmt1)^[width2 - 1 - x] := tmp;
               inc(PByte(pmT1), LineWidth1);
          end;

          dec(mt2);
     end;
end;

// note the result is stored in mt1 again!
// mt1 = mt1*mt2'; where mt2 is an upper triangular matrix
procedure GenericMtxMultTria2T1StoreT1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    tmp : double;
    pMt1 : PDouble;
    pMt2 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');

     pMt2 := mt2;
     for x := 0 to height2 - 1 do
     begin
          pMt1 := mt1;
          for y := 0 to height1 - 1 do
          begin
               tmp := 0;
               valCounter1 := pMt1;
               valCounter2 := pMt2;

               for idx := x to width1 - 1 do
               begin
                    tmp := tmp + valCounter1^*valCounter2^;
                    inc(valCounter1);
                    inc(valCounter2);
               end;

               pMt1^ := tmp;
               inc(PByte(pMt1), LineWidth1);
          end;

          inc(mt1);
          inc(PByte(pMt2), LineWidth2);
          inc(pMt2);
     end;
end;


// calculates mt1 = mt1*mt2', mt2 = lower triangular matrix. diagonal elements are assumed to be 1!
procedure GenericMtxMultLowTria2T2Store1(mt1 : PDouble; LineWidth1 : TASMNativeInt; mt2 : PDouble; LineWidth2 : TASMNativeInt;
  width1, height1, width2, height2 : TASMNativeInt);
var x, y, idx : TASMNativeInt;
    valCounter1 : PDouble;
    valCounter2 : PDouble;
    tmp : double;
    pMT1 : PDouble;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');

     inc(PByte(mt2),(height2 - 1)*LineWidth2);

     for x := 0 to width2 - 1 do
     begin
          pMt1 := mt1;
          for y := 0 to height1 - 1 do
          begin
               tmp := 0;
               valCounter1 := pMt1;
               valCounter2 := mt2;
               for idx := 0 to width2 - x - 2 do
               begin
                    tmp := tmp + valCounter1^*valCounter2^;
                    inc(valCounter1);
                    inc(valCounter2);
               end;

               PConstDoubleArr(pMt1)^[width2 - x - 1] := tmp + valCounter1^;

               inc(PByte(pMt1), LineWidth1);
          end;

          dec(PByte(mt2), LineWidth2);
     end;
end;

end.
