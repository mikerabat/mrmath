// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2025, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit CplxSimpleMatrixOperations;

interface

uses MatrixConst, Math, Types;


// ###########################################
// #### Complex number handling
// ###########################################
// sqrt( val.real^2 + val.imag^2 )
function CAbs( const val : TComplex ) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
// abs(val.real) + abs(val.imag)
function CAbs1( const val : TComplex ) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CConj( const val : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CAdd( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CSub( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CMul(const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CDiv( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CInv( const x1 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function RCMul(const x1 : double; const x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CMax( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CMin( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

// perform x1 + x2*x3
function CAddMul( const x1, x2, x3 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
// performs x1 := x1 + x2*x3
procedure CIncMul( var x1 : TComplex; const x2, x3 : TComplex ); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
// performs x1 := x1 + x2*conj(x3)
procedure CIncMulConj( var x1 : TComplex; const x2, x3 : TComplex ); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

// perform x1 - x2*x3
function CSubMul( const x1, x2, x3 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}


// performs x1 := x1 + x2;
procedure CInc( var x1 : TComplex; const x2 : TComplex ); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
// performs x1 := x1 - x2;
procedure CDec( var x1 : TComplex; const x2 : TComplex ); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

function CNeg( const x1 : TComplex ): TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CCmp( const x, y : TComplex; eps : double = 0 ) : TValueRelationship;

// ###########################################
// #### Higher order function
function CSqrt( const x : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CExp( const x : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CLn( const x : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CSin( const x : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CCos( const x : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function CTan( const x : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}


// ###########################################
// #### Matrix/Vector operations on arrays/matrices

// todo: to be filled with the same function as in SimpleMatrixOPerations.pas
procedure CplxGenericMtxInit( dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt; const value : TComplex );
procedure CplxGenericMtxCopy(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt);

procedure CplxGenericMtxIndex( dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; colIdx, rowIdx : TIntegerDynArray);


procedure CplxGenericColSwap(A, B : PComplex; const LineWidthAB : NativeInt; Height : NativeInt);
procedure CplxGenericRowSwap(A, B : PComplex; width : NativeInt);


// base matrix operations: scale, add, sub, vec mult
procedure CplxGenericMtxAdd( dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
procedure CplxGenericAddVec(A : PComplex; LineWidthA : NativeInt; B : PComplex; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
procedure CplxGenericMtxSub( dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
procedure CplxGenericSubVec(A : PComplex; LineWidthA : NativeInt; B : PComplex; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);

procedure CplxGenericMtxMult(dest : PComplex; const destLineWidth : NativeInt; mt1, mt2 : PComplex; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
procedure CplxGenericMtxMultTransp(dest : PComplex; const destLineWidth : NativeInt; mt1, mt2 : PComplex; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;

// scale and add operations
procedure CplxGenericMtxElemAdd(Dest : PComplex; LineWidth, Width, Height : NativeInt; const Offset : TComplex);
procedure CplxGenericMtxAddAndScale(Dest : PComplex; LineWidth, Width, Height : NativeInt; const Offset, Scale : TComplex);
procedure CplxGenericMtxScaleAndAdd(Dest : PComplex; LineWidth, Width, Height : NativeInt; const Offset, Scale : TComplex);

procedure CplxGenericMtxCumulativeSum(dest : PComplex; destLineWidth : NativeInt; Src : PComplex;
  srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
procedure CplxGenericMtxDifferentiate(dest : PComplex; destLineWidth : NativeInt; Src : PComplex;
  srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);


// calculates Result := sum_0_n-1( x[i]*y[i] );
function CplGenericVecDotMult( x : PComplex; incX : NativeInt; y : PComplex; incY : NativeInt; N : NativeInt ) : TComplex;

// element wise eukledian norm
function CplxGenericMtxElementwiseNorm2(Src : PComplex; srcLineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;

// conjugate transpose:
procedure CplxGenericMtxTranspose(dest : PComplex; const destLineWidth : NativeInt; mt : PComplex; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); overload;
// -> only square matrices are allowed:
procedure CplxGenericMtxTransposeInplace(dest : PComplex; const destLineWidth : NativeInt; n : NativeInt);

// transpose without conjugate:
procedure CplxGenericMtxTransposeNoConj(dest : PComplex; const destLineWidth : NativeInt; mt : PComplex; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); overload;


// apply the principal square on each element
procedure CplxGenericMtxSqrt(dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt);
// apply cabs on each element
procedure CplxGenericMtxAbs(dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt);

// min/max operations using the cabs operator
function CplxGenericMtxMax(mt : PComplex; width, height : NativeInt; const LineWidth : NativeInt) : TComplex;
function CplxGenericMtxMin(mt : PComplex; width, height : NativeInt; const LineWidth : NativeInt) : TComplex;

// performs dext[x, y] := mt1[x, y]*mt2[x, y]
procedure CplxGenericMtxElemMult(dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
procedure CplxGenericMtxElemDiv(dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;

// normalization
procedure CplxGenericMtxNormalize(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
procedure CplxGenericMtxNormalizeMeanVar(dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);

procedure CplxGenericMtxSum(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
function CplxGenericMtxSumSum(Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt) : TComplex;


// Apply a function to a matrix:
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixFunc); overload;
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixObjFunc); overload;
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixMtxRefFunc); overload;
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixMtxRefObjFunc); overload;

// apply a function to a part of a matrix (startx to startx + width -1, starty to starty + height - 1)
procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixFunc); overload;
procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixObjFunc); overload;
procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixMtxRefFunc); overload;
procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixMtxRefObjFunc); overload;

{$IFDEF FPC}
   {.$DEFINE ANONMETHODS}
{$ELSE}
   {$IF CompilerVersion >= 20.0}
      {$DEFINE ANONMETHODS}
   {$IFEND}
{$ENDIF}

{$IFDEF ANONMETHODS}
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixFuncRef); overload;
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixMtxRefFuncRef); overload;

procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TCplxMatrixFuncRef); overload;
procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TCplxMatrixMtxRefFuncRef); overload;
{$ENDIF}

// ###########################################
// #### Variance/Mean
procedure CplxGenericMtxMean(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
procedure CplxGenericMtxVar(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
procedure CplxGenericMtxMeanVar(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);


// ###########################################
// #### sorting/median
procedure CplxGenericMtxMedian(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt;
  width, height : NativeInt; RowWise : boolean; tmp : PComplex = nil);
procedure CplxGenericMtxRollMedian( dest : PComplex; const destLineWidth : NativeInt; Width, Height : NativeInt; order : integer; rowWise : boolean);
procedure CplxGenericMtxSort(dest : PComplex; destLineWidth : NativeInt; width, height : integer; RowWise : boolean; tmp : PComplex = nil);


implementation

uses RollingMedMean, MathUtilFunc;

// ###########################################
// #### Complex number arithmetic helper functions
// ###########################################

function CAbs( const val : TComplex ) : double;
var x, y : double;
    temp : double;
begin
     x := abs( val.real );
     y := abs( val.imag );
     if x = 0
     then
         Result := y
     else if y = 0
     then
         Result := x
     else
     begin
          temp := min(y, x)/max(y, x);
          Result := max(x, y)*sqrt(1 + sqr(temp));
     end;
end;

function CAbs1( const val : TComplex ) : double;
begin
     Result := abs(val.real) + abs(val.imag);
end;

function CConj( const val : TComplex ) : TComplex;
begin
     Result.real := val.real;
     Result.imag := -val.imag;
end;

function CAdd( const x1, x2 : TComplex ) : TComplex;
begin
     Result.real := x1.real + x2.real;
     Result.imag := x1.imag + x2.imag;
end;

function CSub( const x1, x2 : TComplex ) : TComplex;
begin
     Result.real := x1.real - x2.real;
     Result.imag := x1.imag - x2.imag;
end;

procedure CInc( var x1 : TComplex; const x2 : TComplex );
begin
     x1.real := x1.real + x2.real;
     x1.imag := x1.imag + x2.imag;
end;

procedure CDec( var x1 : TComplex; const x2 : TComplex );
begin
     x1.real := x1.real - x2.real;
     x1.imag := x1.imag - x2.imag;
end;

function CNeg( const x1 : TComplex ): TComplex;
begin
     Result.real := -x1.real;
     Result.imag := -x1.imag;
end;

function CMul(const x1, x2 : TComplex ) : TComplex;
begin
     Result.real := x1.real*x2.real - x1.imag*x2.imag;
     Result.imag := x1.real*x2.imag + x1.imag*x2.real;
end;

function CAddMul( const x1, x2, x3 : TComplex ) : TComplex;
begin
     Result.real := x1.real + x2.real*x3.real - x2.imag*x3.imag;
     Result.imag := x1.imag + x2.real*x3.imag + x2.imag*x3.real;
end;

procedure CIncMul( var x1 : TComplex; const x2, x3 : TComplex ); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
begin
     x1.real := x1.real + x2.real*x3.real - x2.imag*x3.imag;
     x1.imag := x1.imag + x2.real*x3.imag + x2.imag*x3.real;
end;

// performs x1 := x1 + x2*conj(x3)
procedure CIncMulConj( var x1 : TComplex; const x2, x3 : TComplex ); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
begin
     x1.real := x1.real + x2.real*x3.real + x2.imag*x3.imag;
     x1.imag := x1.imag - x2.real*x3.imag + x2.imag*x3.real;
end;

function CSubMul( const x1, x2, x3 : TComplex ) : TComplex;
begin
     Result.real := x1.real - x2.real*x3.real + x2.imag*x3.imag;
     Result.imag := x1.imag - x2.real*x3.imag - x2.imag*x3.real;
end;

function CDiv( const x1, x2 : TComplex ) : TComplex;
var denom : double;
    r : double;
begin
     if abs( x2.real ) >= abs(x2.imag) then
     begin
          r := x2.imag/x2.real;
          denom := x2.real + x2.imag*r;
          Result.real := (x1.real + r*x1.imag)/denom;
          Result.imag := (x1.imag - r*x1.real)/denom;
     end
     else
     begin
          r := x2.real/x2.imag;
          denom := x2.imag + x2.real*r;

          Result.real := (x1.real*r + x1.imag)/denom;
          Result.imag := (x1.imag*r - x1.real)/denom;
     end;
end;

// calculates 1/x1
function CInv( const x1 : TComplex ) : TComplex;
var denom : double;
    r : double;
begin
     if Abs(x1.real) >= Abs(x1.imag) then
     begin
          r := x1.imag/x1.Real;
          denom := x1.real + x1.imag*r;
          Result.real := 1/denom;
          Result.imag := -r/denom;
     end
     else
     begin
          r := x1.real/x1.imag;
          denom := x1.imag + r*x1.real;
          Result.real := r/denom;
          Result.imag := -1/denom;
     end;
end;

function RCMul(const x1 : double; const x2 : TComplex ) : TComplex;
begin
     Result.real := x1*x2.real;
     Result.imag := x1*x2.imag;
end;

function CCmp( const x, y : TComplex; eps : double = 0 ) : TValueRelationship;
var r1, r2 : TValueRelationship;
    s1, s2 : integer;
begin
     r1 := CompareValue(x.real, y.real, eps);
     r2 := CompareValue(x.imag, y.imag, eps);

     if (r1 = 0) and (r2 = 0) then
     begin
          Result := 0;
          exit;
     end;

     Result := CompareValue( CAbs(x), CAbs(y), eps );
     // same length? -> first test real value sign, then imag sign
     if Result = 0 then
     begin
          s1 := Math.Sign(x.real);
          s2 := Math.Sign(y.real);

          if s1 = s2 then
          begin
               s1 := Math.Sign(x.imag);
               s2 := Math.Sign(y.imag);

               if (s1 = s2) then
               begin
                    // -> the angle shall define the compare operation
                    Result := CompareValue(arctan2( x.imag, x.real), arctan2( y.imag, y.real), eps);
                    exit;
               end;
          end;

          if s1 > s2
          then
              Result := 1
          else
              Result := -1;
     end;
end;

// ###########################################
// #### Higher order functions
// ###########################################

function CMax( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
begin
     if CCmp( x1, x2 ) >= 0
     then
         Result := x1
     else
         Result := x2;
end;

function CMin( const x1, x2 : TComplex ) : TComplex; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
begin
     if CCmp( x1, x2 ) <= 0
     then
         Result := x1
     else
         Result := x2;
end;

// prinzipal root
function CSqrt( const x : TComplex ) : TComplex;
var a, b : double;
    r, w : double;
    c : double;
begin
     Result := InitComplex(0, 0);
     if (x.real = 0) and (x.imag = 0) then
        exit;

     if x.imag = 0 then
     begin
          if x.real < 0
          then
              Result.imag := sqrt(abs(x.real))
          else
              Result.real := sqrt(x.real);
     end
     else
     begin
          a := abs(x.Real);
          b := abs(x.imag);

          r := Min(a, b)/Max(a, b);
          c := ifthen( a >= b, 1, r);
          w := sqrt(Max(a, b))*sqrt(0.5*(c + sqrt(1 + sqr(r))));

          if x.real >= 0 then
          begin
               Result.real := w;
               Result.imag := x.imag/(2*w);
          end
          else
          begin
               Result.imag := w*math.sign(x.imag);
               Result.real := x.imag/(2*Result.imag);
          end;
     end;
end;

function CExp( const x : TComplex ) : TComplex;
var aSin, aCos : Extended;
begin
     Result.real := Exp(x.real);

     if x.imag = 0 then
     begin
          Result.imag := 0;
     end
     else
     begin
          SinCos(x.imag, aSin, aCos);
          Result.imag := Result.real*aSin;
          Result.real := Result.real*aCos;
     end;
end;

function CLn( const x : TComplex ) : TComplex;
begin
     // prinzipcal value of the complex ln = ln ( abs(x) ) + i*atan2( x.imag, x.real )
     Result.Real := ln( CAbs( x ) );

     if x.imag = 0
     then
         Result.imag := 0
     else
     begin
          // according to doc real may not be 0 (strange)... -> handle it here
          if x.real = 0 then
          begin
               if x.imag > 0
               then
                   Result.imag := pi/2
               else
                   Result.real := -pi/2;
          end
          else
              Result.imag := ArcTan2(x.imag, x.real);
     end;
end;

function CSin( const x : TComplex ) : TComplex;
var x1, x2 : TComplex;
begin
     // equivalent to 1/(2i)*(e^(ix) - e^(-ix))
     if x.imag = 0 then
     begin
          Result.real := Sin( x.real );
          Result.imag := 0;
     end
     else
     begin
          x1 := CExp( CMul( cCplxImag1, x ) );
          x2 := CExp( CMul( cCplxImagM1, x ) );
          Result := CSub( x1, x2 );
          Result := CDiv(Result, InitComplex(0, 2));
     end;
end;

function CCos( const x : TComplex ) : TComplex;
var x1, x2 : TComplex;
begin
     // equivalent to 1/(2)*(e^(ix) + e^(-ix))
     if x.imag = 0 then
     begin
          Result.real := Cos( x.real );
          Result.imag := 0;
     end
     else
     begin
          x1 := CExp( CMul( cCplxImag1, x ) );
          x2 := CExp( CMul( cCplxImagM1, x ) );
          Result := CAdd( x1, x2 );
          Result := RCMul(0.5, Result);
     end;
end;


function CTan( const x : TComplex ) : TComplex;
var a, b : TComplex;
begin
     // tan(x) = i * (1 - e^2ix)/(1 + e^(2ix))
     a := CExp( CMul(InitComplex(0, 2), x));
     b := CSub( cCplxReal1, a );
     a := CAdd( cCplxReal1, a );

     Result := CDiv( b, a );
     Result := CMul( cCplxImag1, Result );
end;


// ###########################################
// #### Complex matrix handling
// ###########################################

procedure CplxGenericMtxInit( dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt; const value : TComplex );
var pDest : PConstComplexArr;
    x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (destLineWidth >= width*sizeof(double)), 'Dimension error');

     pDest := PConstComplexArr( dest );

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              pDest^[x] := value;

          inc(PByte(pDest), destLineWidth);
     end;
end;

procedure CplxGenericMtxCopy(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt);
var x, y : NativeInt;
begin
     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
               PConstComplexArr(dest)^[x] := PConstComplexArr(src)^[x];

          inc(PByte(dest), destLineWidth);
          inc(PByte(src), srcLineWidth);
     end;
end;


procedure CplxGenericMtxIndex( dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; colIdx, rowIdx : TIntegerDynArray);
var pDest : PConstComplexArr;
    pSrc : PConstComplexArr;
    x, y : NativeInt;
begin
     pDest := PConstComplexArr(dest);
     for y := 0 to Length(rowIdx) - 1 do
     begin
          pSrc := PConstComplexArr( CplxGenPtrArr(src, 0, rowIdx[y], srcLineWidth) );

          for x := 0 to Length(colIdx) - 1 do
              pDest^[x] := pSrc^[colIdx[x]];

          inc( PByte(pDest), destLineWidth );
     end;
end;


procedure CplxGenericColSwap(A, B : PComplex; const LineWidthAB : NativeInt; Height : NativeInt);
var tmp : TComplex;
    i : NativeInt;
begin
     for i := 0 to Height - 1 do
     begin
          tmp := A^;
          A^ := B^;
          B^ := tmp;

          inc(PByte(A), LineWidthAB);
          inc(PByte(B), LineWidthAB);
     end;
end;

procedure CplxGenericRowSwap(A, B : PComplex; width : NativeInt);
var i : NativeInt;
    tmp : TComplex;
    pA, pB : PConstComplexArr;
begin
     pA := PConstComplexArr(A);
     pB := PConstComplexArr(B);
     for i := 0 to width - 1 do
     begin
          tmp := pA^[i];
          pA^[i] := pB^[i];
          pB^[i] := tmp;
     end;
end;

procedure CplxGenericMtxNormalize(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean); overload;
var x, y: Integer;
    normVal : TComplex;
    pSrc : PConstComplexArr;
    pDest : PConstComplexArr;
begin
     assert((destLineWidth >= width*sizeof(TComplex)) and (srcLineWidth >= width*sizeof(TComplex)) and (width > 0) and (height > 0), 'Dimension error');

     // implements the normc and normr function in Matlab
     // as of the real matrix we follow here the the formula
     // normVal := 1/sqrt(sum(v.*v))
     // v = v.*normVal
     // same normalization as for real numbers but using complex mult and divide operations...
     if RowWise then
     begin
          for y := 0 to height - 1 do
          begin
               pSrc := PConstComplexArr(Src);
               normVal := cCplxZero;

               for x := 0 to width - 1 do
                   CInc(normVal, CMul(pSrc^[x], pSrc^[x]));
               normVal := CDiv(cCplxReal1, CSqrt(normVal));

               pDest := PConstComplexArr(dest);
               for x := 0 to width - 1 do
                   pDest^[x] := CMul(normVal, pSrc^[x]);

               inc(PByte(dest), destLineWidth);
               inc(PByte(src), srcLineWidth);
          end;
     end
     else
     begin
          for x := 0 to width - 1 do
          begin
               pSrc := PConstComplexArr(Src);
               normVal := cCplxZero;
               for y := 0 to height - 1 do
               begin
                    CInc(normVal, CMul(pSrc^[x], pSrc^[x]));
                    inc(PByte(pSrc), srcLineWidth);
               end;
               normVal := CDiv(cCplxReal1, CSqrt(normVal));

               pSrc := PConstComplexArr(Src);
               pDest := PConstComplexArr(dest);

               for y := 0 to height - 1 do
               begin
                    pDest^[x] := CMul(normVal, pSrc^[x]);

                    inc(PByte(pDest), destLineWidth);
                    inc(PByte(pSrc), srcLineWidth);
               end;
          end;
     end;
end;

procedure CplxGenericMtxNormalizeMeanVar(dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var meanVar : TComplexDynArray;
    x, y : NativeInt;
    invstdDev : TComplex;
const cMinVar : TComplex = (real : 1e-300; Imag : 0);
begin
     if RowWise then
     begin
          SetLength(meanVar, 2*height);

          CplxGenericMtxMeanVar(@meanVar[0], 2*sizeof(TComplex), dest, destLineWidth, width, height, True, True);

          for y := 0 to Height - 1 do
          begin
               invstdDev := cCplxZero;
               if CCmp( meanVar[1 + y*2], cMinVar ) = GreaterThanValue then
                  invstdDev := CDiv( cCplxReal1,  CSqrt(meanVar[1 + y*2]) );

               CplxGenericMtxAddAndScale(Dest, destLineWidth, Width, 1, CNeg(meanVar[0 + y*2]), invstdDev);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          SetLength(meanVar, 2*Width);
          CplxGenericMtxMeanVar( @meanVar[0], width*sizeof(TComplex), dest, destLineWidth, width, height, False, True);

          for x := 0 to Width - 1 do
          begin
               invstdDev := cCplxZero;
               if  CCmp(meanVar[x + width], cMinVar ) = GreaterThanValue then
                  invstdDev := CDiv( cCplxReal1, CSqrt(meanVar[x + width]));

               CplxGenericMtxAddAndScale(Dest, destLineWidth, 1, Height, CNeg(meanVar[x]), invstdDev);
               inc(dest);
          end;
     end;
end;


function CplxGenericMtxElementwiseNorm2(Src : PComplex; srcLineWidth : NativeInt; Width, height : NativeInt; doSqrt : boolean) : double;
var pSrc : PConstComplexArr;
    x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (srcLineWidth >= width*sizeof(double)), 'Dimension error');

     Result := 0;

     pSrc := PConstComplexArr(Src);
     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              Result := Result + sqr(pSrc^[x].real) + sqr(pSrc^[x].imag);
          inc(PByte(pSrc), srcLineWidth);
     end;

     if doSqrt then
        Result := Sqrt(Result);
end;


procedure CplxGenericMtxAdd( dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstComplexArr(dest)^[x] := CAdd( PConstComplexArr(mt1)^[x], PConstComplexArr(mt2)^[x] );

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;


procedure CplxGenericMtxSub( dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstComplexArr(dest)^[x] := CSub( PConstComplexArr(mt1)^[x], PConstComplexArr(mt2)^[x] );

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;

procedure CplxGenericAddVec(A : PComplex; LineWidthA : NativeInt; B : PComplex; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
var pB, pA : PConstComplexArr;
    pB2 : PComplex;
    y : NativeInt;
    x : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');

     if incX = sizeof(TComplex) then
     begin
          pB := PConstComplexArr(B);
          pA := PConstComplexArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to Width - 1 do
                        pA^[x] := CAdd(pA^[x], pB^[x]);

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := CAdd(pA^[x], pB^[y]);

                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end
     else
     begin
          pA := PConstComplexArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    pB2 := PComplex(B);
                    for x := 0 to Width - 1 do
                    begin
                         pA^[x] := CAdd(pA^[x], pB2^);
                         inc(PByte(pB2), incX);
                    end;

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               pB2 := PComplex(B);
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := CAdd(pA^[x], pB2^);

                    inc(PByte(pB2), incX);
                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end;
end;

procedure CplxGenericSubVec(A : PComplex; LineWidthA : NativeInt; B : PComplex; incX : NativeInt; width, Height : NativeInt; rowWise : Boolean);
var pB, pA : PConstComplexArr;
    pB2 : PComplex;
    y : NativeInt;
    x : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');

     if incX = sizeof(TComplex) then
     begin
          pB := PConstComplexArr(B);
          pA := PConstComplexArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to Width - 1 do
                        pA^[x] := CSub(pA^[x], pB^[x]);

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := CSub(pA^[x], pB^[y]);

                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end
     else
     begin
          pA := PConstComplexArr(A);
          if RowWise then
          begin
               for y := 0 to Height - 1 do
               begin
                    pB2 := PComplex(B);
                    for x := 0 to Width - 1 do
                    begin
                         pA^[x] := CSub(pA^[x], pB2^);
                         inc(PByte(pB2), incX);
                    end;

                    inc(PByte(pA), LineWidthA);
               end;
          end
          else
          begin
               pB2 := PComplex(B);
               for y := 0 to Height - 1 do
               begin
                    for x := 0 to width - 1 do
                        pA^[x] := CSub(pA^[x], pB2^);

                    inc(PByte(pB2), incX);
                    inc(PByte(pA), LineWidthA);
               end;
          end;
     end;
end;


procedure CplxGenericMtxCumulativeSum(dest : PComplex; destLineWidth : NativeInt; Src : PComplex;
  srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var val : TComplex;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstComplexArr;
    pVal : PComplex;
    pDest : PComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := cCplxZero;

               pVal2 := PConstComplexArr(pVal1);
               for x := 0 to Width - 1 do
               begin
                    val := CAdd( val, pVal2^[x] );
                    PConstComplexArr(dest)^[x] := val;
               end;

               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := cCplxZero;

               pVal := PComplex(pVal1);
               pDest := dest;
               for y := 0 to Height - 1 do
               begin
                    val := CAdd(val, pVal^);
                    pDest^ := val;
                    inc(PByte(pVal), srcLineWidth);
                    inc(PByte(pDest), destLineWidth);
               end;

               inc(pVal1, sizeof(TComplex));
               inc(dest);
          end;
     end;
end;

procedure CplxGenericMtxDifferentiate(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var x, y : NativeInt;
    pVal11 : PComplex;
    pVal2 : PConstComplexArr;
    pVal1 : PComplex;
    pDest : PComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          pVal2 := PConstComplexArr(src);
          for y := 0 to Height - 1 do
          begin
               for x := 0 to Width - 2 do
                   PConstComplexArr(dest)^[x] := CSub(PConstComplexArr(pVal2)^[x + 1], PConstComplexArr(pVal2)^[x]);

               inc(PByte(pVal2), srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               pVal1 := PComplex(src);
               pVal11 := pVal1;
               inc(PByte(pVal11), srcLineWidth);
               pDest := dest;
               for y := 0 to Height - 2 do
               begin
                    pDest^ := CSub(pVal11^, pVal1^);
                    pVal1 := pVal11;
                    inc(PByte(pVal11), srcLineWidth);
                    inc(PByte(pDest), destLineWidth);
               end;

               inc(src);
               inc(dest);
          end;
     end;
end;

procedure CplxGenericMtxMult(dest : PComplex; const destLineWidth : NativeInt; mt1, mt2 : PComplex; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt);
var x, y, idx : NativeInt;
    destOffset : NativeInt;
    valCounter1 : PConstComplexArr;
    valCounter2 : PComplex;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = height2), 'Dimension error');
     destOffset := destLineWidth - Width2*sizeof(TComplex);
     assert((destOffset >= 0) and (LineWidth1 >= width1*sizeof(TComplex)) and (LineWidth2 >= width2*sizeof(TComplex)), 'Line widths do not match');

     for y := 0 to Height1 - 1 do
     begin
          for x := 0 to Width2 - 1 do
          begin
               dest^ := InitComplex(0, 0);
               valCounter1 := PConstComplexArr(mt1);
               valCounter2 := mt2;
               for idx := 0 to width1 - 1 do
               begin
                    CIncMul(dest^, valCounter1^[idx], valCounter2^);
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

procedure CplxGenericMtxMultTransp(dest : PComplex; const destLineWidth : NativeInt; mt1, mt2 : PComplex; width1 : NativeInt; height1 : NativeInt; width2 : NativeInt; height2 : NativeInt; const LineWidth1, LineWidth2 : NativeInt); overload;
var x, y, idx : NativeInt;
    pMt2 : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width1 > 0) and (height1 > 0) and (width1 = width2), 'Dimension error');
     assert(destLineWidth - height2*sizeof(TComplex) >= 0, 'Destination width error');

     for y := 0 to Height1 - 1 do
     begin
          pDest := dest;
          pMt2 := PConstComplexArr(mt2);
          for x := 0 to height2 - 1 do
          begin
               pdest^ := cCplxZero;
               // performs
               for idx := 0 to width1 - 1 do
                   CIncMulConj(pDest^, PConstComplexArr(mt1)^[idx], pMt2^[idx]);

               inc(PByte(pMt2), LineWidth2);
               inc(pDest);
          end;
          inc(PByte(mt1), LineWidth1);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericMtxTranspose(dest : PComplex; const destLineWidth : NativeInt; mt : PComplex; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); overload;
var valCounter : PComplex;
    y : NativeInt;
    x : NativeInt;
    pMt : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(destLineWidth >= height*sizeof(TComplex), 'Line width does not match');

     for y := 0 to Height - 1 do
     begin
          valCounter := dest;
          pMt := PConstComplexArr(mt);

          for x := 0 to Width - 1 do
          begin
               valCounter^ := CConj(pMt^[x]); // conjugate complex
               inc(PByte(valCounter), destLineWidth);
          end;
          inc(dest);
          inc(PByte(mt), LineWidth);
     end;
end;


procedure CplxGenericMtxTransposeNoConj(dest : PComplex; const destLineWidth : NativeInt; mt : PComplex; const LineWidth : NativeInt; width : NativeInt; height : NativeInt); overload;
var valCounter : PComplex;
    y : NativeInt;
    x : NativeInt;
    pMt : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(destLineWidth >= height*sizeof(TComplex), 'Line width does not match');

     for y := 0 to Height - 1 do
     begin
          valCounter := dest;
          pMt := PConstComplexArr(mt);

          for x := 0 to Width - 1 do
          begin
               valCounter^ := pMt^[x];
               inc(PByte(valCounter), destLineWidth);
          end;
          inc(dest);
          inc(PByte(mt), LineWidth);
     end;
end;

procedure CplxGenericMtxTransposeInplace(dest : PComplex; const destLineWidth : NativeInt; n : NativeInt);
var x, y : NativeInt;
    pDest : PConstComplexArr;
    pDest1 : PComplex;
    tmp : TComplex;
begin
     assert((n > 0), 'Dimension Error');

     for y := 0 to n - 1 do
     begin
          pDest := PConstComplexArr( CplxGenPtr(dest, 0, y, destLineWidth) );
          pDest1 := CplxGenPtr(dest, y, y + 1, destLineWidth);

          // main diagonal conjugate complex
          pDest^[y].imag := -pDest^[y].imag;

          for x := y + 1 to n - 1 do
          begin
               tmp := pDest^[x];
               pDest^[x] := pDest1^;
               pDest1^ := tmp;
               pDest^[x].imag := -pDest^[x].imag;
               pDest1^.imag := -pDest1^.imag;
               inc(PByte(pDest1), destLineWidth);
          end;
     end;
end;

procedure CplxGenericMtxSqrt(dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt);
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     // principal square on each element
     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := 0 to width - 1 do
              pLine^[x] := CSqrt(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericMtxAbs(dest : PComplex; destLineWidth : NativeInt; width, height : NativeInt);
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     // principal square on each element
     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := 0 to width - 1 do
          begin
               pLine^[x].real := CAbs(pLine^[x]);
               pLine^[x].imag := 0;
          end;
          inc(PByte(dest), destLineWidth);
     end;
end;


function CplxGenericMtxMax(mt : PComplex; width, height : NativeInt; const LineWidth : NativeInt) : TComplex;
var x, y : NativeInt;
    pMt : PConstComplexArr;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(TComplex) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := PConstComplexArr(mt);

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
          begin
               if CCmp( pMT^[x], Result ) > 0 then
                  Result := pMT^[x];
          end;

          inc(PByte(pMt), LineWidth);
     end;
end;


function CplxGenericMtxMin(mt : PComplex; width, height : NativeInt; const LineWidth : NativeInt) : TComplex;
var x, y : NativeInt;
    pMt : PConstComplexArr;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(TComplex) <= LineWidth), 'Dimension error');
     Result := mt^;

     pMt := PConstComplexArr(mt);

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
          begin
               if CCmp( pMT^[x], Result ) < 0 then
                  Result := pMT^[x];
          end;

          inc(PByte(pMt), LineWidth);
     end;
end;

procedure CplxGenericMtxElemMult(dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(TComplex) <= LineWidth1) and (width*sizeof(TComplex) <= LineWidth2), 'Dimension error');
     assert((width*sizeof(double) <= destLineWidth), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstComplexArr(dest)^[x] := CMul(PConstComplexArr(mt1)^[x], PConstComplexArr(mt2)^[x] );

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;

procedure CplxGenericMtxElemDiv(dest : PComplex; destLineWidth : NativeInt; mt1, mt2 : PComplex; width : NativeInt; height : NativeInt; LineWidth1, LineWidth2 : NativeInt); overload;
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0), 'Dimension error');
     assert((width*sizeof(TComplex) <= LineWidth1) and (width*sizeof(TComplex) <= LineWidth2), 'Dimension error');
     assert((width*sizeof(double) <= destLineWidth), 'Dimension error');

     for y := 0 to Height - 1 do
     begin
          for x := 0 to Width - 1 do
              PConstComplexArr(dest)^[x] := CDiv(PConstComplexArr(mt1)^[x], PConstComplexArr(mt2)^[x] );

          inc(PByte(dest), destLineWidth);
          inc(PByte(mt1), LineWidth1);
          inc(PByte(mt2), LineWidth2);
     end;
end;

procedure CplxGenericMtxElemAdd(Dest : PComplex; LineWidth, Width, Height : NativeInt; const Offset : TComplex);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstComplexArr(dest)^[x] := CAdd(PConstComplexArr(dest)^[x], Offset);

          inc(PByte(dest), LineWidth);
     end;
end;


procedure CplxGenericMtxAddAndScale(Dest : PComplex; LineWidth, Width, Height : NativeInt; const Offset, Scale : TComplex);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstComplexArr(dest)^[x] := CMul(CAdd(PConstComplexArr(dest)^[x], Offset), Scale);

          inc(PByte(dest), LineWidth);
     end;
end;

procedure CplxGenericMtxScaleAndAdd(Dest : PComplex; LineWidth, Width, Height : NativeInt; const Offset, Scale : TComplex);
var x, y : NativeInt;
begin
     assert((width > 0) and (height > 0) and (width*sizeof(double) <= LineWidth), 'Dimension error');

     for y := 0 to height - 1 do
     begin
          for x := 0 to width - 1 do
              PConstComplexArr(dest)^[x] := CAddMul( Offset, PConstComplexArr(dest)^[x], Scale);

          inc(PByte(dest), LineWidth);
     end;
end;

function CplGenericVecDotMult( x : PComplex; incX : NativeInt; y : PComplex; incY : NativeInt; N : NativeInt ) : TComplex;
var pXa, pYa : PConstComplexArr;
    i : NativeInt;
begin
     if (incX = sizeof(TComplex)) and (incY = sizeof(TComplex)) then
     begin
          pXA := PConstComplexArr(X);
          pYA := PConstComplexArr(Y);

          Result := InitComplex(0, 0);
          for i := 0 to N - 1 do
              Result := CAdd(result, CMul(pXA^[i], pYA^[i]));
     end
     else
     begin
          Result := InitComplex(0, 0);

          for i := 0 to N - 1 do
          begin
               Result := CAdd( Result, CMul(x^, y^));
               inc(PByte(x), incX);
               inc(PByte(y), incY);
          end;
     end;
end;


// ###########################################
// #### Apply a function on a complex matrix
// ###########################################

// Apply a function to a matrix:
procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := 0 to width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixObjFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(double) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := 0 to width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixMtxRefFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := 0 to width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixMtxRefObjFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := 0 to width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

// apply a function to a part of a matrix (startx to startx + width -1, starty to starty + height - 1)
procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     inc(PByte(dest), startY*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := startX to startX + width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixObjFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     inc(PByte(dest), startY*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := startX to startX + width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixMtxRefFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(TComplex) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startx, starty, width, height : NativeInt; func : TCplxMatrixMtxRefObjFunc); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(TComplex) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

{$IFDEF ANONMETHODS}

procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := 0 to width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure CplxGenericMtxFunc(dest : PComplex; const destLineWidth : NativeInt; width, height : NativeInt; func : TCplxMatrixMtxRefFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert(width*sizeof(TComplex) <= destLineWidth, 'Dimension Error');

     pDest := dest;
     for y := 0 to Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := 0 to width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TCplxMatrixFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(TComplex) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     inc(PByte(dest), startY*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(dest);
          for x := startX to startX + width - 1 do
              func(pLine^[x]);
          inc(PByte(dest), destLineWidth);
     end;
end;

procedure GenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TCplxMatrixMtxRefFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(TComplex) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure CplxGenericSubMtxFunc(dest : PComplex; const destLineWidth : NativeInt; startX, startY, width, height : NativeInt; func : TCplxMatrixMtxRefFuncRef); overload;
var x, y : NativeInt;
    pLine : PConstComplexArr;
    pDest : PComplex;
begin
     assert((width > 0) and (height > 0), 'Dimension Error');
     assert((startx + width)*sizeof(TComplex) <= destLineWidth, 'Dimension Error');
     assert((startx >= 0) and (startY >= 0), 'Dimension Error');

     pDest := dest;
     inc(PByte(pDest), starty*destLineWidth);
     for y := startY to startY + Height - 1 do
     begin
          pLine := PConstComplexArr(pdest);
          for x := startX to startX + width - 1 do
              func(pLine^[x], dest, destLineWidth, x, y);
          inc(PByte(pdest), destLineWidth);
     end;
end;

procedure CplxGenericMtxMean(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var val, val2 : TComplex;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstComplexArr;
    pVal : PComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := cCplxZero;
               val2 := cCplxZero;

               // perform the sum via two variables -> so the
               // sse version is actually the same and there are less numerical
               // problems (in tsne this actually makes a difference)
               pVal2 := PConstComplexArr(pVal1);
               x := 0;
               while x < width - 1 do
               begin
                    val := CAdd(val, pVal2^[x]);
                    val2 := CAdd(val2, pVal2^[x + 1]);
                    inc(x, 2);
               end;
               if x < width then
                  val := CAdd(val, pVal2^[x]);

               val := CAdd(val, val2);
               //for x := 0 to Width - 1 do
               //    val := val + pVal2^[x];

               if Width > 0 then
                  dest^ := RCMul(1/Width, val);
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := cCplxZero;

               pVal := PComplex(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := CAdd(val, pVal^);
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
                  dest^ := RCMul(1/height, val);

               inc(pVal1, sizeof(TComplex));
               inc(dest);
          end;
     end;
end;

procedure CplxGenericMtxVar(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
var val : TComplex;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstComplexArr;
    pVal : PComplex;
    meanVal : TComplex;
    aVariance : TComplex;
    tmp : TComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := cCplxZero;

               pVal2 := PConstComplexArr(pVal1);
               for x := 0 to Width - 1 do
                   val := CAdd(val, pVal2^[x]);

               aVariance := cCplxZero;
               if Width > 0 then
               begin
                    meanVal := RCMul(1/width, val);

                    for x := 0 to Width - 1 do
                    begin
                         tmp := CSub(pVal2^[x], meanVal);
                         aVariance := CAdd(aVariance, CMul(tmp, tmp));
                    end;

                    if unbiased
                    then
                        dest^ := RCMul(1/Max(1, width - 1), aVariance)
                    else
                        dest^ := RCMul(1/width, aVariance);
               end
               else
                   dest^ := cCplxZero;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := cCplxZero;

               pVal := PComplex(pVal1);
               for y := 0 to Height - 1 do
               begin
                    val := CAdd(val, pVal^);
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
               begin
                    meanVal := RCMul(1/Height, val);

                    aVariance := cCplxZero;
                    pVal := PComplex(pVal1);
                    for y := 0 to Height - 1 do
                    begin
                         tmp := CSub(pVal^, meanVal);
                         aVariance := CAdd(aVariance, CMul(tmp, tmp));
                         inc(PByte(pVal), srcLineWidth);
                    end;

                    if unbiased
                    then
                        dest^ := RCMul( 1/Max(1, Height - 1), aVariance)
                    else
                        dest^ := RCMul(1/Height, aVariance);
               end
               else
                   dest^ := cCplxZero;

               inc(pVal1, sizeof(TComplex));
               inc(dest);
          end;
     end;
end;

procedure CplxGenericMtxMeanVar(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean; unbiased : boolean);
var x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstComplexArr;
    pVal : PComplex;
    pDest1 : PComplex;
    meanVal : TComplex;
    tmp : TComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert( not RowWise or (destLineWidth >= 2*sizeof(TComplex)), 'Results linewidth is too short');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               meanVal := cCplxZero;

               pVal2 := PConstComplexArr(pVal1);
               for x := 0 to Width - 1 do
                    CInc(meanVal, pVal2^[x]);

               if Width > 0 then
               begin
                    meanVal := RCMul(1/Width, meanVal);
                    dest^ := meanVal;
                    inc(dest);

                    dest^ := cCplxZero;
                    for x := 0 to Width - 1 do
                    begin
                         tmp := CSub(pVal2^[x], meanVal);
                         CInc(dest^, CMul(tmp, tmp) );
                    end;

                    if unbiased
                    then
                        dest^ := RCMul(1/Max(1, width - 1), dest^)
                    else
                        dest^ := RCMul(1/Width, dest^);

                    dec(dest);
               end
               else
               begin
                    dest^ := cCplxZero;
                    inc(dest);
                    dest^ := InitComplex(Infinity, Infinity);
                    dec(dest);
               end;

               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          pDest1 := dest;
          inc(PByte(pDest1), destLineWidth);

          for x := 0 to Width - 1 do
          begin
               meanVal := cCplxZero;

               pVal := PComplex(pVal1);
               for y := 0 to Height - 1 do
               begin
                    CInc(meanVal, pVal^);
                    inc(PByte(pVal), srcLineWidth);
               end;

               if Height > 0 then
               begin
                    meanVal := RCMul(1/Height, meanVal);
                    dest^ := meanVal;

                    pVal := PComplex(pVal1);
                    pDest1^ := cCplxZero;
                    for y := 0 to Height - 1 do
                    begin
                         tmp := CSub( pVal^, meanVal );
                         CInc(pDest1^, CMul(tmp, tmp));
                         inc(PByte(pVal), srcLineWidth);
                    end;

                    if unbiased
                    then
                        pDest1^ := RCMul(1/Max(1, height - 1), pDest1^)
                    else
                        pDest1^ := RCMul(1/Height, pDest1^);
               end
               else
               begin
                    dest^ := cCplxZero;
                    pDest1^ := InitComplex(Infinity, Infinity);
               end;

               inc(pVal1, sizeof(TComplex));
               inc(dest);
               inc(pDest1);
          end;
     end;
end;

procedure CplxGenericMtxSum(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt; RowWise : boolean);
var val : TComplex;
    x, y : NativeInt;
    pVal1 : PByte;
    pVal2 : PConstComplexArr;
    pVal : PComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     pVal1 := PByte(Src);
     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               val := cCplxZero;

               pVal2 := PConstComplexArr(pVal1);
               for x := 0 to Width - 1 do
                   CInc(val, pVal2^[x]);

               dest^ := val;
               inc(pVal1, srcLineWidth);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          for x := 0 to Width - 1 do
          begin
               val := cCplxZero;

               pVal := PComplex(pVal1);
               for y := 0 to Height - 1 do
               begin
                    CInc(val, pVal^);
                    inc(PByte(pVal), srcLineWidth);
               end;

               dest^ := val;
               inc(pVal1, sizeof(double));
               inc(dest);
          end;
     end;
end;

function CplxGenericMtxSumSum(Src : PComplex; srcLineWidth : NativeInt; width, height : NativeInt) : TComplex;
var x, y : NativeInt;
    pVal2 : PConstComplexArr;
    lineVal : TComplex;
begin
     assert((Width >= 0) and (Height >= 0), 'No data assigned');
     Result := cCplxZero;
     for y := 0 to Height - 1 do
     begin
          pVal2 := PConstComplexArr(src);
          lineVal := cCplxZero;
          for x := 0 to Width - 1 do
              CInc(lineVal, pVal2^[x]);

          CInc(Result, lineVal);

          inc(PByte(src), srcLineWidth);
     end;
end;



// ###########################################
// #### Sorting/median
// ###########################################

procedure CplxGenericMtxMedian(dest : PComplex; destLineWidth : NativeInt; Src : PComplex; srcLineWidth : NativeInt;
  width, height : NativeInt; RowWise : boolean; tmp : PComplex = nil);
var x, y : NativeInt;
    pVal1 : PByte;
    pVal : PComplex;
    tmpMem : PComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if (Width = 0) or (height = 0) then
        exit;

     tmpMem := tmp;
     
     pVal1 := PByte(Src);
     if RowWise then
     begin
          if tmp = nil then
             tmpMem := GetMemory(width*sizeof(TComplex));
          
          for y := 0 to Height - 1 do
          begin
               // copy since we destroy the array when calculating the median
               Move(pVal1^, tmpMem^, Width*sizeof(TComplex));
               
               if Width and 1 = 1 
               then
                   dest^ := CplxKthLargest(tmpMem, width, width div 2)
               else
                   dest^ := RCMul(0.5, CAdd( CplxKthLargest(tmpMem, width, width div 2), CplxKthLargest(tmpMem, width, Max(0, width div 2 - 1))));

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
               pVal := PComplex(pVal1);
               for y := 0 to Height - 1 do
               begin
                    PConstComplexArr(tmpMem)^[y] := pVal^;
                    inc(PByte(pVal), srcLineWidth);
               end;

               if height and 1 = 1 
               then
                   dest^ := CplxKthLargest(tmpMem, height, height div 2)
               else
                   dest^ := RCMul(0.5, CAdd(CplxKthLargest(tmpMem, height, height div 2),
                                       CplxKthLargest(tmpMem, height, Max(0, height div 2 - 1))));

               inc(pVal1, sizeof(TComplex));
               inc(dest);
          end;
     end;

     if tmp = nil then
        FreeMemory(tmpMem);
end;

procedure CplxGenericMtxSort(dest : PComplex; destLineWidth : NativeInt; width, height : integer; RowWise : boolean; tmp : PComplex = nil);
var x, y : NativeInt;
    pDest : PComplex;
    tmpMem : PComplex;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if (Width = 0) or (height = 0) then
        exit;

     if RowWise then
     begin
          for y := 0 to Height - 1 do
          begin
               CplxQuickSort(PConstComplexArr(Dest), width);
               inc(PByte(dest), destLineWidth);
          end;
     end
     else
     begin
          tmpMem := tmp;
          if tmp = nil then
             tmpMem := GetMemory(height*sizeof(TComplex));

          for x := 0 to Width - 1 do
          begin
               pDest := dest;

               for y := 0 to Height - 1 do
               begin
                    PConstComplexArr(tmpMem)^[y] := pDest^;
                    inc(PByte(pDest), destLineWidth);
               end;

               CplxQuickSort(PConstComplexArr(tmpMem), height);

               pDest := dest;
               for y := 0 to Height - 1 do
               begin
                    pDest^ := PConstComplexArr(tmpMem)^[y];
                    inc(PByte(pDest), destLineWidth);
               end;

               inc(dest);
          end;

          if tmp = nil then
             FreeMemory(tmpMem);
     end;
end;


procedure CplxGenericMtxRollMedian( dest : PComplex; const destLineWidth : NativeInt; Width, Height : NativeInt; order : integer; rowWise : boolean);
var rollMed : TCplxRollingMedian;
    i : Integer;
begin
     if (width <= 0) or (height <= 0) then
        exit;
     rollMed := TCplxRollingMedian.Create(order);
     try
        if RowWise then
        begin
             for i := 0 to Height - 1 do
             begin
                  rollMed.FilterVals(dest, sizeof(double), Width);
                  rollMed.Clear;
                  inc(PByte(dest), destLineWidth);
             end;
        end
        else
        begin
             for i := 0 to Width - 1 do
             begin
                  rollMed.FilterVals(dest, destLineWidth, Height);
                  rollMed.Clear;
                  inc(dest);
             end;
        end;
     finally
            rollMed.Free;
     end;
end;

{$ENDIF}

end.
