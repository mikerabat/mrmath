// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2020, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit Roots;

// ###########################################
// #### function root finding methods for f(x) = 0
// ###########################################

interface

uses SysUtils, Types, MatrixConst, Matrix;

const cRootTol = 3e-8;
      cPolyRootTol = 2e-6;
      cRootMaxIter = 100;
      cMaxNumRoot = 100;
type
  TRootFunc = function ( const x : double ) : double;
  TRootFuncObj = function ( const x : double ) : double of object;
  {$IFNDEF FPC}
  TRootFuncRef = reference to function ( const x : double ) : double;
  {$ENDIF}

  TRootFunc2 = procedure (const x : double; var y, dydx : double);
  TRootFunc2Obj = procedure (const x : double; var y, dydx : double) of object;
  {$IFNDEF FPC}
  TRootFunc2Ref = reference to procedure (const x : double; var y, dydx : double);
  {$ENDIF}

  TRootRes = (rrOk, rrNoRoot, rrNoConverge );


// find a root ( f(x) = 0 ) in the given interval x0 - x1 with a given tolerance using
// "Brents" method
// if a root was found rrOk is returned and rootX got the x value of the root
// the function iterates at a maximum of 100 iterations
function FindRootBrent( func : TRootFunc; x0, x1 : double; out rootX : double;
  tol : double = cRootTol; numIter : integer = cRootMaxIter ) : TRootRes;

// find a root (f(x) = 0) in the given interval x0 - x1 and tolerance tol using
// newton raphsons method which combines derivaites and bisectioning.
// note here we need to evaluate the function at x and it's derivative at this position!
function FindRootNewtonRaphson( func : TRootFunc2; x0, x1 : double; out rootX : double;
  tol : double = cRootTol; numIter : integer = cRootMaxIter ) : TRootRes;

// ###########################################
// #### roots of polynomals

// these are generally of complex type so functions here use complex numbers

// given the degree aLen complex coefficients a[0..aLen - 1] of the polynomial sum_i=0_aLen-1 a(a_len - 1 - i)*x^i
// the routine successively calls laguerre's method and finds all aLen - 1 roots in "roots". The routine
// supports polishing the complex numbers (also by laguerre's method)
// note: the highest degree coefficient is at index 0 - the same as in PolyRootEig
function PolyRootLaguer( a : PConstComplexArr; aLen : integer; roots : PConstComplexArr;
  polish : boolean = True; maxIter : Integer = cRootMaxIter; tol : double = cPolyRootTol) : TRootRes;


// root of polynomials using the eigenvalue method. This method is the backup method if PolyRootLaguer
// does not work as expcected. This routine works only for real valued polynoms.
// this routine is around 2 times slower than the Laguer method but can handle bad
// behaving polynoms better
function PolyRootEig( a : PConstDoubleArr; aLen : integer; root : PConstComplexArr ) : TRootRes;


// ###########################################
// #### Class implementations of root finding methods
type
  TPolyRootMethod = (rmEigenvalue, rmLaguer);
  TPolyRoot = class(TObject)
  public
    class var tolerance : double;
    class var maxIter : integer;
  public
    // implements root finding for polynoms as  a(1) * x^(N-1) + ... + a(N-1) * x + a(N).
    // before executing the routine the matrix a is vectorized (row major)
    // the resulting matrix "roots" is a width=2, height=n matrix in case there are complex roots.
    // otherwise the second column is stripped only one vector is returned.
    class function PolyRoot( a : TDoubleMatrix; out roots : TDoubleMatrix; method : TPolyRootMethod = rmEigenvalue) : TRootRes; overload;
    class function PolyRoot( a : IMatrix; out roots : IMatrix; method : TPolyRootMethod = rmEigenvalue) : TRootRes; overload;
  end;

type
  TFunctionRoot = class(TObject)
  private
    fRootFunc1_1 : TRootFunc;
    fRootFunc1_2 : TRootFuncObj;
    {$IFNDEF FPC}
    fRootFunc1_3 : TRootFuncRef;
    {$ENDIF}
    fBrentExec : TRootFuncObj;     // point to the function that actually executes the given function type
                                   // ExecFuncBrent1 -> fRootFunc1_1
                                   // ExecFuncBrent2 -> fRootFunc1_2
                                   // ExecFuncBrent3 -> fRootFunc1_3
    fRootFunc2_1 : TRootFunc2;
    fRootFunc2_2 : TRootFunc2Obj;
    {$IFNDEF FPC}
    fRootFunc2_3 : TRootFunc2Ref;
    {$ENDIF}
    fNewtonRaphsonExec : TRootFunc2Obj; // point to the function that actually executes the given function type


    fMaxIter : integer;
    fTol : double;

    function ExecFuncBrent1( const x : double ) : double;
    function ExecFuncBrent2( const x : double ) : double;
    {$IFNDEF FPC}
    function ExecFuncBrent3( const x : double ) : double;
    {$ENDIF}

    procedure ExecFuncNewtonRaphson1( const x : double; var y, dydx : double);
    procedure ExecFuncNewtonRaphson2( const x : double; var y, dydx : double);
    {$IFNDEF FPC}
    procedure ExecFuncNewtonRaphson3( const x : double; var y, dydx : double);
    {$ENDIF}

    function InternalFindRootBrent( x0, x1 : double; out rootX : double ) : TRootRes;
    function InternalFindRootNewtonRaphson( x0, x1 : double; out rootX : double ) : TRootRes;
  public
    class var tolerance : double;
    class var maxIter : integer;
  public
    constructor Create;

    class function FindRootBrent( func : TRootFunc; x0, x1 : double; out rootX : double ) : TRootRes; overload;
    class function FindRootBrent( func : TRootFuncObj; x0, x1 : double; out rootX : double ) : TRootRes; overload;
    {$IFNDEF FPC}
    class function FindRootBrent( func : TRootFuncRef; x0, x1 : double; out rootX : double ) : TRootRes; overload;
    {$ENDIF}

    class function FindRootNewtonRaphson( func : TRootFunc2; x0, x1 : double; out rootX : double) : TRootRes; overload;
    class function FindRootNewtonRaphson( func : TRootFunc2Obj; x0, x1 : double; out rootX : double) : TRootRes; overload;
    {$IFNDEF FPC}
    class function FindRootNewtonRaphson( func : TRootFunc2Ref; x0, x1 : double; out rootX : double) : TRootRes; overload;
    {$ENDIF}
  end;

implementation

uses Math, MathUtilFunc;

// ###########################################
// #### Auxilary functions
// ###########################################

// eigenvalue sort. i1, i2 are expected to be of TComplex
// -> sort according to absolute value (real part) and high to low in imaginary part
function eigValSort( const i1, i2 ) : integer;
begin
     // sort according the real value
     Result := CompareValue( Abs( PComplex(@i2)^.real ), Abs( PComplex(@i1)^.real),
                             cPolyRootTol*(max(Abs( PComplex(@i2)^.real ), Abs( PComplex(@i1)^.real))));

     // imaginary part sort from high to low
     if Result = 0 then
        Result := CompareValue( PComplex(@i2)^.imag, PComplex(@i1)^.imag );
end;


// ###########################################
// #### Root finding
// ###########################################

function FindRootBrent( func : TRootFunc; x0, x1 : double; out rootX : double; tol : double = cRootTol; numIter : integer = cRootMaxIter ) : TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fTol := tol;
        fMaxIter := numIter;

        fRootFunc1_1 := func;
        fBrentExec := {$IFDEF FPC}@{$ENDIF}ExecFuncBrent1;

        Result := InternalFindRootBrent(x0, x1, rootX);
     finally
            Free;
     end;
end;

function FindRootNewtonRaphson( func : TRootFunc2; x0, x1 : double; out rootX : double;
  tol : double = cRootTol; numIter : integer = cRootMaxIter ) : TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fTol := tol;
        fMaxIter := maxIter;
        fRootFunc2_1 := func;
        fNewtonRaphsonExec := {$IFDEF FPC}@{$ENDIF}ExecFuncNewtonRaphson1;

        Result := InternalFindRootNewtonRaphson(x0, x1, rootX);
     finally
            Free;
     end;
end;

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


function CMul(const x1, x2 : TComplex ) : TComplex;
begin
     Result.real := x1.real*x2.real - x1.imag*x2.imag;
     Result.imag := x1.real*x2.imag + x1.imag*x2.real;
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

function RCMul(x1 : double; x2 : TComplex ) : TComplex;
begin
     Result.real := x1*x2.real;
     Result.imag := x1*x2.imag;
end;

function CSqrt( const x : TComplex ) : TComplex;
var a, b : double;
    r, w : double;
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
          w := sqrt(Max(a, b))*sqrt(0.5*(1 + sqrt(1 + sqr(r))));

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

// ###########################################
// #### Polynom root finding
// ###########################################

// find one root for the polynomial a ( sum_i=0_aLen - 1 a[i]*x^i) and given initial value x.
// the routine tries to improve x by Laguerre's method until it converges. The number of iterations
// it took is returned in iters
function Laguer( a : PConstComplexArr; aLen : integer; var x : TComplex; tol : double; maxIter : integer;
                 var iters : integer ) : TRootRes;
var iter, j : integer;
    abx, abp, abm, err : double;
    m : integer;
    dx, x1, b, d, f, g, h, sq, gp, gm, g2 : TComplex;
const cFrac : Array[0..8] of double = (0, 0.5, 0.25, 0.75, 0.13, 0.38, 0.62, 0.88, 1);
      cEPSS : double = 1e-7;
begin
     Result := rrOk;
     m := aLen - 1;
     for iter := 0 to maxIter - 1 do
     begin
          iters := iter + 1;

          b := a^[m];
          err := Cabs(b);
          f := InitComplex(0, 0);
          d := f;
          abx := CAbs(x);

          for j := m - 1 downto 0 do
          begin
               f := Cadd( CMul(x, f), d );
               d := CAdd( CMul(x, d), b );
               b := Cadd( CMul(x, b), a^[j] );
               err := Cabs(b) + abx*err;
          end;

          err := err*cEPSS;

          // estimate of roundoff error in evaluating polynomial
          if CAbs(b) <= err then
             exit;

          g := CDiv(d, b);
          g2 := CMul(g, g);
          h := CSub( g2, RCMul(2, CDiv(f, b)));
          sq := CSQRT( RCMul( (m - 1), CSub( RCMul(m, h), g2)));
          gp := CAdd(g, sq);
          gm := CSub(g, sq);
          abp := Cabs(gp);
          abm := CAbs(gm);

          if abp < abm then
             gp := gm;

          if max(abp, abm) > 0
          then
              dx := CDiv( InitComplex(m, 0), gp )
          else
              dx := RCmul( 1 + abx, InitComplex(cos(iters), sin(iters)));

          x1 := CSub(x, dx);

          if SameValue(x.real, x1.real, tol) and SameValue(x.imag, x1.imag, tol) then
             exit;

          if iters mod ( length(cFrac) + 1) <> 0
          then
              x := x1
          else
              x := CSub(x, RCMul( cFrac[ (iter div length(cFrac)) mod (length(cFrac)) ], dx)); // every now and then we try to break the limit cycle by the fractional step
     end;

     Result := rrNoConverge;
end;

function PolyRootLaguer( a : PConstComplexArr; aLen : integer; roots : PConstComplexArr;
  polish : boolean = True; maxIter : Integer = cRootMaxIter; tol : double = cPolyRootTol) : TRootRes;
var its, j, jj : integer;
    x, b, c : TComplex;
    ad : Array[0..cMaxNumRoot - 1] of TComplex;
begin
     Result := rrNoRoot;
     if aLen > cMaxNumRoot then
        exit;

     // the polynom coefficients are reverse to that what Laguer expects:
     for j := 0 to aLen - 1 do
         ad[j] := a^[aLen - 1 - j];

     for j := aLen - 1 downto 1 do
     begin
          x := InitComplex(0, 0);
          Result := laguer( PConstComplexArr( @ad[0] ), j + 1, x, tol, maxIter, its );

          if Result <> rrOk then
             exit;

          if abs(x.imag) <= abs(tol*2*x.real) then
             x.imag := 0;

          roots^[j - 1] := x;

          // forward deflation
          b := ad[j];
          for jj := j - 1 downto 0 do
          begin
               c := ad[jj];
               ad[jj] := b;
               b := CAdd( CMul(x, b), c);
          end;
     end;

     // ###########################################
     // #### Polish
     if polish then
     begin
          // the polynom coefficients are reverse to that what Laguer expects:
          for j := 0 to aLen - 1 do
              ad[j] := a^[aLen - 1 - j];

          for j := 0 to aLen - 2 do
          begin
               laguer(PConstComplexArr( @ad[0] ), aLen, roots^[j], tol, maxIter, its);
               if abs(roots^[j].imag) <= abs(tol*2*roots^[j].real) then
                  roots^[j].imag := 0;

          end;
     end;

     // ###########################################
     // #### Sort by real parts
     QuickSort( roots^[0], sizeof(TComplex), aLen - 1, {$IFDEF FPC}@{$ENDIF}eigValSort);
end;

function PolyRootEig( a : PConstDoubleArr; aLen : integer; root : PConstComplexArr ) : TRootRes;
var polyMtx : IMatrix;
    eigVals : IMatrix;
    i : Integer;
begin
     polyMtx := TDoubleMatrix.Create(aLen - 1, aLen - 1);
     for i := 0 to alen - 2 do
         polyMtx[i, 0] := -a^[i + 1]/a^[0];

     for i := 0 to aLen - 3 do
         polyMtx[i, i + 1] := 1;

     // ###########################################
     // #### Eigenvalues of this matrix are the polynom roots
     if polyMtx.Eig( eigVals ) = qlOk then
     begin
          Result := rrOk;

          QuickSort( eigVals.StartElement^, eigVals.LineWidth, eigVals.Height, {$IFDEF FPC}@{$ENDIF}eigValSort );

          for i := 0 to eigVals.Height - 1 do
          begin
               root^[i].real := eigVals[0, i];
               root^[i].imag := eigVals[1, i];
          end;
     end
     else
         Result := rrNoConverge;
end;



// ###########################################
// #### class implementations
// ###########################################

{ TPolyRoot }

class function TPolyRoot.PolyRoot(a: TDoubleMatrix; out roots: TDoubleMatrix;
  method: TPolyRootMethod): TRootRes;
var c : TDoubleDynArray;
    cc : TComplexDynArray;
    r : TComplexDynArray;
    i: Integer;
    hasComplex : boolean;
begin
     c := a.SubMatrix;
     roots := nil;

     SetLength(r, length(c) - 1);

     if method = rmEigenvalue then
     begin
          Result := PolyRootEig( @c[0], Length(c), @r[0] );
     end
     else
     begin
          SetLength(cc, Length(c));
          for i := 0 to Length(cc) - 1 do
              cc[i] := InitComplex(c[i], 0);

          Result := PolyRootLaguer( @cc[0], Length(cc), @r[0], True,
                                    TPolyRoot.maxIter, TPolyRoot.tolerance );
     end;

     // ###########################################
     // #### build result
     if Result = rrOk then
     begin
          // check if there are is any complex root:
          hasComplex := False;
          for i := 0 to Length(r) - 1 do
              hasComplex := hasComplex or ( r[i].imag <> 0 );

          if hasComplex then
          begin
               roots := TDoubleMatrix.Create( 2, Length(r) );
               for i := 0 to Length(r) - 1 do
               begin
                    roots[0, i] := r[i].real;
                    roots[1, i] := r[i].imag;
               end;
          end
          else
          begin
               roots := TDoubleMatrix.Create( 1, Length(r) );
               for i := 0 to Length(r) - 1 do
                   roots[0, i] := r[i].real;
          end;
     end;
end;

class function TPolyRoot.PolyRoot(a: IMatrix; out roots: IMatrix;
  method: TPolyRootMethod): TRootRes;
var aroots : TDoubleMatrix;
begin
     Result := PolyRoot( a.GetObjRef, aRoots, method);
     roots := aRoots;
end;

{ TFunctionRoot }

constructor TFunctionRoot.Create;
begin
     fMaxIter := TFunctionRoot.maxIter;
     fTol := TFunctionRoot.tolerance;

     inherited Create;
end;

function TFunctionRoot.ExecFuncBrent1(const x: double): double;
begin
     Result := fRootFunc1_1(x);
end;

function TFunctionRoot.ExecFuncBrent2(const x: double): double;
begin
     Result := fRootFunc1_2(x);
end;

{$IFNDEF FPC}
function TFunctionRoot.ExecFuncBrent3(const x: double): double;
begin
     Result := fRootFunc1_3(x);
end;
{$ENDIF}

procedure TFunctionRoot.ExecFuncNewtonRaphson1(const x: double; var y,
  dydx: double);
begin
     fRootFunc2_1(x, y, dydx);
end;

procedure TFunctionRoot.ExecFuncNewtonRaphson2(const x: double; var y,
  dydx: double);
begin
     fRootFunc2_2(x, y, dydx);
end;

{$IFNDEF FPC}
procedure TFunctionRoot.ExecFuncNewtonRaphson3(const x: double; var y,
  dydx: double);
begin
     fRootFunc2_3(x, y, dydx);
end;
{$ENDIF}

class function TFunctionRoot.FindRootBrent(func: TRootFunc; x0, x1: double;
  out rootX: double): TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fRootFunc1_1 := func;
        fBrentExec := {$IFDEF FPC}@{$ENDIF}ExecFuncBrent1;

        Result := InternalFindRootBrent(x0, x1, rootX);
     finally
            Free;
     end;
end;

{$IFNDEF FPC}
class function TFunctionRoot.FindRootBrent(func: TRootFuncRef; x0, x1: double;
  out rootX: double): TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fRootFunc1_3 := func;
        fBrentExec := {$IFDEF FPC}@{$ENDIF}ExecFuncBrent3;

        Result := InternalFindRootBrent(x0, x1, rootX);
     finally
            Free;
     end;
end;
{$ENDIF}

class function TFunctionRoot.FindRootBrent(func: TRootFuncObj; x0, x1: double;
  out rootX: double): TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fRootFunc1_2 := func;
        fBrentExec := {$IFDEF FPC}@{$ENDIF}ExecFuncBrent2;

        Result := InternalFindRootBrent(x0, x1, rootX);
     finally
            Free;
     end;
end;

function TFunctionRoot.InternalFindRootBrent(x0, x1: double;
  out rootX: double): TRootRes;
var iter : integer;
    a, b, c : double;
    d,e, min1, min2 : double;
    fa, fb, fc : double;
    p, q, r, s, tol1, xm : double;
begin
     rootX := 0;

     a := x0;
     b := x1;
     c := x1;

     d := b - a;
     e := d;

     fa := fBrentExec(x0);
     fb := fBrentExec(x1);

     Result := rrNoRoot;
     // check if there is a root between x0, x1
     if ((fa > 0) and (fb > 0)) or ((fa < 0) and (fb < 0)) then
        exit;

     fc := fb;
     for iter := 0 to fmaxIter - 1 do
     begin
          if ((fb > 0) and (fc > 0)) or ((fb < 0) and (fc < 0)) then
          begin
               c := a;           // rename a, b, c and adjust bounding interval d
               fc := fa;
               d := b - a;
               e := d;
          end;
          if abs(fc) < abs(fb) then
          begin
               a := b;
               b := c;
               c := a;
               fa := fb;
               fb := fc;
               fc := fa;
          end;

          // ###########################################
          // #### Convergence check
          tol1 := 2*cRootTol*abs(b) + 0.5*fTol;
          xm := 0.5*(c - b);
          if (abs(xm) <= tol1) or SameValue(fb, 0, tol1) then
          begin
               Result := rrOk;
               rootX := b;
               exit;
          end;

          // ###########################################
          // #### Attempt inverse quadratic interpolation
          if (abs(e) >= tol1) and (abs(fa) > abs(fb)) then
          begin
               s := fb/fa;
               if (a = c) then
               begin
                    p := 2*xm*s;
                    q := 1 - s;
               end
               else
               begin
                    q := fa/fc;
                    r := fb/fc;
                    p := s*(2*xm*q*(q - r) - (b - a)*(r - 1));
                    q := (q - 1)*(r - 1)*(s - 1);
               end;

               // check if in bounds

               if p > 0 then
                  q := -q;
               p := abs(p);
               min1 := 3*xm*q - abs(tol1*q);
               min2 := abs(e*q);

               if 2*p < min( min1, min2 ) then
               begin
                    // accept interpolation
                    e := d;
                    d := p/q;
               end
               else
               begin
                    // ###########################################
                    // #### interpolation failed - use bisection
                    d := xm;
                    e := d;
               end;
          end
          else
          begin
               // ###########################################
               // #### bounds decrease too slow - use bisection
               d := xm;
               e := d;
          end;

          // ###########################################
          // #### move to next best guess
          a := b;
          fa := fb;
          if abs(d) > tol1
          then
              b := b + d
          else
              b := sign(tol1, xm);

          fb := fBrentExec(b);
     end;

     Result := rrNoConverge;
end;


function TFunctionRoot.InternalFindRootNewtonRaphson(x0, x1: double;
  out rootX: double): TRootRes;
var df, dx, dxold : double;
    f, fh, fl : double;
    temp, xh, xl, rts : double;
    j: Integer;
begin
     // ###########################################
     // #### get the initial values
     fNewtonRaphsonExec( x0, fl, df );
     fNewtonRaphsonExec( x1, fh, df );

     Result := rrNoRoot;
     // check if there is a root crossing
     if ((fl > 0) and (fh > 0)) or ((fl < 0) and (fh < 0)) then
        exit;

     Result := rrOk;
     if (fl = 0) then
     begin
          rootX := fl;
          exit;
     end;
     if (fh = 0) then
     begin
          rootX := fh;
          exit;
     end;

     if fl < 0 then
     begin
          xl := x0;
          xh := x1;
     end
     else
     begin
          xl := x1;
          xh := x0;
     end;

     // ###########################################
     // #### Init values
     rts := 0.5*(x0 + x1);
     dxold := abs(x1 - x0);
     dx := dxold;

     fNewtonRaphsonExec(rts, f, df);
     // ###########################################
     // #### now loop over to find the root
     for j := 0 to fMaxIter - 1 do
     begin
          if ( ((rts - xh)*df - f)*((rts - xl)*df - f) > 0) or
               (abs(2*f) > abs(dxold*df))
          then
          begin
               dxold := dx;
               dx := 0.5*(xh - xl);
               rts := xl + dx;
               // check if change in root is neglibible.
               if SameValue( xl, rts) then
               begin
                    rootX := rts;
                    exit;
               end;
          end
          else
          begin
               dxold := dx;
               dx := f/df;
               temp := rts;
               rts := rts - dx;
               if SameValue(temp, rts) then
               begin
                    rootX := rts;
                    exit;
               end;
          end;

          // ###########################################
          // #### Check for convergence
          if abs(dx) < fTol then
          begin
               rootX := rts;
               exit;
          end;

          // ###########################################
          // #### next step
          fNewtonRaphsonExec(rts, f, df);

          if f < 0
          then
              xl := rts
          else
              xh := rts;
     end;

     // ###########################################
     // #### Maximum number of iterations excceded
     Result := rrNoConverge;
end;


class function TFunctionRoot.FindRootNewtonRaphson(func: TRootFunc2; x0,
  x1: double; out rootX: double): TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fRootFunc2_1 := func;
        fNewtonRaphsonExec := {$IFDEF FPC}@{$ENDIF}ExecFuncNewtonRaphson1;

        Result := InternalFindRootNewtonRaphson(x0, x1, rootX);
     finally
            Free;
     end;
end;

class function TFunctionRoot.FindRootNewtonRaphson(func: TRootFunc2Obj; x0,
  x1: double; out rootX: double): TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fRootFunc2_2 := func;
        fNewtonRaphsonExec := {$IFDEF FPC}@{$ENDIF}ExecFuncNewtonRaphson2;

        Result := InternalFindRootNewtonRaphson(x0, x1, rootX);
     finally
            Free;
     end;
end;

{$IFNDEF FPC}
class function TFunctionRoot.FindRootNewtonRaphson(func: TRootFunc2Ref; x0,
  x1: double; out rootX: double): TRootRes;
begin
     with TFunctionRoot.Create do
     try
        fRootFunc2_3 := func;
        fNewtonRaphsonExec := {$IFDEF FPC}@{$ENDIF}ExecFuncNewtonRaphson3;

        Result := InternalFindRootNewtonRaphson(x0, x1, rootX);
     finally
            Free;
     end;
end;
{$ENDIF}

initialization
  TPolyRoot.tolerance := cPolyRootTol;
  TPolyRoot.maxIter := cRootMaxIter;

  TFunctionRoot.tolerance := cRootTol;
  TFunctionRoot.maxIter := cRootMaxIter;

end.
