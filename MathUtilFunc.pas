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


unit MathUtilFunc;

// ############################################
// #### Utility functions for math utils library
// ############################################

interface

uses Types, MatrixConst, SysUtils;

{$WRITEABLECONST ON}

const cDoubleEpsilon : double = 2.2204460492503131e-016;  // smallest such that 1.0+DBL_EPSILON != 1.0
const cMinDblDivEps : double = 0;     // filled in initialization

function pythag(const A, B : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function sign(const a : double; const b : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
procedure DoubleSwap(var a, b : Double); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

function lcm(a, b : NativeInt) : NativeInt;  // least common multiple
function gcm(a, b : NativeInt) : NativeInt; // greatest common divisior

function Next2Pwr(num : NativeInt; maxSize : NativeInt) : NativeInt;

function eps(const val : double) : double;
function MinDblDiv : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

procedure WriteMtlMtx(const fileName : string; const mtx : TDoubleDynArray; width : integer; prec : integer = 8); overload;
procedure WriteMtlMtx(const fileName : string; mtx : PDouble; LineWidthmtx : NativeInt; width, height : integer; prec : integer = 8); overload;
procedure WriteBinaryMtxColMajor( fn : String; A : PDouble; LineWidthA : NativeInt; w, h  :integer );


type
  TQuickSortFunc = function(const Item1, Item2) : integer;


function DoubleSortFunc(const Item1, Item2) : integer;
procedure QuickSort(var A; ItemSize : integer; ItemCount : integer; ItemSort : TQuickSortFunc); overload;
procedure QuickSort(var A: array of double; StartIdx : integer = 0; EndIdx : integer = -1); overload;
procedure QuickSort(A : PConstDoubleArr; Width : integer); overload;
procedure QuickSort1(var A : TDoubleDynArray); 

// sorts A in ascending order -> making corresponding changes to B
procedure QuickSort2( var A, B : TDoubleDynArray );

// for median - note: the content (sort order) of the array is destroyed!
function KthLargest(var vals : TDoubleDynArray; elemNum : Cardinal) : double; overload;
function KthLargest(valsArr : PDouble; numElem : integer; elemNum : Cardinal) : double; overload;


// exponential integrals (En(x) = int_1_oo e^(-xt)/t^n dt ... x > 0, n = 0, 1, ....
function expInt( n : integer; x : double ) : double;
function ei( x : double ) : double; // Ei(x) + gamma + lnx + x/(1*1!) + x^2/(2*2!) + ...

function GetLocalFMTSet : TFormatSettings;

function Arr( const elements : Array of integer ) : TIntegerDynArray;

// ###########################################
// #### prime stuff
function MillerRubinTest32( n : UInt32; a : UInt32 ) : boolean;

// ###########################################
// #### root finding
// another dive into numerical recipies

// root finding for a function y = f(x) using the modified Newton-Raphsons method
// combined with a bisection step to make it more stabel.
// The hybrid algorithm takes a bisection step whenever Newton-Raphos would take the solution out
// of bounds, or whenever it is not reducing the size of the brackets rapidly enough.
// if return value is true the x value is returned in "root". The maximum deviation from
// the real value can be defined by "xAcc".

// if not derrivative can be provided the root is found by Brents method
type
  TRootFuncWithDerrive = procedure( x : double; var y, dy : double);
  TRootFunc = function( x : double ): double;

function findRoot( func : TRootFunc; out root : double; x1, x2 : double; xAcc : double = 1e-8 ) : boolean; overload;
function findRoot( func : TRootFuncWithDerrive; out root : double; x1, x2 : double; xAcc : double = 1e-8 ) : boolean; overload;

implementation

uses Math, Classes;

// ##########################################
// #### utility function implementation
// ##########################################

function Arr( const elements : Array of integer ) : TIntegerDynArray;
var i: Integer;
begin
     SetLength(Result, Length(elements));

     for i := 0 to Length(elements) - 1 do
         Result[i] := elements[i];
end;

procedure DoubleSwap(var a, b : Double); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
var temp : double;
begin
     temp := a;
     a := b;
     b := temp;
end;

function DoubleSortFunc(const Item1, Item2) : integer;
begin
     Result := CompareValue(PDouble(@Item1)^, PDouble(@Item2)^);
end;

procedure recQs(Lo, hi : integer; A : PByteArray; ItemSize : Integer; ItemCount : integer; ItemSort : TQuickSortFunc; var P, help : Array of Byte);
var I, J: Integer;
begin
     // quick sort implementation of for double values
     repeat
           I := Lo;
           J := Hi;

           Move(A^[ItemSize*((Lo + Hi) div 2)], P[0], ItemSize);
           repeat
                 while ItemSort(A^[ItemSize*I], P[0]) < 0 do
                       Inc(I);
                 while ItemSort(A^[ItemSize*J], P[0]) > 0 do
                       Dec(J);
                 if I <= J then
                 begin
                      Move(A^[ItemSize*I], Help[0], ItemSize);
                      Move(A^[ItemSize*J], A^[ItemSize*I], ItemSize);
                      Move(Help[0], A^[ItemSize*J], ItemSize);

                      Inc(I);
                      Dec(J);
                 end;
           until I > J;

           if Lo < J then
              recQs(Lo, J, A, ItemSize, ItemCount, ItemSort, P, Help);
           Lo := I;
     until I >= Hi;
end;

procedure QuickSort(var A; ItemSize : integer; ItemCount : integer; ItemSort : TQuickSortFunc);
var P : Array of byte;
    Help : Array of Byte;
begin
     if (ItemCount <= 0) or (ItemSize <= 0) then
        exit;

     SetLength(P, ItemSize);
     SetLength(Help, ItemSize);
     recQs(0, ItemCount - 1, @A, ItemSize, ItemCount, ItemSort, P, Help);
end;



procedure QuickSort(var A: array of double; StartIdx : integer = 0; EndIdx : integer = -1);
procedure QS(var A: array of double; iLo, iHi: Integer);
var Lo, Hi : Integer;
    MidVal, T: double;
begin
     Lo := iLo;
     Hi := iHi;
     MidVal := A[(Lo + Hi) div 2];
     repeat
           while A[Lo] < MidVal do Inc(Lo);
           while A[Hi] > MidVal do Dec(Hi);
           if Lo <= Hi then
           begin
                // Swap values
                T := A[Lo];
                A[Lo] := A[Hi];
                A[Hi] := T;
                Inc(Lo);
                Dec(Hi);
           end;
    until Lo > Hi;

    if Hi > iLo then QS(A, iLo, Hi);
    if Lo < iHi then QS(A, Lo, iHi);
end;

begin
     if EndIdx = -1 then
        EndIdx := High(A);

     if (startIdx >= endIdx) or (Length(A) = 0) then
        exit;

     QS(A, startIdx, EndIdx);
end;

procedure QuickSort(A : PConstDoubleArr; Width : integer); overload;
procedure QS(A: PConstDoubleArr; iLo, iHi: Integer);
var Lo, Hi : Integer;
    MidVal, T: double;
begin
     Lo := iLo;
     Hi := iHi;
     MidVal := A^[(Lo + Hi) div 2];
     repeat
           while A^[Lo] < MidVal do Inc(Lo);
           while A^[Hi] > MidVal do Dec(Hi);
           if Lo <= Hi then
           begin
                // Swap values
                T := A^[Lo];
                A^[Lo] := A^[Hi];
                A^[Hi] := T;
                Inc(Lo);
                Dec(Hi);
           end;
    until Lo > Hi;

    if Hi > iLo then QS(A, iLo, Hi);
    if Lo < iHi then QS(A, Lo, iHi);
end;
begin
     if width <= 1 then
        exit;

     QS(A, 0, width - 1);
end;

procedure QuickSort1(var A : TDoubleDynArray);
var iStack : Array[0..50] of integer;
    i, ir, j, k, l : integer;
    jstack : integer;
    n : integer;
    am : Double;
const cInsertionTurnOver = 7;
begin
     n := Length(A);

     l := 0;
     ir := n - 1;
     jstack := 0;

     while true do
     begin
          if ir - l < cInsertionTurnOver then
          begin
               for j := l + 1 to ir do
               begin
                    am := a[j];
                    i := j - 1;
                    while i >= l do
                    begin
                         if a[i] <= am then
                            break;

                         a[i + 1] := a[i];
                         dec(i);
                    end;
                    a[i + 1] := am;
               end;

               if jstack = 0 then
                  exit;

               ir := istack[jstack];
               l := istack[jstack - 1];
               dec(jstack, 2);
          end
          else
          begin
               k := (l + ir) div 2;
               DoubleSwap(a[k], a[l + 1]);

               if a[l] > a[ir] then
                  DoubleSwap(a[l], a[ir]);
               if a[l + 1] > a[ir] then
                  DoubleSwap(a[l + 1], a[ir]);
               if a[l] > a[l + 1] then
                  DoubleSwap(a[l], a[l + 1]);

               i := l + 1;
               j := ir;
               am := a[l + 1];
               while True do
               begin
                    repeat
                          inc(i);
                    until a[i] >= am;
                    repeat
                          dec(j);
                    until a[j] <= am;

                    if (j < i) then
                       break;

                    DoubleSwap(a[i], a[j]);
               end;

               a[l + 1] := a[j];
               a[j] := am;
               inc(jstack, 2);

               // push pointers to larger subarray on stack
               if jStack >= Length(iStack) then
                  raise Exception.Create('Stack too small to sort');

               if (ir - i + 1 >= j - 1) then
               begin
                    istack[jstack] := ir;
                    istack[jstack - 1] := i;
                    ir := j - 1;
               end
               else
               begin
                    istack[jstack] := j - 1;
                    istack[jstack - 1] := l;
                    l := i;
               end;
          end;
     end;
end;

procedure QuickSort2( var A, B : TDoubleDynArray );
var iStack : Array[0..50] of integer;
    i, ir, j, k, l : integer;
    jstack : integer;
    n : integer;
    am, bm : Double;
const cInsertionTurnOver = 7;
begin
     n := Length(A);

     l := 0;
     ir := n - 1;
     jstack := 0;

     while true do
     begin
          if ir - l < cInsertionTurnOver then
          begin
               for j := l + 1 to ir do
               begin
                    am := a[j];
                    bm := b[j];
                    i := j - 1;
                    while i >= l do
                    begin
                         if a[i] <= am then
                            break;

                         a[i + 1] := a[i];
                         b[i + 1] := b[i];
                         dec(i);
                    end;
                    a[i + 1] := am;
                    b[i + 1] := bm;
               end;

               if jstack = 0 then
                  exit;

               ir := istack[jstack];
               l := istack[jstack - 1];
               dec(jstack, 2);
          end
          else
          begin
               k := (l + ir) div 2;
               DoubleSwap(a[k], a[l + 1]);
               DoubleSwap(b[k], b[l + 1]);

               if a[l] > a[ir] then
               begin
                    DoubleSwap(a[l], a[ir]);
                    DoubleSwap(b[l], b[ir]);
               end;
               if a[l + 1] > a[ir] then
               begin
                    DoubleSwap(a[l + 1], a[ir]);
                    DoubleSwap(b[l + 1], b[ir]);
               end;
               if a[l] > a[l + 1] then
               begin
                    DoubleSwap(a[l], a[l + 1]);
                    DoubleSwap(b[l], b[l + 1]);
               end;

               i := l + 1;
               j := ir;
               am := a[l + 1];
               bm := b[l + 1];
               while True do
               begin
                    repeat
                          inc(i);
                    until a[i] >= am;
                    repeat
                          dec(j);
                    until a[j] <= am;

                    if (j < i) then
                       break;

                    DoubleSwap(a[i], a[j]);
                    DoubleSwap(b[i], b[j]);
               end;

               a[l + 1] := a[j];
               a[j] := am;
               b[l + 1] := b[j];
               b[j] := bm;
               inc(jstack, 2);

               // push pointers to larger subarray on stack
               if jStack >= Length(iStack) then
                  raise Exception.Create('Stack too small to sort');

               if (ir - i + 1 >= j - 1) then
               begin
                    istack[jstack] := ir;
                    istack[jstack - 1] := i;
                    ir := j - 1;
               end
               else
               begin
                    istack[jstack] := j - 1;
                    istack[jstack - 1] := l;
                    l := i;
               end;
          end;
     end;
end;

function eps(const val : double) : double;
begin
     Result := val*cDoubleEpsilon;
end;

function MinDblDiv : double;
var small : double;
begin
     Result := MinDouble;

     small := 1/MaxDouble;

     if small > Result then
        Result := small*(1 + eps(1));
end;

function lcm(a, b : NativeInt) : NativeInt;  // least common multiple
begin
     Result := (abs(a) div gcm(a, b)) * abs(b);
end;

// from https://en.wikipedia.org/wiki/Euclidean_algorithm
function gcm(a, b : NativeInt) : NativeInt; // greatest common divisior
var t : NativeInt;
begin
     while b <> 0 do
     begin
          t := b;
          b := a mod b;
          a := t;
     end;

     Result := a;
end;

function Next2Pwr(num : NativeInt; maxSize : NativeInt) : NativeInt;
begin
     Result := 1;
     while (Result < maxSize) and (Result < num) do
           Result := Result shl 1;
end;

// computes the eukledian distance without the destructive underflow or overflow
function pythag(const A, B : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
var absA, absB : double;
begin
     absA := abs(A);
     absB := abs(B);

     if absA = 0
     then
         Result := absB
     else if absB = 0
     then
         Result := absA
     else if absA > absB
     then
         Result := absa*sqrt(1 + sqr(absb/absa))
     else
         Result := absb*sqrt(1 + sqr(absa/absb));
end;

function sign(const a : double; const b : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
begin
     if b >= 0
     then
         Result := abs(a)
     else
         Result := -abs(a);
end;

function GetLocalFMTSet : TFormatSettings;
begin
     {$IF DEFINED(FPC)}
     Result := DefaultFormatSettings;
     {$ELSE}
        {$IF (CompilerVersion <= 21)}
        GetLocaleFormatSettings(0, Result);
        {$ELSE}
        Result := TFormatSettings.Create;
        {$IFEND}
     {$IFEND}
end;

// writes the matrix such that matlab can read it nicely
procedure WriteMtlMtx(const fileName : string; const mtx : TDoubleDynArray; width : integer; prec : integer = 8);
var s : UTF8String;
    x, y : integer;
    ft : TFormatSettings;
begin
     ft := GetLocalFMTSet;

     ft.DecimalSeparator := '.';

     with TFileStream.Create(fileName, fmCreate or fmOpenWrite) do
     try
        for y := 0 to (Length(mtx) div width) - 1 do
        begin
             s := '';
             for x := 0 to width - 1 do
                 s := s + UTF8String(Format('%.*f,', [prec, mtx[x + y*width]], ft));

             s[length(s)] := #13;
             s := s + #10;

             WriteBuffer(s[1], Length(s));
        end;
     finally
            Free;
     end;
end;

procedure WriteMtlMtx(const fileName : string; mtx : PDouble; LineWidthmtx : NativeInt; width, height : integer; prec : integer = 8); overload;
var s : UTF8String;
    x, y : integer;
    pMTx : PConstDoubleArr;
    ft : TFormatSettings;
begin
     ft := GetLocalFMTSet;

     ft.DecimalSeparator := '.';

     with TFileStream.Create(fileName, fmCreate or fmOpenWrite) do
     try
        for y := 0 to height - 1 do
        begin
             pMtx := GenPtrArr(mtx, 0, y, LineWidthmtx);

             s := '';
             for x := 0 to width - 1 do
                 s := s + UTF8String(Format('%.*f,', [prec, pMTx^[x]], ft));

             s[length(s)] := #13;
             s := s + #10;

             WriteBuffer(s[1], Length(s));
        end;
     finally
            Free;
     end;
end;


// mostly used as output to directly test with various lapack packages
procedure WriteBinaryMtxColMajor( fn : String; A : PDouble; LineWidthA : NativeInt;
  w, h  :integer );
var x, y : integer;
    pA : PDouble;
begin
     with TFileStream.Create(fn, fmCreate or fmOpenWrite ) do
     try
        WriteBuffer( w, SizeOf(w));
        WriteBuffer( h, sizeof(h));

        for x := 0 to w - 1 do
        begin
             for y := 0 to h - 1 do
             begin
                  pA := GenPtr(A, x, y, LineWidthA);
                  WriteBuffer(pA^, sizeof(double) );
             end;
        end;

     finally
            free;
     end;
end;


function KthLargest(var vals : TDoubleDynArray; elemNum : Cardinal) : double;
begin
     Result := KthLargest(@vals[0], Length(vals), elemNum);
end;

function KthLargest(valsArr : PDouble; numElem : integer; elemNum : Cardinal) : double; overload;
var i, ir, j, l, mid : Cardinal;
    a : double;
    vals : PConstDoubleArr;    
begin
     vals := PConstDoubleArr(valsArr);
     
     Result := 0;
     l := 0;
     if numElem = 0 then
        exit;
     ir := numElem - 1;

     while True do
     begin
          if ir <= l + 1 then
          begin
               if (ir = l + 1) and (vals^[ir] < vals^[l]) then
                  DoubleSwap(vals^[l], vals^[ir]);
               Result := vals^[elemNum];
               exit;
          end;

          mid := (l + ir) div 2;
          DoubleSwap(vals^[mid], vals^[l + 1]);
          if vals^[l] > vals^[ir] then
             DoubleSwap(vals^[l], vals^[ir]);
          if vals^[l + 1] > vals^[ir] then
             DoubleSwap(vals^[l + 1], vals^[ir]);
          if vals^[l] > vals^[l + 1] then
             DoubleSwap(vals^[l], vals^[l + 1]);

          i := l + 1;
          j := ir;
          a := vals^[l + 1];
          while True do
          begin
               repeat
                     inc(i);
               until vals^[i] >= a;
               repeat
                     dec(j);
               until vals^[j] <= a;

               if j < i then
                  break;

               DoubleSwap(vals^[i], vals^[j]);
          end;
          vals^[l + 1] := vals^[j];
          vals^[j] := a;
          if j >= elemNum then
             ir := j - 1;
          if j <= elemNum then
             l := i;
     end;
end;

function ei( x : double ) : double;
const cMaxIter = 100;
      cEuler = 0.5521566;
      cFPMin = 1.0e-30;
      cEPS = 6.0e-8;
var k : integer;
    fact, prev, sum, term : double;
begin
     if x <= 0 then
        raise Exception.Create('Bad argument in ei');
     if x < cFPMin then
     begin
          Result := ln(x) + cEuler;
          exit;
     end;
     if x <= -ln(cEPS) then
     begin
          sum := 0;
          fact := 1;
          for k := 1 to cMaxIter do
          begin
               fact := fact*x/k;
               term := fact/k;
               sum := sum + term;
               if term < cEPS*term then
               begin
                    Result := sum + ln(x) + cEuler;
                    exit;    
               end;
          end;

          raise Exception.Create('Series failed in ei');
     end
     else
     begin
          sum := 0;
          term := 0;
          for k := 1 to cMaxIter do
          begin
               prev := term;
               term := term*k/x;
               if term < cEPS then
                  break;
               if term < prev 
               then
                   sum := sum + term
               else
               begin
                    sum := sum - prev;
                    break;
               end;
          end;

          Result := exp(x)*(1.0 + sum)/x;
     end;
end;

function expInt( n : integer; x : double ) : double;
const cMaxIter = 100;
      cEuler = 0.5772156649;
      cFPMin = 1.0e-30;
      cEPS = 1.0e-7;
var i, ii, nm1 : integer;
    a, b, c, d, del, fact, h , psi : double;
begin
     nm1 := n - 1;
     if (n < 0) or (x < 0) or ( (x = 0) and ( n in [0, 1] ) ) then
        raise Exception.Create('Bad Argument for expint');

     if n = 0 
     then
         Result := exp(-x)/x
     else
     begin
          if x = 0 
          then
              Result := 1/nm1
          else
          begin
               if x > 1 then
               begin
                    b := x + n;
                    c := 1/cFPMin;
                    d := 1/b;
                    h := d;
                    for i := 1 to cMaxIter do
                    begin
                         a := -i*(nm1 + i);
                         b := b + 2;
                         d := 1/(a*d + b);
                         c := b + a/c;
                         del := c*d;
                         h := h*del;
                         if abs(del - 1) < cEPS then
                         begin
                              Result := h*exp(-x);
                              exit;
                         end;
                    end;

                    raise Exception.Create('Continued fraction failed in expint');
               end
               else
               begin
                    if nm1 <> 0 
                    then
                        Result := 1/nm1
                    else
                        Result := -ln(x) - cEuler;

                    fact := 1;

                    for i := 1 to cMaxIter do
                    begin
                         fact := -fact*x/i;
                         if i <> nm1 
                         then
                             del := -fact/(i - nm1)
                         else
                         begin
                              psi := -cEuler;
                              for ii := 1 to nm1 do
                                  psi := psi + 1/ii;

                              del := fact*(-ln(x) + psi);
                         end;

                         Result := Result + del;
                         if abs(del) < abs(Result)*cEPS then
                            exit;
                    end;

                    raise Exception.Create('Series failed in expint');                    
               end;
          end;
     end;
end;


// ###########################################
// #### Prime number stuff
// ###########################################

// miller rubin test for 32 bit (aka the integer range we can build with simple instructions)
function MillerRubinTest32( n : UInt32; a : UInt32 ) : boolean;
var m : UInt32;
    d : UInt32;
    e : UInt32;
    p, q : UInt64;
begin
     // only valid for odd inputs
     if n and 1 = 0 then
        exit(False);

     Result := True;

     m := n - 1;
     d := m shr 1;
     e := 1;

     while (d and 1) = 0 do
     begin
          d := d shr 1;
          inc(e);
     end;

     p := a;
     q := a;
     d := d shr 1;
     while d <> 0 do
     begin
          q := q*q;
          q := q mod n;
          if d and 1 <> 0 then
             p := (p*q) mod n;

          d := d shr 1;
     end;
     if (p = 1) or (p = m) then
        exit;

     dec(e);

     while e <> 0 do
     begin
          p := (p*p) mod n;

          if p = m then
             exit;

          if p <= 1 then
             break;

          dec(e);
     end;

     Result := False;
end;

// ###########################################
// #### function root finding
// ###########################################

function findRootNewtonRaphson( func : TRootFuncWithDerrive; out root : double; x1, x2 : double; xAcc : double = 1e-8 ) : boolean;
var j : integer;
    df, dx, dxold, f, fh, fl : double;
    temp, xh, xl, rts : double;
const MAXITER = 100;
begin
     func(x1, fl, df);
     func(x2, fh, df);

     root := nan;

     Result := False;
     if ( (fh < 0) and (fl < 0) ) or ( (fh > 0) and (fl > 0) ) then
        exit;

     if fh = 0 then
     begin
          root := x2;
          Result := True;
          exit;
     end;
     if fl = 0 then
     begin
          root := x1;
          Result := True;
          exit;
     end;

     if fl < 0 then
     begin
          xl := x1;
          xh := x2;
     end
     else
     begin
          xl := x2;
          xh := x1;
     end;


     rts := 0.5*(x1 + x2);
     dxold := abs(x2 - x1);
     dx := dxold;
     func( rts, f, df );

     j := 0;
     while j < MAXITER do
     begin
          // check progress:
          // use bisect if out of range or not fast enough
          if (((rts - xh)*df-f)*((rts-xl)*df-f) > 0) or
             (abs(2*f) > abs(dxold*df))
          then
          begin
               // bisect
               dxold := dx;
               dx := 0.5*(xh - xl);
               rts := xl + dx;
               if xl = rts then
                  break;
          end
          else
          begin
               // Newton
               dxold := dx;
               dx := f/df;
               temp := rts;
               rts := rts - dx;
               if temp = rts then
                  break;
          end;

          if abs(dx) < xacc then
             break;

          func(rts, f, df);
          if f < 0
          then
              xl := rts
          else
              xh := rts;

          inc(j);
     end;

     Result := j < MAXITER;
     if Result then
        root := rts;
end;


function findRootBrent( func : TRootFunc; out root : double; x1, x2 : double; xAcc : double = 1e-8 ) : boolean;
var iter : integer;
    a, b, c, d, e, min1, min2 : double;
    fa, fb, fc, p, q, r, s, tol1, xm : double;
const MAXITER = 100;
begin
     a := x1;
     b := x2;
     c := x2;
     fa := func(a);
     fb := func(b);
     d := (c - b)*0.5;
     e := d;

     Result := False;
     root := NAN;
     if ((fa > 0) and (fb > 0)) or ((fa < 0) and (fb < 0)) then
        exit;

     fc := fb;
     iter := 0;
     while iter < MAXITER do
     begin
          if ((fb > 0) and (fc > 0)) or ((fb < 0) and (fc < 0)) then
          begin
               c := a;
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

          tol1 := 2*eps(1)*abs(b) + 0.5*xacc;
          xm := 0.5*(c - b);

          // ###########################################
          // #### Stop criterion
          if (abs(xm) <= tol1) or (fb = 0) then
             break;


          if (abs(e) >= tol1) and (abs(fa) > abs(fb)) then
          begin
               s := fb/fa;
               if a = c then
               begin
                    p := 2*xm*s;
                    q := -s;
               end
               else
               begin
                    q := fa/fc;
                    r := fb/fc;
                    p := s*(2*xm*q*(q -r) - (b - a)*(r - 1));
                    q := (q - 1)*(r - 1)*(s - 1);
               end;

               if (p > 0) then
                  q := -q;

               p := abs(p);
               min1 := 3*xm*q - abs(tol1*q);
               min2 := abs(e*q);


               if 2*p < Min( min1, min2 ) then
               begin
                    e := d;
                    d := p/q;
               end
               else
               begin
                    d := xm;
                    e := d;
               end;
          end
          else
          begin
               d := xm;
               e := d;
          end;

          a := b;
          fa := fb;
          if abs(d) > tol1
          then
              b := b + d
          else
              b := b + sign( tol1, xm );

          // ###########################################
          // #### next function call
          fb := func(b);

          inc(iter);
     end;

     Result := iter < MAXITER;
     if Result then
        root := b;
end;


function findRoot( func : TRootFuncWithDerrive; out root : double; x1, x2 : double; xAcc : double = 1e-8 ) : boolean;
begin
     Result := findRootNewtonRaphson( func, root, x1, x2, xAcc);
end;

function findRoot( func : TRootFunc; out root : double; x1, x2 : double; xAcc : double = 1e-8 ) : boolean;
begin
     Result := findRootBrent( func, root, x1, x2, xAcc);
end;



initialization
  cMinDblDivEps := MinDblDiv/eps(1);

end.
