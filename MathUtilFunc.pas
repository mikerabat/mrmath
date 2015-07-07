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

uses Types;

{$WRITEABLECONST ON}

const cDoubleEpsilon : double = 2.2204460492503131e-016;  // smallest such that 1.0+DBL_EPSILON != 1.0
const cMinDblDivEps : double = 0;     // filled in initialization

function pythag(const A, B : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function sign(a : double; b : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
procedure DoubleSwap(var a, b : Double); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function binom(n, k : integer) : int64;

function eps(const val : double) : double;
function MinDblDiv : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

procedure WriteMtlMtx(const fileName : string; const mtx : TDoubleDynArray; width : integer; prec : integer = 8);

type
  TQuickSortFunc = function(const Item1, Item2) : integer;


function DoubleSortFunc(const Item1, Item2) : integer;
procedure QuickSort(var A; ItemSize : integer; ItemCount : integer; ItemSort : TQuickSortFunc); overload;
procedure QuickSort(var A: array of double; StartIdx : integer = 0; EndIdx : integer = -1); overload;

// for median - note: the content (sort order) of the array is destroyed!
function KthLargest(var vals : TDoubleDynArray; elemNum : Cardinal) : double; overload;
function KthLargest(valsArr : PDouble; numElem : integer; elemNum : Cardinal) : double; overload;

implementation

uses SysUtils, Math, Classes, MatrixConst;

// ##########################################
// #### utility function implementation
// ##########################################

function DoubleSortFunc(const Item1, Item2) : integer;
begin
     Result := CompareValue(PDouble(@Item1)^, PDouble(@Item2)^);
end;

procedure QuickSort(var A; ItemSize : integer; ItemCount : integer; ItemSort : TQuickSortFunc);
var P : Array of byte;
    Help : Array of Byte;
procedure Qs(Lo, hi : integer);
var I, J: Integer;
begin
     // quick sort implementation of for double values
     repeat
           I := Lo;
           J := Hi;

           Move(PByteArray(@A)[ItemSize*((Lo + Hi) div 2)], P[0], ItemSize);
           repeat
                 while ItemSort(PByteArray(@A)[ItemSize*I], P[0]) < 0 do
                       Inc(I);
                 while ItemSort(PByteArray(@A)[ItemSize*J], P[0]) > 0 do
                       Dec(J);
                 if I <= J then
                 begin
                      Move(PByteArray(@A)[ItemSize*I], Help[0], ItemSize);
                      Move(PByteArray(@A)[ItemSize*J], PByteArray(@A)[ItemSize*I], ItemSize);
                      Move(Help[0], PByteArray(@A)[ItemSize*J], ItemSize);

                      Inc(I);
                      Dec(J);
                 end;
           until I > J;

           if Lo < J then
              Qs(Lo, J);
           Lo := I;
     until I >= Hi;
end;
begin
     if (ItemCount <= 0) or (ItemSize <= 0) then
        exit;

     SetLength(P, ItemSize);
     SetLength(Help, ItemSize);
     Qs(0, ItemCount - 1);
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

// implements "n over k" -> binominal koefficients.
// see: http://de.wikipedia.org/wiki/Binomialkoeffizient
function binom(n, k : integer) : int64;
var tmp : integer;
    i : integer;
begin
     if k = 0 then
     begin
          Result := 1;
          exit;
     end
     else if 2*k > n
     then
         k := n - k;

     Result := n;
     tmp := n + 1;
     for i := 2 to k do
     begin
          Result := Result*(tmp - i);
          Result := Result div i;
     end;
end;

procedure DoubleSwap(var a, b : Double); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
var temp : double;
begin
     temp := a;
     a := b;
     b := temp;
end;

// computes the eukledian distance without the destructive underflow or overflow
function pythag(const A, B : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
var absA, absB : double;
begin
     absA := abs(A);
     absB := abs(B);

     if absA > absB
     then
         Result := absa*sqrt(1 + sqr(absb/absa))
     else if absb <> 0
     then
         Result := absb*sqrt(1 + sqr(absa/absb))
     else
         Result := 0;
end;

function sign(a : double; b : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
begin
     if b >= 0
     then
         Result := abs(a)
     else
         Result := -abs(a);
end;

// writes the matrix such that matlab can read it nicely
procedure WriteMtlMtx(const fileName : string; const mtx : TDoubleDynArray; width : integer; prec : integer = 8);
var s : UTF8String;
    x, y : integer;
    ft : TFormatSettings;    
begin
     {$IF DEFINED(FPC) or (CompilerVersion <= 21)}
     GetLocaleFormatSettings(0, ft);
     {$ELSE}
     ft := TFormatSettings.Create;
     {$IFEND}

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

procedure SwapD(var elem1, elem2 : double); inline;
var help : double;
begin
     help := elem1;
     elem1 := elem2;
     elem2 := help;
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
                  swapD(vals^[l], vals^[ir]);
               Result := vals^[elemNum];
               exit;
          end;

          mid := (l + ir) div 2;
          swapD(vals^[mid], vals^[l + 1]);
          if vals^[l] > vals^[ir] then
             SwapD(vals^[l], vals^[ir]);
          if vals^[l + 1] > vals^[ir] then
             SwapD(vals^[l + 1], vals^[ir]);
          if vals^[l] > vals^[l + 1] then
             SwapD(vals^[l], vals^[l + 1]);

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

               SwapD(vals^[i], vals^[j]);
          end;
          vals^[l + 1] := vals^[j];
          vals^[j] := a;
          if j >= elemNum then
             ir := j - 1;
          if j <= elemNum then
             l := i;
     end;
end;

initialization
  cMinDblDivEps := MinDblDiv/eps(1);

end.
