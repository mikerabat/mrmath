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

function pythag(const A, B : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function sign(a : double; b : double) : double; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
procedure DoubleSwap(var a, b : Double); {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
function binom(n, k : integer) : int64;

function eps(const val : double) : double;

//procedure ShowMtx(mtx : PDouble; LineWidth : integer; Width : integer; height : integer);

implementation

uses SysUtils;

function eps(const val : double) : double;
const cDoubleEpsilon = 2.2204460492503131e-016; // smallest such that 1.0+DBL_EPSILON != 1.0
begin
     Result := val*cDoubleEpsilon;
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
     if b > 0
     then
         Result := abs(a)
     else
         Result := -abs(a);
end;

end.
