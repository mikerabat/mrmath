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

unit Utilities;

// ########################################################
// #### Utility functions
// ########################################################

interface

uses Types;

type
  TQuickSortFunc = function(const Item1, Item2) : integer; 


function DoubleSortFunc(const Item1, Item2) : integer;
procedure QuickSort(var A; ItemSize : integer; ItemCount : integer; ItemSort : TQuickSortFunc);

// for median - note: the content (sort order) of the array is destroyed!
function KthLargest(var vals : TDoubleDynArray; elemNum : Cardinal) : double; overload;
function KthLargest(valsArr : PDouble; numElem : integer; elemNum : Cardinal) : double; overload;

implementation

uses SysUtils, Math, MatrixConst;

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

           Move(PByteArray(@A)[ItemSize*((Lo + Hi) shr 1)], P[0], ItemSize);
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

end.
