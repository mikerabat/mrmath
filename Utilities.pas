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

type
  TQuickSortFunc = function(const Item1, Item2) : integer; 


function DoubleSortFunc(const Item1, Item2) : integer;
procedure QuickSort(var A; ItemSize : integer; ItemCount : integer; ItemSort : TQuickSortFunc);

implementation

uses SysUtils, Math;

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

end.
