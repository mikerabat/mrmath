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


unit BaseMatrixTestCase;

interface

uses TestFramework, Classes, SysUtils, Types;

type
 // Testmethoden für Klasse TDoubleMatrix

 TBaseMatrixTestCase = class(TTestCase)
 protected
   procedure WriteMatlabData(const fileName : string; const data : Array of double; width : integer);
   procedure TryClearCache;
   function WriteMtx(const data : Array of Double; width : integer) : string; overload;
   function CheckMtx(const data1 : Array of Double; const data2 : Array of double; w : integer = -1; h : integer = -1; const epsilon : double = 1e-4) : boolean;
   function CheckMtxIdx(const data1 : Array of Double; const data2 : Array of double; var idx : integer; w : integer = -1; h : integer = -1; const epsilon : double = 1e-4) : boolean;
   function WriteMtxDyn(const data : TDoubleDynArray; width : integer) : string; overload;
 end;



implementation

{ TBaseMatrixTestCase }

function TBaseMatrixTestCase.CheckMtx(const data1, data2: array of double; w, h : integer;
  const epsilon: double): boolean;
var i : integer;
begin
     Result := CheckMtxIdx(data1, data2, i, w, h, epsilon);
end;

function TBaseMatrixTestCase.CheckMtxIdx(const data1, data2: array of double;
  var idx: integer; w, h : integer; const epsilon: double): boolean;
var i : integer;
    miss : integer;
    x, y : integer;
begin
     if (w < 0) and (h < 0) then
     begin
          w := Length(data1);
          h := 1;
          miss := 0;
     end
     else
         miss := (Length(data1) - w*h) div h;

     Result := High(data1) = High(data2);
     if Result then
     begin
          i := 0;
          for y := 0 to h - 1 do
          begin
               for x := 0 to w - 1 do
               begin
                    Result := Result and (abs(data1[i] - data2[i]) <= epsilon);
                    if not Result then
                    begin
                         idx := i;
                         exit;
                    end;
                    inc(i);
               end;

               inc(i, miss);
          end;
     end;
end;

procedure TBaseMatrixTestCase.TryClearCache;
var mem : PInteger;
	   i : integer;
    pMem : PInteger;
const cNumElem = 5000000;
      cMaxCacheSize = cNumElem*sizeof(integer);
begin
	    // allocates a realy big piece of memory reads and writes it -
     // the cache should then be clear
     mem := GetMemory(cMaxCacheSize);
     pMem := mem;
     for i := 0 to cNumElem - 1 do
     begin
      	   pMem^ := i;
          inc(pMem);
     end;
     FreeMem(mem);
end;

procedure TBaseMatrixTestCase.WriteMatlabData(const fileName: string;
  const data: array of double; width: integer);
var i : integer;
    s : string;
begin
     // write a file which can be read into matlab using the load command
     // the procedure is usefull to verify the results against this program.
     with TStringList.Create do
     try
        BeginUpdate;
        s := '';
        for i := 0 to Length(data) - 1 do
        begin
             s := s + Format('%.9f,', [data[i]]);

             if i mod width = width - 1 then
             begin
                  s[length(s)] := ';';
                  Add(s);
                  s := '';
             end;
        end;
        EndUpdate;

        SaveToFile(FileName, TEncoding.ASCII);
     finally
            Free;
     end;
end;

function TBaseMatrixTestCase.WriteMtx(const data: array of Double;
  width: integer): string;
var x, y : integer;
begin
     Result := '';

     for y := 0 to (High(data) + 1) div width - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Result + Format('%.3f ', [data[x + y*width]]);

          Result := Result + #13#10;
     end;
end;

function TBaseMatrixTestCase.WriteMtxDyn(const data: TDoubleDynArray;
  width: integer) : string;
var x, y : integer;
begin
     Result := '';

     for y := 0 to Length(data) div width - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Result + Format('%.4f ', [data[x + y*width]]);

          Result := Result + #13#10;
     end;
end;

initialization
  {$IF CompilerVersion > 21}
  FormatSettings.DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$IFEND}

end.
