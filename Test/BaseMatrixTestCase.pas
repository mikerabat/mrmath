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

uses
  Windows,
  {$IFDEF FPC} fpcunit, testregistry, {$ELSE} TestFramework, {$ENDIF}
  Classes, SysUtils, Types, Matrix;

type

 { TBaseMatrixTestCase }

 TBaseMatrixTestCase = class(TTestCase)
 protected
   {$IFDEF FPC}
   procedure CheckEqualsMem(p1, p2 : PByte; memSize : integer; const msg : string);
   procedure Status(const msg : string);
   {$ENDIF}

   procedure FillMatrix(mtxSize : integer; out x, y : TDoubleDynArray; out p1, p2 : PDouble);
   procedure WriteMatlabData(const fileName : string; const data : Array of double; width : integer);
   procedure TryClearCache;
   function WriteMtx(const data : Array of Double; width : integer; prec : integer = 3) : string; overload;
   function CheckMtx(const data1 : Array of Double; const data2 : Array of double; w : integer = -1; h : integer = -1; const epsilon : double = 1e-4) : boolean;
   function CheckMtxIdx(const data1 : Array of Double; const data2 : Array of double; out idx : integer; w : integer = -1; h : integer = -1; const epsilon : double = 1e-4) : boolean;
   function WriteMtxDyn(const data : TDoubleDynArray; width : integer; prec : integer = 3) : string; overload;
 end;

 TBaseImgTestCase = class(TBaseMatrixTestCase)
 protected
   procedure ImageFromMatrix(img : TDoubleMatrix; w, h : integer; const FileName : string);
   function LoadImages(out w, h : integer; path : string = 'Images'; ext : string = '*.jpg') : TDoubleMatrix;
 end;



implementation

uses Graphics, {$IFNDEF FPC} JPEG, {$ENDIF} Math;

{ TBaseMatrixTestCase }

{$IFDEF FPC}
procedure TBaseMatrixTestCase.CheckEqualsMem(p1, p2: PByte; memSize: integer;
  const msg: string);
var cnt : integer;
begin
     for cnt := 0 to memSize - 1 do
     begin
          if p1^ <> p2^ then
             raise Exception.Create(msg);

          inc(p1);
          inc(p2);
     end;
end;

procedure TBaseMatrixTestCase.Status(const msg: string);
begin
     WriteLn(msg);
end;

{$ENDIF}

function TBaseMatrixTestCase.CheckMtx(const data1: array of Double;
  const data2: array of double; w: integer; h: integer; const epsilon: double
  ): boolean;
var i : integer;
begin
     Result := CheckMtxIdx(data1, data2, i, w, h, epsilon);
end;

function TBaseMatrixTestCase.CheckMtxIdx(const data1: array of Double;
  const data2: array of double; out idx: integer; w: integer; h: integer;
  const epsilon: double): boolean;
var i : integer;
    miss : integer;
    x, y : integer;
begin
     idx := 0;
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
                  if i = Length(data) - 1 then
                     system.Delete(s, Length(s), 1);
                  Add(s);
                  s := '';
             end;
        end;
        EndUpdate;

        SaveToFile(FileName {$IF not Defined(FPC) and (CompilerVersion >= 20)} , TEncoding.ASCII {$IFEND});
     finally
            Free;
     end;
end;

procedure TBaseMatrixTestCase.FillMatrix(mtxSize: integer; out x,
  y: TDoubleDynArray; out p1, p2: PDouble);
var px : PDouble;
    py : PDouble;
    idx : integer;
begin
     SetLength(x, mtxSize);
     SetLength(y, mtxSize);

     p1 := GetMemory(mtxSize*sizeof(double));
     p2 := GetMemory(mtxSize*sizeof(double));

     // fill randomly:
     px := @x[0];
     py := @y[0];
     for Idx := 0 to mtxSize - 1 do
     begin
          px^ := random;
          py^ := random;
          inc(px);
          inc(py);
     end;

     px := p1;
     py := p2;
     for Idx := 0 to mtxSize - 1 do
     begin
          px^ := x[idx];
          py^ := y[idx];
          inc(px);
          inc(py);
     end;
end;

function TBaseMatrixTestCase.WriteMtx(const data: array of Double;
  width: integer; prec : integer = 3): string;
var x, y : integer;
begin
     Result := '';

     for y := 0 to (High(data) + 1) div width - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Result + Format('%.*f ', [prec, data[x + y*width]]);

          Result := Result + #13#10;
     end;
end;

function TBaseMatrixTestCase.WriteMtxDyn(const data: TDoubleDynArray;
  width: integer; prec : integer = 3) : string;
var x, y : integer;
begin
     Result := '';

     for y := 0 to Length(data) div width - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Result + Format('%.*f ', [prec, data[x + y*width]]);

          Result := Result + #13#10;
     end;
end;


procedure TBaseImgTestCase.ImageFromMatrix(img: TDoubleMatrix; w, h : integer;
  const FileName: string);
var bmp : TBitmap;
    x, y : integer;
    idx : integer;
    pScanLine : PRGBTriple;
begin
     // create an image from the reconstructed matrix
     bmp := TBitmap.Create;
     try
        bmp.Width := W;
        bmp.Height := H;
        bmp.PixelFormat := pf24bit;

        idx := 0;
        for y := 0 to bmp.Height - 1 do
        begin
             pScanLine := bmp.ScanLine[y];

             for x := 0 to bmp.Width - 1 do
             begin
                  pScanline^.rgbtBlue := Max(0, Min(255, Round(img[0, idx])));
                  pScanline^.rgbtRed := pScanline^.rgbtBlue;
                  pScanline^.rgbtGreen := pScanline^.rgbtBlue;

                  inc(pScanLine);
                  inc(idx);
             end;
        end;

        bmp.SaveToFile(FileName);
     finally
            bmp.Free;
     end;
end;

function TBaseImgTestCase.LoadImages(out w, h : integer; path : string = 'Images'; ext : string = '*.jpg' ): TDoubleMatrix;
var imgNum : integer;
    img : TPicture;
    bmp : TBitmap;
    sr : TSearchRec;
    pScanLine : PRGBTriple;
    idx : integer;
    x, y : integer;
    numImg : integer;
begin
     // load a bunch of images and calculate a PCA. Note the images
     Result := nil;
     imgNum := 0;
     w := 0;
     h := 0;
     path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + '\' + path);

     numImg := 0;
     if FindFirst(Path + ext, 0, sr) = 0 then
     begin
          repeat
                inc(numImg);
          until FindNext(sr) <> 0;
     end;

     FindClose(sr);

     if FindFirst(Path + ext, 0, sr) = 0 then
     begin
          repeat
                img := TPicture.Create;
                try
                   img.LoadFromFile(path + sr.name);

                   bmp := TBitmap.Create;
                   try
                      bmp.SetSize(img.Width, img.Height);
                      bmp.PixelFormat := pf24bit;
                      bmp.Canvas.Draw(0, 0, img.Graphic);

                      if not Assigned(Result) then
                      begin
                           w := bmp.Width;
                           h := bmp.Height;
                           Result := TDoubleMatrix.Create(numImg, bmp.Width*bmp.Height);
                      end;

                      // create matrix from image
                      idx := 0;
                      for y := 0 to bmp.Height - 1 do
                      begin
                           pScanLine := bmp.ScanLine[y];

                           for x := 0 to bmp.Width - 1 do
                           begin
                                Result[imgNum, idx] := Round(pScanline^.rgbtBlue*0.1140 + pScanline^.rgbtRed*0.2989 + pScanline^.rgbtGreen*0.5870);
                                inc(pScanLine);
                                inc(idx);
                           end;
                      end;

                      inc(imgNum);
                   finally
                          bmp.Free;
                   end;
                finally
                       img.Free;
                end;
          until FindNext(sr) <> 0;
     end;
     FindClose(sr);
end;

initialization
  {$IFDEF FPC}
  DecimalSeparator := '.';
  {$ELSE}
  {$IF CompilerVersion > 21}
  FormatSettings.DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$IFEND}
  {$ENDIF}

end.
