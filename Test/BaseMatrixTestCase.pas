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

  {$IFNDEF FPC}
  {$IFDEF MSWINDOWS} //wrc
  {$IF CompilerVersion >= 23.0}
  Winapi.Windows,
  {$ELSE}
  Windows,
  {$IFEND}
  {$ENDIF}     //wrc
  TestFramework,
  {$ELSE}
  fpcunit, testregistry,
  {$IFDEF MSWINDOWS} Windows, {$ELSE} lcltype, {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, Types, Matrix, MatrixConst;

type

 { TBaseMatrixTestCase }

 TBaseMatrixTestCase = class(TTestCase)
 protected
   function BaseDataPath : string;

   {$IFDEF FPC}
   procedure CheckEqualsMem(p1, p2 : PByte; memSize : integer; const msg : string);
   procedure Status(const msg : string);
   {$ENDIF}

   function CheckMtxVal( const mtx : Array of double; value : double) : boolean; overload;
   function CheckMtxVal( data : PDouble; lineWidth: TASMNativeInt; width, height : integer; value : double) : boolean; overload;
   procedure FillMatrix(mtxSize : integer; out x, y : TDoubleDynArray; out p1, p2 : PDouble);
   procedure AllocAlignedMtx(mtxSize : integer; out pX : PDouble; out pMem : PByte); overload;
   procedure AllocAlignedMtx(mtxWidth, mtxHeight : integer; out pX : PDouble; out pMem : PByte; out LineWidthAligned : TASMNativeInt); overload;
   procedure FillAlignedMtx(mtxSize : integer; out pX : PDouble; out pMem : PByte); overload;
   procedure FillAlignedMtx(mtxWidth, mtxHeight : integer; out pX : PDouble; out pMem : PByte; out LineWidthAligned : TASMNativeInt); overload;
   procedure FillUnalignedMtx(mtxWidth, mtxHeight : integer; out pX : PDouble; out pMem : PByte; out LineWidthAligned : TASMNativeInt);
   procedure WriteMatlabData(const fileName : string; const data : Array of double; width : integer);
   procedure TryClearCache;
   function WriteMtx(const data : Array of Double; width : integer; prec : integer = 3) : string; overload;
   function WriteMtx(data : PDouble; LineWidth : TASMNativeInt; width : integer; height : integer; prec : integer = 3) : string; overload;
   function WriteMtx(mtx : TDoubleMatrix; prec : integer = 3) : string; overload;
   function CheckMtx(const data1 : Array of Double; const data2 : Array of double; w : integer = -1; h : integer = -1; const epsilon : double = 1e-4) : boolean;
   function CheckMtxIdx(const data1 : Array of Double; const data2 : Array of double; out idx : integer; w : integer = -1; h : integer = -1; const epsilon : double = 1e-4) : boolean; overload;
   function CheckMtxIdx(data1, data2 : PDouble; const mtxSize : integer; out idx : integer; const epsilon : double = 1e-4) : boolean; overload;
   function CheckMtxIdx(data1, data2 : PDouble; LineWidth1, LineWidth2 : TASMNativeInt; mtxWidth, mtxHeight : integer; out idx : integer; const epsilon : double = 1e-4) : boolean; overload;
   function WriteMtxDyn(const data : TDoubleDynArray; width : integer; prec : integer = 3) : string; overload;
 end;

 TBaseImgTestCase = class(TBaseMatrixTestCase)
 protected
   procedure ImageFromMatrix(img : TDoubleMatrix; w, h : integer; const FileName : string);
   function LoadImages(out w, h : integer; path : string = 'Images'; ext : string = '*.jpg') : TDoubleMatrix;
 end;



implementation

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FMX}
 System.UIConsts, System.UITypes, FMX.Graphics, FMX.utils, FMX.Types,  Math;
 {$ELSE}
 Graphics, {$IFNDEF FPC} JPEG, {$ENDIF} Math;
 {$ENDIF}
{ TBaseMatrixTestCase }

{$IFDEF FPC}

var AllowStatusWrite : boolean = True;  // global variable which is set to false in case a pure gui application is compiled

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
     try
        if AllowStatusWrite then
           WriteLn(msg);
     except
           AllowStatusWrite := False;
     end;
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
                    Result := (abs(data1[i] - data2[i]) <= epsilon);
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

function TBaseMatrixTestCase.CheckMtxIdx(data1, data2: PDouble;
  const mtxSize: integer; out idx: integer; const epsilon : double = 1e-4): boolean;
var counter : integer;
begin
     Result := True;
     for counter := 0 to mtxSize - 1 do
     begin
          Result := SameValue( data1^, data2^, epsilon );

          if not Result then
          begin
               idx := counter;
               break;
          end;
          inc(data1);
          inc(data2);
     end;
end;

function TBaseMatrixTestCase.CheckMtxIdx(data1, data2: PDouble; LineWidth1,
  LineWidth2: TASMNativeInt; mtxWidth, mtxHeight : integer; out idx: integer;
  const epsilon: double): boolean;
var p1, p2 : PConstDoubleArr;
    x, y : integer;
begin
     Result := True;
     p1 := PConstDoubleArr(data1);
     p2 := PConstDoubleArr(data2);
     for y := 0 to mtxHeight - 1 do
     begin
          for x := 0 to mtxWidth - 1 do
          begin
               if not SameValue(p1^[x], p2^[x], Epsilon) then
               begin
                    Result := SameValue(p1^[x], p2^[x], Epsilon) ;
                    idx := x + y*mtxWidth;
                    exit;
               end;
          end;
          inc(PByte(p1), LineWidth1);
          inc(PByte(p2), LineWidth2);
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

procedure TBaseMatrixTestCase.AllocAlignedMtx(mtxSize: integer; out
  pX: PDouble; out pMem: PByte);
begin
     pMem := AllocMem(mtxSize*sizeof(double) + $20);

     // allign to 32 bytes
     pX := AlignPtr32(pMem);
end;

procedure TBaseMatrixTestCase.AllocAlignedMtx(mtxWidth, mtxHeight: integer; out
  pX: PDouble; out pMem: PByte; out LineWidthAligned: TASMNativeInt);
var widthAligned : integer;
begin
     widthAligned := mtxWidth;
     if widthAligned mod 4 <> 0 then
        inc(widthAligned, 4 - widthAligned mod 4);

     pMem := AllocMem(widthAligned*mtxHeight*sizeof(double) + $40);
     LineWidthAligned := sizeof(double)*widthAligned;

     // allign to 32 bytes
     pX := AlignPtr64(pMem);
end;

procedure TBaseMatrixTestCase.FillAlignedMtx(mtxSize: integer; out pX: PDouble;
  out pMem: PByte);
var ptr : PConstDoubleArr;
    counter : TASMNativeInt;
begin
     pMem := GetMemory(mtxSize*sizeof(double) + $20);

     // allign to 32 bytes
     pX := AlignPtr32(pMem);
     ptr := PConstDoubleArr(pX);

     for counter := 0 to mtxSize - 1 do
         ptr^[counter] := (counter + 1); // random;
end;

procedure TBaseMatrixTestCase.FillAlignedMtx(mtxWidth, mtxHeight : integer; out pX: PDouble;
  out pMem: PByte; out LineWidthAligned: TASMNativeInt);
var ptr : PConstDoubleArr;
    y, counter : TASMNativeInt;
    widthAligned : integer;
begin
     widthAligned := mtxWidth;
     if widthAligned mod 4 <> 0 then
        inc(widthAligned, 4 - widthAligned mod 4);

     pMem := GetMemory(widthAligned*mtxHeight*sizeof(double) + 64); //$20);
     LineWidthAligned := sizeof(double)*widthAligned;

     // allign to 32 bytes
     pX := AlignPtr64(pMem);
     ptr := PConstDoubleArr(pX);

     for y := 0 to mtxHeight - 1 do
     begin
          for counter := 0 to mtxWidth - 1 do
              ptr^[counter] := random;

          inc(PByte(ptr), LineWidthAligned);
     end;
end;

procedure TBaseMatrixTestCase.FillUnalignedMtx(mtxWidth, mtxHeight: integer;
  out pX: PDouble; out pMem: PByte; out LineWidthAligned: TASMNativeInt);
var ptr : PConstDoubleArr;
    y, counter : TASMNativeInt;
    widthUnAligned : integer;
begin
     widthUnAligned := mtxWidth + 1;

     pMem := GetMemory(widthUnAligned*mtxHeight*sizeof(double) + $8);
     LineWidthAligned := sizeof(double)*widthUnAligned;

     // allign to 32 bytes
     pX := PDouble( TASMNativeUInt(pMem));
     if TASMNativeUInt(px) and $1F = 0 then
        inc(px);
     ptr := PConstDoubleArr(pX);

     for y := 0 to mtxHeight - 1 do
     begin
          for counter := 0 to mtxWidth - 1 do
              ptr^[counter] := random;

          inc(PByte(ptr), LineWidthAligned);
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

function TBaseMatrixTestCase.WriteMtx(data : PDouble; LineWidth : TASMNativeInt; width : integer; height : integer; prec : integer = 3) : string; 
var x, y : integer;
    pData : PConstDoubleArr;
begin
     Result := '';

     for y := 0 to height - 1 do
     begin
          pData := PConstDoubleArr( data );
          for x := 0 to width - 1 do
              Result := Result + Format('%.*f ', [prec, pData^[x]]);

          inc(PByte(data), LineWidth);
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

{$IFNDEF FMX}
procedure TBaseImgTestCase.ImageFromMatrix(img: TDoubleMatrix; w, h : integer;
  const FileName: string);
var bmp : TBitmap;
    x, y : integer;
    idx : integer;
    pScanLine : PRGBQUAD;
begin
     // create an image from the reconstructed matrix
     bmp := TBitmap.Create;
     try
        bmp.Width := W;
        bmp.Height := H;
        bmp.PixelFormat := pf32bit;

        idx := 0;
        for y := 0 to bmp.Height - 1 do
        begin
             pScanLine := bmp.ScanLine[y];

             for x := 0 to bmp.Width - 1 do
             begin
                  pScanline^.rgbBlue := Max(0, Min(255, Round(img[0, idx])));
                  pScanline^.rgbRed := pScanline^.rgbBlue;
                  pScanline^.rgbGreen := pScanline^.rgbBlue;

                  inc(pScanLine);
                  inc(idx);
             end;
        end;

        bmp.SaveToFile(FileName);
     finally
            bmp.Free;
     end;
end;
{$ENDIF}
{$IFDEF FMX}
procedure TBaseImgTestCase.ImageFromMatrix(img: TDoubleMatrix; w, h : integer;
  const FileName: string);
var bmp : TBitmap;
    x, y : integer;
    idx : integer;
    //pScanLine : PRGBQUAD;
    Color :  TAlphaColor;
    bd1 : TBitMapData;
    p1 : PAlphaColorArray;
begin
     // create an image from the reconstructed matrix
     bmp := TBitmap.Create;
     try
        bmp.Width := W;
        bmp.Height := H;
        //bmp.PixelFormat := pf32bit;
       // bmp.PixelFormat := RGBA32F;
        bmp.Map(TMapAccess.ReadWrite, bd1);
        idx := 0;
        for y := 0 to bmp.Height - 1 do
        begin
             //pScanLine := bmp.ScanLine[y];
             p1 := PAlphaColorArray(bd1.GetScanline(y));
             for x := 0 to bmp.Width - 1 do
             begin
                  TAlphaColorRec(Color).B := Max(0, Min(255, Round(img[0, idx])));
                  TAlphaColorRec(Color).R := TAlphaColorRec(Color).B;
                  TAlphaColorRec(Color).G := TAlphaColorRec(Color).B;
                  p1^[x] := Color;
                //   Result[imgNum, idx] := Round(TAlphaColorRec(PixelToAlphaColor(@p1,img.PixelFormat)).B*0.1140 + TAlphaColorRec(PixelToAlphaColor(@p1,img.pixelformat)).R*0.2989 + TAlphaColorRec(PixelToAlphaColor(@p1,img.Pixelformat)).G*0.5870);
                  inc(idx);
             end;
        end;

        bmp.SaveToFile(FileName);
     finally
            bmp.Unmap(bd1);
            bmp.Free;
     end;
end;
{$ENDIF}

{$IFNDEF FMX}
function TBaseImgTestCase.LoadImages(out w, h : integer; path : string = 'Images'; ext : string = '*.jpg' ): TDoubleMatrix;
var imgNum : integer;
    img : TPicture;
    bmp : TBitmap;
    sr : TSearchRec;
    pScanLine : PRGBQUAD;
    idx : integer;
    x, y : integer;
    numImg : integer;
    sl : TStringList;
    i : integer;
    basePath : string;
begin
     // load a bunch of images and calculate a PCA. Note the images
     Result := nil;
     imgNum := 0;
     w := 0;
     h := 0;

     basePath := BaseDataPath;

     path := IncludeTrailingPathDelimiter(basePath + path);
     sl := TStringList.Create;

     numImg := 0;
     if FindFirst(Path + ext, 0, sr) = 0 then
     begin
          repeat
                sl.Add( Path + sr.Name );
                inc(numImg);
          until FindNext(sr) <> 0;
     end;

     FindClose(sr);

     sl.Sort;

     for i := 0 to sl.Count - 1 do
     begin
          img := TPicture.Create;
          try
             img.LoadFromFile(sl[i]);

             bmp := TBitmap.Create;
             try
                bmp.SetSize(img.Width, img.Height);
                bmp.PixelFormat := pf32bit;
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
                          Result[imgNum, idx] := Round(pScanline^.rgbBlue*0.1140 + pScanline^.rgbRed*0.2989 + pScanline^.rgbGreen*0.5870);
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
     end;
     sl.Free;
end;
{$ENDIF}
{$IFDEF FMX}
function TBaseImgTestCase.LoadImages(out w, h : integer; path : string = 'Images'; ext : string = '*.jpg' ): TDoubleMatrix;
var imgNum : integer;
    img : TBitmap;
    sr : TSearchRec;
    bd1 : TBitMapData;
    p1 : PAlphaColorArray;
    idx : integer;
    x, y : integer;
    numImg : integer;
    sl : TStringList;
    i : integer;
    basePath : string;
begin
     // load a bunch of images and calculate a PCA. Note the images
     Result := nil;
     imgNum := 0;
     w := 0;
     h := 0;
     basePath := BaseDataPath;

     path := IncludeTrailingPathDelimiter(basePath + path);
     sl := TStringList.Create;

     numImg := 0;
     if FindFirst(Path + ext, 0, sr) = 0 then
     begin
          repeat
                sl.Add( Path + sr.Name );
                inc(numImg);
          until FindNext(sr) <> 0;
     end;

     FindClose(sr);

     sl.Sort;

     for i := 0 to sl.Count - 1 do
     begin
          img := TBitMap.Create;
          try
             img.LoadFromFile(sl[i]);

             if not Assigned(Result) then
             begin
                  w := img.Width;
                  h := img.Height;
                  Result := TDoubleMatrix.Create(numImg,img.Width*img.Height);
             end;
             img.Map(TMapAccess.Read, bd1);
             // create matrix from image
             idx := 0;
             for y := 0 to img.Height - 1 do
             begin
                 // pScanLine := bmp.bitmapdata.ScanLine[y];
                  p1 := PAlphaColorArray(bd1.GetScanline(y));
                  for x := 0 to img.Width - 1 do
                  begin
                       //Result[imgNum, idx] := Round(pScanline^.rgbBlue*0.1140 + pScanline^.rgbRed*0.2989 + pScanline^.rgbGreen*0.5870);
                       Result[imgNum, idx] := Round(TAlphaColorRec(PixelToAlphaColor(@(p1^[x]),img.PixelFormat)).B*0.1140 + TAlphaColorRec(PixelToAlphaColor(@(p1^[x]),img.pixelformat)).R*0.2989 + TAlphaColorRec(PixelToAlphaColor(@(p1^[x]),img.Pixelformat)).G*0.5870);
                       inc(idx);
                  end;
             end;
             img.Unmap(bd1);
             inc(imgNum);
          finally
                 img.Free;
          end;
     end;
     sl.Free;
end;
{$ENDIF}

function TBaseMatrixTestCase.WriteMtx(mtx: TDoubleMatrix;
  prec: integer): string;
begin
     Result := WriteMtx( mtx.StartElement, mtx.LineWidth, mtx.Width, mtx.Height, prec);
end;

function TBaseMatrixTestCase.BaseDataPath: string;
begin
     if ParamCount > 0
     then
         Result := IncludeTrailingPathDelimiter(ParamStr(1))
     else
         Result := IncludeTrailingPathDelimiter( ExtractFilePath(ParamStr(0)) );
end;

function TBaseMatrixTestCase.CheckMtxVal(data: PDouble; lineWidth: TASMNativeInt;
  width, height: integer; value: double): boolean;
var x, y: Integer;
    pMtx : PConstDoubleArr;
begin
     Result := True;
     pMtx := PConstDoubleArr( data );
     for y := 0 to Height - 1 do
     begin
          for x := 0 to width - 1 do
              Result := Result and (pMtx^[x] = value);

          inc(PByte(pMtx), LineWidth);
     end;
end;


function TBaseMatrixTestCase.CheckMtxVal(const mtx: array of double;
  value: double): boolean;
begin
     Result := CheckMtxVal(@mtx[0], Length(mtx)*sizeof(double), Length(mtx), 1, value);
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
