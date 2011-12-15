unit TestPCA;

interface

uses
  Windows, TestFramework, Classes, SysUtils, BaseMatrixTestCase, Matrix;

type
 // Testmethoden für Klasse TDoubleMatrix

 TTestPCA = class(TBaseMatrixTestCase)
 private
   procedure ImageFromMatrix(img : TDoubleMatrix; w, h : integer; const FileName : string);
   function LoadImages(var w, h : integer) : TDoubleMatrix;
 published
   procedure TestPCASimple;
   procedure TestPCAImages;
   procedure TestPCAWeighted;
   procedure TestFastRobustPCA;
 end;

implementation

uses PCA, Dialogs, Graphics, JPeg, Math, BinaryReaderWriter, BaseMathPersistence;

{ TTestEigensystems }

procedure TTestPCA.ImageFromMatrix(img: TDoubleMatrix; w, h : integer;
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

function TTestPCA.LoadImages(var w, h : integer): TDoubleMatrix;
var path : string;
    imgNum : integer;
    jpg : TJPEGImage;
    bmp : TBitmap;
    sr : TSearchRec;
    pScanLine : PRGBTriple;
    idx : integer;
    x, y : integer;
begin
     // load a bunch of images and calculate a PCA. Note the images
     Result := nil;
     imgNum := 0;
     w := 0;
     h := 0;
     path := ExtractFilePath(ParamStr(0)) + '\Images\';
     if FindFirst(Path + '*.jpg', 0, sr) = 0 then
     begin
          repeat
                jpg := TJPEGImage.Create;
                try
                   jpg.LoadFromFile(path + sr.name);

                   bmp := TBitmap.Create;
                   try
                      bmp.Assign(jpg);
                      assert(bmp.PixelFormat = pf24bit);

                      if not Assigned(Result) then
                      begin
                           w := bmp.Width;
                           h := bmp.Height;
                           Result := TDoubleMatrix.Create(20, bmp.Width*bmp.Height);
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
                       jpg.Free;
                end;
          until FindNext(sr) <> 0;
     end;
     FindClose(sr);
end;

procedure TTestPCA.TestFastRobustPCA;
var Examples : TDoubleMatrix;
    img : TDoubleMatrix;
    feature : TDoubleMatrix;
    w, h : integer;
    i : integer;
  //  start : Cardinal;
  //  stop : Cardinal;
    x : integer;
    props : TFastRobustPCAProps;
begin
     Examples := LoadImages(w, h);

     // ############################################
     // #### Calculate PCA on images
     props.NumSubSubSpaces := 100;
     props.SubSpaceSizes := 0.003;
     props.Start := 50;
     props.stop := 20;
     props.ReductionFactor := 0.75;
     props.SubSpaceCutEPS := 0;

     with TFastRobustPCA.Create([pcaTransposedEigVec]) do
     try
        SetProperties(props);
        Check(PCA(Examples, 0.8, True) = True, 'Error in PCA');

        // create 50% random occlusion
        for i := 0 to 5 do
        begin
             for x := 0 to Round(0.5*Examples.Height) do
                 Examples[i, random(Examples.Height - 1)] := 255;
        end;

        // create a few examples along the first mode
    //    start := GetTickCount;
        for i := 0 to 5 do
        begin
             Examples.SetSubMatrix(i, 0, 1, Examples.Height);
             feature := ProjectToFeatureSpace(Examples);
             try
                img := Reconstruct(feature);
                try
                   ImageFromMatrix(img, w, h, Format('%s\bmpr_%d.bmp', [ExtractFilePath(ParamStr(0)), i - 2]));
                finally
                       img.Free;
                end;
             finally
                    feature.Free;
             end;

             // ############################################
             // #### For comparison -> project to feature space in a non robust way
             feature := ProjectToFeatureSpaceNonRobust(Examples);
             try
                img := Reconstruct(feature);
                try
                   ImageFromMatrix(img, w, h, Format('%s\bmpnr_%d.bmp', [ExtractFilePath(ParamStr(0)), i - 2]));
                finally
                       img.Free;
                end;
             finally
                    feature.Free;
             end;
        end;
     //   stop := GetTickCount;

        //ShowMessage(IntToStr(stop - start));
     finally
            Free;
     end;

     FreeAndNil(examples);
end;

procedure TTestPCA.TestPCAImages;
var Examples : TDoubleMatrix;
    img : TDoubleMatrix;
    feature : TDoubleMatrix;
    w, h : integer;
    i : integer;
begin
     Examples := LoadImages(w, h);

     // ############################################
     // #### Calculate PCA on images
     with TMatrixPCA.Create([pcaEigVals, pcaMeanNormalizedData, pcaTransposedEigVec]) do
     try
        Check(PCA(Examples, 0.95, True) = True, 'Error in PCA');

        // create a few examples along the first mode
        feature := TDoubleMatrix.Create(1, NumModes);
        try
           for i in [0, 1, 2, 3, 4 ] do
           begin
                feature[0, 0] := (i - 2)*sqrt(EigVals[0,0]);
                img := Reconstruct(feature);
                try
                   ImageFromMatrix(img, w, h, Format('%s\bmp1_%d.bmp', [ExtractFilePath(ParamStr(0)), i - 2]));
                finally
                       img.Free;
                end;
           end;
        finally
               feature.Free;
        end;
     finally
            Free;
     end;

     FreeAndNil(examples);
end;

procedure TTestPCA.TestPCASimple;
const cData : Array[0..19] of double =
  ( 2.5000e+001,  2.5000e+001,  2.5000e+001,  2.5300e+001,  2.5200e+001,  2.5200e+001,  2.4800e+001,  2.4700e+001,  2.5100e+001,  2.4700e+001,
    1.0100e+001,  9.9000e+000,  1.0000e+001,  9.8000e+000,  9.9000e+000,  9.8000e+000,  1.0100e+001,  1.0300e+001,  1.0000e+001,  1.0100e+001);
var Examples : TDoubleMatrix;
    pca1, pca2 : TMatrixPCA;
    writer : TBinaryReaderWriter;
begin
     Examples := TDoubleMatrix.Create;
     try
        Examples.Assign(cData, 10, 2);
        Examples.TransposeInPlace;

        pca2 := nil;
        pca1 := TMatrixPCA.Create([pcaEigVals, pcaMeanNormalizedData, pcaTransposedEigVec]);

        try
           pca1.PCA(Examples, 0.80, True);

           writer := TBinaryReaderWriter.Create;
           writer.SaveToFile(pca1, 'pca.dat');
           writer.Free;

           pca2 := ReadObjFromFile('pca.dat') as TMatrixPCA;

           Check(assigned(pca2), 'Error loading failed');
           Check( CheckMtx(pca2.EigVecs.SubMatrix, pca1.EigVecs.SubMatrix), 'Error loading failed');
           Check( CheckMtx(pca2.Mean.SubMatrix, pca1.Mean.SubMatrix), 'Error loading failed');
        finally
               pca1.free;
               pca2.Free;
        end;
     finally
            Examples.Free;
     end;
end;

procedure TTestPCA.TestPCAWeighted;
var Examples : TDoubleMatrix;
    img : TDoubleMatrix;
    feature : TDoubleMatrix;
    w, h : integer;
    i : integer;
begin
     Examples := LoadImages(w, h);

     // ############################################
     // #### Calculate PCA on images
     with TMatrixPCA.Create([pcaEigVals, pcaMeanNormalizedData, pcaTransposedEigVec]) do
     try
        Check(TemporalWeightPCA(Examples, 0.95, True, [0.001, 0.001, 0.001, 0.002, 0.1, 0.2, 0.001, 0.002, 0.001, 0.002, 10, 0.002, 0.001, 22, 0.001, 0.002, 0.001, 0.002, 0.001, 0.002]) = True, 'Error in PCA');
        //Check(TemporalWeightPCA(Examples, 0.95, True, [1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20, 1/20]) = True, 'Error in PCA');

        // create a few examples along the first mode
        feature := TDoubleMatrix.Create(1, NumModes);
        try
           for i in [0, 1, 2, 3, 4 ] do
           begin
                feature[0, 0] := (i - 2)*sqrt(EigVals[0,0]);
                img := Reconstruct(feature);
                try
                   ImageFromMatrix(img, w, h, Format('%s\bmp2_%d.bmp', [ExtractFilePath(ParamStr(0)), i - 2]));
                finally
                       img.Free;
                end;
           end;
        finally
               feature.Free;
        end;
     finally
            Free;
     end;

     FreeAndNil(examples);
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TTestPCA.Suite);

end.
