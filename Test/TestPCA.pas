// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2014, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestPCA;

interface

uses {$IFDEF FPC} testregistry {$ELSE} TestFramework {$ENDIF}, 
     Classes, SysUtils, BaseMatrixTestCase, Matrix;

type
 TTestPCA = class(TBaseImgTestCase)
 published
   procedure TestPCASimple;
   procedure TestPCAImages;
   procedure TestPCAIncremental;
   procedure TestPCAWeightedIncremental;
   procedure TestPCAWeighted;
   procedure TestFastRobustPCA;
   procedure TestFastRubustIncrementalPCA;
 end;

implementation

uses PCA, 
     {$IFDEF MACOS} FMX.Types, {$ENDIF}
     Graphics, BinaryReaderWriter, BaseMathPersistence, IncrementalPCA,
  JSONReaderWriter;

{ TTestEigensystems }

procedure TTestPCA.TestPCASimple;
const cData : Array[0..19] of double =
  ( 2.5000e+001,  2.5000e+001,  2.5000e+001,  2.5300e+001,  2.5200e+001,  2.5200e+001,  2.4800e+001,  2.4700e+001,  2.5100e+001,  2.4700e+001,
    1.0100e+001,  9.9000e+000,  1.0000e+001,  9.8000e+000,  9.9000e+000,  9.8000e+000,  1.0100e+001,  1.0300e+001,  1.0000e+001,  1.0100e+001);
var Examples : TDoubleMatrix;
    pca1, pca2 : TMatrixPCA;
    writer : TBinaryReaderWriter;
    jsonWrite : TJsonReaderWriter;
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

           jsonWrite := TJsonReaderWriter.Create;
           jsonWrite.SaveToFile(pca1, 'pca.json');
           jsonWrite.Free;

           pca2 := ReadObjFromFile('pca.dat') as TMatrixPCA;

           Check(assigned(pca2), 'Error loading failed');
           Check( CheckMtx(pca2.EigVecs.SubMatrix, pca1.EigVecs.SubMatrix), 'Error loading failed');
           Check( CheckMtx(pca2.Mean.SubMatrix, pca1.Mean.SubMatrix), 'Error loading failed');

           pca2.Free;
           pca2 := ReadObjFromFile('pca.json') as TMatrixPCA;
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
     finally
            Free;
     end;

     FreeAndNil(examples);
end;

procedure TTestPCA.TestFastRubustIncrementalPCA;
var Examples : TDoubleMatrix;
    img : TDoubleMatrix;
    feature : TDoubleMatrix;
    w, h : integer;
    i : integer;
    //start : Cardinal;
    //stop : Cardinal;
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

     with TFastRobustIncrementalPCA.Create([pcaEigVals]) do
     try
        NumEigenvectorsToKeep := 6;

        SetProperties(props);

        for i := 0 to Examples.Width - 1 do
        begin
             Examples.SetSubMatrix(i, 0, 1, Examples.Height);
             Check(UpdateEigenspace(Examples), 'Error updating eigenspace');
        end;

        Examples.UseFullMatrix;

        // create 50% random occlusion
        for i := 0 to 5 do
        begin
             for x := 0 to Round(0.5*Examples.Height) do
                 Examples[i, random(Examples.Height - 1)] := 255;
        end;

        // create a few examples along the first mode
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

procedure TTestPCA.TestPCAIncremental;
var Examples : TDoubleMatrix;
    img : TDoubleMatrix;
    feature : TDoubleMatrix;
    w, h : integer;
    i : integer;
begin
     Examples := LoadImages(w, h);

     // ############################################
     // #### Calculate PCA on images
     with TIncrementalPCA.Create([pcaEigVals]) do
     try
        NumEigenvectorsToKeep := 20;

        for i := 0 to Examples.Width - 1 do
        begin
             Examples.SetSubMatrix(i, 0, 1, Examples.Height);
             Check(UpdateEigenspace(Examples), 'Error updating eigenspace');
        end;

        // create a few examples along the first mode
        feature := TDoubleMatrix.Create(1, NumModes);
        try
           for i in [0, 1, 2, 3, 4 ] do
           begin
                feature[0, 0] := (i - 2)*sqrt(EigVals[0,0])/3;
                img := Reconstruct(feature);
                try
                   ImageFromMatrix(img, w, h, Format('%s\bmp_inc_%d.bmp', [ExtractFilePath(ParamStr(0)), i - 2]));
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

procedure TTestPCA.TestPCAWeightedIncremental;
var Examples : TDoubleMatrix;
    img : TDoubleMatrix;
    feature : TDoubleMatrix;
    w, h : integer;
    i : integer;
    numExamples : integer;
begin
     Examples := LoadImages(w, h);
     numExamples := Examples.Width;

     // ############################################
     // #### Calculate PCA on images
     with TIncrementalPCA.Create([pcaEigVals]) do
     try
        NumEigenvectorsToKeep := 6;

        for i := 0 to Examples.Width - 1 do
        begin
             Examples.SetSubMatrix(i, 0, 1, Examples.Height);
             Check(UpdateEigenspaceWeighted(Examples, 1/numExamples), 'Error updating eigenspace');
        end;

        // create a few examples along the first mode
        feature := TDoubleMatrix.Create(1, NumModes);
        try
           for i in [0, 1, 2, 3, 4 ] do
           begin
                feature[0, 0] := (i - 2)*sqrt(EigVals[0,0]);
                img := Reconstruct(feature);
                try
                   ImageFromMatrix(img, w, h, Format('%s\bmp_incw_%d.bmp', [ExtractFilePath(ParamStr(0)), i - 2]));
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
  RegisterTest(TTestPCA{$IFNDEF FPC}.Suite{$ENDIF});

end.
