unit TestNMF;

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

interface

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
     Classes, SysUtils, BaseMatrixTestCase;

type
 {$IFDEF FMX} [TestFixture] {$ENDIF}
 TTestNMF = class(TBaseImgTestCase)
 private
   procedure OnNMFProgress(Sender : TObject; progress : integer);
 published
   procedure TestSimplePatternDiv;
   procedure TestSimplePatternEukl;
   procedure TestSimplePatternAltLeastSquare;
   procedure TestNMFImages;
   procedure TestNMFPersistence;
 end;

implementation

uses NNMF, Matrix, OptimizedFuncs, BinaryReaderWriter,
  BaseMathPersistence;

{ TTestNMF }

procedure TTestNMF.OnNMFProgress(Sender: TObject; progress: integer);
begin
     Status('NMF Progress: ' + IntToStr(progress));
end;

procedure TTestNMF.TestNMFImages;
var x : TDoubleMatrix;
    w, h : integer;
    nmf : TNNMF;
    imgs : TDoubleMatrix;
    i: Integer;
    nmfProps : TNNMFProps;
    maxval : double;
begin
     // load images
     x := LoadImages(w, h, 'NMFTest', '*.bmp');

     // create nmf with standard properties
     nmf := TNNMF.Create;
     try
        nmf.OnProgress := {$IFDEF FPC}@{$ENDIF}OnNMFProgress;
        nmfProps.MaxIter := 400;
        nmfProps.tolUpdate := 0;
        nmfProps.method := nnmfDivergence;
        nmfProps.RankOfBasis := 10;

        nmf.SetProperties(nmfProps);
        nmf.CalcNMF(x);

        Status('W: ' + IntToStr(nmf.w.width) + ',' + IntTostr(nmf.w.height));
        Status('H: ' + IntToStr(nmf.H.width) + ',' + IntTostr(nmf.H.height));

        // write images into the directory
        imgs := nmf.Reconstruct(nmf.H);

        for i := 0 to imgs.Width - 1 do
        begin
             imgs.SetSubMatrix(i, 0, 1, imgs.Height);
             ImageFromMatrix(imgs, w, h, Format('%s%snmf_%d.bmp', [ExtractFilePath(ParamStr(0)), PathDelim, i]));
        end;

        for i := 0 to nmf.W.Width - 1 do
        begin
             nmf.W.SetSubMatrix(i, 0, 1, nmf.W.Height);
             maxval := nmf.W.Max;
             nmf.W.ScaleInPlace(255/maxVal);
             ImageFromMatrix(nmf.W, w, h, Format('%s%snmf_w_%d.bmp', [ExtractFilePath(ParamStr(0)), PathDelim, i]));
        end;

        imgs.Free;

        
     finally
            nmf.Free;
            x.Free;
     end;
end;

procedure TTestNMF.TestNMFPersistence;
var V : IMatrix;
    nmf : TNNMF;
    nmfLoad : TNNMF;
    params : TNNMFProps;
begin
     // just check if an object can be stored and loaded again
     V := TDoubleMatrix.CreateEye(10);
     nmf := TNNMF.Create;
     try
        FillChar(params, sizeof(params), 0);
        params.MaxIter := 100;
        params.tolUpdate := 1e-4;
        params.method := nnmfAlternateLeastSquare;
        params.RankOfBasis := 8;
        params.UseLastResIfFail := True;
        params.DoUpdateWithEPSMtx := True;

        nmf.SetProperties(params);
        RandSeed := 331;

        Check( nmf.CalcNMF(V.GetObjRef) <> nmFailed, 'NMF Failed');

        nmf.SaveToFile('nmf.dat', TBinaryReaderWriter);

        nmfLoad := ReadObjFromFile('nmf.dat') as TNNMF;
        try
           Check( CheckMtx(nmfLoad.W.SubMatrix, nmf.W.SubMatrix), 'NMF Load failed');
        finally
               nmfLoad.Free;
        end;
     finally
            nmf.Free;
     end;
     
end;

procedure TTestNMF.TestSimplePatternAltLeastSquare;
var V : IMatrix;
    nmf : TNNMF;
    params : TNNMFProps;
    res : TNMFRes;
{$IFNDEF FPC}
const cExpectedW : Array[0..79] of double = 
   ( 0, 0, 0, 0, 0, 0.3249, 0, 0,
     0, 0.2142, 0, 0, 0, 0, 0, 0, 
     0, 0, 0.3913, 0, 0, 0, 0, 0,
     0, 0, 0, 0.1510, 0, 0, 0, 0,
     0, 0.1920, 0, 0, 0, 0.0003, 0, 0,
     0.24939, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0.1045, 0,
     0, 0, 0, 0, 0, 0, 0, 0.1482,
     0, 0, 0, 0, 0.3559, 0, 0, 0, 
     0.12453, 0, 0, 0.0058, 0, 0.0037, 0.00079, 0.00098 );
{$ENDIF}
begin
     V := TDoubleMatrix.CreateEye(10);
     nmf := TNNMF.Create;
     params.MaxIter := 100;
     params.tolUpdate := 1e-4;
     params.method := nnmfAlternateLeastSquare;
     params.RankOfBasis := 8;
     params.UseLastResIfFail := True;
     params.DoUpdateWithEPSMtx := True;
     nmf.SetProperties(params);

     RandSeed := 331;

     res := nmf.CalcNMF(V.GetObjRef);
     if res <> nmFailed then
     begin
          Status(WriteMtx(nmf.W.SubMatrix, nmf.W.Width));
          Status(WriteMtx(nmf.H.SubMatrix, nmf.H.Width));
     end;

     Check(res <> nmFailed, 'Error calculating NMF from an eye matrix');
     {$IFNDEF FPC}
     // note the FPC uses a different random generator -> results may differ
     Check(CheckMtx(nmf.W.SubMatrix, cExpectedW), 'Error calculating the nmf transformation matrix');
     {$ENDIF}

     nmf.Free;
end;

procedure TTestNMF.TestSimplePatternDiv;
var V : IMatrix;
    nmf : TNNMF;
    params : TNNMFProps;
    res : TNMFRes;
{$IFNDEF FPC}
const cExpectedW : Array[0..79] of double = 
   ( 0, 0, 0, 0, 0, 1, 0, 0,
     0, 1, 0, 0, 0, 0, 0, 0, 
     0, 0, 0.5, 0, 0, 0, 0, 0,
     0, 0, 0.5, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 1,
     1, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 1, 0,
     0, 0, 0, 0.5, 0, 0, 0, 0,
     0, 0, 0, 0, 1, 0, 0, 0, 
     0, 0, 0, 0.5, 0, 0, 0, 0 );
{$ENDIF}
begin
     V := TDoubleMatrix.CreateEye(10);
     nmf := TNNMF.Create;
     params.MaxIter := 100;
     params.tolUpdate := 1e-4;
     params.method := nnmfDivergence;
     params.RankOfBasis := 8;
     params.UseLastResIfFail := True;
     params.DoUpdateWithEPSMtx := True;
     nmf.SetProperties(params);

     RandSeed := 331;

     res := nmf.CalcNMF(V.GetObjRef);
     if res <> nmFailed then
     begin
          Status(WriteMtx(nmf.W.SubMatrix, nmf.W.Width));
          Status(WriteMtx(nmf.H.SubMatrix, nmf.H.Width));
     end;

     Check(res <> nmFailed, 'Error calculating NMF from an eye matrix');
     {$IFNDEF FPC}
     // note the FPC uses a different random generator -> results may differ
     Check(CheckMtx(nmf.W.SubMatrix, cExpectedW), 'Error calculating the nmf transformation matrix');
     {$ENDIF}

     nmf.Free;
end;

procedure TTestNMF.TestSimplePatternEukl;
var V : IMatrix;
    nmf : TNNMF;
    params : TNNMFProps;
    res : TNMFRes;
{$IFNDEF FPC}
const cExpectedW : Array[0..79] of double = 
   ( 0.2285, 0, 0, 0, 0, 0.0005, 0, 0,
     0, 0.2378, 0, 0.0001, 0, 0, 0, 0, 
     0, 0, 0.2486, 0, 0, 0, 0, 0.0002,
     0, 0, 0.0001, 0, 0, 0.2476, 0, 0,
     0.0005, 0, 0, 0, 0, 0, 0, 0.2019,
     0.0030, 0, 0, 0, 0.1650, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0.1880, 0.0001,
     0, 0, 0.0406, 0.0556, 0, 0, 0.0010, 0,
     0, 0, 0, 0, 0.1503, 0.0034, 0, 0.0009, 
     0.0002, 0, 0, 0.2514, 0, 0, 0, 0 );
{$ENDIF}
begin
     V := TDoubleMatrix.CreateEye(10);
     nmf := TNNMF.Create;
     params.MaxIter := 100;
     params.tolUpdate := 1e-4;
     params.method := nnmfEukledian;
     params.RankOfBasis := 8;
     params.UseLastResIfFail := True;
     params.DoUpdateWithEPSMtx := True;
     nmf.SetProperties(params);

     RandSeed := 331;

     res := nmf.CalcNMF(V.GetObjRef);
     if res <> nmFailed then
     begin
          Status(WriteMtx(nmf.W.SubMatrix, nmf.W.Width));
          Status(WriteMtx(nmf.H.SubMatrix, nmf.H.Width));
     end;

     Check(res <> nmFailed, 'Error calculating NMF from an eye matrix');
     {$IFNDEF FPC}
     // note the FPC uses a different random generator -> results may differ
     Check(CheckMtx(nmf.W.SubMatrix, cExpectedW), 'Error calculating the nmf transformation matrix');
     {$ENDIF}

     nmf.Free;
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TTestNMF{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
