// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2019, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################


unit TestSpecialFunc;

interface

// ###########################################
// #### Simple tests for the special functions defined in statitiscts and mathutilfunc
// ###########################################

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry, {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF}, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils;

type
  // testmethoden für die matrix funktionen
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TTestSpecialFuncs = class(TBaseMatrixTestCase)
  published
    procedure TestQuickSort1;
    procedure TestBigQuickSort1;
    procedure TestQuickSort2;
    procedure TestGammaLN;
    procedure TestExponentialIntegral;
    procedure TestIncompleteBeta;
    procedure TestGammaP;
    
  end;

implementation

uses MathUtilFunc, Types, Statistics, MtxTimer, Math, Matrix;

{ TTestEM }

procedure TTestSpecialFuncs.TestBigQuickSort1;
var i : integer;
    A, B : TDoubleDynArray;
    start1, stop1 : Int64;
    start2, stop2 : int64;
function CheckArr : boolean;
var i : integer;
begin
     Result := a[0] = b[0];
     for i := 1 to Length(a) - 1 do
         Result := Result and (a[i - 1] <= a[i]) and (a[i] = b[i]);
end;
begin
     SetLength(A, 500000);
     SetLength(B, 500000);

     for i := 0 to Length(A) - 1 do
     begin
          A[i] := random(10000);
          B[i] := A[i];
     end;

     start1 := MtxGetTime;
     QuickSort(A);
     stop1 := MtxGetTime;

     start2 := MtxGetTime;
     QuickSort1(B);
     stop2 := MtxGetTime;

     Status(Format('Sort took: %.3fms, %.3fms', [(stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

     Check( CheckArr, 'Big sort failed');
end;

procedure TTestSpecialFuncs.TestQuickSort1;
var a : TDoubleDynArray;
    i : integer;
function CheckArr : boolean;
var i : integer;
begin
     Result := True;
     for i := 1 to Length(a) - 1 do
         Result := Result and (a[i - 1] <= a[i]);
end;
begin
     // two tests on short arrays -> already sorted, randomized
     SetLength(a, 5);
     for I := 0 to Length(a) - 1 do
         a[i] := i;

     QuickSort1(A);
     Check( CheckArr, 'Small already sorted items failed' );

     for I := 0 to Length(a) - 1 do
         a[i] := Random(100);

     QuickSort1(A);
     Check( CheckArr, 'Small sort failed' );

     // long ones
     SetLength(a, 110);
     for I := 0 to Length(a) - 1 do
         a[i] := i;

     QuickSort1(A);
     Check( CheckArr, 'Big already sorted items failed' );

     for I := 0 to Length(a) - 1 do
         a[i] := Random(100);

     QuickSort1(A);
     Check( CheckArr, 'Big sort failed' );
end;

procedure TTestSpecialFuncs.TestQuickSort2;
var a, b : TDoubleDynArray;
    i : integer;
function CheckArrs : boolean;
var i : integer;
begin
     Result := a[0] = b[0];
     for i := 1 to Length(a) - 1 do
         Result := Result and (a[i - 1] <= a[i]) and (a[i] = b[i]);
end;
begin
     // two tests on short arrays -> already sorted, randomized
     SetLength(a, 5);
     SetLength(b, 5);
     for I := 0 to Length(a) - 1 do
     begin
          a[i] := i;
          b[i] := i;
     end;

     QuickSort2(A, B);
     Check( CheckArrs, 'Small already sorted items failed' );

     for I := 0 to Length(a) - 1 do
     begin
          a[i] := Random(100);
          b[i] := a[i];;
     end;

     QuickSort2(A, B);
     Check( CheckArrs, 'Small sort failed' );

     // long ones
     SetLength(a, 110);
     SetLength(b, 110);
     for I := 0 to Length(a) - 1 do
     begin
          a[i] := i;
          b[i] := i;
     end;

     QuickSort2(A, B);
     Check( CheckArrs, 'Big already sorted items failed' );

     for I := 0 to Length(a) - 1 do
     begin
          a[i] := Random(100);
          b[i] := a[i];
     end;

     QuickSort2(A, B);
     Check( CheckArrs, 'Big sort failed' );
end;

procedure TTestSpecialFuncs.TestExponentialIntegral;
var n : integer;
    xy : TDoubleDynArray;
    i : integer;

begin
     SetLength(xy, 2*1000);
     for n := 0 to 5 do
     begin
          for i := 0 to 1000 - 1 do
          begin
               xy[i*2 + 0] := 1e-5 + i*3/1001;
               xy[i*2 + 1] := expInt( n, xy[i*2 + 0] );
          end;

          WriteMatlabData( 'expInt_' + IntTosTr(n) + '.txt', xy, 2);
     end;
end;

procedure TTestSpecialFuncs.TestGammaLN;
const cTestVals : Array[0..5] of double = (0.01, 0.2, 1.1, 3.3, 45, 87);
var xln2, xln1 : double;
    i : integer;
    hasExp1, hasExp2 : boolean;
begin
     for i := 0 to High(cTestVals) do
     begin
          xln1 := ln( Gamma(cTestVals[i]) );
          xln2 := Gammaln(cTestVals[i]);

          Check( SameValue(xln1, xln2, 1e-5), 'gammaln failed');
     end;

     hasExp1 := False;
     hasExp2 := False;
     try
        {$IFDEF FPC}xln1 := {$ENDIF}ln( Gamma(-1));
     except
           hasExp1 := True;
     end;
     try
        gammaln(-1);
     except
           hasExp2 := True;
     end;

     Check( hasExp1 and hasExp2, 'Gammaln of -1 may not be defined');
end;

procedure TTestSpecialFuncs.TestGammaP;
var x, y : double;
    i, j : integer;
    mat : IMatrix;
    idx : integer;
    ref : IMatrix;
begin
     ref := MatrixFromTxtFile(BaseDataPath + 'gammaInc_ref.txt');
     mat := TDoubleMatrix.Create(4, 100);
     idx := 0;
     for i := 0 to 9 do
     begin
          // between 0 and 1
          y := 0.001 + i/10;
          for j := 0 to 9 do
          begin
               // between 0 and 9/4
               x := 0.002 + j/4;

               mat[0, idx] := x;
               mat[1, idx] := y;
               mat[2, idx] := GammaP(y, x);
               mat[3, idx] := GammaQ(y, x);

               check( SameValue( mat[2, idx] + mat[3, idx], 1, 1e-6 ), 'Incomplete gamma calculation failed');
               
               inc(idx);
          end;
     end;
     // MatrixToTxtFile('matqp.txt', mat.GetObjRef);
     
     for i := 0 to mat.Height - 1 do
         Check( SameValue( mat[2, i], ref.Vec[i], 1e-5), 'Incomplete Gamma failed to check on the reference');
end;

procedure TTestSpecialFuncs.TestIncompleteBeta;
const cA : Array[0..7] of double = (0.01, 0.1, 0.9, 1, 1.5, 3, 6, 7.2);
      cX : Array[0..3] of double = (0.01, 0.2, 0.7, 0.95);
      cW : Array[0..4] of double = (0.2, 0.7, 1, 2, 7.2);
var ia, ix, iw : integer;
    idx : integer;
    imx : IMatrix;
    imRef : IMatrix;
begin
     imRef := MatrixFromTxtFile(BaseDataPath + 'betaInc_ref.txt');  // generated in matlab with above values
     
     imx := TDoubleMatrix.Create( 4, Length(cA)*Length(cX)*Length(cW) );
     idx := 0;
     for ia := 0 to high(cA) do
         for ix := 0 to High(cX) do
             for iw := 0 to High(cW) do
             begin
                  imx[0, idx] := cA[ ia ];
                  imx[1, idx] := cX[ ix ];
                  imx[2, idx] := cW[ iW ];

                  imx[3, idx] := betaI(cA[ ia ], cW[ iw ], cX[ ix ]);
                  inc(idx);
             end;


     // ###########################################
     // #### check agains reference
     for ia := 0 to imRef.Height - 1 do
         check( SameValue( imRef.Vec[ia], imx[3, ia], 1e-6), 'Error in incomplete beta function evaluation');
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TTestSpecialFuncs{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}


end.
