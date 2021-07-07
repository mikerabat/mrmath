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


unit TestEigensystems;

interface

{$IFDEF MACOS}
  {$DEFINE FMX}
{$ENDIF}

uses {$IFDEF FPC} testregistry {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
     Classes, SysUtils, BaseMatrixTestCase;

type
 {$IFDEF FMX} [TestFixture] {$ENDIF}
 TTestEigensystems = class(TBaseMatrixTestCase)
 published
  procedure TestTridiagonalHousholderMethod;
  procedure TestQLImplicitShift;
  procedure TestHessenberg;
  procedure TestHessenberg2;
  procedure TestHessenbergBlk;
  procedure TestHessenbergCmplx;
  procedure TestHessenberg3;    
  procedure TestHessenbergBig;
  procedure TestHessenbergThr;
  procedure TestFullHessenberg;
  
  procedure TestEigVec1;
  procedure TestEigVecComplex;
  procedure TestBalance;
  procedure TestLinearDependentVectors;
 end;

implementation

uses Eigensystems, MatrixConst, MatrixASMStubSwitch, Matrix, Types, MtxTimer, 
  BlockSizeSetup, MtxThreadPool;

{ TTestEigensystems }

procedure TTestEigensystems.TestBalance;
const B : Array[0..8] of double = (3, 1, 1, 2, -2, -1, 5, -1, 8);
      EigVals : Array[0..2] of double = (2.757, 8.862, -2.619);
      EigVec : Array[0..8] of double = (1, 0.1599, -0.2125, 0.5969, -0.0626, 1.0000, -0.8399, 1, 0.1942);
var dest : Array[0..8] of double;
    Eivec : Array[0..8] of double;
    Wr : Array[0..2] of double;
    Wi : Array[0..2] of double;
begin
     FillChar(Eivec[0], sizeof(Eivec), 0);
     
     Move(B, dest, sizeof(B));
     Check(qlOk = MatrixUnsymEigVecInPlace(@dest[0], 3*sizeof(double), 3, @Wr[0], sizeof(double), @wi[0], sizeof(double), 
                              @eivec[0], 3*sizeof(double)), 'Error no convergence');
     MatrixNormEivecInPlace(@Eivec[0], 3*SizeOf(double), 3, @WI[0], sizeof(double));

     Check(CheckMtx(EigVals, Wr, -1, -1, 0.001), 'Error wrong eigenvalues');
     Check(CheckMtx(EigVec, Eivec, -1, -1, 0.001), 'Error wrong eigenvectors');
end;


procedure TTestEigensystems.TestEigVec1;
const B : Array[0..8] of double = (3, 1, 1, 2, -2, -1, 5, -1, 8);
      EigVals : Array[0..2] of double = (2.757, 8.862, -2.619);
      EigVec : Array[0..8] of double = (1, 0.1599, -0.2125, 0.5969, -0.0626, 1.0000, -0.8399, 1, 0.1942);
var dest : Array[0..8] of double;
    Eivec : Array[0..8] of double;
    Wr : Array[0..2] of double;
    Wi : Array[0..2] of double;
begin
     FillChar(wr, sizeof(wr), 0);
     FillChar(wi, sizeof(wi), 0);
     FillChar(Eivec[0], sizeof(Eivec), 0);
     Move(B, dest, sizeof(dest));
     
     checK(qlOk = MatrixUnsymEigVecInPlace(@dest[0], 3*sizeof(double), 3, @wr[0], sizeof(double), @wi[0], sizeof(double), 
                                           @eivec[0], 3*sizeof(double)), 'Error no convergence');

     MatrixNormEivecInPlace(@Eivec[0], 3*SizeOf(double), 3, @WI[0], sizeof(double));

     Check(CheckMtx(EigVals, Wr, -1, -1, 0.001), 'Error wrong eigenvalues');
     Check(CheckMtx(EigVec, Eivec, -1, -1, 0.001), 'Error wrong eigenvectors');
end;

procedure TTestEigensystems.TestEigVecComplex;
const A : Array[0..15] of double = (10, 9, 8, 9, 2, 8, 4, 7, 6, 5, 6, 2, 5, 0, 8, 4);
      EigValsR : Array[0..3] of double = (23.2603, 0.9905, 0.9905, 2.7587);
      EigValsI : Array[0..3] of double = (0, 4.5063, -4.5063, 0);
      EigVec : Array[0..15] of double = (1, -0.358, -0.1699, -0.921, 0.498, -0.421, -0.604, 1, 0.548, -0.152, 0.669, 0.715, 0.487, 1, 0, -0.894);
var dest : Array[0..15] of double;
    Eivec : Array[0..15] of double;
    Wr : Array[0..3] of double;
    Wi : Array[0..3] of double;
begin
     FillChar(wr, sizeof(wr), 0);
     FillChar(wi, sizeof(wi), 0);
     FillChar(Eivec[0], sizeof(Eivec), 0);
     Move(A, dest, sizeof(dest));
     checK(qlOk = MatrixUnsymEigVecInPlace(@dest[0], 4*sizeof(double), 4, @wr[0], sizeof(double), @wi[0], sizeof(double), 
                                           @eivec[0], 4*sizeof(double)), 'Error no convergence');

     MatrixNormEivecInPlace(@Eivec[0], 4*SizeOf(double), 4, @WI[0], sizeof(double));

     Check(CheckMtx(EigValsR, WR, -1, -1, 0.001), 'Error wrong real Eigenvalue part');
     Check(CheckMtx(EigValsI, WI, -1, -1, 0.001), 'Error wrong imaginary Eigenvalue part');
     Check(CheckMtx(EigVec, Eivec, -1, -1, 0.001), 'Error wrong eigenvector');
end;

procedure TTestEigensystems.TestFullHessenberg;
const A : Array[0..24] of double = (3, 1, 1, 2, -2, -1, 5, -1, 8, 0, 1, -1, 1, 4, 3, 2.2, 1, 0, -1, -1, 6, -6, -2, -2, 4);
      Res : Array[0..8] of double = (3, 1.4, 1, 5, 7.6, -1, 0.4, -4.84, -1.6);
      cHess : Array[0..24] of double = (3.00000,  -1.16115,   0.01819,   0.46244,  -2.90474,
                                        6.54523,   3.23436,   1.99892,   2.70807,   6.29351,
                                        -0.133,   -5.19787,  -2.68521,   0.34935,   4.62237,
                                        -0.292,   -0.195,  -1.96271,   2.80563,   3.66568,
                                        -0.795,   0.260,   0.950,   2.41975,   5.64523 );
      cQ : Array[0..24] of double = ( 1.00000,   0.00000,   0.00000,   0.00000,   0.00000,
                                      0.00000,  -0.15278,  -0.43603,  -0.78587,  -0.41103,
                                      0.00000,   0.15278,  -0.75146,   0.55545,  -0.32161,
                                      0.00000,   0.33612,   0.47957,   0.08991,  -0.80557,
                                      0.00000,   0.91670,  -0.12327,  -0.25652,   0.28047 );
var dest : Array[0..24] of double;
    tau : Array[0..4] of double;
    Q : Array[0..24] of double;
begin
     Move( A, dest, sizeof(A));
     FillChar( tau, sizeof(tau), 0);

     MatrixHessenberg2InPlace(@dest[0], 5*sizeof(double), 5, @tau[0], HessBlockSize);
     Move( dest, Q, sizeof(A));
     MatrixQFromHessenbergDecomp(@Q[0], 5*sizeof(double), 5, @tau[0]);

     Status( WriteMtx( dest, 5 ) );
     Status( '' );
     Status( WriteMtx( Q, 5 ) );

     Check(CheckMtx(cHess, dest, -1, -1, 0.0005), 'Error Hessenberg');
     Check(CheckMtx(cQ, Q), 'Error Q on Hessenberg');
end;

procedure TTestEigensystems.TestHessenberg;
const A : Array[0..8] of double = (3, 1, 1, 2, -2, -1, 5, -1, 8);
      Res : Array[0..8] of double = (3, 1.4, 1, 5, 7.6, -1, 0.4, -4.84, -1.6);
      EigVals : Array[0..2] of double = (2.757, 8.862, -2.619);
var dest : Array[0..8] of double;
    Wr : Array[0..2] of double;
    Wi : Array[0..2] of double;
    checkEig : boolean;
    i, j : integer;
begin
     MatrixHessenberg(@dest[0], 3*sizeof(double), @A[0], 3*sizeof(double), 3);
     Check(CheckMtx(dest, Res), 'Error Hessenberg');

     // check eigenvalues on hessenberg matrix
     MatrixEigHessenberg(@dest[0], 3*sizeof(double), 3, @Wr[0], sizeof(double), @Wi[0], sizeof(double));

     for i := 0 to High(EigVals) do
     begin
          checkEig := False;
          for j := 0 to High(Wr) do
              checkEig := checkEig or (Abs(EigVals[i] - WR[j]) < 0.001);

          if not CheckEig then
             break;
     end;

     Check(CheckEig, 'Error wrong eigenvalues: ' + WriteMtx(WR, 3) + #13#10 + WriteMtx(WI, 3));
end;

procedure TTestEigensystems.TestHessenberg2;
const A : Array[0..15] of double = (    1.0000,    0.5000,    0.3333,    0.2500,
                                        2.0000,    0.1100,    0.2500,    1.1100,
                                        0.3333,    0.6667,    1.0000,    0.7500,
                                        2.0000,    0.9222,    0.2500,    0.1250 );
var dest : Array[0..15] of double;
begin
     move(a, dest, sizeof(dest));
     MatrixHessenbergPermInPlace(@dest[0], 4*sizeof(double), 4, nil, 0);
     Status(WriteMtx(dest, 4));
end;

type
 THessMtx = Array[0..24] of double;
 PHessMtx = ^THessMtx;

var h2 : IMatrix;
    

procedure TTestEigensystems.TestHessenberg3;
// output from octave and c++ test project.
const cHess : Array[0..24] of double = 
  (   1.00000,     -4.54148,     -5.29807,      1.20998,      1.95995,
  -1006.28028,   4514.67312,   5312.95547,  -1128.38174,   2006.28847,
      0.09840,   -204.36815,   -299.97621,    176.22006,   -216.11119,
      0.98398,     -0.05682,     80.94364,   -542.06002,    357.64421,
      0.04920,      0.76003,     0.96501,      4.59370,     37.36312
 );

// this set is actually numerically unstable and delivers different outcome than
// lapack (error accumulted)
//const A : array[0..24] of double = (1, 2, 3, 4, 5, 
//                                   10, 20, 30, 40, 50, 
//                                   100, 200, 300, 400, 500, 
//                                   1000, 2000, 3000, 4000, 5000, 
//                                   50, 40, 30, 20, 10);
const A : array[0..24] of double = (1, 2, 3, 4, 5, 
                                   10, 20, 30, 40, 50, 
                                   100, 200, -300, 400, 500, 
                                   1000, -2000, 3000, 4000, 5000, 
                                   50, 12, 34, 25, -10);
var   h1 : Array[0..24] of double;
      h2 : Array[0..24] of double;
      t1, t2 : Array[0..4] of double;
begin
     Move( A, h1, sizeof(A));
     Move( A, h2, sizeof(A));
     Status( WriteMtx(A, 5 ) );
     Status('');
     
     InitMathFunctions(itFPU, False);
     MatrixHessenberg2InPlace( @h1[0], 5*sizeof(double), 5, @t1[0], 32 );
     // blocked test
     MatrixHessenberg2InPlace( @h2[0], 5*sizeof(double), 5, @t2[0], 2 );
     
     Status( WriteMtx( h1, 5 ) );
     Status('');
     Status( WriteMtx( h2, 5 ) );
     Check( CheckMtx( cHess, h1 ), 'Unblocked hessenberg failed' );
     Check( CheckMtx( cHess, h2 ), 'Blocked hessenberg failed' );
end;

// test routine in matlab:
//% testing the delphi hess decomposition
//w = [7, 43, 48, 189, 256, 500, 789, 1024];
//
//for i = 1:length(w),
//    hessInput = load( sprintf( 'D:\\hessinp_%d.txt', w(i) ) );
//    hessOutput = load( sprintf( 'D:\\hessout_%d.txt', w(i)));
//    
//    tic;
//    h = hess( hessInput );
//    toc
//    
//    d = hessOutput - h;
//    sprintf( 'Maximum error %d: %f', w(i), max( max( d ) ) )
//endfor

procedure TTestEigensystems.TestHessenbergBig;
//const cBlkWidths : Array[0..7] of integer = (1, 9, 16, 24, 43, 189, 256, 500);
const cBlkWidths : Array[0..8] of integer = (1, 7, 43, 48, 189, 256, 500, 789, 1024);
var iter : integer;
    w : integer;
    A, A2, A3, A4 : TDoubleDynArray;
    tau, tau2, tau3, tau4 : TDoubleDynArray;
    i : integer;
    start1, stop1 : Int64;
    start2, stop2 : Int64;
    start3, stop3 : Int64;
    start4, stop4 : Int64;
    //x, y: Integer;
begin
     InitMathFunctions(itFPU, False);
     for iter := 0 to Length(cBlkWidths) - 1 do
        begin
             InitMathFunctions(itFPU, False);
             w := cBlkWidths[iter];

             SetLength(A, w*w);
             SetLength(tau, w);
             SetLength(tau2, w);
             SetLength(tau3, w);
             SetLength(tau4, w);

             RandSeed := 15;
             for i := 0 to Length(A) - 1 do
                 A[i] := Random - 0.5;
             A2 := Copy(A, 0, Length(A));
             A3 := copy(A, 0, Length(A));
             A4 := copy(A, 0, Length(A));

             //WriteMatlabData(Format('D:\hessinp_%d.txt', [w]), A2, w);
             start1 := MtxGetTime;
             MatrixHessenberg2InPlace(@a[0], w*sizeof(double), w, @tau[0], w);
             stop1 := MtxGetTime;

             start2 := MtxGetTime;
             MatrixHessenberg2InPlace(@a2[0], w*sizeof(double), w, @tau2[0], HessBlockSize);
             stop2 := MtxGetTime;

             InitMathFunctions(itSSE, False);
             start3 := MtxGetTime;
             MatrixHessenberg2InPlace(@a3[0], w*sizeof(double), w, @tau3[0], HessBlockSize);
             stop3 := MtxGetTime;

             InitMathFunctions(itAVX, False);
             start4 := MtxGetTime;
             MatrixHessenberg2InPlace(@a4[0], w*sizeof(double), w, @tau4[0], HessBlockSize);
             stop4 := MtxGetTime;

             Status(Format('BigHESS took (%d): %.3fms, %.3fms, %.3fms, %.3fms', [w, (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000, (stop3 - start3)/mtxfreq*1000, (stop4 - start4)/mtxfreq*1000]));

             check( CheckMtx(a, a2), 'Blocked and unblocked hess issued different outputs');
             Check(CheckMtx(A, A3), 'SSE failed');
             Check(CheckMtx(A, A4), 'AVX failed');

             // clear out the lower half
             //for y := 2 to w - 1 do
//                 for x := 0 to y - 2 do
//                     A2[ y*w + x ] := 0;
             
             //WriteMatlabData(Format('D:\hessout_%d.txt', [w]), A2, w);
        end;
end;

procedure TTestEigensystems.TestHessenbergBlk;
const A : Array[0..15] of double = (10, 9, 8, 9, 2, 8, 4, 7, 6, 5, 6, 2, 5, 0, 8, 4);
      cHess : Array[0..15] of double = (
            10.00000,  -13.76786,    6.03685,   -0.05077,
            -8.06226,  12.70769,  -1.81271,  2.98164,
            0.59629,  -6.66616, 4.78400,   1.93003,
            0.49691, 0.19484, -5.38802,  0.50831 );
var hess : Array[0..15] of double;
    tau : Array[0..3] of double;
begin
     Move( A, hess, sizeof(A));
     FillChar(tau, sizeof(tau), 0);

     InitMathFunctions(itFPU, False);
     MatrixHessenberg2InPlace( @A[0], 4*sizeof(double), 4, @tau[0], 2);

     Check(CheckMtx( A, cHess), 'blocked hessenberg failed');
end;

procedure TTestEigensystems.TestHessenbergCmplx;
const A : Array[0..15] of double = (10, 9, 8, 9, 2, 8, 4, 7, 6, 5, 6, 2, 5, 0, 8, 4);
      EigVals : Array[0..3,0..1] of double = ( (23.2603, 0), (0.9905, -4.5063), (0.9905, 4.5063), (2.7587, 0));
var dest : Array[0..15] of double;
    Wr : Array[0..3] of double;
    Wi : Array[0..3] of double;
begin
     MatrixHessenberg(@dest[0], 4*sizeof(double), @A[0], 4*sizeof(double), 4);

     // check eigenvalues on hessenberg matrix
     MatrixEigHessenberg(@dest[0], 4*sizeof(double), 4, @Wr[0], sizeof(double), @Wi[0], sizeof(double));

     Check((abs(EigVals[0,0] - wr[0]) < 0.0001), 'Error on eigvals calculation');
     Check((abs(EigVals[1,0] - wr[1]) < 0.0001), 'Error on eigvals calculation');
     Check((abs(EigVals[2,0] - wr[2]) < 0.0001), 'Error on eigvals calculation');
     Check((abs(EigVals[3,0] - wr[3]) < 0.0001), 'Error on eigvals calculation');

     Check((abs(EigVals[0,1] - wi[0]) < 0.0001), 'Error on eigvals calculation');
     Check((abs(EigVals[1,1] - wi[1]) < 0.0001), 'Error on eigvals calculation');
     Check((abs(EigVals[2,1] - wi[2]) < 0.0001), 'Error on eigvals calculation');
     Check((abs(EigVals[3,1] - wi[3]) < 0.0001), 'Error on eigvals calculation');
end;

procedure TTestEigensystems.TestHessenbergThr;
const cBlkWidths : Array[0..8] of integer = (1, 7, 43, 48, 189, 256, 500, 789, 1024);
var iter : integer;
    w : integer;
    A, A2, A3, A4 : TDoubleDynArray;
    tau, tau2, tau3, tau4 : TDoubleDynArray;
    i : integer;
    start1, stop1 : Int64;
    start2, stop2 : Int64;
begin
     InitMtxThreadPool;
     InitMathFunctions(itAVX, False);
     for iter := 0 to Length(cBlkWidths) - 1 do
     begin
          w := cBlkWidths[iter];

          SetLength(A, w*w);
          SetLength(tau, w);
          SetLength(tau2, w);
          SetLength(tau3, w);
          SetLength(tau4, w);

          RandSeed := 15;
          for i := 0 to Length(A) - 1 do
              A[i] := Random - 0.5;
          A2 := Copy(A, 0, Length(A));
          A3 := copy(A, 0, Length(A));
          A4 := copy(A, 0, Length(A));

          start1 := MtxGetTime;
          MatrixHessenberg2InPlace(@a[0], w*sizeof(double), w, @tau[0], HessBlockSize);
          stop1 := MtxGetTime;

          start2 := MtxGetTime;
          ThrMtxHessenberg2InPlace(@a2[0], w*sizeof(double), w, @tau2[0], HessBlockSize);
          stop2 := MtxGetTime;

          Status(Format('Threaded HESS took (%d): %.3fms, %.3fms', [w, (stop1 - start1)/mtxfreq*1000, (stop2 - start2)/mtxfreq*1000]));

          check( CheckMtx(a, a2), 'Blocked and unblocked hess issued different outputs');
     end;

     FinalizeMtxThreadPool;
end;

procedure TTestEigensystems.TestLinearDependentVectors;
const B : Array[0..8] of double = (3, 1, 1, 30, 10, 10, 5, -1, 8);
      EigVals : Array[0..2] of double = (0, 11.618, 9.382);
      EigVec : Array[0..8] of double = (-0.4737, 0.1, 0.1, 1, 1, 1.0000, 0.4211, -0.1382, -0.3618);
var Eivec : Array[0..8] of double;
    Wr : Array[0..2] of double;
    Wi : Array[0..2] of double;
begin
     FillChar(EiVec, sizeof(EiVec), 0);
     FillChar(wr, sizeof(wr), 0);
     FillChar(wi, sizeof(wi), 0);
     Check(qlOk = MatrixUnsymEigVec(@B[0], 3*sizeof(double), 3, @Wr[0], sizeof(double), @Wi[0], sizeof(double), @Eivec[0], 3*sizeof(double)), 'error in convergence of eigenvector routine');
     MatrixNormEivecInPlace(@Eivec[0], 3*sizeof(double), 3, @wi[0], sizeof(double));
     
     Check(CheckMtx(EigVals, Wr, -1, -1, 0.001), 'Error wrong eigenvalues');
     Check(CheckMtx(EigVec, Eivec, -1, -1, 0.001), 'Error wrong eigenvectors');
end;

procedure TTestEigensystems.TestQLImplicitShift;
const A : Array[0..8] of double = (3, 2, 1, 2, -2, -1, 1, -1, 8);
      EigVals : Array[0..2] of double = (-2.8521, 3.6206, 8.2315);
var dest : Array[0..8] of double;
    D : Array[0..2] of double;
    E : Array[0..2] of double;
    res : TEigenvalueConvergence;
    checkEig : boolean;
    i, j : Integer;
begin
     FillChar(D[0], sizeof(D), 0);
     FillChar(E[0], sizeof(E), 0);
     MatrixTridiagonalHouse(@dest[0], 3*sizeof(double), @A[0], 3*sizeof(double), 3, @d[0], sizeof(double), @e[0], sizeof(double));
     res := MatrixTridiagonalQLImplicitInPlace(@dest[0], 3*sizeof(double), 3, @d[0], sizeof(double), @E[0], sizeof(Double));

     Check(res = qlOk, 'Error no convergence.');
     for i := 0 to High(EigVals) do
     begin
          checkEig := False;
          for j := 0 to High(D) do
              checkEig := checkEig or (Abs(EigVals[i] - D[j]) < 0.001);

          if not CheckEig then
             break;
     end;
     Check(checkEig, 'Error wrong eigenvalues');
end;

procedure TTestEigensystems.TestTridiagonalHousholderMethod;
const A : Array[0..8] of double = (3, 2, 1, 2, -2, -1, 1, -1, 8);
var dest : Array[0..8] of double;
    D : Array[0..2] of double;
    E : Array[0..2] of double;
begin
     FillChar(D[0], sizeof(D), 0);
     FillChar(E[0], sizeof(E), 0);
     MatrixTridiagonalHouse(@dest[0], 3*sizeof(double), @A[0], 3*sizeof(double), 3, @d[0], sizeof(double), @e[0], sizeof(double));

     // Check on trigonitality
     Check((dest[2] = 0) and (dest[6] = 0), 'Error no tridiagonal matrix');
end;

initialization
{$IFNDEF FMX}
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TTestEigensystems{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
