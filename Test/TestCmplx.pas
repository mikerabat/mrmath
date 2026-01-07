unit TestCmplx;


// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2025, Michael R. . All rights reserved.
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

uses {$IFDEF FPC} testregistry {$ELSE}  {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF} {$ENDIF} ,
     Classes, SysUtils, MatrixConst, BaseMatrixTestCase;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}
  TestSimpleCmplxFunctions = class(TBaseMatrixTestCase)
  private
    procedure DoubleVal( var x : TComplex );
    procedure SubMtxDblVal( var Value : TComplex; const data : PComplex; LineWidth : NativeInt; x, y : NativeInt );
  published
    procedure TestBaseCmplxFunc;
    procedure TestExpLN;
    procedure TestSinCosTan;
    // matrix stuff
    procedure TestCplxInit;
    procedure TestCplxAdd;
    procedure TestCplAddVec;
    procedure TestCplxSub;
    procedure TestCplSubVec;
    procedure TestCplxMult;
    procedure TestCplxMultTransp;
    procedure TestVecDotMult;

    procedure TestElemAddScale;
    procedure TestCplxNorm;
    procedure TestCplxTranspose;
    procedure TestCplxMtxSqrt;
    procedure TestCplxMtxMinMax;
    procedure TestCplElemMulDiv;
    procedure TestCplxNormalize;

    procedure TestRowColSwap;

    procedure TestApplyFunc;
  end;

implementation

uses CplxSimpleMatrixOperations, Math;

procedure LocDoubleVal( var x : TComplex );
begin
     x := RCMul(2, x);
end;

procedure TestSimpleCmplxFunctions.DoubleVal(var x: TComplex);
begin
     x := RCMul(2, x);
end;


procedure locSubMtxDblVal( var Value : TComplex; const data : PComplex; LineWidth : NativeInt; x, y : NativeInt );
begin
     value := RCMul(2, value);
end;

procedure TestSimpleCmplxFunctions.SubMtxDblVal( var Value : TComplex; const data : PComplex; LineWidth : NativeInt; x, y : NativeInt );
begin
     value := RCMul(2, value);
end;

{ TestSimleCmplxFunctions }

procedure TestSimpleCmplxFunctions.TestApplyFunc;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
var a, b, c, d, e : Array[0..3] of TComplex;
  i: Integer;
begin
     move( cA2x2, a, sizeof(a));
     move( cA2x2, b, sizeof(b));
     move( cA2x2, c, sizeof(c));
     move( cA2x2, d, sizeof(c));
     move( cA2x2, e, sizeof(c));

     CplxGenericMtxFunc( @a[0], 2*sizeof(TComplex), 2, 2, {$ifdef FPC}@{$endif}LocDoubleVal);
     CplxGenericMtxFunc( @b[0], 2*sizeof(TComplex), 2, 2, {$ifdef FPC}@{$endif}DoubleVal);
     CplxGenericMtxFunc( @c[0], 2*sizeof(TComplex), 2, 2,
                    procedure (var x : TComplex)
                    begin
                         x := RCMul(2, x);
                    end
                    );

     CplxGenericMtxFunc( @d[0], 2*sizeof(TComplex), 2, 2, {$ifdef FPC}@{$endif}locSubMtxDblVal);
     CplxGenericMtxFunc( @e[0], 2*sizeof(TComplex), 2, 2, {$ifdef FPC}@{$endif}SubMtxDblVal);

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], RCMul(2, cA2x2[i])) = 0, 'Ref tewst to apply function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], b[i]) = 0, 'Apply object function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], c[i]) = 0, 'Apply reference function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], d[i]) = 0, 'Apply extended function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], e[i]) = 0, 'Apply extended object function failed');

     // ###########################################
     // #### Test on submatrices
     move( cA2x2, a, sizeof(a));
     move( cA2x2, b, sizeof(b));
     move( cA2x2, c, sizeof(c));
     move( cA2x2, d, sizeof(c));
     move( cA2x2, e, sizeof(c));

     // apply only on the last element
     CplxGenericSubMtxFunc( @a[0], 2*sizeof(TComplex), 1, 1, 1, 1, {$ifdef FPC}@{$endif}LocDoubleVal);
     CplxGenericSubMtxFunc( @b[0], 2*sizeof(TComplex), 1, 1, 1, 1, {$ifdef FPC}@{$endif}DoubleVal);
     CplxGenericSubMtxFunc( @c[0], 2*sizeof(TComplex), 1, 1, 1, 1,
                    procedure (var x : TComplex)
                    begin
                         x := RCMul(2, x);
                    end
                    );

     CplxGenericSubMtxFunc( @d[0], 2*sizeof(TComplex), 1, 1, 1, 1, {$ifdef FPC}@{$endif}locSubMtxDblVal);
     CplxGenericSubMtxFunc( @e[0], 2*sizeof(TComplex), 1, 1, 1, 1, {$ifdef FPC}@{$endif}SubMtxDblVal);

     for i := 0 to Length(a) - 2 do
         Check(CCmp( a[i], cA2x2[i]) = 0, 'Ref test to apply sub function failed');
     Check(CCmp(a[High(a)], RCMul(2, cA2x2[high(a)])) = 0, 'Ref test to apply sub function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], b[i]) = 0, 'Sub matrix: Apply object function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], c[i]) = 0, 'Sub matrix: Apply reference function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], d[i]) = 0, 'Sub matrix: Apply extended function failed');

     for i := 0 to Length(a) - 1 do
         Check(CCmp( a[i], e[i]) = 0, 'Sub matrix: Apply extended object function failed');
end;

procedure TestSimpleCmplxFunctions.TestBaseCmplxFunc;
const cAddRes : Array[0..2] of TComplex = ( (real: 3; imag : -3), (real : -1; imag : 4), (real : 2; imag : 1) );
      cSubRes : Array[0..2] of TComplex = ( (real: 3; imag : -3), (real : -1; imag : 4), (real : 4; imag : -7) );
      cMulDivRes : Array[0..2] of TComplex = ( (real: 3; imag : -3), (real : -1; imag : 4), (real : 9; imag : 15) );
      cSQRTRes : Array[0..1] of TComplex = ( (real: 5; imag: 12), (real: 3; imag: 2));
var z : TComplex;
begin
     z := CAdd( cAddRes[0], cAddRes[1] );
     Check( CCmp( z, cAddRes[2] ) = 0, 'Complex add test failed');
     z := CSub( cSubRes[0], cSubRes[1] );
     Check( CCmp( z, cSubRes[2] ) = 0, 'Complex sub test failed');
     z := CMul( cMulDivRes[0], cMulDivRes[1] );
     Check( CCmp( z, cMulDivRes[2] ) = 0, 'Complex mul test failed');
     z := CDiv( cMulDivRes[2], cMulDivRes[1] );
     Check( CCmp( z, cMulDivRes[0] ) = 0, 'Complex div test failed');
     z := CDiv( cMulDivRes[2], cMulDivRes[0] );
     Check( CCmp( z, cMulDivRes[1] ) = 0, 'Complex div test failed');

     z := CSqrt( cSQRTRes[0] );
     Check( CCmp( z, cSQRTRes[1] ) = 0, 'Complex principal squareroot failed');

end;

procedure TestSimpleCmplxFunctions.TestCplAddVec;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cB2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -1), (real: 5; imag: 2),
                                          (real: 10; imag: 2), (real: -4; imag: -4) );

      cRes2x2_1 : Array[0..3] of TComplex = ( (real: 3; imag: -4), (real: 9; imag: -2),
                                              (real: 2; imag: 1), (real: 9; imag: 6) );

      cRes2x2_2 : Array[0..3] of TComplex = ( (real: 12; imag: -1), (real: 14; imag: -2),
                                              (real: -3; imag: -2), (real: 0; imag: 0) );

      cRes2x2_3 : Array[0..3] of TComplex = ( (real: 3; imag: -4), (real: 5; imag: -5),
                                              (real: 11; imag: 4), (real: 14; imag: 6) );

      cRes2x2_4 : Array[0..3] of TComplex = ( (real: 7; imag: -1), (real: 0; imag: -8),
                                              (real: 6; imag: 4), (real: 0; imag: 0) );

var z2x2 : Array[0..3] of TComplex;
    i : Integer;
begin
     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     // the vector as row of b
     CplxGenericAddVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[0], sizeof(TComplex), 2, 2, True);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_1[i] ) = 0, 'Complex matrix vec add failed');

     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     CplxGenericAddVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[2], sizeof(TComplex), 2, 2, False);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_2[i] ) = 0, 'Complex matrix vec add failed');

     // the vector as column of b
     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     CplxGenericAddVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[0], 2*sizeof(TComplex), 2, 2, False);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_3[i] ) = 0, 'Complex matrix vec add failed');

     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     CplxGenericAddVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[1], 2*sizeof(TComplex), 2, 2, True);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_4[i] ) = 0, 'Complex matrix vec add failed');
end;


procedure TestSimpleCmplxFunctions.TestCplElemMulDiv;
var a, b, c : Array[0..15] of TComplex;
    i : Integer;
begin
     for i := 0 to Length(a) - 1 do
         a[i] := InitComplex(i, i + 1);

     CplxGenericMtxElemMult( @b[0], 4*sizeof(TComplex), @a[0], @a[0], 4, 4, 4*sizeof(TComplex), 4*sizeof(TComplex));

     for i := 0 to Length(b) - 1 do
         Check( CCmp( CMul(a[i], a[i]), b[i] ) = 0, 'Element wise mult failed');

     CplxGenericMtxElemDiv( @c[0], 4*sizeof(TComplex), @b[0], @a[0], 4, 4, 4*sizeof(TComplex), 4*sizeof(TComplex));

     for i := 0 to Length(b) - 1 do
         Check( CCmp( a[i], c[i], 1e-8 ) = 0, 'Element wise div failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxAdd;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cB2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -1), (real: 5; imag: 2),
                                          (real: 10; imag: 2), (real: -4; imag: -4) );
      cRes2x2 : Array[0..3] of TComplex = ( (real: 3; imag: -4), (real: 9; imag: -2),
                                          (real: 11; imag: 4), (real: 0; imag: 0) );
var z2x2 : Array[0..3] of TComplex;
    i : Integer;
begin
     CplxGenericMtxAdd(@z2x2[0], 2*sizeof(TComplex), @cA2x2[0], @cB2x2[0], 2, 2, 2*sizeof(TComplex), 2*sizeof(TComplex));

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2[i] ) = 0, 'Complex matrix add failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxInit;
var z8x8 : Array[0..63] of TComplex;
    ref : TComplex;
    i: Integer;
begin
     ref := InitComplex(3, 2);
     CplxGenericMtxInit(@z8x8[0], 8*sizeof(TComplex), 8, 8, ref);

     for i := 0 to Length(z8x8) - 1 do
         Check( CCmp( z8x8[i], ref ) = 0, 'Cmplx matrix init Failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxMtxMinMax;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
var res1, res2 : TComplex;
begin
     res1 :=  CplxGenericMtxMax( @cA2x2[0], 2, 2, 2*sizeof(TComplex));
     res2 :=  CplxGenericMtxMin( @cA2x2[0], 2, 2, 2*sizeof(TComplex));

     Check( CCmp(res1, cA2x2[3] ) = 0, 'Cmplx max failed');
     Check( CCmp(res2, cA2x2[2] ) = 0, 'Cmplx min failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxMtxSqrt;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );

      cRes2x2 : Array[0..3] of TComplex = ( (real: 1.6741; imag: - 0.8960), (real: 2.1974; imag: - 0.9102),
                                          (real: 1.2720; imag: 0.7862), (real: 2.1974; imag: 0.9102) );

var a : Array[0..3] of TComplex;
    i : integer;
begin
     Move(cA2x2, a, sizeof(a));
     CplxGenericMtxSqrt( @a[0], 2*sizeof(TComplex), 2, 2);

     for i := 0 to High(a) do
         Check( CCmp( a[i], cRes2x2[i], 1e-3) = 0, 'Elementwise sqrt on matrix failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxMult;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cB2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -1), (real: 5; imag: 2),
                                          (real: 10; imag: 2), (real: -4; imag: -4) );


      // a*b
      cRes1_2x2 : Array[0..3] of TComplex = ( (real: 47; imag: -37), (real: -16; imag: -11),
                                          (real: 35; imag: 49), (real: 1; imag: -20) );
      // b*a
      cRes2_2x2 : Array[0..3] of TComplex = ( (real: 0; imag: 7), (real: 12; imag: 20),
                                          (real: 30; imag: -38), (real: 48; imag: -64) );
var z2x2 : Array[0..3] of TComplex;
    i : integer;
begin
     CplxGenericMtxMult(@z2x2[0], 2*sizeof(TComplex), @cA2x2[0], @cB2x2[0], 2, 2, 2, 2, 2*sizeof(TComplex), 2*sizeof(TComplex));

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes1_2x2[i] ) = 0, 'Complex matrix sub failed');

     CplxGenericMtxMult(@z2x2[0], 2*sizeof(TComplex), @cB2x2[0], @cA2x2[0], 2, 2, 2, 2, 2*sizeof(TComplex), 2*sizeof(TComplex));

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2_2x2[i] ) = 0, 'Complex matrix sub failed');

end;

procedure TestSimpleCmplxFunctions.TestCplxMultTransp;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cB2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -1), (real: 5; imag: 2),
                                          (real: 10; imag: 2), (real: -4; imag: -4) );

      // a*b'
      cRes1_2x2 : Array[0..3] of TComplex = ( (real: 27; imag: -17), (real: -6; imag: -26),
                                          (real: 15; imag: 29), (real: 6; imag: -10) );

      // b*a'
      cRes2_2x2 : Array[0..3] of TComplex = ( (real: 27; imag: -17), (real: 15; imag: 29),
                                          (real: -6; imag: -26), (real: 6; imag: -10) );
var z2x2 : Array[0..3] of TComplex;
    i : integer;
begin
     CplxGenericMtxMultTransp(@z2x2[0], 2*sizeof(TComplex), @cA2x2[0], @cB2x2[0], 2, 2, 2, 2, 2*sizeof(TComplex), 2*sizeof(TComplex));

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes1_2x2[i] ) = 0, 'Complex matrix sub failed');

     CplxGenericMtxMultTransp(@z2x2[0], 2*sizeof(TComplex), @cB2x2[0], @cA2x2[0], 2, 2, 2, 2, 2*sizeof(TComplex), 2*sizeof(TComplex));

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2_2x2[i] ) = 0, 'Complex matrix sub failed');

end;


procedure TestSimpleCmplxFunctions.TestCplxNorm;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cRes1 : TComplex = ( real: 4.4319; imag: -4.9640);
      cRes2 : TComplex = ( real: 1.2872; imag: - 3.1075);
      cRes3 : TComplex = ( real: 4.0492; imag: -4.9392);
var a, b, c : TComplex;
begin
     a := CplxGenericMtxElementwiseNorm2(@cA2x2[0], 2*sizeof(TComplex), 2, 1, True);
     b := CplxGenericMtxElementwiseNorm2(@cA2x2[0], 2*sizeof(TComplex), 2, 2, True);
     c := CplxGenericMtxElementwiseNorm2(@cA2x2[0], 3*sizeof(TComplex), 3, 1, True);

     Check( CCmp( a, cRes1, 1e-3 ) = 0, 'Complex norm failed');
     Check( CCmp( b, cRes2, 1e-3 ) = 0, 'Complex norm failed');
     Check( CCmp( c, cRes3, 1e-3 ) = 0, 'Complex norm failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxSub;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cB2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -1), (real: 5; imag: 2),
                                          (real: 10; imag: 2), (real: -4; imag: -4) );
      cRes2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -2), (real: -1; imag: -6),
                                          (real: -9; imag: 0), (real: 8; imag: 8) );
var z2x2 : Array[0..3] of TComplex;
    i : Integer;
begin
     CplxGenericMtxSub(@z2x2[0], 2*sizeof(TComplex), @cA2x2[0], @cB2x2[0], 2, 2, 2*sizeof(TComplex), 2*sizeof(TComplex));

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2[i] ) = 0, 'Complex matrix sub failed');
end;

procedure TestSimpleCmplxFunctions.TestCplxTranspose;
var a : Array[0..15] of TComplex;
    b,c : Array[0..15] of TComplex;
    i : integer;
begin
     for i := 0 to Length(a) - 1 do
         a[i] := InitComplex(i, i+1);

     CplxGenericMtxTranspose(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 4, 4);
     check(CCmp( InitComplex(4, -5), b[1]) = 0, 'Cmplx Transpose failed');
     for i := 0 to 3 do
         Check(CCmp( CConj( a[i*4 + i] ), b[i*4 + i] ) = 0, 'Diagonal failed');

     CplxGenericMtxTransposeNoConj(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 4, 4);
     check(CCmp( InitComplex(4, 5), b[1]) = 0, 'Cmplx Transpose failed');
     for i := 0 to 3 do
         Check(CCmp( a[i*4 + i], b[i*4 + i] ) = 0, 'Diagonal failed');

     CplxGenericMtxTranspose(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 3, 3);
     check(CCmp( InitComplex(4, -5), b[1]) = 0, 'Cmplx Transpose failed');
     for i := 0 to 2 do
         Check(CCmp( CConj( a[i*4 + i] ), b[i*4 + i] ) = 0, 'Diagonal failed');
     CplxGenericMtxTransposeNoConj(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 3, 3);
     check(CCmp( InitComplex(4, 5), b[1]) = 0, 'Cmplx Transpose failed');
     for i := 0 to 2 do
         Check(CCmp( a[i*4 + i], b[i*4 + i] ) = 0, 'Diagonal failed');

     CplxGenericMtxTranspose(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 2, 2);
     check(CCmp( InitComplex(4, -5), b[1]) = 0, 'Cmplx Transpose failed');
     for i := 0 to 1 do
         Check(CCmp( CConj( a[i*4 + i] ), b[i*4 + i] ) = 0, 'Diagonal failed');
     CplxGenericMtxTransposeNoConj(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 2, 2);
     check(CCmp( InitComplex(4, 5), b[1]) = 0, 'Cmplx Transpose failed');
     for i := 0 to 1 do
         Check(CCmp( a[i*4 + i], b[i*4 + i] ) = 0, 'Diagonal failed');

     for i := 0 to Length(a) - 1 do
     begin
          a[i] := InitComplex(i, i+1);
          c[i] := InitComplex(i, i+1);
     end;

     CplxGenericMtxTranspose(@b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 4, 4);
     CplxGenericMtxTransposeInplace(@c[0], 4*sizeof(TComplex), 4);
     for i := 0 to Length(a) - 1 do
         Check(CCmp(b[i], c[i]) = 0, 'In place transpose failed');
end;

procedure TestSimpleCmplxFunctions.TestElemAddScale;
const cAddVal : TComplex = (real: 3; imag: -1);
      cMulVal : TComplex = (real : 2; imag: 2);

      cRes1 : TComplex = (real: 4; imag: -1);
      cRes2 : TComplex = (real: 10; imag: 6);
      cRes3 : TComplex = (real: 5; imag: 1);

var x : Array[0..31] of TComplex;
    i : Integer;
begin
     CplxGenericMtxInit( @x[0], 8*sizeof(TComplex), 8, 4, InitComplex(1, 0) );
     CplxGenericMtxElemAdd(@x[0], 8*sizeof(TComplex), 8, 4, cAddVal);
     for i := 0 to Length(x) - 1 do
         Check( CCmp(cRes1, x[i]) = 0, 'Elementwise add failed');

     CplxGenericMtxInit( @x[0], 8*sizeof(TComplex), 8, 4, InitComplex(1, 0) );
     CplxGenericMtxAddAndScale(@x[0], 8*sizeof(TComplex), 8, 4, cAddVal, cMulVal);
     for i := 0 to Length(x) - 1 do
         Check( CCmp(cRes2, x[i]) = 0, 'Elementwise add failed');

     CplxGenericMtxInit( @x[0], 8*sizeof(TComplex), 8, 4, InitComplex(1, 0) );
     CplxGenericMtxScaleAndAdd(@x[0], 8*sizeof(TComplex), 8, 4, cAddVal, cMulVal);
     for i := 0 to Length(x) - 1 do
         Check( CCmp(cRes3, x[i]) = 0, 'Elementwise add failed');
end;

procedure TestSimpleCmplxFunctions.TestExpLN;
const c1 : TComplex = (real: 2; imag : 0);
      c2 : TComplex = (real: 0; imag : 2);
      c3 : TComplex = (real: 2; imag : 2);
      c4 : TComplex = (real: -2; imag : 1);
      c5 : TComplex = (real: -2; imag : -3);
var x1, x2, x3, x4, x5 : TComplex;
begin
     x1 := CExp( c1 );
     x2 := CExp( c2 );
     x3 := CExp( c3 );
     x4 := CExp( c4 );
     x5 := CExp( c5 );

     Check( CCmp( InitComplex( exp(2), 0 ), x1) = 0, 'Exponential failed' );
     Check( SameValue( -0.4161, x2.real, 1e-3 ) and SameValue(0.9093, x2.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -3.0749, x3.real, 1e-3 ) and SameValue(6.7188, x3.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( 0.0731, x4.real, 1e-3 ) and SameValue(0.1139, x4.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -0.1340, x5.real, 1e-3 ) and SameValue(-0.0191, x5.imag, 1e-3 ), 'Exponential failed' );

     x1 := CLn( c1 );
     x2 := CLn( c2 );
     x3 := CLn( c3 );
     x4 := CLn( c4 );
     x5 := CLn( c5 );

     Check( CCmp( InitComplex( ln(2), 0 ), x1) = 0, 'Complex ln failed' );
     Check( SameValue( 0.6931, x2.real, 1e-3 ) and SameValue(1.5708, x2.imag, 1e-3 ), 'Complex ln failed' );
     Check( SameValue( 1.0397, x3.real, 1e-3 ) and SameValue(0.7854, x3.imag, 1e-3 ), 'Complex ln failed' );
     Check( SameValue( 0.8047, x4.real, 1e-3 ) and SameValue(2.6779, x4.imag, 1e-3 ), 'Complex ln failed' );
     Check( SameValue( 1.2825, x5.real, 1e-3 ) and SameValue(-2.1588, x5.imag, 1e-3 ), 'Complex ln failed' );
end;

procedure TestSimpleCmplxFunctions.TestCplxNormalize;
var a : Array[0..15] of TComplex;
    b, c : Array[0..15] of TComplex;
    i : Integer;
// values from matlab ;) - tested with calls to normc, normr
const cColRes : Array[0..15] of TComplex =
      (  (real: 0.0333; imag: +0.0300), (real: 0.0871; imag: + 0.0244), (real: 0.1311; imag: + 0.0200), (real: 0.1674; imag:  + 0.0166),
         (real: 0.2862; imag: + 0.0167), (real: 0.3157; imag: + 0.0131), (real: 0.3390; imag: + 0.0104), (real: 0.3576; imag:  + 0.0084),
         (real: 0.5392; imag: + 0.0035), (real: 0.5443; imag: + 0.0019), (real: 0.5468; imag: + 0.0008), (real: 0.5477; imag:  + 0.0002),
         (real: 0.7922; imag: - 0.0097), (real: 0.7729; imag: - 0.0094), (real: 0.7547; imag: - 0.0088), (real: 0.7379; imag:  - 0.0080));

      cRowRes : Array[0..15] of TComplex =
      (  (real: 0.1262; imag: +0.0854), (real: 0.3377; imag: + 0.0447), (real: 0.5493; imag: + 0.0039), (real: 0.7609; imag:  - 0.0368),
         (real: 0.3696; imag: + 0.0112), (real: 0.4510; imag: + 0.0046), (real: 0.5324; imag:  - 0.0019), (real: 0.6138; imag: - 0.0085),
         (real: 0.4226; imag: + 0.0040), (real: 0.4721; imag: + 0.0015), (real: 0.5217; imag: - 0.0009), (real: 0.5713; imag:  - 0.0034),
         (real: 0.4451; imag: + 0.0020), (real: 0.4806; imag: + 0.0007), (real: 0.5162; imag: - 0.0005), (real: 0.5518; imag:  - 0.0018));

begin
     // normalize vector to abs(x) = 1
     for i := 0 to High(a) do
         a[i] := InitComplex(i, i + 1);

     CplxGenericMtxNormalize( @b[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 4, 4, True );
     for i := 0 to High(b) do
         Check( CCmp( b[i], cRowRes[i], 1e-3 ) = 0, 'Row normalization failed');

     CplxGenericMtxNormalize( @c[0], 4*sizeof(TComplex), @a[0], 4*sizeof(TComplex), 4, 4, False );
     for i := 0 to High(c) do
         Check( CCmp( c[i], cColRes[i], 1e-3 ) = 0, 'Column normalization failed');
end;

procedure TestSimpleCmplxFunctions.TestCplSubVec;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );

      cB2x2 : Array[0..3] of TComplex = ( (real: 1; imag: -1), (real: 5; imag: 2),
                                          (real: 10; imag: 2), (real: -4; imag: -4) );

      cRes2x2_1 : Array[0..3] of TComplex = ( (real: 1; imag: -2), (real: -1; imag: -6),
                                              (real: 0; imag: 3), (real: -1; imag: 2) );

      cRes2x2_2 : Array[0..3] of TComplex = ( (real: -8; imag: -5), (real: -6; imag: -6),
                                              (real: 5; imag: 6), (real: 8; imag: 8) );

      cRes2x2_3 : Array[0..3] of TComplex = ( (real: 1; imag: -2), (real: 3; imag: -3),
                                              (real: -9; imag: 0), (real: -6; imag: 2) );

      cRes2x2_4 : Array[0..3] of TComplex = ( (real: -3; imag: -5), (real: 8; imag: 0),
                                              (real: -4; imag: 0), (real: 8; imag: 8) );

var z2x2 : Array[0..3] of TComplex;
    i : Integer;
begin
     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     // the vector as row of b
     CplxGenericSubVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[0], sizeof(TComplex), 2, 2, True);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_1[i] ) = 0, 'Complex matrix vec add failed');

     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     CplxGenericSubVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[2], sizeof(TComplex), 2, 2, False);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_2[i] ) = 0, 'Complex matrix vec add failed');

     // the vector as column of b
     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     CplxGenericSubVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[0], 2*sizeof(TComplex), 2, 2, False);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_3[i] ) = 0, 'Complex matrix vec add failed');

     Move( cA2x2[0], z2x2[0], sizeof(z2x2) );
     CplxGenericSubVec(@z2x2[0], 2*sizeof(TComplex), @cB2x2[1], 2*sizeof(TComplex), 2, 2, True);
     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cRes2x2_4[i] ) = 0, 'Complex matrix vec add failed');
end;


procedure TestSimpleCmplxFunctions.TestRowColSwap;
const cA2x2 : Array[0..3] of TComplex = ( (real: 2; imag: -3), (real: 4; imag: -4),
                                          (real: 1; imag: 2), (real: 4; imag: 4) );
      cResCol2x2 : Array[0..3] of TComplex = ( (real: 4; imag: -4), (real: 2; imag: -3),
                                          (real: 4; imag: 4), (real: 1; imag: 2) );
      cResRow2x2 : Array[0..3] of TComplex = ( (real: 1; imag: 2), (real: 4; imag: 4),
                                          (real: 2; imag: -3), (real: 4; imag: -4) );
var z2x2 : Array[0..3] of TComplex;
    i : Integer;
begin
     Move( cA2x2[0], z2x2[0], sizeof(z2x2));
     CplxGenericColSwap( @z2x2[0], @z2x2[1], 2*sizeof(TComplex), 2 );

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cResCol2x2[i] ) = 0, 'Complex matrix sub failed');

     Move( cA2x2[0], z2x2[0], sizeof(z2x2));
     CplxGenericRowSwap( @z2x2[0], @z2x2[2], 2 );

     for i := 0 to Length(z2x2) - 1 do
         Check( CCmp( z2x2[i], cResRow2x2[i] ) = 0, 'Complex matrix sub failed');
end;

procedure TestSimpleCmplxFunctions.TestSinCosTan;
const c1 : TComplex = (real: 2; imag : 0);
      c2 : TComplex = (real: 0; imag : 2);
      c3 : TComplex = (real: 2; imag : 2);
      c4 : TComplex = (real: -2; imag : 1);
      c5 : TComplex = (real: -2; imag : -3);
var x1, x2, x3, x4, x5 : TComplex;
begin
     x1 := csin(c1);
     x2 := csin(c2);
     x3 := csin(c3);
     x4 := csin(c4);
     x5 := csin(c5);

     Check( CCmp( InitComplex( sin(2), 0 ), x1) = 0, 'Exponential failed' );
     Check( SameValue( 0, x2.real, 1e-3 ) and SameValue(3.6269, x2.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( 3.4210, x3.real, 1e-3 ) and SameValue(- 1.5093, x3.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -1.4031, x4.real, 1e-3 ) and SameValue(-0.4891, x4.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -9.1545, x5.real, 1e-3 ) and SameValue(4.1689, x5.imag, 1e-3 ), 'Exponential failed' );

     x1 := ccos(c1);
     x2 := ccos(c2);
     x3 := ccos(c3);
     x4 := ccos(c4);
     x5 := ccos(c5);

     Check( CCmp( InitComplex( cos(2), 0 ), x1) = 0, 'Exponential failed' );
     Check( SameValue( 3.7622, x2.real, 1e-3 ) and SameValue(0, x2.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -1.5656, x3.real, 1e-3 ) and SameValue(-3.2979, x3.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -0.6421, x4.real, 1e-3 ) and SameValue(1.0686, x4.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -4.1896, x5.real, 1e-3 ) and SameValue(-9.1092, x5.imag, 1e-3 ), 'Exponential failed' );

     x1 := CTan(c1);
     x2 := CTan(c2);
     x3 := CTan(c3);
     x4 := CTan(c4);
     x5 := CTan(c5);

     Check( CCmp( InitComplex( tan(2), 0 ), x1) = 0, 'Exponential failed' );
     Check( SameValue( 0, x2.real, 1e-3 ) and SameValue(0.9640, x2.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( -0.0284, x3.real, 1e-3 ) and SameValue(1.0238, x3.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( 0.2435, x4.real, 1e-3 ) and SameValue(1.1667, x4.imag, 1e-3 ), 'Exponential failed' );
     Check( SameValue( 0.0038, x5.real, 1e-3 ) and SameValue(-1.0032, x5.imag, 1e-3 ), 'Exponential failed' );
end;

procedure TestSimpleCmplxFunctions.TestVecDotMult;
const cX : Array[0..3] of TComplex = ( (real:1; imag:1), (real:2; imag:2), (real:3; imag:3), (real:4; imag:4) );
      cY : Array[0..3] of TComplex = ( (real:-1; imag:-1), (real:1; imag:1), (real:2; imag:2), (real:3; imag:-3) );
var x1, x2 : TComplex;
begin
     x1 := CplGenericVecDotMult( @cX[0], sizeof(TComplex), @cY[0], sizeof(TComplex), 4);

     x2 := CplGenericVecDotMult( @cX[0], 2*sizeof(TComplex), @cY[0], 2*sizeof(TComplex), 2);

end;

initialization
{$IFNDEF FMX}
  RegisterTest(TestSimpleCmplxFunctions{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}


end.
