// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2023, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestWavelets;

interface


{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

// ###########################################
// #### Simple root finding tests.
// ###########################################

uses {$IFDEF FPC} testregistry, {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF}, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils, Types;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}

  { TRootFinding }

  TWaveletTests = class(TBaseMatrixTestCase)
  published
    procedure TestSimpleDWT;
    procedure TestDyadicDWT;
    procedure TestContinousWT;
  end;


implementation

uses Wavelets, Math, MatrixConst, MatrixASMStubSwitch;

{ TWaveletTests }

procedure TWaveletTests.TestContinousWT;
var sinData : TDoubleDynArray;
    i : Integer;
    wfl : TWavelet;
    c : TDynArrayofDoubleArray;
    levels : TIntegerDynArray;
const cMaxLevel : integer = 8;
      cLen : integer = 1024;
begin
     SetLength(sinData, cLen);
     for i := 0 to cLen - 1 do
         sinData[i] := sin( i*2*pi/100 );

     //WriteMatlabData('D:\wavein.txt', sinData, Length(sinData));

     SetLength( levels, 4 );
     for i := 0 to Length(levels) - 1 do
         levels[i] := i;

     wfl := TWavelet.Create(wtMexicanHat);
     try
        wfl.CWT(sinData, levels);

        SetLength(c, Length(levels));
        for i := 0 to Length(levels) - 1 do
        begin
             c[i] := wfl.getDetailC(i);

             // WriteMatlabData('d:\c_' + IntToStr(i) + '.txt', c[i], Length(c[i]));
        end;

        // continuous wavelet transform has no reconstruction property. Just check if input
        // matches output
        for i := 0 to Length(c) - 1 do
        begin
             Check( Length(c[i]) = Length(sinData), 'Output length is wrong');
        end;
     finally
            wfl.Free;
     end;
end;


procedure TWaveletTests.TestDyadicDWT;
var sinData : TDoubleDynArray;
    i : Integer;
    wfl : TWavelet;
    c : TDynArrayofDoubleArray;
    r : TDynArrayofDoubleArray;
const cMaxLevel : integer = 8;
      cLen : integer = 1024;
begin
     SetLength(sinData, cLen);
     for i := 0 to cLen - 1 do
         sinData[i] := sin( i*2*pi/100 );

     //WriteMatlabData('D:\wavein.txt', sinData, Length(sinData));

     // compare against matlab:
     // x = load('D:\wavein.txt');
     // dwtmode('per')
     // [swa, swd] = swt(x, 8, 'bior2.2');
     // c3 = load('D:\c_3.txt');
     // r3 = load('D:\r_3.txt');
     // figure; plot(swd(4, :) - c3);   // shows error...


     wfl := TWavelet.Create(wtBiortho22);
     try
        wfl.DyadicDWT(sinData, cMaxLevel);

        SetLength(c, cMaxLevel);
        SetLength(r, cMaxLevel);
        for i := 0 to cMaxLevel - 1 do
        begin
             c[i] := wfl.getDetailC(i);
             r[i] := wfl.getApproximationC(i);

             check( Length(c[i]) = Length(sinData), 'Wrong length');
             check( Length(r[i]) = Length(sinData), 'Wrong length');

             //WriteMatlabData('d:\c_' + IntToStr(i) + '.txt', c[i], Length(c[i]));
             //WriteMatlabData('d:\r_' + IntToStr(i) + '.txt', r[i], Length(r[i]));
        end;
     finally
            wfl.Free;
     end;
end;


procedure TWaveletTests.TestSimpleDWT;
var sinData : TDoubleDynArray;
    i : Integer;
    wfl : TWavelet;
    c : TDynArrayofDoubleArray;
    r : TDynArrayofDoubleArray;
    j: Integer;
const cMaxLevel : integer = 8;
      cLen : integer = 1024;
begin
     SetLength(sinData, cLen);
     for i := 0 to cLen - 1 do
         sinData[i] := sin( i*2*pi/100 );

     InitMathFunctions(itFPU, False);

     //WriteMatlabData('D:\wavein.txt', sinData, Length(sinData));

     wfl := TWavelet.Create(wtDaubechies2);
     try
        wfl.DWT(sinData, cMaxLevel);

        SetLength(c, cMaxLevel);
        SetLength(r, cMaxLevel);
        for i := 0 to cMaxLevel - 1 do
        begin
             c[i] := wfl.getDetailC(i);
             r[i] := wfl.getApproximationC(i);

             //WriteMatlabData('d:\c_' + IntToStr(i) + '.txt', c[i], Length(c[i]));
             //WriteMatlabData('d:\r_' + IntToStr(i) + '.txt', r[i], Length(r[i]));
        end;

        //WriteMatlabData('D:\recons.txt', reconstrSig, Length(reconstrSig));

        for j := 0 to Length(c) - 1 do
        begin
             Check( Length(c[j]) = Length(r[j]), 'Length test failed');
        end;
     finally
            wfl.Free;
     end;
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TWaveletTests{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}

end.
