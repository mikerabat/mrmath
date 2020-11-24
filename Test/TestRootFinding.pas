// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit TestRootFinding;

interface

{$IFDEF MACOS}
   {$DEFINE FMX}
{$ENDIF}

// ###########################################
// #### Simple root finding tests.
// ###########################################

uses {$IFDEF FPC} testregistry, {$ELSE} {$IFDEF FMX}DUnitX.TestFramework {$ELSE}TestFramework {$ENDIF}, {$ENDIF}
     BaseMatrixTestCase, Classes, SysUtils, matrix;

type
  {$IFDEF FMX} [TestFixture] {$ENDIF}

  { TRootFinding }

  TRootFinding = class(TBaseMatrixTestCase)
  published
    procedure TestRootBrent;
    procedure TestRootNewtonRaphson;
    procedure TestRootClass;
    procedure TestPolyRootsLaguerre;
    procedure TestPolyRootEig;
  private
    function cosFunc(const x : double) : double;
    procedure cosFunc2(const x: double; var y, dxdy: double);
  end;

implementation

uses Roots, Math, MatrixConst;


{ TRootFinding }

function TRootFinding.cosFunc(const x : double) : double;
begin
    Result := 0.2*cos( x + pi/8 );
end;

procedure TRootFinding.cosFunc2(const x : double; var y, dxdy : double);
begin
     y := 0.2*cos( x + pi/8 );
     dxdy := -0.2*sin(x);
end;

function SquareFunc( const x : double ) : double;
begin
     Result := sqr(x) - 2*x - 2;
end;

procedure PolyFunc( const x : double; var y, dxdy : double );
begin
     y := 0.5*x*x*x + 2*x*x - 0.2*x - 0.2;
     dxDy := 3*0.5*x*x + 2*2*x - 0.2;
end;

procedure TRootFinding.TestPolyRootsLaguerre;
const cRootReal : Array[0..3] of TComplex = ((real: 0.5; imag : 0),
                                             (real: 2; imag : 0),
                                             (real: -0.2; imag : 0),
                                             (real: -0.2; imag : 0));
      cRootImag : Array[0..3] of TComplex = ((real: 0.5; imag : 0),
                                             (real: 2; imag : 0),
                                             (real: -0.2; imag : 0),
                                             (real: +0.2; imag : 0));
      cRoots : Array[0..2] of double = ( -4.07408, 0.35256, -0.27848 );
      cRoots1 : Array[0..2] of TComplex = ( (real: -4.12063; imag: 0),
                                            (real:  0.06032; imag: 0.30567),
                                            (real:  0.06032; imag: -0.30567 ) );
var rootx : Array[0..2] of TComplex;
    i : Integer;
begin
     FillChar(rootX, sizeof(rootx), 0);

     // test with real roots
     Check( PolyRootLaguer( @cRootReal[0], Length(cRootReal), @rootx[0]) = rrOk, 'Failed to find roots');

     for i := 0 to Length(rootX) - 1 do
     begin
          Status( Format('%.3f + i*%.3f', [rootX[i].real, rootX[i].imag] ));
          Check( SameValue(rootX[i].real, cRoots[i], 1e-4) and (rootX[i].imag = 0), 'Root is not the same');
     end;

     Status(#13#10 + 'Test Complex roots:');

     // complex roots:
     Check( PolyRootLaguer( @cRootImag[0], Length(cRootImag), @rootx[0]) = rrOk, 'Failed to find roots');

     for i := 0 to Length(rootX) - 1 do
     begin
          Status( Format('%.3f + i*%.3f', [rootX[i].real, rootX[i].imag] ));
          Check( SameValue(rootX[i].real, cRoots1[i].real, 1e-4) and
                 SameValue(rootX[i].imag, cRoots1[i].imag, 1e-4), 'Root is not the same');
     end;
end;


procedure TRootFinding.TestPolyRootEig;
const cRootReal : Array[0..3] of double = (0.5, 2, -0.2, -0.2);
      cRootImag : Array[0..3] of double = (0.5, 2, -0.2, +0.2);
      cRoots : Array[0..2] of double = ( -4.07408, 0.35256, -0.27848 );
      cRoots1 : Array[0..2] of TComplex = ( (real: -4.12063; imag: 0),
                                            (real:  0.06032; imag: 0.30567),
                                            (real:  0.06032; imag: -0.30567 ) );
var rootx : Array[0..2] of TComplex;
    i : Integer;
begin
     FillChar(rootX, sizeof(rootx), 0);

     // test with real roots
     Check( PolyRootEig( @cRootReal[0], Length(cRootReal), @rootx[0]) = rrOk, 'Failed to find roots');

     for i := 0 to Length(rootX) - 1 do
     begin
          Status( Format('%.3f + i*%.3f', [rootX[i].real, rootX[i].imag] ));
          Check( SameValue(rootX[i].real, cRoots[i], 1e-4) and (rootX[i].imag = 0), 'Root is not the same');
     end;

     Status(#13#10 + 'Test Complex roots:');

     // complex roots:
     Check( PolyRootEig( @cRootImag[0], Length(cRootImag), @rootx[0]) = rrOk, 'Failed to find roots');

     for i := 0 to Length(rootX) - 1 do
     begin
          Status( Format('%.3f + i*%.3f', [rootX[i].real, rootX[i].imag] ));
          Check( SameValue(rootX[i].real, cRoots1[i].real, 1e-4) and
                 SameValue(rootX[i].imag, cRoots1[i].imag, 1e-4), 'Root is not the same');
     end;
end;

procedure TRootFinding.TestRootBrent;
var rootX1, rootX2 : double;
const cRoots : Array[0..1] of double = (2.73205, -0.73205);
begin
     check( FindRootBrent( {$IFDEF FPC}@{$ENDIF}squareFunc, 1, 4, rootX1) = rrOk, 'Failed to find root');
     check( FindRootBrent( {$IFDEF FPC}@{$ENDIF}squareFunc, -3, 1, rootX2) = rrOk, 'Failed to find root');

     Status( Format('Found root @%.3f, %.3f', [rootX1, rootX2]));

     check( SameValue( rootX1, cRoots[0], 1e-4 ), 'Bad root found');
     check( SameValue( rootX2, cRoots[1], 1e-4 ), 'Bad root found');
end;

procedure TRootFinding.TestRootClass;
var rootX1, rootX2 : double;
    {$IFNDEF FPC} rootX3 : double; {$ENDIF}

const cRoot = 1.1781;
begin
     Check( TFunctionRoot.FindRootBrent( {$IFDEF FPC}@{$ENDIF}cosfunc, pi/4, 3*pi/4, rootX1 ) = rrOk, 'FindRoot failed');
     {$IFNDEF FPC}
     Check( TFunctionRoot.FindRootBrent( function (const x : double) : double
                                         begin
                                              Result := 0.2*cos( x + pi/8 );
                                         end ,
                                         pi/4, 3*pi/4, rootX2 ) = rrOk, 'FindRoot failed');
     Check(rootX1 = rootX2, 'FindRoot failed');
     {$ENDIF}
     Status(Format('Brent returned zero cross for 0.2*cos(x + pi/8)) = %.6f', [rootX1]));

     check(SameValue(rootX1, cRoot, 1e-3), 'Root failed');

     Check( TFunctionRoot.FindRootNewtonRaphson( {$IFDEF FPC}@{$ENDIF}cosfunc2, pi/4, 3*pi/4, rootX2 ) = rrOk, 'FindRoot failed');
     Status(Format('Newton Raphson returned zero cross for 0.2*cos(x + pi/8)) = %.6f', [rootX2]));

     {$IFNDEF FPC}
     Check( TFunctionRoot.FindRootNewtonRaphson( procedure (const x : double; var y, dxdy : double)
                                                 begin
                                                      y := 0.2*cos( x + pi/8 );
                                                      dxdy := -0.2*sin(x);
                                                 end ,
                                         pi/4, 3*pi/4, rootX3 ) = rrOk, 'FindRoot failed');
     Check( rootX2 = rootX3, 'FindRoot failed');
     {$ENDIF}

     Check(SameValue( rootX1, rootX2, 1e-5), 'Newton raphosn failed');
end;

procedure TRootFinding.TestRootNewtonRaphson;
var rootX1, rootX2, rootX3 : double;
const cRoots : Array[0..2] of double = ( -4.07408, -0.27848, 0.35256 );
begin
     Check( FindRootNewtonRaphson({$IFDEF FPC}@{$ENDIF}PolyFunc, -5, -1, rootX1) = rrOk, 'Failed to find root');
     Check( FindRootNewtonRaphson({$IFDEF FPC}@{$ENDIF}PolyFunc, -1, 0, rootX2) = rrOk, 'Failed to find root');
     Check( FindRootNewtonRaphson({$IFDEF FPC}@{$ENDIF}PolyFunc, 0, 3, rootX3) = rrOk, 'Failed to find root');

     Status( Format('Found root @%.3f, %.3f, %.3f', [rootX1, rootX2, rootX3]));

     check( SameValue( rootX1, cRoots[0], 1e-4 ), 'Bad root found');
     check( SameValue( rootX2, cRoots[1], 1e-4 ), 'Bad root found');
     check( SameValue( rootX3, cRoots[2], 1e-4 ), 'Bad root found');
end;

initialization
{$IFNDEF FMX}
  RegisterTest(TRootFinding{$IFNDEF FPC}.Suite{$ENDIF});
{$ENDIF}


end.
