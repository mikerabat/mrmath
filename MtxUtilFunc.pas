// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2020, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit MtxUtilFunc;

interface

uses Matrix;

// ###########################################
// #### special matrix utility functions
// ###########################################


// ###########################################
// #### Matrix exponential, squareroot, log

// matrix exponential based on Pade's approximation
// source is based on the octave implementation and uses
// traceshifting, balancing 
function expm( A : IMatrix ) : IMatrix; overload;
function expm( A : TDoubleMatrix ) : TDoubleMatrix; overload;


implementation

uses Types, Math, Eigensystems, MatrixConst;

// ###########################################
// #### Matrix exponential 
function expm( A : IMatrix ) : IMatrix;
begin
     Result := expm(A.GetObjRef);
end;

function expm( A : TDoubleMatrix ) : TDoubleMatrix;
var n : integer;
    tr : double;
    i: Integer;
    scale : TDoubleDynArray;
    aNorm : double;
    s : integer;
    aa : IMatrix;
    a2 : IMatrix;
    x, y : IMatrix;
    id : IMatrix;
    r : IMatrix;
    k: Integer;
    m : extended;
const cFactX : Array[0..3] of double =  ( 1.1666666666666667e-1,
                                          1.6025641025641026e-3,
                                          4.8562548562548563e-6, 
                                          1.9270852604185938e-9
                                        );

      cFactY : Array[0..3] of double =  ( 5.0000000000000000e-1,
                                          1.6666666666666667e-2, 
                                          1.0683760683760684e-4,
                                          1.3875013875013875e-7
                                        );
begin
     n := A.Width;
     tr := A.Trace/n;
     
     id := TDoubleMatrix.CreateEye(A.Width);
     
     SetLength(scale, A.Width);
     aa := A.Clone;
     // traceshift
     if tr > 0 then
     begin
          for i := 0 to A.Width - 1 do
              aa[i,i] := aa[i,i] - tr;
     end;

     // balancing
     MatrixBalanceInPlace( aa.StartElement, aa.LineWidth, aa.Width, @scale[0], sizeof(double) );

     // norm
     aNorm := aa.ElementwiseNorm2;
     
     frexp( aNorm, m, s );
     aa.ScaleInPlace( IntPower( 2, -s ) );

     // Pade approximation for exp(A).     
     a2 := aa.Mult(aa);
     
     x := a2.Scale(cFactX[3]);
     x.AddInplace( id.Scale(cFactX[2]) as IMatrix );
     x.MultInPlace(a2);
     x.AddInplace( id.Scale(cFactX[1]) as IMatrix );
     x.MultInPlace(a2);
     x.AddInplace( id.Scale(cFactX[0]) as IMatrix );
     x.MultInPlace(a2);
     x.AddInplace(id);

     y := a2.Scale(cFactY[3]);
     y.AddInplace( id.Scale(cFactY[2]) as IMatrix );
     y.MultInPlace(a2);
     y.AddInplace( id.Scale(cFactY[1]) as IMatrix );
     y.MultInPlace(a2);
     y.AddInplace( id.Scale(cFactY[0]) as IMatrix );
     y.MultInPlace(aa);

     // (x - y) \ (x + y)

     // this could be made more robust when using QR or SVD instead
     r := X.Sub(Y);
     if r.InvertInPlace = leSingular then
        raise ELinEQSingularException.Create('Failed to calculate matrix inverse');

     r.MultInPlace( x.Add(y) as IMatrix );

     // undo scaling by repeated squaring
     for k := 0 to s - 1 do
         r.MultInPlace(r);
     
     // inverse balancing
     MatrixBalanceBackInPlace( r.StartElement, r.LineWidth, r.Width, @scale[0], sizeof(double) );
     
     // inverse trace reduction
     if tr > 0 then
        r.ScaleInPlace( exp(tr) );

     // take over data 
     Result := TDoubleMatrix.Create;
     Result.TakeOver(r);
end;

end.
