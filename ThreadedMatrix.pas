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


unit ThreadedMatrix;

// ##############################################################
// #### Threaded versions of matrix operations
// ##############################################################

interface

uses Matrix, MatrixConst;

{$IFDEF FPC}
   {.$DEFINE ANONMETHODS}
{$ELSE}
   {$IF CompilerVersion >= 20.0}
      {$DEFINE ANONMETHODS}
   {$IFEND}
{$ENDIF}

// ##############################################################
// #### Some overridden function which allows some multi threaded
// operations on the data
type
  TThreadedMatrix = class(TDoubleMatrix)
  protected
    class function ResultClass : TDoubleMatrixClass; override;
    class function ClassIdentifier : String; override;

    function QR(out ecosizeR : TDoubleMatrix; out tau : TDoubleMatrix) : TQRResult; override;
    function Hess(out h, tau: TDoubleMatrix): TEigenvalueConvergence; override;
  public
    procedure MultInPlace(Value : TDoubleMatrix); override;
    function Mult(Value : TDoubleMatrix) : TDoubleMatrix; override;
    procedure MultInPlaceT1(Value : TDoubleMatrix); override;
    function MultT1(Value : TDoubleMatrix) : TDoubleMatrix; override;
    procedure MultInPlaceT2(Value : TDoubleMatrix); override;
    function MultT2(Value : TDoubleMatrix) : TDoubleMatrix; override;

    procedure AddAndScaleInPlace(const aOffset, aScale : double); override;
    function AddAndScale(const aOffset, aScale : double) : TDoubleMatrix; override;

    procedure ElementwiseFuncInPlace(func : TMatrixFunc); overload; override;
    function ElementwiseFunc(func : TMatrixFunc) : TDoubleMatrix; overload; override;
    procedure ElementwiseFuncInPlace(func : TMatrixObjFunc); overload; override;
    function ElementwiseFunc(func : TMatrixObjFunc) : TDoubleMatrix; overload; override;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFunc); overload; override;
    function ElementwiseFunc(func : TMatrixMtxRefFunc) : TDoubleMatrix; overload; override;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefObjFunc); overload; override;
    function ElementwiseFunc(func : TMatrixMtxRefObjFunc) : TDoubleMatrix; overload; override;

    {$IFDEF ANONMETHODS}
    procedure ElementwiseFuncInPlace(func : TMatrixFuncRef); overload; override;
    function ElementwiseFunc(func : TMatrixFuncRef) : TDoubleMatrix; overload; override;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFuncRef); overload; override;
    function ElementwiseFunc(func : TMatrixMtxRefFuncRef) : TDoubleMatrix; overload; override;
    {$ENDIF}


    // note: due to the multi threaded offset the add and sub functions seem to be
    // faster if implemented single threaded
    procedure AddInplace(Value : TDoubleMatrix); override;
    function Add(Value : TDoubleMatrix) : TDoubleMatrix; override;
    procedure SubInPlace(Value : TDoubleMatrix); override;
    function Sub(Value : TDoubleMatrix) : TDoubleMatrix; override;

    function Median(RowWise : boolean) : TDoubleMatrix; override;
    procedure RollMedianInPlace(RowWise : boolean; order : integer); override;
    procedure SortInPlace(RowWise : boolean); override;

    // threaded lin alg functions
    function InvertInPlace : TLinEquResult; override;
    function Invert : TDoubleMatrix; overload; override;
    function Invert( out InvMtx : TDoubleMatrix ) : TLinEquResult; overload; override;

    function SolveLinEQ(Value : TDoubleMatrix; numRefinements : integer = 0) : TDoubleMatrix; override;
    procedure SolveLinEQInPlace(Value : TDoubleMatrix; numRefinements : integer = 0); override;
    function Determinant: double; override;
    function QRFull(out Q, R : TDoubleMatrix) : TQRResult; override;
    function Cholesky(out Chol : TDoubleMatrix) : TCholeskyResult; overload; override;
    function SVD(out U, V, W : TDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; override;

    function SymEig(out EigVals : TDoubleMatrix; out EigVect : TDoubleMatrix) : TEigenvalueConvergence; overload; override;
    function SymEig(out EigVals : TDoubleMatrix) : TEigenvalueConvergence; overload; override;

    // solves least sqaures A*x = b using the QR decomposition
    function SolveLeastSquaresInPlace(out x : TDoubleMatrix; b : TDoubleMatrix) : TQRResult; overload; override;
    function SolveLeastSquares(out x : TDoubleMatrix; b : TDoubleMatrix) : TQRResult; overload; override;
    
    class procedure InitThreadPool;
    class procedure FinalizeThreadPool;
  end;

implementation

uses SysUtils, ThreadedMatrixOperations, MtxThreadPool, BlockSizeSetup,
     Math, LinAlgQR, LinAlgCholesky, LinAlgLU, LinAlgSVD, MatrixASMStubSwitch, 
     Eigensystems, BaseMathPersistence;

{ TThreadedMatrix }

class function TThreadedMatrix.ClassIdentifier: String;
begin
     Result := 'TMTX';
end;

class function TThreadedMatrix.ResultClass: TDoubleMatrixClass;
begin
     Result := TThreadedMatrix;
end;

function TThreadedMatrix.Add(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((Value.Width = fSubWidth) and (Value.Height = fSubHeight), 'Dimension error');

     Result := ResultClass.Create;
     Result.SetWidthHeight(fSubWidth, fSubHeight);
     ThrMatrixAdd(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TThreadedMatrix.AddAndScale(const aOffset,
  aScale: double): TDoubleMatrix;
begin
     assert((width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self, True);

     Result.AddAndScaleInPlace(aOffset, aScale);
end;

procedure TThreadedMatrix.AddAndScaleInPlace(const aOffset, aScale: double);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     ThrMatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, aOffset, aScale);
end;

procedure TThreadedMatrix.AddInplace(Value: TDoubleMatrix);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     // inplace matrix addition
     assert((fSubWidth = Value.Width) and (fSubHeight = Value.Height), 'Matrix dimensions do not match');
     ThrMatrixAdd(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     ThrMatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixObjFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     ThrMatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixObjFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixMtxRefFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     ThrMatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixMtxRefObjFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     ThrMatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefObjFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;


{$IFDEF ANONMETHODS}

function TThreadedMatrix.ElementwiseFunc(func: TMatrixFuncRef): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     ThrMatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixFuncRef);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixMtxRefFuncRef): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     ThrMatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefFuncRef);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

{$ENDIF}

class procedure TThreadedMatrix.FinalizeThreadPool;
begin
     MtxThreadPool.FinalizeMtxThreadPool;
end;

class procedure TThreadedMatrix.InitThreadPool;
begin
     MtxThreadPool.InitMtxThreadPool;
end;

function TThreadedMatrix.Invert: TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     Result := ResultClass.Create;
     try
        Result.Assign(Self, True);

        if ThrMatrixInverse(Result.StartElement, Result.LineWidth, fSubWidth, fLinEQProgress) = leSingular then
           raise ELinEQSingularException.Create('Singular matrix');
     except
           Result.Free;
           raise;
     end;
end;

function TThreadedMatrix.Invert(out InvMtx: TDoubleMatrix): TLinEquResult;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     InvMtx := ResultClass.Create;
     InvMtx.Assign(Self, True);

     Result := ThrMatrixInverse(InvMtx.StartElement, InvMtx.LineWidth, fSubWidth, fLinEQProgress);
     if Result <> leOk then
        FreeAndNil(invMtx);
end;

function TThreadedMatrix.InvertInPlace: TLinEquResult;
var dt : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     dt := ResultClass.Create(Width, Height);
     try
        dt.Assign(self, True);
        Result := ThrMatrixInverse(dt.StartElement, dt.LineWidth, fSubWidth, fLinEQProgress);
        if Result = leOk then
           Assign(dt);

        FreeAndNil(dt);
     except
           dt.Free;
           raise;
     end;
end;

function TThreadedMatrix.Median(RowWise: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          ThrMatrixMedian(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          ThrMatrixMedian(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end;
end;

procedure TThreadedMatrix.RollMedianInPlace(RowWise: boolean; order: integer);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     ThrMatrixRollMedianInPlace(StartElement, LineWidth, width, height, order, rowWise);
end;


function TThreadedMatrix.Mult(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.Width, fSubHeight);

     if (Value.Width = 1) and (Value.LineWidth = sizeof(double))
     then
         ThrMatrixVecMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, Width, Height, LineWidth, Value.LineWidth, 1, 0)
     else
         ThrMatrixMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);
end;

procedure TThreadedMatrix.MultInPlace(Value: TDoubleMatrix);
var res : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fOffsetX = 0) and (fOffsetY = 0) and (Width = fSubWidth) and (Height = fSubHeight), 'Operation only allowed on full matrices');
     assert((fSubWidth = Value.Height), 'Dimension error');

     res := ResultClass.Create(Value.Width, fSubHeight);

     try
        if (Value.Width = 1) and (Value.LineWidth = sizeof(double))
        then
            ThrMatrixVecMult(res.StartElement, res.LineWidth, StartElement, Value.StartElement, Width, Height, LineWidth, Value.LineWidth, 1, 0)
        else
            ThrMatrixMult(res.StartElement, res.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);

        Assign(res);
        res.Free;
     except
           res.Free;
           raise;
     end;
end;

procedure TThreadedMatrix.MultInPlaceT1(Value: TDoubleMatrix);
var res : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fOffsetX = 0) and (fOffsetY = 0) and (Width = fSubWidth) and (Height = fSubHeight), 'Operation only allowed on full matrices');
     assert((fSubHeight = Value.Height), 'Dimension error');

     res := ResultClass.Create(Value.Width, fSubWidth);

     try
        ThrMatrixMultT1(res.StartElement, res.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);

        Assign(res);
        res.Free;
     except
           res.Free;
           raise;
     end;
end;

procedure TThreadedMatrix.MultInPlaceT2(Value: TDoubleMatrix);
var res : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fOffsetX = 0) and (fOffsetY = 0) and (Width = fSubWidth) and (Height = fSubHeight), 'Operation only allowed on full matrices');
     assert((fSubWidth = Value.Width), 'Dimension error');

     res := ResultClass.Create(Value.Height, fSubHeight);
     try
        ThrMatrixMultT2(res.StartElement, res.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);

        Assign(res);
        res.Free;
     except
           res.Free;
           raise;
     end;
end;

function TThreadedMatrix.MultT1(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.Width, fSubWidth);

     ThrMatrixMultT1(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);
end;

function TThreadedMatrix.MultT2(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.Height, fSubHeight);

     ThrMatrixMultT2(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);
end;

function TThreadedMatrix.QR(out ecosizeR, tau: TDoubleMatrix): TQRResult;
begin
     ecosizeR := ResultClass.Create;
     tau := ResultClass.Create(width, 1);
     try
        ecosizeR.Assign(self);
        Result := ThrMatrixQRDecomp(ecosizeR.StartElement, ecosizeR.LineWidth, width, height, tau.StartElement, nil, QRBlockSize, fLinEQProgress);
     except
           FreeAndNil(ecosizeR);
           FreeAndNil(tau);

           raise;
     end;
end;

function TThreadedMatrix.Hess(out h,
  tau: TDoubleMatrix): TEigenvalueConvergence;
begin
     h := ResultClass.Create;
     tau := ResultClass.Create(width, 1);
     try
        h.Assign(self);
        Result := ThrMtxHessenberg2InPlace(h.StartElement, h.LineWidth, width, tau.StartElement, HessBlockSize);
     except
           FreeAndNil(h);
           FreeAndNil(tau);

           raise;
     end;
end;

function TThreadedMatrix.QRFull(out Q, R: TDoubleMatrix): TQRResult;
var tau : TDoubleMatrix;
    tmp : TDoubleMatrix;
    x, y : integer;
    pdata : PConstDoubleArr;
begin
     Q := nil;
     R := nil;

     // ###########################################
     // #### First create a compact QR decomposition
     Result := QR(R, Tau);

     if Result <> qrOK then
     begin
          FreeAndNil(R);
          exit;
     end;

     // ###########################################
     // #### Calculation Q from the previous operation
     tmp := ResultClass.Create;
     try
        tmp.Assign(R);
     
        ThrMatrixQFromQRDecomp(tmp.StartElement, tmp.LineWidth, Width, Height, tau.StartElement, QRBlockSize, nil, fLinEQProgress);

        // now assign only the relevant parts of Q (or just copy it if we have a square matrix)
        if Width = height 
        then
            Q := tmp
        else
        begin
             // economy size Q:
             tmp.SetSubMatrix(0, 0, Math.min(tmp.Width, tmp.Height), tmp.Height);
          
             Q := ResultClass.Create;
             Q.Assign(tmp, True);
             tmp.Free;
             
             tmp := R;
             tmp.SetSubMatrix(0, 0, tmp.Width, Math.Min(tmp.Width, tmp.Height));
             R := ResultClass.Create;
             R.Assign(tmp, True);
             tmp.Free;
        end;       

        // clear R so we only have un upper triangle matrix
        // zero out the parts occupied by Q
        for y := 1 to R.Height - 1 do
        begin
             pData := PConstDoubleArr( R.StartElement );
             inc(PByte(pData), y*R.LineWidth);
             for x := 0 to Math.Min(r.Width, y) - 1 do
                 pData^[x] := 0;
        end;
     finally
            Tau.Free;
     end;
end;

function TThreadedMatrix.Cholesky(out Chol: TDoubleMatrix): TCholeskyResult;
var x, y : integer;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Cholesky decomposition is only allowed on square matrices');
     chol := ResultClass.Create;
     try
        chol.Assign(Self);
        // block wise cholesky decomposition
        Result := ThrMatrixCholeskyDecomp(chol.StartElement, chol.LineWidth, chol.Width, 0, nil, fLinEQProgress);

        if Result = crOk then
        begin
             // zero out the upper elements
             for y := 0 to fSubWidth - 1 do
             begin
                  for x := y + 1 to fSubWidth - 1 do
                      Chol[x, y] := 0;
             end;
        end
        else
            FreeAndNil(chol);
     except
           FreeAndNil(chol);
           Result := crNoPositiveDefinite;
     end;
end;

function TThreadedMatrix.SolveLeastSquares(out x: TDoubleMatrix;
  b: TDoubleMatrix): TQRResult;
var tmpA : TDoubleMatrix;
begin
     CheckAndRaiseError( width <= Height, 'Height must be at least width');
     
     tmpA := Clone;
     try
        x := ResultClass.Create( 1, Width );

        Result := ThrMatrixQRSolve( x.StartElement, x.LineWidth, tmpA.StartElement, tmpA.LineWidth, b.StartElement, b.LineWidth, width, height);

        if Result <> qrOK then
           FreeAndNil(x);
     finally
            tmpA.Free;
     end;
end;

function TThreadedMatrix.SolveLeastSquaresInPlace(out x: TDoubleMatrix;
  b: TDoubleMatrix): TQRResult;
begin
     CheckAndRaiseError( width <= Height, 'Height must be at least width');
     
     x := ResultClass.Create( 1, Width );

     Result := ThrMatrixQRSolve( x.StartElement, x.LineWidth, StartElement, LineWidth, b.StartElement, b.LineWidth, width, height );

     if Result <> qrOK then
        FreeAndNil(x);
end;

function TThreadedMatrix.SolveLinEQ(Value: TDoubleMatrix;
  numRefinements: integer): TDoubleMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fSubWidth = fSubHeight) and (Value.Height = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(Value.Width, fSubHeight);
     try
        if ThrMatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement,
                               Value.LineWidth, Result.StartElement,
                               Result.LineWidth, Value.Width, numRefinements, fLinEQProgress) = leSingular
     then
         raise ELinEQSingularException.Create('Matrix is singular');

     except
           FreeAndNil(Result);
           raise;
     end;
end;

procedure TThreadedMatrix.SolveLinEQInPlace(Value: TDoubleMatrix;
  numRefinements: integer);
var dt : TDoubleMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fSubWidth = fSubHeight) and (Value.Height = fSubHeight), 'Dimension error');

     dt := ResultClass.Create(Value.Width, fSubHeight);
     try
        if ThrMatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement, Value.LineWidth,
                               dt.StartElement, dt.LineWidth, Value.Width,
                               numRefinements, fLinEQProgress) = leSingular
        then
            raise ELinEQSingularException.Create('Matrix is singular');

        Assign(dt, False);
     finally
            dt.Free;
     end;
end;

procedure TThreadedMatrix.SortInPlace(RowWise: boolean);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     ThrMatrixSort(StartElement, LineWidth, width, Height, RowWise);
end;

function TThreadedMatrix.Determinant: double;
begin
     assert((Width > 0) and (Height = Width), 'Determinant only allowed on square matrices');
     Result := ThrMatrixDeterminant(StartElement, LineWidth, fSubWidth);
end;

function TThreadedMatrix.Sub(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((Value.Width = fSubWidth) and (Value.Height = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(fSubWidth, fSubHeight);
     ThrMatrixSub(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                  Width*sizeof(double), Value.LineWidth);
end;

procedure TThreadedMatrix.SubInPlace(Value: TDoubleMatrix);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     
     // inplace matrix substraction
     assert((fSubWidth = Value.Width) and (fSubHeight = Value.Height), 'Matrix dimensions do not match');
     ThrMatrixSub(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TThreadedMatrix.SVD(out U, V, W: TDoubleMatrix;
  onlyDiagElements: boolean): TSVDResult;
var pW : PConstDoubleArr;
    wArr : PByte;
    minWH : NativeInt;
    i : Integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'Dimension error');

     U := nil;
     V := nil;
     W := nil;
     wArr := nil;
     try
        minWH := Math.Min(fSubWidth, fSubHeight);

        W := ResultClass.Create(ifthen(onlyDiagElements, 1, minWH), minWH);

        pW := PConstDoubleArr( W.StartElement );
        if not onlyDiagElements then
        begin
             wArr := MtxAlloc( minWH*sizeof(double));
             pW := PConstDoubleArr( wArr );
        end;

        if width > height then
        begin
             V := Transpose;
             U := ResultClass.Create( fSubHeight, fSubHeight );

             Result := ThrMatrixSVDInPlace(V.StartElement, V.LineWidth, Height, Width, pW, U.StartElement, U.LineWidth, SVDBlockSize, fLinEQProgress );

             // we need a final transposition on the matrix U and V
             U.TransposeInPlace;
             V.TransposeInPlace;
        end
        else
        begin
             U := Clone;
             V := ResultClass.Create(fSubWidth, fSubWidth);

             Result := ThrMatrixSVDInPlaceEx(U.StartElement, U.LineWidth, Width, Height, pW, V.StartElement, V.LineWidth, 
                                           StartElement, LineWidth,
                                           SVDBlockSize, fLinEQProgress );
        end;

        if Result <> srOk then
        begin
             FreeAndNil(u);
             FreeAndNil(V);
             FreeAndNil(W);
        end
        else if not onlyDiagElements then
        begin
             for i := 0 to minWH - 1 do
                 W[i, i] := pW^[i];
        end;

        if Assigned(wArr) then
           FreeMem(wArr);
     except
           FreeAndNil(u);
           FreeAndNil(V);
           FreeAndNil(W);

           if Assigned(wArr) then
              FreeMem(wArr);

           raise;
     end;
end;

function TThreadedMatrix.SymEig(out EigVals: TDoubleMatrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vecs : TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Eigenvalue calculation only allowed on square matrices');

     EigVals := nil;

     dt := ResultClass.Create(1, fSubHeight);
     vecs := ResultClass.Create(fSubWidth, fSubWidth);
     try
        vecs.Assign(self, True);
        Result := ThrMtxEigUpperSymmetricMatrixInPlace2(vecs.StartElement, vecs.LineWidth, fSubWidth,
                                                        PConstDoubleArr( dt.StartElement ), True,
                                                        SymEigBlockSize, fLinEQProgress);
        if Result = qlOk then
        begin
             dt.TransposeInPlace;
             EigVals := dt;
             vecs.Free;
        end
        else
        begin
             dt.Free;
             vecs.Free;
        end;
     except
           FreeAndNil(dt);
           FreeAndNil(vecs);
           raise;
     end;
end;

function TThreadedMatrix.SymEig(out EigVals,
  EigVect: TDoubleMatrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vecs : TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Eigenvalue calculation only allowed on square matrices');

     EigVals := nil;
     EigVect := nil;

     dt := ResultClass.Create(fSubWidth, 1);
     vecs := ResultClass.Create(fSubWidth, fSubWidth);
     try
        vecs.Assign(self, True);
        Result := ThrMtxEigUpperSymmetricMatrixInPlace2(vecs.StartElement, vecs.LineWidth, fSubWidth,
                                                        PConstDoubleArr( dt.StartElement ), False,
                                                        SymEigBlockSize, fLinEQProgress);
        if Result = qlOk then
        begin
             dt.TransposeInPlace;
             EigVals := dt;
             EigVect := vecs;
        end
        else
        begin
             dt.Free;
             vecs.Free;
        end;
     except
           FreeAndNil(dt);
           FreeAndNil(vecs);

           raise;
     end;
end;

initialization
  RegisterMathIO(TThreadedMatrix);

end.

