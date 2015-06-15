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

// ##############################################################
// #### Some overridden function which allows some multi threaded
// operations on the data
// todo: implement more functions multi threaded!
type
  TThreadedMatrix = class(TDoubleMatrix)
  protected
    class function ResultClass : TDoubleMatrixClass; override;
    class function ClassIdentifier : String; override;
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

    // note: due to the multi threaded offset the add and sub functions seem to be
    // faster if implemented single threaded
    procedure AddInplace(Value : TDoubleMatrix); override;
    function Add(Value : TDoubleMatrix) : TDoubleMatrix; override;
    procedure SubInPlace(Value : TDoubleMatrix); override;
    function Sub(Value : TDoubleMatrix) : TDoubleMatrix; override;

    function Median(RowWise : boolean) : TDoubleMatrix; override;

    // threaded lin alg functions
    function InvertInPlace : TLinEquResult; override;
    function Invert : TDoubleMatrix; override;
    function SolveLinEQ(Value : TDoubleMatrix; numRefinements : integer = 0) : TDoubleMatrix; override;
    procedure SolveLinEQInPlace(Value : TDoubleMatrix; numRefinements : integer = 0); override;
    function Determinant: double; override;
    function QR(out ecosizeR : TDoubleMatrix; out tau : TDoubleMatrix) : TQRResult; override;
    function QRFull(out Q, R : TDoubleMatrix) : TQRResult; override;

    class procedure InitThreadPool;
    class procedure FinalizeThreadPool;
  end;

implementation

uses SysUtils, ThreadedMatrixOperations, MtxThreadPool, ThreadedLinAlg, BlockSizeSetup,
     Math;

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

function TThreadedMatrix.Mult(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.Width, fSubHeight);

     if (Value.Width = 1) and (Value.LineWidth = sizeof(double))
     then
         ThrMatrixVecMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Height, LineWidth)
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
            ThrMatrixVecMult(res.StartElement, res.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Height, LineWidth)
        else
            ThrMatrixMult(res.StartElement, res.LineWidth, StartElement, Value.StartElement, Width, Height, Value.Width, Value.Height, LineWidth, Value.LineWidth);

        Assign(res);
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

end.
