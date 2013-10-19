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


unit Matrix;

// ############################################
// #### Base matrix operations
// ############################################

interface

uses SysUtils, Classes, Types, MatrixConst, BaseMathPersistence;

type
  TDoubleMatrix = class;
  IMatrix = interface(IMathPersistence)
    ['{8B76CD12-4314-41EB-BD98-302A024AA0EC}']
    function StartElement : PDouble; 
    function LineWidth : integer; 

    procedure SetLinEQProgress(value : TLinEquProgress);
    function GetLinEQProgress : TLinEquProgress;

    function GetItems(x, y: integer): double; register;
    procedure SetItems(x, y: integer; const Value: double); register;

    function GetSubWidth : integer;
    function GetSubHeight : integer;
    procedure SetWidth(const Value : integer);
    procedure SetHeight(const Value : integer);
    procedure SetWidthHeight(const Width, Height : integer; AssignMem : boolean = True);

    property Width : integer read GetSubWidth write SetWidth;
    property Height : integer read GetSubHeight write SetHeight;

    property LinEQProgress : TLinEquProgress read GetLinEQProgress write SetLinEQProgress;

    // general access
    property Items[x, y : integer] : double read GetItems write SetItems; default;

    procedure Clear;
    function GetObjRef : TDoubleMatrix;

    function SubMatrix : TDoubleDynArray;
    procedure SetSubMatrix(x, y, Subwidth, Subheight : integer);
    procedure UseFullMatrix;

    procedure SetRow(row : integer; const Values : Array of Double); overload;
    procedure SetRow(row : integer; Values : TDoubleMatrix; ValRow : integer = 0); overload;
    procedure SetRow(row : integer; Values : IMatrix; ValRow : integer = 0); overload;
    procedure SetColumn(col : integer; const Values : Array of Double); overload;
    procedure SetColumn(col : integer; Values : TDoubleMatrix; ValCols : integer = 0); overload;
    procedure SetColumn(col : integer; Values : IMatrix; ValCols : integer = 0); overload;

    function Reshape(newWidth, newHeight : integer) : TDoubleMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer);

    // ###################################################
    // #### Simple matrix utility functions
    function Max : double;
    function Min : double;

    procedure Normalize(RowWise : boolean);
    function ElementwiseNorm2 : double;

    // ###################################################
    // #### Base Matrix operations
    procedure TransposeInPlace;
    function Transpose : TDoubleMatrix;

    procedure AddInplace(Value : TDoubleMatrix); overload;
    function Add(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure SubInPlace(Value : TDoubleMatrix); overload;
    function Sub(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure MultInPlace(Value : TDoubleMatrix); overload;
    function Mult(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : TDoubleMatrix); overload;
    function ElementWiseMult(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure AddInplace(Value : IMatrix); overload;
    function Add(Value : IMatrix) : TDoubleMatrix; overload;
    procedure SubInPlace(Value : IMatrix); overload;
    function Sub(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlace(Value : IMatrix); overload;
    function Mult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : IMatrix); overload;
    function ElementWiseMult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure AddAndScaleInPlace(const Offset, Scale : double);
    function AddAndScale(const Offset, Scale : double) : TDoubleMatrix;

    function Mean(RowWise : boolean) : TDoubleMatrix; overload;
    procedure MeanInPlace(RowWise : boolean); overload;
    function Sum(RowWise : boolean) : TDoubleMatrix;
    procedure SumInPlace(RowWise : boolean);

    function Scale(const Value : double) : TDoubleMatrix;
    procedure ScaleInPlace(const Value : Double);
    function ScaleAndAdd(const Offset, Scale : double) : TDoubleMatrix;
    procedure ScaleAndAddInPlace(const Offset, Scale : double);
    function SQRT(const Value : double) : TDoubleMatrix;
    procedure SQRTInPlace(const Value : double);

    procedure ElementwiseFuncInPlace(func : TMatrixFunc); overload;
    function ElementwiseFunc(func : TMatrixFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixObjFunc); overload;
    function ElementwiseFunc(func : TMatrixObjFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFunc); overload;
    function ElementwiseFunc(func : TMatrixMtxRefFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefObjFunc); overload;
    function ElementwiseFunc(func : TMatrixMtxRefObjFunc) : TDoubleMatrix; overload;

    // ###################################################
    // #### Linear System solver A*x = B -> use A.SolveLinEQ(B) -> x
    function SolveLinEQ(Value : TDoubleMatrix; numRefinements : integer = 0) : TDoubleMatrix; overload;
    function SolveLinEQ(Value : IMatrix; numRefinements : integer = 0) : TDoubleMatrix; overload;
    procedure SolveLinEQInPlace(Value : TDoubleMatrix; numRefinements : integer = 0); overload;
    procedure SolveLinEQInPlace(Value : IMatrix; numRefinements : integer = 0); overload;
    function InvertInPlace : TLinEquResult;
    function Invert : TDoubleMatrix;
    function PseudoInversionInPlace : TSVDResult;
    function PseudoInversion(out Mtx : TDoubleMatrix) : TSVDResult; overload;
    function PseudoInversion(out Mtx : IMatrix) : TSVDResult; overload;

    function Determinant : double;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : double);

    // ###################################################
    // #### Matrix transformations
    function SVD(out U, V, W : TDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SVD(out U, V, W : IMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SymEig(out EigVals : TDoubleMatrix; out EigVect : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix; out EigVect : IMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix; out EigVect : TDoubleMatrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix; out EigVect : IMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix)  : TEigenvalueConvergence; overload;
    function Cholesky(out Chol : TDoubleMatrix) : TCholeskyResult; overload;
    function Cholesky(out Chol : IMatrix) : TCholeskyResult; overload;
    function QR(out R : TDoubleMatrix) : TQRResult; overload;
    function QR(out R : IMatrix) : TQRResult; overload;

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(Value : TDoubleMatrix); overload;
    procedure Assign(Value : IMatrix); overload;
    procedure Assign(Value : TDoubleMatrix; OnlySubElements : boolean); overload;
    procedure Assign(Value : IMatrix; OnlySubElements : boolean); overload;
    procedure Assign(const Mtx : Array of double; W, H : integer); overload;
    procedure AssignSubMatrix(Value : TDoubleMatrix; X : integer = 0; Y : integer = 0); overload;
    procedure AssignSubMatrix(Value : IMatrix; X : integer = 0; Y : integer = 0); overload;

    function Clone : IMatrix;
  end;

// #################################################
// #### Builds base matrix operations
  TDoubleMatrix = class(TBaseMathPersistence, IMatrix)
  protected
    // matrix persistence functions
    function GetItems(x, y: integer): double; register;
    procedure SetItems(x, y: integer; const Value: double); register;

    procedure DefineProps; override;
    class function ClassIdentifier : String; override;
    procedure OnLoadStringProperty(const Name : String; const Value : String); overload; override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); overload; override;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); override;

    procedure SetLinEQProgress(value : TLinEquProgress);
    function GetLinEQProgress : TLinEquProgress;

    procedure ReserveMem(width, height: integer);
  private
    fMemory : Pointer;
    fData : PConstDoubleArr;       // 16 byte aligned pointer:
    fLineWidth : integer;
    fWidth : integer;
  protected
    fHeight : integer;
    fName : String;
    fSubWidth : integer;
    fSubHeight : integer;
    fOffsetX : integer;
    fOffsetY : integer;

    fLinEQProgress : TLinEquProgress;

    procedure SetData(data : PDouble; srcLineWidth, width, height : integer);
    procedure Clear;
    procedure SetWidth(const Value : integer);
    procedure SetHeight(const Value : integer);
    function GetSubWidth : integer;
    function GetSubHeight : integer;
    procedure SetWidthHeight(const Width, Height : integer; AssignMem : boolean = True);
    function GetObjRef : TDoubleMatrix;

    // direct access functionality (use only when you know what you are doing!)
    function StartElement : PDouble; {$IF CompilerVersion >= 17.0} inline; {$IFEND}
    function LineWidth : integer; {$IF CompilerVersion >= 17.0} inline; {$IFEND}
  public
    property Width : integer read GetSubWidth write SetWidth;
    property Height : integer read GetSubHeight write SetHeight;

    property Name : string read fName write fName;

    property LineEQProgress : TLinEquProgress read GetLinEQProgress write SetLinEQProgress;

    // general access
    property Items[x, y : integer] : double read GetItems write SetItems; default;
    function SubMatrix : TDoubleDynArray;
    procedure SetSubMatrix(x, y, Subwidth, Subheight : integer);
    procedure UseFullMatrix;

    procedure SetRow(row : integer; const Values : Array of Double); overload;
    procedure SetRow(row : integer; Values : TDoubleMatrix; ValRow : integer = 0); overload;
    procedure SetRow(row : integer; Values : IMatrix; ValRow : integer = 0); overload;
    procedure SetColumn(col : integer; const Values : Array of Double); overload;
    procedure SetColumn(col : integer; Values : TDoubleMatrix; ValCols : integer = 0); overload;
    procedure SetColumn(col : integer; Values : IMatrix; ValCols : integer = 0); overload;

    function Reshape(newWidth, newHeight : integer) : TDoubleMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer);

    // ###################################################
    // #### Simple matrix utility functions
    function Max : double;
    function Min : double;

    procedure Normalize(RowWise : boolean);
    function ElementwiseNorm2 : double;

    // ###################################################
    // #### Base Matrix operations
    procedure TransposeInPlace;
    function Transpose : TDoubleMatrix;

    procedure AddInplace(Value : TDoubleMatrix); overload; virtual;
    function Add(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure SubInPlace(Value : TDoubleMatrix); overload; virtual;
    function Sub(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure MultInPlace(Value : TDoubleMatrix); overload; virtual;
    function Mult(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure ElementWiseMultInPlace(Value : TDoubleMatrix); overload; virtual;
    function ElementWiseMult(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure AddInplace(Value : IMatrix); overload;
    function Add(Value : IMatrix) : TDoubleMatrix; overload;
    procedure SubInPlace(Value : IMatrix); overload;
    function Sub(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlace(Value : IMatrix); overload;
    function Mult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : IMatrix); overload;
    function ElementWiseMult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure AddAndScaleInPlace(const Offset, Scale : double); virtual;
    function AddAndScale(const Offset, Scale : double) : TDoubleMatrix; virtual;

    function Mean(RowWise : boolean) : TDoubleMatrix; overload;
    procedure MeanInPlace(RowWise : boolean); overload;
    function Sum(RowWise : boolean) : TDoubleMatrix;
    procedure SumInPlace(RowWise : boolean);

    function Scale(const Value : double) : TDoubleMatrix;
    procedure ScaleInPlace(const Value : Double);
    function ScaleAndAdd(const Offset, Scale : double) : TDoubleMatrix;
    procedure ScaleAndAddInPlace(const Offset, Scale : double);
    function SQRT(const Value : double) : TDoubleMatrix;
    procedure SQRTInPlace(const Value : double);

    procedure ElementwiseFuncInPlace(func : TMatrixFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixFunc) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixObjFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixObjFunc) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixMtxRefFunc) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefObjFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixMtxRefObjFunc) : TDoubleMatrix; overload; virtual;

    // ###################################################
    // #### Linear System solver A*x = B -> use A.SolveLinEQ(B) -> x
    function SolveLinEQ(Value : TDoubleMatrix; numRefinements : integer = 0) : TDoubleMatrix; overload; virtual;
    function SolveLinEQ(Value : IMatrix; numRefinements : integer = 0) : TDoubleMatrix; overload;
    procedure SolveLinEQInPlace(Value : TDoubleMatrix; numRefinements : integer = 0); overload; virtual;
    procedure SolveLinEQInPlace(Value : IMatrix; numRefinements : integer = 0); overload;
    function InvertInPlace : TLinEquResult; virtual;
    function Invert : TDoubleMatrix; virtual;
    function PseudoInversionInPlace : TSVDResult;
    function PseudoInversion(out Mtx : TDoubleMatrix) : TSVDResult; overload;
    function PseudoInversion(out Mtx : IMatrix) : TSVDResult; overload;

    function Determinant : double; virtual;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : double);

    // ###################################################
    // #### Matrix transformations
    function SVD(out U, V, W : TDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SVD(out U, V, W : IMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SymEig(out EigVals : TDoubleMatrix; out EigVect : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix; out EigVect : IMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix; out EigVect : TDoubleMatrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix; out EigVect : IMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix)  : TEigenvalueConvergence; overload;
    function Cholesky(out Chol : TDoubleMatrix) : TCholeskyResult; overload;
    function Cholesky(out Chol : IMatrix) : TCholeskyResult; overload;
    function QR(out R : TDoubleMatrix) : TQRResult; overload;
    function QR(out R : IMatrix) : TQRResult; overload;

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(Value : TDoubleMatrix); overload;
    procedure Assign(Value : IMatrix); overload;
    procedure Assign(Value : TDoubleMatrix; OnlySubElements : boolean); overload;
    procedure Assign(Value : IMatrix; OnlySubElements : boolean); overload;
    procedure Assign(const Mtx : Array of double; W, H : integer); overload;
    procedure AssignSubMatrix(Value : TDoubleMatrix; X : integer = 0; Y : integer = 0); overload;
    procedure AssignSubMatrix(Value : IMatrix; X : integer = 0; Y : integer = 0); overload;

    function Clone : IMatrix;

    constructor Create; overload;
    constructor Create(width, height : integer; const initVal : double = 0); overload;
    constructor Create(data : PDouble; LineWidth : integer; width, height : integer); overload;
    constructor Create(const Data : TDoubleDynArray; width, height : integer); overload;
    destructor Destroy; override;
  end;

type
  TDoubleMatrixDynArr = Array of TDoubleMatrix;

implementation

uses Math, contnrs, OptimizedFuncs, LinearAlgebraicEquations,
     Eigensystems;


{$IFNDEF CPUX64}
type
  NativeUInt = Cardinal;
{$ENDIF}


{ TDoubleMatrix }

function TDoubleMatrix.Add(Value: IMatrix): TDoubleMatrix;
begin
     Result := Add(Value.GetObjRef);
end;

function TDoubleMatrix.AddAndScale(const Offset, Scale: double): TDoubleMatrix;
begin
     assert((width > 0) and (Height > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(self, True);

     Result.AddAndScaleInPlace(Offset, Scale);
end;

procedure TDoubleMatrix.AddAndScaleInPlace(const Offset, Scale: double);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Offset, Scale);
end;

procedure TDoubleMatrix.AddInplace(Value: IMatrix);
begin
     AddInplace(Value.GetObjRef);
end;

procedure TDoubleMatrix.AddInplace(Value: TDoubleMatrix);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     // inplace matrix addition
     assert((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Matrix dimensions do not match');
     MatrixAdd(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.Assign(const Mtx: array of double; W, H : integer);
begin
     assert((W*H > 0) and (Length(Mtx) = W*H), 'Dimension error');

     SetWidthHeight(W, H);
     MatrixCopy(StartElement, LineWidth, @Mtx[0], W*sizeof(double), W, H);
end;

procedure TDoubleMatrix.AssignSubMatrix(Value: IMatrix; X, Y: integer);
begin
     AssignSubMatrix(Value.GetObjRef, X, Y);
end;

procedure TDoubleMatrix.Assign(Value: IMatrix; OnlySubElements: boolean);
begin
     Assign(Value.GetObjRef, OnlySubElements);
end;

procedure TDoubleMatrix.Assign(Value: IMatrix);
begin
     Assign(Value.GetObjRef);
end;

procedure TDoubleMatrix.AssignSubMatrix(Value: TDoubleMatrix; X, Y: integer);
var pSelf, pValue : PDouble;
begin
     assert((Value.Width <= Width) and (Value.Height <= Height), 'Dimension error');
     if (Value.Width = 0) or (Value.Height = 0) then
        exit;

     pSelf := StartElement;
     inc(PByte(pSelf), Y*LineWidth);
     inc(pSelf, X);
     pValue := Value.StartElement;

     MatrixCopy(pSelf, LineWidth, pValue, Value.LineWidth, Value.Width, Value.Height);
end;

procedure TDoubleMatrix.Assign(Value: TDoubleMatrix; OnlySubElements: boolean);
begin
     fName := Value.Name;

     if OnlySubElements then
     begin
          SetWidthHeight(Value.Width, Value.Height);
          MatrixCopy(StartElement, LineWidth, Value.StartElement, Value.LineWidth, Value.Width, Value.Height);
     end
     else
     begin
          SetWidthHeight(Value.fWidth, Value.fHeight);
          MatrixCopy(StartElement, LineWidth, PDouble(Value.fData), Value.LineWidth, Value.fWidth, Value.fHeight);
     end;
end;

function TDoubleMatrix.Add(Value : TDoubleMatrix) : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := TDoubleMatrix.Create(fSubWidth, fSubHeight);
     MatrixAdd(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.Assign(Value: TDoubleMatrix);
begin
     Assign(Value, False);
end;

function TDoubleMatrix.Cholesky(out Chol: TDoubleMatrix): TCholeskyResult;
var p : TDoubleDynArray;
    x, y : integer;
begin
     assert((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Cholesky decomposition is only allowed on square matrices');
     chol := TDoubleMatrix.Create(fSubWidth, fSubHeight);
     try
        SetLength(p, fSubWidth);

        Result := MatrixCholesky(chol.StartElement, chol.LineWidth, StartElement, LineWidth, fSubWidth, @p[0], sizeof(double), fLinEQProgress);

        if Result = crOk then
        begin
             // copy diagonal elements which are stored in p
             for y := 0 to fSubWidth - 1 do
             begin
                  Chol[y, y] := p[y];

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

constructor TDoubleMatrix.Create;
begin
     inherited Create;

     SetWidthHeight(1, 1);
end;

constructor TDoubleMatrix.Create(width, height: integer; const initVal : double);
var pData : PDouble;
    x, y : integer;
begin
     inherited Create;

     SetWidthHeight(width, height);

     if initVal <> 0 then
     begin
          for y := 0 to height - 1 do
          begin
               pData := StartElement;
               inc(PByte(pData), y*LineWidth);

               for x := 0 to Width - 1 do
               begin
                    pData^ := initVal;
                    inc(pData);
               end;
          end;
     end;
end;

function TDoubleMatrix.Determinant: double;
begin
     assert((Width > 0) and (Height = Width), 'Determinant only allowed on square matrices');
     Result := MatrixDeterminant(StartElement, LineWidth, fSubWidth);
end;

function TDoubleMatrix.SolveLinEQ(Value: TDoubleMatrix; numRefinements : integer) : TDoubleMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fSubWidth = fSubHeight) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := TDoubleMatrix.Create(Value.fSubWidth, fSubHeight);
     try
        if MatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement, Value.LineWidth, Result.StartElement,
                            Result.LineWidth, Value.fSubWidth, numRefinements, fLinEQProgress) = leSingular
     then
         raise ELinEQSingularException.Create('Matrix is singular');

     except
           FreeAndNil(Result);
           raise;
     end;
end;

function TDoubleMatrix.SolveLinEQ(Value: IMatrix;
  numRefinements: integer): TDoubleMatrix;
begin
     Result := SolveLinEQ(Value.GetObjRef, numRefinements);
end;

procedure TDoubleMatrix.SolveLinEQInPlace(Value: IMatrix;
  numRefinements: integer);
begin
     SolveLinEQInPlace(Value.GetObjRef, numRefinements);
end;
procedure TDoubleMatrix.SolveLinEQInPlace(Value: TDoubleMatrix; numRefinements : integer);
var dt : TDoubleMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fSubWidth = fSubHeight) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     dt := TDoubleMatrix.Create(Value.fSubWidth, fSubHeight);
     try
        if MatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement, Value.LineWidth, dt.StartElement,
                            dt.LineWidth, Value.fSubWidth, numRefinements, fLinEQProgress) = leSingular
        then
            raise ELinEQSingularException.Create('Matrix is singular');

        Assign(dt, False);
     finally
            dt.Free;
     end;
end;

function TDoubleMatrix.SQRT(const Value: double): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(Self, True);

     MatrixSQRT(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

procedure TDoubleMatrix.SQRTInPlace(const Value: double);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     MatrixSQRT(StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TDoubleMatrix.Eig(out EigVals: TDoublematrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    pReal, pImag : PDouble;
begin
     assert((fSubWidth > 0) and (fSubHeight = fSubWidth), 'Eigenvalues are only defined for square matrices');

     EigVals := nil;
     dt := TDoubleMatrix.Create(fSubWidth, 2);
     pReal := dt.StartElement;
     pImag := pReal;
     inc(pImag);
     Result := MatrixEigHessenberg(StartElement, LineWidth, fSubWidth, pReal, dt.LineWidth, pImag, dt.LineWidth);
     if Result = qlOk
     then
         EigVals := dt
     else
         dt.Free;
end;

function TDoubleMatrix.Eig(out EigVals, EigVect: TDoubleMatrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vec : TDoubleMatrix;
    pReal, pImag : PDouble;
begin
     assert((fSubWidth > 0) and (fSubHeight = fSubWidth), 'Eigenvalues are only defined for square matrices');

     EigVals := nil;
     EigVect := nil;
     dt := nil;
     vec := nil;
     try
        dt := TDoubleMatrix.Create(fSubWidth, 2);
        pReal := dt.StartElement;
        pImag := pReal;
        inc(pImag);

        vec := TDoubleMatrix.Create(fSubWidth, fSubHeight);
        Result := MatrixEigVecHessenbergInPlace(StartElement, LineWidth, fSubWidth, pReal, dt.LineWidth, pImag, dt.LineWidth,
                                                vec.StartElement, vec.LineWidth);
        if Result = qlOk then
        begin
             EigVals := dt;
             EigVect := vec;
        end
        else
        begin
             dt.Free;
             vec.Free;
        end;
     except
           dt.Free;
           vec.Free;

           raise;
     end;
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixObjFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixObjFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixMtxRefFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixMtxRefObjFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefObjFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementWiseMult(Value: TDoubleMatrix) : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     Result := TDoubleMatrix.Create;
     Result.Assign(self);
     MatrixElemMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.ElementWiseMultInPlace(Value: TDoubleMatrix);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     MatrixElemMult(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TDoubleMatrix.ElementwiseNorm2: double;
begin
     Result := 0;

     if (Width > 0) and (Height > 0) then
        Result := MatrixElementwiseNorm2(StartElement, LineWidth, Width, Height);
end;

function TDoubleMatrix.GetItems(x, y: integer): double;
var pData : PConstDoubleArr;
begin
     assert((x >= 0) and (x < fSubWidth) and (y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := fData;
     inc(PByte(pData), (fOffsetY + y)*fLineWidth);

     Result := pData[fOffsetX + x];
end;

function TDoubleMatrix.GetLinEQProgress: TLinEquProgress;
begin
     Result := fLinEQProgress;
end;

function TDoubleMatrix.GetObjRef: TDoubleMatrix;
begin
     Result := self;
end;

function TDoubleMatrix.GetSubHeight: integer;
begin
     Result := fSubHeight;
end;

function TDoubleMatrix.GetSubWidth: integer;
begin
     Result := fSubWidth;
end;

function TDoubleMatrix.Invert: TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     Result := TDoubleMatrix.Create;
     try
        Result.Assign(Self, True);

        if MatrixInverseInPlace(Result.StartElement, Result.LineWidth, fSubWidth, fLinEQProgress) = leSingular then
           raise ELinEQSingularException.Create('Singular matrix');
     except
           Result.Free;
           raise;
     end;
end;

function TDoubleMatrix.InvertInPlace: TLinEquResult;
var dt : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     dt := TDoubleMatrix.Create(Width, Height);
     try
        dt.Assign(self, True);
        Result := MatrixInverseInPlace(dt.StartElement, dt.LineWidth, fSubWidth, fLinEQProgress);
        if Result = leOk then
           Assign(dt);

        FreeAndNil(dt);
     except
           dt.Free;
           raise;
     end;
end;

function TDoubleMatrix.LineWidth: integer;
begin
     Result := fLineWidth;
end;

// set all values where mask = true to the new value
procedure TDoubleMatrix.MaskedSetValue(const Mask: array of boolean;
  const newVal: double);
var y : integer;
    x : integer;
    maskIdx : integer;
    pValue2 : PConstDoubleArr;
    pValue1 : PByte;
begin
     assert(fSubWidth*fSubHeight = Length(Mask), 'Error number of mask elements must be the same as the matrix dimension');
     pValue1 := PByte(StartElement);
     maskIdx := 0;
     for y := 0 to fsubHeight - 1 do
     begin
          pValue2 := PConstDoubleArr(pValue1);

          for x := 0 to fSubWidth - 1 do
          begin
               if not mask[maskidx] then
                  pValue2^[x] := newVal;

               inc(maskidx);
          end;
          inc(pValue1, LineWidth);
     end;
end;

function TDoubleMatrix.Max: double;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     Result := MatrixMax(StartElement, fSubWidth, fSubHeight, LineWidth);
end;

function TDoubleMatrix.Mean(RowWise: boolean): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := TDoubleMatrix.Create(1, fSubHeight);

          MatrixMean(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end
     else
     begin
          Result := TDoubleMatrix.Create(fSubWidth, 1);

          MatrixMean(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end;
end;

procedure TDoubleMatrix.MeanInPlace(RowWise: boolean);
var dl : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     dl := Mean(RowWise);
     try
        Assign(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.Min: double;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     Result := MatrixMin(StartElement, fSubWidth, fSubHeight, LineWidth);
end;

function TDoubleMatrix.Mult(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := TDoubleMatrix.Create(Value.fSubWidth, fSubHeight);

     MatrixMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.MultInPlace(Value: TDoubleMatrix);
var dl : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fOffsetX = 0) and (fOffsetY = 0) and (fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');
     assert((fSubWidth = Value.fSubHeight), 'Dimension error');

     dl := Mult(Value);
     try
        Assign(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.Scale(const Value: double): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := TDoubleMatrix.Create;
     Result.Assign(Self, True);

     MatrixAddAndScale(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, 0, Value);
end;

function TDoubleMatrix.ScaleAndAdd(const Offset, Scale: double): TDoubleMatrix;
begin
     assert((width > 0) and (Height > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     Result.Assign(self, True);

     Result.ScaleAndAddInPlace(Offset, Scale);
end;

procedure TDoubleMatrix.ScaleAndAddInPlace(const Offset, Scale: double);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     
     MatrixScaleAndAdd(StartElement, LineWidth, fSubWidth, fSubHeight, Offset, Scale);
end;

procedure TDoubleMatrix.ScaleInPlace(const Value: Double);
begin
     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, 0, Value);
end;

procedure TDoubleMatrix.Normalize(RowWise: boolean);
var dt : TDoubleMatrix;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     if RowWise
     then
         dt := TDoubleMatrix.Create(Width, 1)
     else
         dt := TDoubleMatrix.Create(1, Height);

     try
        MatrixNormalize(dt.StartElement, dt.LineWidth, StartElement, LineWidth, Width, Height, RowWise);
        Assign(dt);
     finally
            dt.Free;
     end;
end;

function TDoubleMatrix.PseudoInversion(out mtx : TDoubleMatrix) : TSVDResult;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(self);
        mtx.fLinEQProgress := fLinEQProgress;
        Result := mtx.PseudoInversionInPlace;
        mtx.fLinEQProgress := nil;
     except
           FreeAndNil(mtx);

           raise;
     end;
end;

function TDoubleMatrix.PseudoInversion(out Mtx: IMatrix): TSVDResult;
var outMtx : TDoubleMatrix;
begin
     Result := PseudoInversion(outMtx);
     Mtx := outMtx;
end;

function TDoubleMatrix.PseudoInversionInPlace: TSVDResult;
var mtx : TDoubleMatrix;
begin
     assert((width > 0) and (height > 0), 'Dimension error');

     mtx := TDoubleMatrix.Create(height, width);
     try
        Result := MatrixPseudoinverse(mtx.StartElement, mtx.LineWidth, StartElement, LineWidth, Width, Height, fLinEQProgress);

        if Result = srOk then
           Assign(mtx);
     finally
            mtx.Free;
     end;
end;

function TDoubleMatrix.QR(out R: IMatrix): TQRResult;
var outR : TDoubleMatrix;
begin
     Result := QR(outR);
     R := outR;
end;

function TDoubleMatrix.QR(out R: TDoubleMatrix): TQRResult;
var C : TDoubleDynArray;
    D : TDoubleDynArray;
    x, y : Integer;
begin
     assert((fSubWidth > 0) and (fSubWidth = fSubHeight), 'QE decomposition is only allowed on square matrices');
     R := TDoubleMatrix.Create;
     try
        R.Assign(self);

        SetLength(C, width);
        SetLength(D, width);
        Result := MatrixQRDecompInPlace(R.StartElement, R.LineWidth, Width, @C[0], sizeof(double), @D[0], sizeof(double), fLinEQProgress);

        for x := 0 to R.Width - 1 do
        begin
             R[x, x] := D[x];
             for y := x + 1 to R.Height - 1 do
                 R[x, y] := 0;
        end;
     except
           FreeAndNil(R);

           raise;
     end;
end;

procedure TDoubleMatrix.ReserveMem(width, height : integer);
var numLineElems : integer;
begin
     if Assigned(fMemory) then
        FreeMem(fMemory);

     fData := nil;
     fMemory := nil;
     fLineWidth := 0;
     // create an always aligned matrix
     if (width > 0) and (height > 0) then
     begin
          // take special care for vectores - there are optimized functions
          // for those types (e.g. vector matrix multiplication)
          if width > 1
          then
              numLineElems := width + width and $01
          else
              numLineElems := width;
          fMemory := AllocMem(16 + height*numLineElems*sizeof(double));
          fData := Pointer(NativeUInt(fMemory) + 16 - NativeUInt(fMemory) and $0F);

          fLineWidth := numLineElems*sizeof(double);
     end;
end;

function TDoubleMatrix.Reshape(newWidth, newHeight: integer) : TDoubleMatrix;
begin
     assert((fWidth > 0) and (fHeight > 0), 'Error operation not allowed on empty matrices');
     assert((newWidth*newHeight) = (fWidth*fHeight), 'Error new dimension does not fit into the old one');

     Result := TDoubleMatrix.Create;
     try
        Result.Assign(Self, True);
        Result.ReshapeInPlace(newWidth, newHeight);
     except
           FreeAndNil(Result);

           raise;
     end;
end;

procedure TDoubleMatrix.ReshapeInPlace(newWidth, newHeight: integer);
var oldData : PConstDoubleArr;
    oldOrigPtr : Pointer;
    numSkip : integer;
    pActLine : PConstDoubleArr;
    x, y, z : integer;
    actElemIdx : integer;
    i : Integer;
begin
     assert((fWidth > 0) and (fHeight > 0), 'Error operation not allowed on empty matrices');
     assert((fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');
     assert((newWidth*newHeight) = (fWidth*fHeight), 'Error new dimension does not fit into the old one');

     // check if the line width fits the old width -> then we only have to adjust the
     // line width parameter, otherwise copy. Note we take here into account that
     // the new line width could cause unaligned operations!
     if fLineWidth = fWidth*sizeof(double)
     then
         fLineWidth := newWidth*sizeof(double)
     else
     begin
          oldOrigPtr := fMemory;
          oldData := PConstDoubleArr(fData);

          fMemory := nil;
          fData := nil;

          numSkip := (fLineWidth - fWidth*sizeof(double)) div sizeof(double);
          ReserveMem(newWidth, newHeight);

          // now copy to new structure
          z := 0;
          actElemIdx := 0;
          for y := 0 to newHeight - 1 do
          begin
               pActLine := PConstDoubleArr(NativeUInt(fData) + NativeUInt(fLineWidth*y));

               for x := 0 to newWidth - 1 do
               begin
                    pActLine^[x] := oldData^[z];
                    inc(actElemIdx);
                    inc(z);

                    if actElemIdx = fWidth then
                    begin
                         for i := 0 to numSkip - 1 do
                             inc(z);

                         actElemIdx := 0;
                    end;
               end;
          end;
          FreeMem(oldOrigPtr);
     end;

     fWidth := newWidth;
     fHeight := newHeight;
     fSubWidth := newWidth;
     fSubHeight := newHeight;
     fOffsetX := 0;
     fOffsetY := 0;
end;

procedure TDoubleMatrix.SetColumn(col : integer; Values: TDoubleMatrix; ValCols : integer);
var pVal1 : PDouble;
    pVal2 : PDouble;
    y : integer;
begin
     assert(Values.fSubHeight = fSubHeight, 'Dimension error');
     assert((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

     pVal1 := StartElement;
     inc(pVal1, col);

     pVal2 := Values.StartElement;
     inc(pVal2, ValCols);

     for y := 0 to fSubHeight - 1 do
     begin
          pVal1^ := pVal2^;
          inc(PByte(pVal1), LineWidth);
          inc(PByte(pVal2), Values.LineWidth);
     end;
end;

procedure TDoubleMatrix.SetColumn(col: integer; Values: IMatrix;
  ValCols: integer);
begin
     SetColumn(col, values.GetObjRef, ValCols);
end;

procedure TDoubleMatrix.SetData(data: PDouble; srcLineWidth, width,
  height: integer);
begin
     ReserveMem(width, height);

     MatrixCopy(StartElement, LineWidth, data, srcLineWidth, width, height);
end;

procedure TDoubleMatrix.SetColumn(col : integer; const Values: array of Double);
var pVal1 : PDouble;
    y : integer;
begin
     assert(fSubHeight = Length(Values), 'Dimension error');
     assert((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

     pVal1 := StartElement;
     inc(pVal1, col);

     for y := 0 to fSubHeight - 1 do
     begin
          pVal1^ := Values[y];
          inc(PByte(pVal1), LineWidth);
     end;
end;

procedure TDoubleMatrix.SetHeight(const Value: integer);
begin
     SetWidthHeight(fWidth, Value);
end;

procedure TDoubleMatrix.SetItems(x, y: integer; const Value: double);
var pData : PConstDoubleArr;
begin
     assert((x >= 0) and (x < fSubWidth), 'Dimension error');
     assert((y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := fData;
     inc(PByte(pData), (fOffsetY + y)*fLineWidth);

     pData^[fOffsetX + x] := Value;
end;

procedure TDoubleMatrix.SetLinEQProgress(value: TLinEquProgress);
begin
     fLinEQProgress := value;
end;

procedure TDoubleMatrix.SetRow(row : integer; Values: TDoubleMatrix; ValRow : integer);
var pVal1 : PDouble;
    pVal2 : PDouble;
begin
     assert(Values.Width = fSubWidth, 'Dimension Error');
     assert((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     pVal1 := StartElement;
     inc(PByte(pVal1), row*LineWidth);

     pVal2 := Values.StartElement;
     inc(PByte(pVal2), Values.LineWidth*ValRow);

     Move(pVal2^, pVal1^, fSubWidth*sizeof(double));
end;

procedure TDoubleMatrix.SetRow(row : integer; const Values: array of Double);
var pVal1 : PDouble;
begin
     assert(Length(Values) = fSubWidth, 'Dimension Error');
     assert((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     pVal1 := StartElement;
     inc(PByte(pVal1), row*LineWidth);

     Move(Values[0], pVal1^, fSubWidth*sizeof(double));
end;

procedure TDoubleMatrix.SetSubMatrix(x, y, Subwidth, Subheight: integer);
begin
     assert((x >= 0) and (x + SubWidth <= fWidth), 'Dimension x error');
     assert((y >= 0) and (y + SubHeight <= fHeight), 'Dimension y error');

     fOffsetX := x;
     fOffsetY := y;
     fSubWidth := Subwidth;
     fSubHeight := Subheight;
end;

procedure TDoubleMatrix.SetWidth(const Value: integer);
begin
     SetWidthHeight(Value, fHeight);
end;

procedure TDoubleMatrix.SetWidthHeight(const Width, Height: integer; AssignMem : boolean);
begin
     assert((Width > 0) and (Height > 0), 'Dimension error');

     if AssignMem
     then
         ReserveMem(Width, Height)
     else
         ReserveMem(0, 0);

     fWidth := Width;
     fHeight := Height;
     fSubWidth := Width;
     fSubHeight := Height;
end;

function TDoubleMatrix.StartElement: PDouble;
begin
     if (fWidth <> 0) and (fHeight <> 0)
     then
         Result := PDouble(NativeUInt(fData) + NativeUInt(sizeof(double)*fOffsetX + fOffsetY*fLineWidth))
     else
         Result := nil;
end;

function TDoubleMatrix.Sub(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := TDoubleMatrix.Create(fSubWidth, fSubHeight);
     MatrixSub(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.SubInPlace(Value: TDoubleMatrix);
begin
     assert((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');
     MatrixSub(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TDoubleMatrix.SubMatrix: TDoubleDynArray;
var pRes : PDouble;
    pVal : PDouble;
    i : integer;
begin
     Result := nil;
     if fSubWidth*fSubHeight = 0 then
        exit;

     SetLength(Result, fSubWidth*fSubHeight);
     pVal := StartElement;
     pRes := @Result[0];
     for i := 0 to fSubHeight - 1 do
     begin
          Move(pVal^, pRes^, sizeof(double)*fSubWidth);
          inc(PByte(pVal), LineWidth);
          inc(pRes, fSubWidth);
     end;
end;

function TDoubleMatrix.Sum(RowWise: boolean): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := TDoubleMatrix.Create(1, fSubHeight);

          MatrixSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end
     else
     begin
          Result := TDoubleMatrix.Create(fSubWidth, 1);

          MatrixSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end;
end;

procedure TDoubleMatrix.SumInPlace(RowWise: boolean);
var dl : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     dl := Sum(RowWise);
     try
        Assign(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.SVD(out U, V, W: IMatrix; onlyDiagElements: boolean): TSVDResult;
var uObj, vObj, wObj : TDoubleMatrix;
begin
     Result := SVD(uObj, vObj, wObj, onlyDiagElements);
     U := uObj;
     V := vObj;
     W := wObj;
end;

function TDoubleMatrix.SVD(out U, V, W: TDoubleMatrix; onlyDiagElements : boolean): TSVDResult;
var dim : integer;
begin
     Assert((Width > 0) and (Height > 0), 'Dimension error');

     dim := Math.Min(fSubWidth, fSubHeight);
     U := nil;
     V := nil;
     W := Nil;
     try
        U := TDoubleMatrix.Create(Dim, fSubHeight);
        V := TDoubleMatrix.Create(fSubWidth, fSubWidth);
        if onlyDiagElements
        then
            W := TDoubleMatrix.Create(1, fSubWidth)
        else
            W := TDoubleMatrix.Create(fSubWidth, fSubWidth);

        // create three matrices -> Matrix W is a diagonal matrix with the singular values stored in the diagonal
        Result := MatrixSVD(StartElement, LineWidth, fSubWidth, fSubHeight, U.StartElement, U.LineWidth, W.StartElement, W.LineWidth, V.StartElement, V.LineWidth, fLinEQProgress);

        if Result <> srOk then
        begin
             FreeAndNil(u);
             FreeAndNil(V);
             FreeAndNil(W);
        end;

     except
           FreeAndNil(u);
           FreeAndNil(V);
           FreeAndNil(W);
           
           raise;
     end;
end;

function TDoubleMatrix.SymEig(out EigVals: TDoubleMatrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vec : TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Eigenvalue calculation only allowed on square matrices');

     EigVals := nil;

     dt := TDoubleMatrix.Create(fSubWidth, 1);
     vec := TDoubleMatrix.Create(fSubWidth, fSubWidth);
     try
        Result := MatrixEigTridiagonalMatrix(vec.StartElement, vec.LineWidth, StartElement, LineWidth, fSubWidth, dt.StartElement, dt.LineWidth);
        if Result = qlOk then
        begin
             EigVals := dt;
             vec.Free;
        end
        else
        begin
             dt.Free;
             vec.Free;
        end;
     except
           FreeAndNil(dt);
           FreeAndNil(vec);
           raise;
     end;
end;

function TDoubleMatrix.SymEig(out EigVals,
  EigVect: TDoubleMatrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vec : TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Eigenvalue calculation only allowed on square matrices');

     EigVals := nil;
     EigVect := nil;

     dt := TDoubleMatrix.Create(fSubWidth, 1);
     vec := TDoubleMatrix.Create(fSubWidth, fSubWidth);
     try
        Result := MatrixEigTridiagonalMatrix(vec.StartElement, vec.LineWidth, StartElement, LineWidth, fSubWidth, dt.StartElement, dt.LineWidth);
        if Result = qlOk then
        begin
             EigVals := dt;
             EigVect := vec;
        end
        else
        begin
             dt.Free;
             vec.Free;
        end;
     except
           FreeAndNil(dt);
           FreeAndNil(vec);

           raise;
     end;
end;

function TDoubleMatrix.Transpose : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := TDoubleMatrix.Create(fSubHeight, fSubWidth);
     MatrixTranspose(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight);
end;

procedure TDoubleMatrix.TransposeInPlace;
var dt : TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     // transpose is only allowed on full matrix
     assert((fOffsetX = 0) and (fOffsetY = 0) and (fSubWidth = fWidth) and (fSubHeight = fHeight), 'Operation only allowed on full matrices');

     dt := Transpose;
     try
        Assign(dt);
     finally
            dt.Free;
     end;
end;

procedure TDoubleMatrix.UseFullMatrix;
begin
     fOffsetX := 0;
     fOffsetY := 0;
     fSubWidth := fWidth;
     fSubHeight := fHeight;
end;

// ###########################################################
// #### Persistence functions
procedure TDoubleMatrix.DefineProps;
var origSubWidth, origSubHeight, origOffsetX, origOffsetY : integer;
begin
     AddStringProperty('Name', fName);
     AddIntProperty('Width', fWidth);
     AddIntProperty('Height', fHeight);

     origSubWidth := fSubWidth;
     origSubHeight := fSubHeight;
     origOffsetX := fOffsetX;
     origOffsetY := fOffsetY;

     UseFullMatrix;
     AddDoubleArr('data', SubMatrix);
     SetSubMatrix(origOffsetX, origOffsetY, origSubWidth, origSubHeight);

     // note: add the offsets after we data -> this ensures that variables are not ovewritten
     AddIntProperty('SubWidth', fSubWidth);
     AddIntProperty('SubHeight', fSubHeight);
     AddIntProperty('OffsetX', fOffsetX);
     AddIntProperty('OffsetY', fOffsetY);
end;

destructor TDoubleMatrix.Destroy;
begin
     Clear;
     
     inherited;
end;

procedure TDoubleMatrix.OnLoadDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     if Length(Value) > 0 then
     begin
          assert(Length(value) = fWidth*fHeight);
          Assign(Value, fWidth, fHeight);
     end;
end;

procedure TDoubleMatrix.OnLoadStringProperty(const Name, Value: String);
begin
     if CompareText(Name, 'Name') = 0
     then
         fName := Value
     else
         inherited;
end;

procedure TDoubleMatrix.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     if CompareText(Name, 'Width') = 0
     then
         fWidth := Value
     else if CompareText(Name, 'Height') = 0
     then
         fHeight := Value
     else if CompareText(Name, 'SubWidth') = 0
     then
         fSubWidth := Value
     else if CompareText(Name, 'SubHeight') = 0
     then
         fSubHeight := Value
     else if CompareText(Name, 'OffsetX') = 0
     then
         fOffsetX := Value
     else if CompareText(Name, 'OffsetY') = 0
     then
         fOffsetY := Value
     else
end;

function TDoubleMatrix.Cholesky(out Chol: IMatrix): TCholeskyResult;
var outChol : TDoubleMatrix;
begin
     Result := Cholesky(outChol);
     Chol := outChol;
end;

class function TDoubleMatrix.ClassIdentifier: String;
begin
     Result := 'MTX';
end;

procedure TDoubleMatrix.Clear;
begin
     ReserveMem(0, 0);
     
     fWidth := 0;
     fHeight := 0;
     fSubWidth := 0;
     fSubHeight := 0;
end;

function TDoubleMatrix.Clone: IMatrix;
begin
     Result := TDoubleMatrix.Create(Width, Height);
     Result.Assign(Self);
end;

constructor TDoubleMatrix.Create(data: PDouble; LineWidth, width,
  height: integer);
begin
     inherited Create;

     fWidth := width;
     fHeight := Height;
     fMemory := data;
     fData := PConstDoubleArr(data);
     fLineWidth := LineWidth;

     fOffsetX := 0;
     fOffsetY := 0;
     fSubWidth := fWidth;
     fSubHeight := fHeight;
     assert(width*sizeof(double) <= LineWidth, 'Dimension error');
end;

procedure TDoubleMatrix.SetRow(row: integer; Values: IMatrix; ValRow: integer);
begin
     SetRow(row, Values.GetObjRef, ValRow);
end;

function TDoubleMatrix.ElementWiseMult(Value: IMatrix): TDoubleMatrix;
begin
     Result := ElementWiseMult(Value.GetObjRef);
end;

procedure TDoubleMatrix.ElementWiseMultInPlace(Value: IMatrix);
begin
     ElementWiseMultInPlace(Value.GetObjRef);
end;

function TDoubleMatrix.Mult(Value: IMatrix): TDoubleMatrix;
begin
     Result := Mult(Value.GetObjRef);
end;

procedure TDoubleMatrix.MultInPlace(Value: IMatrix);
begin
     MultInPlace(Value.GetObjRef);
end;

function TDoubleMatrix.Sub(Value: IMatrix): TDoubleMatrix;
begin
     Result := Sub(Value.GetObjRef);
end;

procedure TDoubleMatrix.SubInPlace(Value: IMatrix);
begin
     SubInPlace(Value.GetObjRef);
end;

function TDoubleMatrix.SymEig(out EigVals, EigVect: IMatrix): TEigenvalueConvergence;
var outEigVals, outEigVect : TDoubleMatrix;
begin
     Result := SymEig(outEigVals, outEigVect);
     EigVals := outEigVals;
     EigVect := outEigVect;
end;

function TDoubleMatrix.SymEig(out EigVals: IMatrix): TEigenvalueConvergence;
var outEigVals : TDoubleMatrix;
begin
     Result := SymEig(outEigVals);
     EigVals := outEigVals;
end;

function TDoubleMatrix.Eig(out EigVals, EigVect: IMatrix): TEigenvalueConvergence;
var outEigVals, outEigVect : TDoubleMatrix;
begin
     Result := Eig(outEigVals, outEigVect);
     EigVals := outEigVals;
     EigVect := outEigVect;
end;

function TDoubleMatrix.Eig(out EigVals: IMatrix): TEigenvalueConvergence;
var outEigVals : TDoubleMatrix;
begin
     Result := Eig(outEigVals);
     EigVals := outEigVals;
end;

constructor TDoubleMatrix.Create(const Data: TDoubleDynArray; width,
  height: integer);
begin
     inherited Create;

     Assign(Data, Width, height);
end;

initialization
   RegisterMathIO(TDoubleMatrix);

end.
