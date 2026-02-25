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


unit DblMatrix;

// ############################################
// #### Base matrix operations
// ############################################

interface

uses SysUtils, Classes, Types, MatrixConst, Matrix,
     BaseMathPersistence, RandomEng;

{$I 'mrMath_CPU.inc'}

{$IFDEF FPC}
   {.$DEFINE ANONMETHODS}
{$ELSE}
   {$IF CompilerVersion >= 20.0}
      {$DEFINE ANONMETHODS}
   {$IFEND}
{$ENDIF}

type
  TDoubleMatrix = class;
  IDoubleMatrix = interface(IMatrix)
    ['{8B76CD12-4314-41EB-BD98-302A024AA0EC}']
    function SubMatrix : TDoubleDynArray;

    procedure SetRow(row : integer; const Values : Array of Double); overload;
    procedure SetColumn(col : integer; const Values : Array of Double); overload;

    procedure SetValue(const initVal : double); overload;
    procedure SetValueSubMtx(const initVal : double; x, y, aWidth, aHeight : NativeInt); overload;  // sets the value in a subsection of the matrix

    // ###################################################
    // #### Simple matrix utility functions
    function Max : double;
    function Min : double;
    function SumAll : double; overload;

    function Trace : double;

    // ###########################################
    // #### Reintroduced functions for double matrices
    function Mult(Value : IDoubleMatrix) : IDoubleMatrix;
    procedure MultInPlace(Value : IDoubleMatrix);
    function MultT1(Value : IDoubleMatrix) : IDoubleMatrix;
    function MultT2(Value : IDoubleMatrix) : IDoubleMatrix;
    procedure MultInPlaceT1(Value : IDoubleMatrix);
    procedure MultInPlaceT2(Value : IDoubleMatrix);

    function Add(Value : IDoubleMatrix) : IDoubleMatrix;
    procedure AddInplace(Value: IDoubleMatrix);
    function AddVec(aVec : IDoubleMatrix; rowWise : Boolean) : IDoubleMatrix;
    procedure AddVecInPlace(Value: IDoubleMatrix; rowWise: Boolean);
    procedure SubInplace(Value: IDoubleMatrix);
    function Sub(Value : IDoubleMatrix) : IDoubleMatrix;
    function SubVec(aVec : IDoubleMatrix; rowWise : Boolean) : IDoubleMatrix;
    procedure SubVecInPlace(Value: IDoubleMatrix; rowWise: Boolean);
    function Transpose : IDoubleMatrix;
    function ElementWiseMult(Value : IDoubleMatrix) : IDoubleMatrix;
    function ElementWiseDiv(Value : IDoubleMatrix) : IDoubleMatrix;
    function AddAndScale(const Offset, Scale : double) : IDoubleMatrix;
    function AddConst(const Value : double) : IDoubleMatrix;
    function Scale(const Value : double) : IDoubleMatrix;
    function SQRT : IDoubleMatrix;
    function ScaleAndAdd(const aOffset, aScale : double) : IDoubleMatrix;
    function Mean(RowWise : boolean) : IDoubleMatrix;
    function Variance(RowWise : boolean; unbiased : boolean = True) : IDoubleMatrix;
    function Std(RowWise : boolean; unbiased : boolean = True) : IDoubleMatrix;
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : IDoubleMatrix;
    procedure Assign(Value : IDoubleMatrix); overload;
    procedure AssignEx(Value : IDoubleMatrix; OnlySubElements : boolean); overload;
    procedure AssignSubMatrix(Value : IDoubleMatrix; X : integer = 0; Y : integer = 0);
    procedure Append(Value : IDoubleMatrix; appendColumns : boolean); overload;
    function Reshape(newWidth, newHeight : integer; RowMajor : boolean = False) : IDoubleMatrix;
    function Sum(RowWise : boolean) : IDoubleMatrix; overload;
    procedure TakeOver(Value : IDoubleMatrix);
    function Clone : IDoubleMatrix;
    function Diff(RowWise : boolean) : IDoubleMatrix;
    function RepeatMatrix(numX, numY : integer) : IDoubleMatrix;
    function AsVector( ColMajor : boolean = False ) : IDoubleMatrix;
    function SubColMtxIdx( colIdx : TIntegerDynArray ) : IDoubleMatrix; overload;
    function SubRowMtxIdx( rowIdx : TIntegerDynArray ) : IDoubleMatrix; overload;
    function SubMtxIdx( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : IDoubleMatrix; overload;
    function SubColMtx( fromIdx, toIdx : integer ) : IDoubleMatrix; overload;
    function SubRowMtx( fromIdx, toIdx : integer ) : IDoubleMatrix; overload;
    function SubMtx( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : IDoubleMatrix; overload;
    function Median(RowWise : boolean) : IDoubleMatrix;
    function RollMedian(RowWise : boolean; order : integer) : IDoubleMatrix;

    procedure ElementWiseMultInPlace(Value : IDoubleMatrix);
    procedure ElementWiseDivInPlace(Value : IDoubleMatrix);

    procedure AddAndScaleInPlace(const Offset, Scale: double);

    // ###################################################
    // #### Base Matrix operations that extend the base class
    procedure ElementwiseFuncInPlace(func : TMatrixFunc); overload;
    function ElementwiseFunc(func : TMatrixFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixObjFunc); overload;
    function ElementwiseFunc(func : TMatrixObjFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFunc); overload;
    function ElementwiseFunc(func : TMatrixMtxRefFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefObjFunc); overload;
    function ElementwiseFunc(func : TMatrixMtxRefObjFunc) : TDoubleMatrix; overload;

    {$IFDEF ANONMETHODS}
    procedure ElementwiseFuncInPlace(func : TMatrixFuncRef); overload;
    function ElementwiseFunc(func : TMatrixFuncRef) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFuncRef); overload;
    function ElementwiseFunc(func : TMatrixMtxRefFuncRef) : TDoubleMatrix; overload;
    {$ENDIF}

    // ###################################################
    // #### Linear System solver A*x = B -> use A.SolveLinEQ(B) -> x
    procedure SolveLinEQInPlace(Value: IDoubleMatrix; numRefinements : integer = 0);

    function Invert : IDoubleMatrix; overload;
    function InvertEx( out InvMtx : IDoubleMatrix ) : TLinEquResult; overload;

    // solves least sqaures A*x = b using the QR decomposition
    procedure SolveLeastSquaresInPlace(out x : IDoubleMatrix; b : IDoubleMatrix); overload;
    procedure SolveLeastSquares(out x : IDoubleMatrix; b : IDoubleMatrix); overload;

    function PseudoInversionInPlace : TSVDResult;
    function PseudoInversion(out Mtx : IDoubleMatrix) : TSVDResult; overload;
    function Determinant : double;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : double);

    // ###################################################
    // #### Matrix transformations
    function SVD(out U, V, W : IDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult;

    // the symmetric eigenvalue calculation actually references only the upper part of the matrix
    // -> therefore symmetry does not need to be maintained explicitly
    function SymEig(out EigVals : IDoubleMatrix; out EigVect : IDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IDoubleMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IDoubleMatrix; out EigVect : IDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IDoubleMatrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix; out EigVect : IDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone) : TEigenvalueConvergence; overload;
    function Cholesky(out Chol : IDoubleMatrix) : TCholeskyResult;

    // only internaly used -> use the other QR or QRFull methods instead
    //function QR(out R : TDoubleMatrix; out tau : TDoubleMatrix) : TQRResult; overload;
    //function QR(out R : IDoubleMatrix; out tau : IDoubleMatrix) : TQRResult; overload;
    procedure QR(out R : IDoubleMatrix);
    procedure QRFull(out Q, R : IDoubleMatrix);

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(const Mtx : Array of double; W, H : integer); overload;
    procedure AssignDyn(Value : TDoubleDynArray; W, H : integer);
  end;

// #################################################
// #### Builds base matrix operations
  TDoubleMatrixClass = class of TDoubleMatrix;
  TDoubleMatrix = class(TBaseMatrix, IDoubleMatrix)
  private
    type
      TLocConstDoubleArr = Array[0..MaxInt div sizeof(double) - 1] of double;
      PLocConstDoubleArr = ^TLocConstDoubleArr;
  protected
    // used to determine which class type to use as a result (for the real matrices!)
    // e.g. the threaded class does not override all standard functions but
    // the resulting class type shall always be the threaded class!
    class function ResultClass : TDoubleMatrixClass; virtual;
    class function NumberType : TNumberType; override;

    function GetItems(x, y: integer): double; override;
    procedure SetItems(x, y: integer; const Value: double); override;
    // Access as vector -> same as GetItem(idx mod width, idx div width)
    function GetVecItem(idx: integer): double; override;
    procedure SetVecItem(idx: integer; const Value: double); override;

    // matrix persistence functions
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    class function ClassIdentifier : String; override;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); override;

    class function GetItemSize : integer; override;
  private
    fObj : TObject;                // arbitrary object

    procedure MtxRandWithEng(var value : double);
    procedure MtxRand(var value : double);
  protected
    // ###########################################
    // #### Reintroduced functions
    function IDoubleMatrix.Mult = MultD;
    function IDoubleMatrix.Transpose = TransposeD;
    function IDoubleMatrix.Add = AddD;
    procedure IDoubleMatrix.AddInPlace = AddInPlaceD;
    function IDoubleMatrix.Sub = SubD;
    procedure IDoubleMatrix.SubInplace = SubInPlaceD;
    procedure IDoubleMatrix.SubVecInPlace = SubVecInPlaceD;
    function IDoubleMatrix.MultT1 = MultT1D;
    procedure IDoubleMatrix.MultInPlaceT1 = MultInPlaceT1D;
    procedure IDoubleMatrix.MultInPlace = MultInPlaceD;
    function IDoubleMatrix.AddVec = AddVecD;
    procedure IDoubleMatrix.AddVecInPlace = AddVecInPlaceD;
    function IDoubleMatrix.SubVec = SubVecD;
    function IDoubleMatrix.ElementWiseMult = ElementWiseMultD;
    function IDoubleMatrix.ElementWiseDiv = ElementWiseDivD;
    function IDoubleMatrix.AddConst = AddConstD;
    function IDoubleMatrix.Scale = ScaleD;
    function IDoubleMatrix.SQRT = SQRTD;
    function IDoubleMatrix.ScaleAndAdd = ScaleAndAddD;
    function IDoubleMatrix.AddAndScale = AddAndScaleD;
    function IDoubleMatrix.Mean = MeanD;
    function IDoubleMatrix.Variance = VarianceD;
    function IDoubleMatrix.Std = StdD;
    function IDoubleMatrix.MeanVariance = MeanVarianceD;
    function IDoubleMatrix.MultT2 = MultT2D;
    procedure IDoubleMatrix.MultInPlaceT2 = MultInPlaceT2D;
    procedure IDoubleMatrix.Assign = AssignD;
    procedure IDoubleMatrix.AssignEx = AssignExD;
    procedure IDoubleMatrix.AssignSubMatrix = AssignSubMatrixD;
    procedure IDoubleMatrix.Append = AppendD;
    function IDoubleMatrix.Reshape = ReshapeD;
    function IDoubleMatrix.Invert = InvertD;
    function IDoubleMatrix.InvertEx = InvertExD;
    function IDoubleMatrix.Sum = SumD;
    procedure IDoubleMatrix.TakeOver = TakeOverD;
    function IDoubleMatrix.Clone = CloneD;
    function IDoubleMatrix.Diff = DiffD;
    function IDoubleMatrix.RepeatMatrix = RepeatMatrixD;
    function IDoubleMatrix.AsVector = AsVectorD;
    function IDoubleMatrix.SubColMtxIdx = SubColMtxIdxD;
    function IDoubleMatrix.SubRowMtxIdx = SubRowMtxIdxD;
    function IDoubleMatrix.SubMtxIdx = SubMtxIdxD;
    function IDoubleMatrix.SubColMtx = SubColMtxD;
    function IDoubleMatrix.SubRowMtx = SubRowMtxD;
    function IDoubleMatrix.SubMtx = SubMtxD;
    function IDoubleMatrix.Median = MedianD;
    function IDoubleMatrix.RollMedian = RollMedianD;
    procedure IDoubleMatrix.ElementWiseMultInPlace = ElementWiseMultInPlaceD;
    procedure IDoubleMatrix.ElementWiseDivInPlace = ElementWiseDivInPlaceD;
    procedure IDoubleMatrix.AddAndScaleInPlace = AddAndScaleInPlace;
    procedure IDoubleMatrix.SolveLinEQInPlace = SolveLinEQInPlaceD;

    function MultD(Value : IDoubleMatrix) : IDoubleMatrix;
    procedure MultInPlaceD(Value : IDoubleMatrix);
    procedure MultInPlaceT1D(Value : IDoubleMatrix);
    function TransposeD : IDoubleMatrix;
    function AddD(Value : IDoubleMatrix) : IDoubleMatrix;
    procedure AddInplaceD(Value: IDoubleMatrix);
    function SubD(Value : IDoubleMatrix) : IDoubleMatrix;
    function MultT1D(Value : IDoubleMatrix) : IDoubleMatrix;
    function AddVecD(aVec : IDoubleMatrix; rowWise : Boolean) : IDoubleMatrix;
    procedure AddVecInPlaceD(Value: IDoubleMatrix; rowWise: Boolean);
    function SubVecD(aVec : IDoubleMatrix; rowWise : Boolean) : IDoubleMatrix;
    procedure SubVecInPlaceD(Value: IDoubleMatrix; rowWise: Boolean);
    function ElementWiseMultD(Value : IDoubleMatrix) : IDoubleMatrix;
    function ElementWiseDivD(Value : IDoubleMatrix) : IDoubleMatrix;
    function AddConstD(const Value : double) : IDoubleMatrix;
    function ScaleD(const Value : double) : IDoubleMatrix;
    function SQRTD : IDoubleMatrix;
    function ScaleAndAddD(const aOffset, aScale : double) : IDoubleMatrix;
    function AddAndScaleD(const Offset, Scale : double) : IDoubleMatrix;
    function MeanD(RowWise : boolean) : IDoubleMatrix;
    function VarianceD(RowWise : boolean; unbiased : boolean = True) : IDoubleMatrix;
    function StdD(RowWise : boolean; unbiased : boolean = True) : IDoubleMatrix;
    function MeanVarianceD(RowWise : boolean; unbiased : boolean = True) : IDoubleMatrix;
    function MultT2D(Value : IDoubleMatrix) : IDoubleMatrix;
    procedure AssignD(Value : IDoubleMatrix); overload;
    procedure AssignExD(Value : IDoubleMatrix; OnlySubElements : boolean); overload;
    procedure AssignSubMatrixD(Value : IDoubleMatrix; X : integer = 0; Y : integer = 0);
    procedure AppendD(Value : IDoubleMatrix; appendColumns : boolean); overload;
    function ReshapeD(newWidth, newHeight : integer; RowMajor : boolean = False) : IDoubleMatrix;
    function InvertD : IDoubleMatrix; overload;
    function InvertExD( out InvMtx : IDoubleMatrix ) : TLinEquResult; overload;
    function SumD(RowWise : boolean) : IDoubleMatrix; overload;
    procedure TakeOverD(Value : IDoubleMatrix);
    function CloneD : IDoubleMatrix;
    function DiffD(RowWise : boolean) : IDoubleMatrix;
    function RepeatMatrixD(numX, numY : integer) : IDoubleMatrix;
    function AsVectorD( ColMajor : boolean = False ) : IDoubleMatrix;
    function SubColMtxIdxD( colIdx : TIntegerDynArray ) : IDoubleMatrix; overload;
    function SubRowMtxIdxD( rowIdx : TIntegerDynArray ) : IDoubleMatrix; overload;
    function SubMtxIdxD( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : IDoubleMatrix; overload;
    function SubColMtxD( fromIdx, toIdx : integer ) : IDoubleMatrix; overload;
    function SubRowMtxD( fromIdx, toIdx : integer ) : IDoubleMatrix; overload;
    function SubMtxD( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : IDoubleMatrix; overload;
    function MedianD(RowWise : boolean) : IDoubleMatrix;
    function RollMedianD(RowWise : boolean; order : integer) : IDoubleMatrix;
    procedure SolveLinEQInPlaceD(Value: IDoubleMatrix; numRefinements : integer);


    // ###########################################
    // #### Interface methods - overrides from the base class that use the interface
    procedure SetColumnI(col : integer; Values : IMatrix; ValCols : integer = 0); overload; override;
    procedure SetRowI(row : integer; Values : IMatrix; ValRow : integer = 0); overload; override;

    function SubColMtxIdxI( colIdx : TIntegerDynArray ) : IMatrix; overload; override;
    function SubRowMtxIdxI( rowIdx : TIntegerDynArray ) : IMatrix; overload; override;
    function SubMtxIdxI( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : IMatrix; overload; override;
    function SubColMtxI( fromIdx, toIdx : integer ) : IMatrix; overload; override;
    function SubRowMtxI( fromIdx, toIdx : integer ) : IMatrix; overload; override;
    function SubMtxI( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : IMatrix; overload; override;

    function ReshapeI(newWidth, newHeight : integer; RowMajor : boolean = False) : IMatrix; override;
    function AsVectorI( ColMajor : boolean = False ) : IMatrix; override;

    // ###################################################
    // #### Simple matrix utility functions
    function AbsI : IMatrix; overload; override;
    function DiagI(createDiagMtx : boolean) : IMatrix; overload; override;
    function DiagFromOffsetI(K : integer) : IMatrix; overload; override; // returns the diagonal on o ffset k

    // ###################################################
    // #### Base Matrix operations
    function TransposeI : IMatrix; overload; override;
    function SQRTI : IMatrix; override;

    procedure SubInPlaceD(Value : IDoubleMatrix);
    procedure MultInPlaceT2D(Value : IDoubleMatrix);

    procedure ElementWiseMultInPlaceD(Value : IDoubleMatrix);
    procedure ElementWiseDivInPlaceD(Value : IDoubleMatrix);

    function MeanI(RowWise : boolean) : IMatrix; override;
    function VarianceI(RowWise : boolean; unbiased : boolean = True) : IMatrix; override;
    function StdI(RowWise : boolean; unbiased : boolean = True) : IMatrix; override;

    // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVarianceI(RowWise : boolean; unbiased : boolean = True) : IMatrix; override;

    function MedianI(RowWise : boolean) : IMatrix; override;
    function RollMedianI(RowWise : boolean; order : integer) : IMatrix; override;

    function SortI(RowWise : boolean) : IMatrix; override;

    function DiffI(RowWise : boolean) : IMatrix; override;
    function SumI(RowWise : boolean) : IMatrix; overload; override;
    function CumulativeSumI(RowWise : boolean) : IMatrix; override;

    function AddConstI(const Value : double) : IMatrix; overload; override;
    function ScaleI(const Value : double) : IMatrix; override;
    function ScaleAndAddI(const aOffset, aScale : double) : IMatrix; override;

    // matrix inversion (based on LU decomposition)
    function InvertI : IMatrix; overload; override;
    function InvertExI( out InvMtx : IMatrix ) : TLinEquResult; overload; override;

    // moves data from Value to self and clears original object
    function CloneI : IMatrix; override;

    // ###################################################
    // #### Special functions
    function RepeatMatrixI(numX, numY : integer) : IMatrix; override;

    function EigValsMtxToCplxOrDouble( mtx : TDoubleMatrix ) : IMatrix;

  public
    // general access
    property Items[x, y : integer] : double read GetItems write SetItems; default;
    property Vec[idx : integer] : double read GetVecItem write SetVecItem; // matrix as vector
    function SubMatrix : TDoubleDynArray;

    procedure InitRandom(method : TRandomAlgorithm; var seed : LongInt); override;

    function Reshape(newWidth, newHeight : integer; ColMajor : boolean = False) : TDoubleMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer; ColMajor : boolean = False); override;

    function AsVector(ColMajor: boolean = False) :  TDoubleMatrix;

    function GetType : TMatrixType; override;

    procedure SetRow(row : integer; const Values : Array of Double); overload;
    procedure SetRow(row : integer; Values : TDoubleMatrix; ValRow : integer = 0); overload;
    procedure SetColumn(col : integer; const Values : Array of Double); overload;
    procedure SetColumn(col : integer; Values : TDoubleMatrix; ValCols : integer = 0); overload;

    procedure SetValue(const initVal : double); overload;
    procedure SetValueSubMtx(const initVal : double; x, y, aWidth, aHeight : NativeInt); overload;  // sets the value in a subsection of the matrix

    procedure SetData(data : PDouble; srcLineWidth, width, height : integer);

    procedure SwapRow( idx1, idx2 : integer ); override;
    procedure SwapColumn( idx1, idx2 : integer ); override;

    procedure Normalize(RowWise : boolean); override;
    procedure NormZeroMeanVarOne(RowWise : boolean); override;
    function ElementwiseNorm2(doSqrt : boolean = True) : double; override;

    function AddConst(const Value : double) : TDoubleMatrix; overload;
    procedure AddAndScaleInPlace(const Offset, Scale : double); override;

    function SubColMtxIdx( colIdx : TIntegerDynArray ) : TDoubleMatrix; overload;
    function SubRowMtxIdx( rowIdx : TIntegerDynArray ) : TDoubleMatrix; overload;
    function SubMtxIdx( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : TDoubleMatrix; overload;
    function SubColMtx( fromIdx, toIdx : integer ) : TDoubleMatrix; overload;
    function SubRowMtx( fromIdx, toIdx : integer ) : TDoubleMatrix; overload;
    function SubMtx( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : TDoubleMatrix; overload;

    procedure RepeatMatrixInPlace(numX, numY : integer); override;

    // ###################################################
    // #### Simple matrix utility functions
    function Max : double;
    function Min : double;
    function SumAll : double; overload;

    procedure AbsInPlace; override;
    procedure DiagInPlace(createDiagMtx : boolean); override;

    function Abs: TDoubleMatrix; overload;

    function Diag(createDiagMtx : boolean) : TDoubleMatrix; overload;
    function DiagFromOffset(K : integer) : TDoubleMatrix; overload; // returns the diagonal on offset k
    function Trace : double;

    // ###################################################
    // #### Base Matrix operations
    function Transpose : TDoubleMatrix; overload;
    procedure TransposeInPlace; override;

    procedure AddInplace(Value : TDoubleMatrix); overload; virtual;
    function Add(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure AddVecInPlace(Value : TDoubleMatrix; rowWise : Boolean); overload;
    function AddVec(aVec : TDoubleMatrix; rowWise : Boolean) : TDoubleMatrix; overload;

    procedure SubInPlace(Value : TDoubleMatrix); overload; virtual;
    function Sub(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;

    procedure SubVecInPlace(Value : TDoubleMatrix; rowWise : Boolean); overload;
    function SubVec(aVec : TDoubleMatrix; rowWise : Boolean) : TDoubleMatrix; overload;

    procedure MultInPlace(Value : TDoubleMatrix); overload; virtual;
    function Mult(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure MultInPlaceT1(Value : TDoubleMatrix); overload; virtual;
    function MultT1(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure MultInPlaceT2(Value : TDoubleMatrix); overload; virtual;
    function MultT2(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure ElementWiseMultInPlace(Value : TDoubleMatrix); overload; virtual;
    function ElementWiseMult(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;

    procedure ElementWiseDivInPlace(Value : TDoubleMatrix); overload; virtual;
    function ElementWiseDiv(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;

    function AddAndScale(const Offset, Scale : double) : TDoubleMatrix; virtual;

    function Mean(RowWise : boolean) : TDoubleMatrix;
    procedure MeanInPlace(RowWise : boolean); override;
    function Variance(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    function Std(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure VarianceInPlace(RowWise : boolean; unbiased : boolean = True); override;
    procedure StdInPlace(RowWise : boolean; unbiased : boolean = True); override;

    procedure MeanVarianceInPlace(RowWise : boolean; unbiased : boolean = True); override;

     // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;

    function Median(RowWise : boolean) : TDoubleMatrix; virtual;
    procedure MedianInPlace(RowWise : boolean); override;
    function RollMedian(RowWise : boolean; order : integer) : TDoubleMatrix;
    procedure RollMedianInPlace(RowWise : boolean; order : integer); override;

    function Sort(RowWise : boolean) : TDoubleMatrix;
    procedure SortInPlace(RowWise : boolean); override;

    function Diff(RowWise : boolean) : TDoubleMatrix;
    procedure DiffInPlace(RowWise : boolean); override;
    function Sum(RowWise : boolean) : TDoubleMatrix; overload;
    procedure SumInPlace(RowWise : boolean); overload; override;
    procedure SumInPlace(RowWise : boolean; keepMemory : boolean); overload; override;

    function CumulativeSum(RowWise : boolean) : TDoubleMatrix;
    procedure CumulativeSumInPlace(RowWise : boolean); override;

    procedure AddConstInPlace(const Value : double); overload; override;
    function Scale(const Value : double) : TDoubleMatrix;
    function ScaleAndAdd(const aOffset, aScale : double) : TDoubleMatrix;
    procedure ScaleInPlace(const Value : Double); override;
    procedure ScaleAndAddInPlace(const aOffset, aScale : double); override;
    procedure SQRTInPlace; override;

    function SQRT : TDoubleMatrix;

    procedure ElementwiseFuncInPlace(func : TMatrixFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixFunc) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixObjFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixObjFunc) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixMtxRefFunc) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefObjFunc); overload; virtual;
    function ElementwiseFunc(func : TMatrixMtxRefObjFunc) : TDoubleMatrix; overload; virtual;

    {$IFDEF ANONMETHODS}
    procedure ElementwiseFuncInPlace(func : TMatrixFuncRef); overload; virtual;
    function ElementwiseFunc(func : TMatrixFuncRef) : TDoubleMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFuncRef); overload; virtual;
    function ElementwiseFunc(func : TMatrixMtxRefFuncRef) : TDoubleMatrix; overload; virtual;
    {$ENDIF}


    // ###################################################
    // #### Linear System solver A*x = B -> use A.SolveLinEQ(B) -> x
    function SolveLinEQ(Value : TDoubleMatrix; numRefinements : integer = 0) : TDoubleMatrix; overload; virtual;
    procedure SolveLinEQInPlace(Value : TDoubleMatrix; numRefinements : integer = 0); overload; virtual;

    // solves least sqaures A*x = b using the QR decomposition
    procedure SolveLeastSquaresInPlace(out x : TDoubleMatrix; b : TDoubleMatrix); overload; virtual;
    procedure SolveLeastSquaresInPlace(out x: IDoubleMatrix; b: IDoubleMatrix); overload;
    procedure SolveLeastSquares(out x : TDoubleMatrix; b : TDoubleMatrix) ; overload; virtual;
    procedure SolveLeastSquares(out x : IDoubleMatrix; b: IDoubleMatrix); overload;

    // Matrix inversion (via LU decomposition)
    function Invert : TDoubleMatrix; overload; virtual;
    function InvertEx( out InvMtx : TDoubleMatrix ) : TLinEquResult; overload; virtual;
    function InvertInPlace : TLinEquResult; override;
    function Determinant : double; virtual;

    // pseudoinversion via svd
    function PseudoInversionInPlace : TSVDResult;
    function PseudoInversion(out Mtx : TDoubleMatrix) : TSVDResult; overload;
    function PseudoInversion(out Mtx : IDoubleMatrix) : TSVDResult; overload;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : double);
    function RepeatMatrix(numX, numY : integer) : TDoubleMatrix; overload;

    // ###################################################
    // #### Matrix transformations
    function SVD(out U, V, W : TDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload; virtual;
    function SVD(out U, V, W : IDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;

    // the symmetric eigenvalue calculation actually references only the upper part of the matrix
    // -> therefore symmetry does not need to be maintained explicitly
    function SymEig(out EigVals : TDoubleMatrix; out EigVect : TDoubleMatrix) : TEigenvalueConvergence; overload; virtual;
    function SymEig(out EigVals : TDoubleMatrix) : TEigenvalueConvergence; overload; virtual;
    function SymEig(out EigVals : IDoubleMatrix; out EigVect : IDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IDoubleMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix; out EigVect : TDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IDoubleMatrix; out EigVect : IDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IDoubleMatrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix; out EigVect : IDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix)  : TEigenvalueConvergence; overload;

    function Cholesky(out Chol : TDoubleMatrix) : TCholeskyResult; overload; virtual;
    function Cholesky(out Chol : IDoubleMatrix) : TCholeskyResult; overload;

    // cleared lower triangle qr decomposition -> R only
    procedure QR(out R : TDoubleMatrix); overload;
    procedure QR(out R : IDoubleMatrix); overload;
    procedure QRFull(out Q, R : TDoubleMatrix); overload; virtual;
    procedure QRFull(out Q, R : IDoubleMatrix); overload;

    // qr decomposition without clearing the lower triangular matrix and factors tau
    procedure QR(out ecosizeR : TDoubleMatrix; out tau : TDoubleMatrix); overload; virtual;
    procedure QR(out ecosizeR : IDoubleMatrix; out tau : IDoubleMatrix); overload;
    procedure Hess( out h : TDoubleMatrix; out tau : TDoubleMatrix ); overload; virtual;


    // hessenberg decomposition. Lower part is zeroed out
    procedure Hess( out h : TDoubleMAtrix ); overload;
    procedure Hess( out h : IDoubleMatrix ); overload;
    // for the full transformation: Q*H*Q' = A
    procedure HessFull(out Q, h : TDoubleMatrix); overload;
    procedure HessFull(out Q, h : IDoubleMatrix); overload;

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(const Mtx : Array of double; W, H : integer); overload;
    procedure AssignDyn(Value : TDoubleDynArray; W, H : integer);
    procedure Assign(Value : TDoubleMatrix); overload;
    procedure AssignEx(Value : TDoubleMatrix; OnlySubElements : boolean);

    procedure AssignSubMatrix(Value : TDoubleMatrix; X : integer = 0; Y : integer = 0); overload;

    procedure Append(Value : TDoubleMatrix; appendColumns : boolean); overload;

    // actually the same as assign but it takes over the data and leaves an empty matrix object behind.
    procedure TakeOver(Value : TDoubleMatrix); overload;

    function Clone : TDoubleMatrix;

    constructor Create(aWidth, aHeight : integer; NoLineWidthGap : boolean); overload;
    constructor Create; overload;
    constructor CreateVec( aLen : integer; const initVal : double = 0 );
    constructor Create(aWidth, aHeight : integer; const initVal : double = 0); overload;
    constructor CreateEye(aWidth : integer);
    constructor Create(data : PDouble; aLineWidth : NativeInt; aWidth, aHeight : integer); overload;
    constructor CreateCpy( aWidth, aHeight : integer; data : PDouble; aLineWidth : integer);   // different params than in create since delphi complains
    constructor CreateDyn(const Data : TDoubleDynArray; aWidth, aHeight : integer); overload;
    constructor CreateDyn(const Data : TDoubleDynArray; fromDataIdx : integer; aWidth, aHeight : integer); overload;
    constructor Create(const Mtx : Array of double; W, H : integer); overload;
    constructor CreateRand(aWidth, aHeight : integer; method : TRandomAlgorithm; var seed : LongInt); overload; // uses random engine
    constructor CreateRand(aWidth, aHeight : integer; const seed : LongInt; method : TRandomAlgorithm); overload; // uses random engine
    constructor CreateRand(aWidth, aHeight : integer); overload; // uses system default random
    constructor CreateLinSpace(aVecLen : integer; const StartVal : double; const EndVal : double);

    class procedure CheckForRealMtx( mtx : IMatrix );
  end;

type
  TDoubleMatrixDynArr = Array of TDoubleMatrix;
  IDoubleMatrixDynArr = Array of IDoubleMatrix;


// default class used in derrived methods like in the global subspace methods:
// override the class value to use different matrix class implementations
// e.g. the threaded version
type
  TMatrixClass = class(TBaseMathPersistence)
  private
    fMatrixClass : TDoubleMatrixClass;
    function GetMtxClass: TDoubleMatrixClass;
    procedure SetMtxClass(const Value: TDoubleMatrixClass);
  protected
    procedure DefineProps; override;
  public
    property MatrixClass : TDoubleMatrixClass read GetMtxClass write SetMtxClass;

    class var DefMatrixClass : TDoubleMatrixClass;
  end;


function MatrixFromTxtFile( fileName : string; mtxClass : TDoubleMatrixClass ) : TDoubleMatrix; overload;
function MatrixFromTxtFile( fileName : string ) : TDoubleMatrix; overload;

procedure MatrixToTxtFile(const fileName : string; const mtx : TDoubleMatrix; prec : integer = 8);
procedure WriteBinary(const fn : string; mtx : IDoubleMatrix);

implementation

uses Math, MatrixASMStubSwitch, Eigensystems, LinAlgSVD, LinAlgQR, BlockSizeSetup, LinAlgCholesky,
     LinAlgLU, MathUtilFunc, CplxMatrix;


// ###########################################
// #### Inline functions (need to be first)
// ###########################################

procedure TDoubleMatrix.SetItems(x, y: integer; const Value: double);
var pData : PLocConstDoubleArr;
begin
     CheckAndRaiseError((x >= 0) and (x < fSubWidth), 'Dimension error');
     CheckAndRaiseError((y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := PLocConstDoubleArr(StartElement);
     inc(PByte(pData), y*fLineWidth);

     pData^[fOffsetX + x] := Value;
end;

function TDoubleMatrix.GetItems(x, y: integer): double;
var pData : PLocConstDoubleArr;
begin
     CheckAndRaiseError((x >= 0) and (x < fSubWidth) and (y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := PLocConstDoubleArr(StartElement);
     inc(PByte(pData), y*fLineWidth);

     Result := pData^[x];
end;

class function TDoubleMatrix.GetItemSize: integer;
begin
     Result := sizeof(double);
end;

procedure TDoubleMatrix.SetVecItem(idx: integer; const Value: double);
begin
     if idx < fSubWidth 
     then
         SetItems(idx, 0, Value)
     else
         SetItems(idx mod fSubWidth, idx div fSubWidth, Value);
end;

function TDoubleMatrix.GetVecItem(idx: integer): double;
begin
     if idx < fSubWidth
     then
         Result := GetItems(idx, 0)
     else
         Result := GetItems(idx mod fSubWidth, idx div fSubWidth);
end;

// ###########################################
// #### TDoubleMatrix
// ###########################################

function TDoubleMatrix.Abs: TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     MatrixAbs(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

function TDoubleMatrix.AbsI: IMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     MatrixAbs(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

procedure TDoubleMatrix.AbsInPlace;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAbs(StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TDoubleMatrix.AddConst(const Value: double): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     MatrixElemAdd(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, Value);
end;

function TDoubleMatrix.AddConstD(const Value: double): IDoubleMatrix;
begin
     Result := AddConst(Value);
end;

function TDoubleMatrix.AddConstI(const Value: double): IMatrix;
begin
     Result := AddConst(Value);
end;

//function TDoubleMatrix.AddI(Value: IMatrix): IMatrix;
//begin
//     CheckForRealMtx(Value);
//
//     Result := Add( Value.GetObjRef as TDoubleMatrix );
//end;

function TDoubleMatrix.AddAndScale(const Offset, Scale: double): TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.AssignEx(self, True);

     Result.AddAndScaleInPlace(Offset, Scale);
end;

function TDoubleMatrix.AddAndScaleD(const Offset, Scale: double): IDoubleMatrix;
begin
     Result := AddAndScale(Offset, Scale);
end;

procedure TDoubleMatrix.AddAndScaleInPlace(const Offset, Scale: double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Offset, Scale);
end;

procedure TDoubleMatrix.AddConstInPlace(const Value: double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Value, 1);
end;

function TDoubleMatrix.AddD(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := Add(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.AddInplaceD(Value: IDoubleMatrix);
begin
     AddInplace(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.AddInplace(Value: TDoubleMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     // inplace matrix addition
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Matrix dimensions do not match');
     MatrixAdd(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.Assign(const Mtx: array of double; W, H : integer);
begin
     CheckAndRaiseError((W*H > 0) and (Length(Mtx) = W*H), 'Dimension error');

     SetWidthHeight(W, H);
     MatrixCopy(StartElement, LineWidth, @Mtx[0], W*sizeof(double), W, H);
end;

procedure TDoubleMatrix.AssignD(Value: IDoubleMatrix);
begin
     Assign(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.AssignDyn(Value: TDoubleDynArray; W, H: integer);
begin
     CheckAndRaiseError((W*H > 0) and (Length(Value) = W*H), 'Dimension error');

     SetWidthHeight(W, H);
     MatrixCopy(StartElement, LineWidth, @Value[0], W*sizeof(double), W, H);
end;

procedure TDoubleMatrix.AssignEx(Value: TDoubleMatrix;
  OnlySubElements: boolean);
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
          fSubWidth := Value.fSubWidth;
          fSubHeight := Value.fSubHeight;
     end;
end;

procedure TDoubleMatrix.AssignExD(Value: IDoubleMatrix;
  OnlySubElements: boolean);
begin
     AssignEx(Value.GetObjRef as TDoubleMatrix, onlySubElements);
end;

procedure TDoubleMatrix.Assign(Value: TDoubleMatrix);
begin
     AssignEx(Value, False);
end;

procedure TDoubleMatrix.AssignSubMatrix(Value: TDoubleMatrix; X, Y: integer);
var pSelf, pValue : PDouble;
begin
     CheckAndRaiseError((Value.Width <= Width + x) and (Value.Height <= Height + y), 'Dimension error');
     if (Value.Width = 0) or (Value.Height = 0) then
        exit;

     pSelf := GenPtr(StartElement, x, y, LineWidth);
     pValue := Value.StartElement;

     MatrixCopy(pSelf, LineWidth, pValue, Value.LineWidth, Value.Width, Value.Height);
end;

procedure TDoubleMatrix.AssignSubMatrixD(Value: IDoubleMatrix; X, Y: integer);
begin
     AssignSubMatrix(Value.GetObjRef as TDoubleMatrix, X, Y);
end;

function TDoubleMatrix.AsVectorI(ColMajor: boolean): IMatrix;
begin
     Result := AsVector(ColMajor);
end;

procedure TDoubleMatrix.Append(Value: TDoubleMatrix; appendColumns: boolean);
var pStart : PDouble;
begin
     if AppendColumns then
     begin
          CheckAndRaiseError(Value.Height = fSubHeight, 'Dimension error, number of rows are not equal');
          Resize(fSubWidth + Value.Width, fSubHeight);
          pStart := StartElement;
          inc(pStart, fSubWidth - Value.Width);
          MatrixCopy(pStart, LineWidth, Value.StartElement, Value.LineWidth, Value.Width, Value.Height );
     end
     else
     begin
          CheckAndRaiseError(Value.Width = fSubWidth, 'Dimension error, number of columns are not equal');
          Resize(fSubWidth, Value.Height + fSubHeight);
          pStart := GenPtr(StartElement, 0, fSubHeight - Value.Height, LineWidth);
          MatrixCopy(pStart, LineWidth, Value.StartElement, Value.LineWidth, Value.Width, Value.Height );
     end;
end;

procedure TDoubleMatrix.AppendD(Value: IDoubleMatrix; appendColumns: boolean);
begin
     Append(Value.GetObjRef as TDoubleMatrix, appendColumns);
end;

function TDoubleMatrix.Add(Value : TDoubleMatrix) : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(fSubWidth, fSubHeight);
     MatrixAdd(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.AddVecInPlace(Value: TDoubleMatrix; rowWise: Boolean);
var incX : NativeInt;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     if rowWise
     then
         CheckAndRaiseError((Value.VecLen = Width), 'Arguments dimension error')
     else
         CheckAndRaiseError((Value.VecLen = Height), 'Arguments dimension error');
     incX := sizeof(double);

     if value.Width = 1 then
        incX := value.LineWidth;

     MatrixAddVec(StartElement, LineWidth, Value.StartElement, incX, Width, Height, RowWise);
end;

function TDoubleMatrix.AddVec(aVec: TDoubleMatrix;
  rowWise: Boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     if rowWise
     then
         CheckAndRaiseError((aVec.VecLen = Width), 'Arguments dimension error')
     else
         CheckAndRaiseError((aVec.VecLen = Height), 'Arguments dimension error');

     Result := Clone;
     Result.AddVecInPlace(aVec, rowWise);
end;

function TDoubleMatrix.AddVecD(aVec: IDoubleMatrix;
  rowWise: Boolean): IDoubleMatrix;
begin
     Result := AddVec(aVec.GetObjRef as TDoubleMatrix, rowWise);
end;

//function TDoubleMatrix.AddVecI(aVec: IMatrix; rowWise: Boolean): IMatrix;
//begin
//     CheckForRealMtx(aVec);
//
//     Result := AddVec(aVec.GetObjRef as TDoubleMatrix, rowWise);
//end;
//
procedure TDoubleMatrix.AddVecInPlaceD(Value: IDoubleMatrix; rowWise: Boolean);
begin
     CheckForRealMtx(Value);

     AddVecInPlace(value.GetObjRef as TDoubleMatrix, rowWise);
end;

function TDoubleMatrix.Cholesky(out Chol: TDoubleMatrix): TCholeskyResult;
var x, y : integer;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Cholesky decomposition is only allowed on square matrices');
     chol := ResultClass.Create;
     try
        chol.Assign(Self);
        // block wise cholesky decomposition
        Result := MatrixCholeskyInPlace4(chol.StartElement, chol.LineWidth, chol.Width, 0, fLinEQProgress);

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

constructor TDoubleMatrix.Create;
begin
     inherited Create;

     SetWidthHeight(1, 1);
end;

constructor TDoubleMatrix.Create(data: PDouble; aLineWidth : NativeInt; aWidth,
  aHeight: integer);
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (aLineWidth >= aWidth*sizeof(double)), 'Dimension error');

     inherited Create(PByte(data), aLineWidth, aWidth, aHeight);
end;

constructor TDoubleMatrix.Create(aWidth, aHeight: integer; const initVal : double);
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     if initVal <> 0 then
        SetValue(initVal);
end;

constructor TDoubleMatrix.CreateDyn(const Data : TDoubleDynArray; aWidth, aHeight : integer);
begin
     CheckAndRaiseError((aWidth*aHeight > 0) and (Length(Data) >= aWidth*aHeight), 'Dimension error');

     inherited Create;

     SetWidthHeight(aWidth, aHeight);
     MatrixCopy(StartElement, LineWidth, @Data[0], aWidth*sizeof(double), aWidth, aHeight);
end;

constructor TDoubleMatrix.CreateEye(aWidth: integer);
var i : integer;
begin
     inherited Create;

     SetWidthHeight(aWidth, aWidth);

     for i := 0 to width - 1 do
         Items[i, i] := 1;
end;

constructor TDoubleMatrix.CreateRand(aWidth, aHeight: integer;
  method: TRandomAlgorithm; var seed: LongInt);
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     InitRandom(method, seed);
end;

constructor TDoubleMatrix.CreateLinSpace(aVecLen: integer; const StartVal,
  EndVal: double);
var value : double;
    pVec : PDouble;
    counter: Integer;
    dx : double;
begin
     assert(vecLen >= 0, 'Error: initialized by negative len');

     inherited Create;

     SetWidthHeight(1, aVecLen);

     if aVecLen = 0 then
        exit;

     dx := (EndVal - StartVal)/Math.Max(1, aVecLen - 1);

     pVec := StartElement;
     value := startVal;
     for counter := 0 to aVecLen - 1 do
     begin
          pVec^ := value;
          value := value + dx;
          inc(PByte(pVec), LineWidth);
     end;

     // account for small accumulated errors - so at least the last
     // value is as expected
     if aVecLen > 1 then
        Vec[vecLen - 1] := EndVal;
end;

constructor TDoubleMatrix.Create(const Mtx: array of double; W, H: integer);
begin
     CheckAndRaiseError((W*H > 0) and (Length(Mtx) = W*H), 'Dimension error');

     inherited Create;

     SetWidthHeight(W, H);
     MatrixCopy(StartElement, LineWidth, @Mtx[0], W*sizeof(double), W, H);
end;

constructor TDoubleMatrix.Create(aWidth, aHeight: integer;
  NoLineWidthGap: boolean);
begin
     CheckAndRaiseError((aWidth > 0) and (aHeight > 0), 'Dimension error');

     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     // just shorten the linewidth -> so we get continous element access
     if NoLineWidthGap then
        fLineWidth := aWidth*SizeOf(double);
end;

constructor TDoubleMatrix.CreateCpy(aWidth, aHeight : integer; data : PDouble; aLineWidth : integer);
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (aLineWidth >= aWidth*sizeof(double)), 'Dimension error');

     inherited Create;

     if (data <> nil) then
     begin
          SetWidthHeight(aWidth, aHeight);
          MatrixCopy( StartElement, LineWidth, data, aLineWidth, aWidth, aHeight);
     end;
end;

constructor TDoubleMatrix.CreateDyn(const Data: TDoubleDynArray; fromDataIdx,
  aWidth, aHeight: integer);
begin
     CheckAndRaiseError((aWidth*aHeight > 0) and (Length(Data) - fromDataIdx >= aWidth*aHeight), 'Dimension error');

     inherited Create;

     SetWidthHeight(aWidth, aHeight);
     MatrixCopy(StartElement, LineWidth, @Data[fromDataIdx], aWidth*sizeof(double), aWidth, aHeight);
end;

procedure TDoubleMatrix.MtxRand(var value: double);
begin
     value := Random;
end;

procedure TDoubleMatrix.MtxRandWithEng(var value: double);
begin
     value := TRandomGenerator(fObj).Random;
end;

constructor TDoubleMatrix.CreateRand(aWidth, aHeight: integer);
var tmp : TDoubleMatrix;
begin
     inherited Create;

     // note: since the random function depends on internal states of the random engine
     // we need to make sure this is NEVER called in threads -> internal states
     // could be reused and the result is not what we expect from random numbers
     // (first surfaced in threaded cholesky decomposition on my 32core machine)
     tmp := TDoubleMatrix.Create( aWidth, aHeight );
     tmp.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}MtxRand);
     TakeOver(tmp);

     tmp.Free;
end;

constructor TDoubleMatrix.CreateRand(aWidth, aHeight: integer;
  const seed: Integer; method: TRandomAlgorithm);
var tmp : integer;
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);
     InitRandom(method, tmp);
end;

constructor TDoubleMatrix.CreateVec( aLen : integer; const initVal : double = 0 );
begin
     inherited Create;

     SetWidthHeight( aLen, 1 );

     if initVal <> 0 then
        SetValue( initVal );   
end;

function TDoubleMatrix.CumulativeSum(RowWise: boolean): TDoubleMatrix;
begin
     Result := ResultClass.Create(Width, Height);
     MatrixCumulativeSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, width, height, RowWise);
end;

function TDoubleMatrix.CumulativeSumI(RowWise: boolean): IMatrix;
begin
     Result := CumulativeSum(RowWise);
end;

procedure TDoubleMatrix.CumulativeSumInPlace(RowWise: boolean);
begin
     MatrixCumulativeSum(StartElement, LineWidth, StartElement, LineWidth, Width, Height, RowWise);
end;

function TDoubleMatrix.Determinant: double;
begin
     CheckAndRaiseError((Width > 0) and (Height = Width), 'Determinant only allowed on square matrices');
     Result := MatrixDeterminant(StartElement, LineWidth, fSubWidth);
end;

function TDoubleMatrix.Diag(createDiagMtx : boolean): TDoubleMatrix;
var x : integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if createDiagMtx then
     begin
          if (width = 1) then
          begin
               Result := ResultClass.Create(height, height);
               for x := 0 to Height - 1 do
                   Result[x, x] := Vec[x];
          end
          else if height = 1 then
          begin
               Result := ResultClass.Create(width, width);
               for x := 0 to width - 1 do
                   Result[x, x] := Vec[x];
          end
          else
          begin
               Result := ResultClass.Create(Math.Min(Width, Height), Math.Min(Width, Height));
               for x := 0 to Math.Min(Width, Height) - 1 do
                   Result[x, x] := Items[x, x];
          end;
     end
     else
     begin
          Result := ResultClass.Create(1, Math.Min(Width, Height));
          for x := 0 to Result.Height - 1 do
              Result[Math.Min(x, Result.width - 1), x] := Items[x, x];
     end;
end;

function TDoubleMatrix.DiagFromOffset(K: integer): TDoubleMatrix;
var x : integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     // vector converted to diagonal matrix
     if (width = 1) or (height = 1) then
     begin
          if k < 0 then
          begin
               Result := ResultClass.Create(VecLen, VecLen + System.abs(k));

               for x := 0 to VecLen - 1 do
                   Result[x, x - k] := Vec[x];
          end
          else
          begin
               Result := ResultClass.Create(VecLen + System.abs(k), VecLen);

               for x := 0 to VecLen - 1 do
                   Result[x + k, x] := Vec[x];
          end;
     end
     else
     begin
          if k < 0 
          then
              CheckAndRaiseError( (System.abs(k) < height), 'Offset out of bounds')
          else
              CheckAndRaiseError( (System.abs(k) < width), 'Offset out of bounds');
         
          // return a vector of the diagonal
          if k < 0 then
          begin
               Result := ResultClass.Create(1, Math.Min(Width, Height + k));
               for x := 0 to Math.Min(Width, Height + k) - 1 do
                   Result.Vec[x] := Items[x, x - k];
          end
          else
          begin
               Result := ResultClass.Create(1, Math.Min(Width - k, Height));
               for x := 0 to Math.Min(Width - k, Height) - 1 do
                   Result.Vec[x] := Items[x + k, x];
          end;
     end;
end;

function TDoubleMatrix.DiagI(createDiagMtx: boolean): IMatrix;
begin
     Result := Diag(createDiagMtx);
end;

function TDoubleMatrix.DiagFromOffsetI(K: integer): IMatrix;
begin
     Result := DiagFromOffset(K);
end;

procedure TDoubleMatrix.DiagInPlace(createDiagMtx : boolean);
var dl : IMatrix;
begin
     dl := Diag(createDiagMtx);

     TakeOver(dl.GetObjRef as TDoubleMatrix);
end;

function TDoubleMatrix.Diff(RowWise: boolean): TDoubleMatrix;
var newWidth : integer;
    newHeight : integer;
begin
     Result := nil;
     if (width = 0) or (height = 0) then
        exit;

     newWidth := Width;
     newHeight := Height;
     if RowWise
     then
         dec(newWidth)
     else
         dec(newHeight);

     Result := ResultClass.Create(newWidth, newHeight);
     MatrixDiff(Result.StartElement, Result.LineWidth, StartElement, LineWidth, Width, Height, RowWise);
end;

function TDoubleMatrix.DiffD(RowWise: boolean): IDoubleMatrix;
begin
     Result := Diff(RowWise);
end;

function TDoubleMatrix.DiffI(RowWise: boolean): IMatrix;
begin
     Result := Diff(RowWise);
end;

procedure TDoubleMatrix.DiffInPlace(RowWise: boolean);
begin
     if (width = 0) or (height = 0) then
        exit;

     MatrixDiff(StartElement, LineWidth, StartElement, LineWidth, Width, height, RowWise);

     // just reduce the sub width
     if RowWise
     then
         dec(fSubWidth)
     else
         dec(fSubHeight);
end;

function TDoubleMatrix.SolveLinEQ(Value: TDoubleMatrix; numRefinements : integer) : TDoubleMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = fSubHeight) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(Value.fSubWidth, fSubHeight);
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

procedure TDoubleMatrix.SolveLeastSquares(out x : TDoubleMatrix; b : TDoubleMatrix);
var tmpA : TDoubleMatrix;
begin
     CheckAndRaiseError( width <= Height, 'Height must be at least width');
     
     tmpA := Clone;
     try
        x := ResultClass.Create( 1, Width );

        MatrixQRSolve( x.StartElement, x.LineWidth, tmpA.StartElement, tmpA.LineWidth, b.StartElement, b.LineWidth, width, height);
     finally
            tmpA.Free;
     end;
end;

procedure TDoubleMatrix.SolveLeastSquares(out x : IDoubleMatrix; b: IDoubleMatrix);
var tmp : TDoubleMatrix;
begin
     SolveLeastSquares(tmp, b.GetObjRef as TDoubleMatrix);

     x := tmp;
end;

procedure TDoubleMatrix.SolveLeastSquaresInPlace(out x: TDoubleMatrix;
  b: TDoubleMatrix);
begin
     CheckAndRaiseError( width <= Height, 'Height must be at least width');

     x := ResultClass.Create( 1, Width );

     MatrixQRSolve( x.StartElement, x.LineWidth, StartElement, LineWidth, b.StartElement, b.LineWidth, width, height);
end;

procedure TDoubleMatrix.SolveLeastSquaresInPlace(out x: IDoubleMatrix;
  b: IDoubleMatrix);
var tmp : TDoubleMatrix;
begin
     SolveLeastSquaresInPlace(tmp, b.GetObjRef as TDoubleMatrix);

     x := tmp;
end;

procedure TDoubleMatrix.SolveLinEQInPlace(Value: TDoubleMatrix; numRefinements : integer);
var dt : TDoubleMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = fSubHeight) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     dt := ResultClass.Create(Value.fSubWidth, fSubHeight);
     try
        if MatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement, Value.LineWidth, dt.StartElement,
                            dt.LineWidth, Value.fSubWidth, numRefinements, fLinEQProgress) = leSingular
        then
            raise ELinEQSingularException.Create('Matrix is singular');

        TakeOver(dt);
     finally
            dt.Free;
     end;
end;

procedure TDoubleMatrix.SolveLinEQInPlaceD(Value: IDoubleMatrix;
  numRefinements: integer);
begin
     SolveLinEQInPlace(Value.GetObjRef as TDoubleMatrix, numRefinements);
end;

function TDoubleMatrix.SQRT: TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     MatrixSQRT(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

function TDoubleMatrix.SQRTD: IDoubleMatrix;
begin
     Result := SQRT;
end;

function TDoubleMatrix.SQRTI: IMatrix;
begin
     Result := Sqrt;
end;

procedure TDoubleMatrix.SQRTInPlace;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     MatrixSQRT(StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TDoubleMatrix.Eig(out EigVals, EigVect: IDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone): TEigenvalueConvergence;
var outEigVals, outEigVect : TDoubleMatrix;
begin
     Result := Eig(outEigVals, outEigVect, normEigVecs);
     EigVals := outEigVals;
     EigVect := outEigVect;
end;

function TDoubleMatrix.Eig(out EigVals: IDoubleMatrix): TEigenvalueConvergence;
var outEigVals : TDoubleMatrix;
begin
     Result := Eig(outEigVals);
     EigVals := outEigVals;
end;

function TDoubleMatrix.Eig(out EigVals: TDoublematrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    pReal, pImag : PDouble;
    dummy : TDoubleMatrix;
    perm : TIntegerDynArray;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight = fSubWidth), 'Eigenvalues are only defined for square matrices');

     EigVals := nil;
     dt := ResultClass.Create(2, fSubHeight);
     pReal := dt.StartElement;
     pImag := pReal;
     inc(pImag);

     dummy := TDoubleMatrix.Create;
     dummy.Assign(self);

     SetLength(perm, fSubWidth);
     MatrixHessenbergPerm(dummy.StartElement, dummy.LineWidth, StartElement, LineWidth, fSubWidth, @perm[0], sizeof(integer));
     Result := MatrixEigHessenbergInPlace(dummy.StartElement, dummy.LineWidth, fSubWidth, pReal, dt.LineWidth, pImag, dt.LineWidth);
     dummy.Free;

     if Result = qlOk
     then
         EigVals := dt
     else
         dt.Free;
end;

function TDoubleMatrix.Eig(out EigVals: IMatrix; out EigVect: IDoubleMatrix;
  normEigVecs: TEigenvalueEivecNormalize): TEigenvalueConvergence;
var eTmp, eVecTmp : TDoubleMatrix;
begin
     Result := Eig(eTmp, eVecTmp, normEigVecs);

     try
        EigVals := EigValsMtxToCplxOrDouble(eTmp);
        EigVect := eVecTmp;
     finally
            eTmp.Free;
     end;
end;

function TDoubleMatrix.Eig(out EigVals: IMatrix): TEigenvalueConvergence;
var tmp : TDoubleMatrix;
begin
     EigVals := nil;
     Result :=  Eig(tmp);

     try
        EigVals := EigValsMtxToCplxOrDouble(tmp);
     finally
            tmp.Free;
     end;
end;

function TDoubleMatrix.EigValsMtxToCplxOrDouble(mtx: TDoubleMatrix): IMatrix;
var i : integer;
begin
     // check result type
     for i := 0 to mtx.Height - 1 do
     begin
          if not SameValue(mtx[1, i], 0) then
          begin
               Result := TCplxMatrix.CreateCpy(1, mtx.Height, PComplex(mtx.StartElement), mtx.LineWidth);
               break;
          end;
     end;
     // double matrix -> remove the extra column
     if not Assigned(Result) then
        Result := TDoubleMatrix.CreateCpy(1, mtx.Height, mtx.StartElement, mtx.LineWidth);
end;

function TDoubleMatrix.Eig(out EigVals, EigVect: TDoubleMatrix; normEigVecs : TEigenvalueEivecNormalize = enNone): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vecs : TDoubleMatrix;
    pReal, pImag : PDouble;
    dummy : TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight = fSubWidth), 'Eigenvalues are only defined for square matrices');

     EigVals := nil;
     EigVect := nil;
     dt := nil;
     vecs := nil;
     try
        dt := ResultClass.Create(2, fSubHeight);
        pReal := dt.StartElement;
        pImag := pReal;
        inc(pImag);

        vecs := ResultClass.Create(fSubWidth, fSubHeight);

        // create a copy since the original content is destroyed!
        dummy := TDoubleMatrix.Create(self.Width, self.Height, True);
        try
           dummy.Assign(self);
           Result := MatrixUnsymEigVecInPlace(dummy.StartElement, dummy.LineWidth,
                                              fSubWidth,
                                              pReal, dt.LineWidth, pImag, dt.LineWidth,
                                              vecs.StartElement, vecs.LineWidth);
        finally
               dummy.Free;
        end;

        if Result = qlOk then
        begin
             if normEigVecs <> enNone then
                MatrixNormEivecInPlace(vecs.StartElement, vecs.LineWidth, fSubWidth, pImag, dt.LineWidth, normEigVecs = enLen);
             
             EigVals := dt;
             EigVect := vecs;
        end
        else
        begin
             dt.Free;
             vecs.Free;
        end;
     except
           dt.Free;
           vecs.Free;

           raise;
     end;
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixFunc): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixObjFunc): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixObjFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixMtxRefFunc): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

function TDoubleMatrix.ElementwiseFunc(func: TMatrixMtxRefObjFunc): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func: TMatrixMtxRefObjFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

{$IFDEF ANONMETHODS}
procedure TDoubleMatrix.ElementwiseFuncInPlace(func : TMatrixFuncRef);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementwiseFunc(func : TMatrixFuncRef) : TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

procedure TDoubleMatrix.ElementwiseFuncInPlace(func : TMatrixMtxRefFuncRef);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TDoubleMatrix.ElementwiseFunc(func : TMatrixMtxRefFuncRef) : TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     MatrixFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;

{$ENDIF}


function TDoubleMatrix.ElementWiseMult(Value: TDoubleMatrix) : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     Result := ResultClass.Create( Width, Height );
     MatrixElemMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TDoubleMatrix.ElementWiseMultD(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := ElementWiseMult(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.ElementWiseMultInPlaceD(Value: IDoubleMatrix);
begin
     ElementWiseMultInPlace(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.ElementWiseMultInPlace(Value: TDoubleMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     MatrixElemMult(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TDoubleMatrix.ElementWiseDiv(Value: TDoubleMatrix): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     Result := ResultClass.Create;
     Result.Assign(self);
     MatrixElemDiv(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TDoubleMatrix.ElementWiseDivD(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := ElementWiseDiv(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.ElementWiseDivInPlace(Value: TDoubleMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     MatrixElemDiv(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.ElementWiseDivInPlaceD(Value: IDoubleMatrix);
begin
     ElementWiseDivInPlace(Value.GetObjRef as TDoubleMatrix);
end;

function TDoubleMatrix.ElementwiseNorm2(doSqrt : boolean = True): double;
begin
     Result := 0;

     if (Width > 0) and (Height > 0) then
        Result := MatrixElementwiseNorm2(StartElement, LineWidth, Width, Height, doSqrt);
end;

function TDoubleMatrix.GetType: TMatrixType;
begin
     Result := mtReal;
end;

function TDoubleMatrix.Invert: TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     Result := ResultClass.Create;
     try
        Result.AssignEx(Self, True);

        if MatrixInverseInPlace(Result.StartElement, Result.LineWidth, fSubWidth, fLinEQProgress) = leSingular then
           raise ELinEQSingularException.Create('Singular matrix');
     except
           Result.Free;
           raise;
     end;
end;

function TDoubleMatrix.InvertD: IDoubleMatrix;
begin
     Result := Invert;
end;

function TDoubleMatrix.InvertEx(out InvMtx : TDoubleMatrix): TLinEquResult;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     InvMtx := ResultClass.Create;
     InvMtx.AssignEx(Self, True);

     Result := MatrixInverseInPlace(InvMtx.StartElement, InvMtx.LineWidth, fSubWidth, fLinEQProgress);
     if Result <> leOk then
        FreeAndNil(invMtx);
end;

function TDoubleMatrix.InvertExD(out InvMtx: IDoubleMatrix): TLinEquResult;
var outVal : TDoubleMatrix;
begin
     Result := InvertEx(outVal);
     InvMtx := outVal;
end;

function TDoubleMatrix.InvertExI(out InvMtx: IMatrix): TLinEquResult;
var x : TDoubleMatrix;
begin
     Result := InvertEx(x);
     InvMtx := x;
end;

function TDoubleMatrix.InvertI: IMatrix;
begin
     Result := Invert;
end;

function TDoubleMatrix.InvertInPlace: TLinEquResult;
var dt : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     dt := ResultClass.Create(Width, Height);
     try
        dt.AssignEx(self, True);
        Result := MatrixInverseInPlace(dt.StartElement, dt.LineWidth, fSubWidth, fLinEQProgress);
        if Result = leOk then
           TakeOver(dt);

        FreeAndNil(dt);
     except
           dt.Free;
           raise;
     end;
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
     CheckAndRaiseError(fSubWidth*fSubHeight = Length(Mask), 'Error number of mask elements must be the same as the matrix dimension');
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
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := MatrixMax(StartElement, fSubWidth, fSubHeight, LineWidth);
end;

function TDoubleMatrix.Mean(RowWise: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise
     then
         Result := ResultClass.Create(1, fSubHeight)
     else
         Result := ResultClass.Create(fSubWidth, 1);

     MatrixMean(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
end;

function TDoubleMatrix.MeanD(RowWise: boolean): IDoubleMatrix;
begin
     Result := Mean(RowWise);
end;

function TDoubleMatrix.MeanI(RowWise: boolean): IMatrix;
begin
     Result := Mean(RowWise);
end;

procedure TDoubleMatrix.MeanInPlace(RowWise: boolean);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Mean(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.MeanVariance(RowWise, unbiased: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(2, fSubHeight);

          MatrixMeanVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 2);

          MatrixMeanVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end;
end;

function TDoubleMatrix.MeanVarianceD(RowWise, unbiased: boolean): IDoubleMatrix;
begin
     Result := MeanVariance(RowWise, unbiased);
end;

function TDoubleMatrix.MeanVarianceI(RowWise, unbiased: boolean): IMatrix;
begin
     Result := MeanVariance(RowWise, unbiased);
end;

procedure TDoubleMatrix.MeanVarianceInPlace(RowWise, unbiased: boolean);
var dt : TDoubleMatrix;
begin
     dt := MeanVariance(RowWise, unbiased);
     try
        TakeOver(dt);
     finally
            dt.Free;
     end;
end;

function TDoubleMatrix.Median(RowWise: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          MatrixMedian(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, nil);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          MatrixMedian(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, nil);
     end;
end;

function TDoubleMatrix.MedianD(RowWise: boolean): IDoubleMatrix;
begin
     Result := Median(RowWise);
end;

function TDoubleMatrix.MedianI(RowWise: boolean): IMatrix;
begin
     Result := Median(RowWise);
end;

procedure TDoubleMatrix.MedianInPlace(RowWise: boolean);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Median(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.RollMedian(RowWise: boolean;
  order: integer): TDoubleMatrix;
begin
     Result := Clone;
     Result.RollMedianInPlace(RowWise, order);
end;

function TDoubleMatrix.RollMedianD(RowWise: boolean;
  order: integer): IDoubleMatrix;
begin
     Result := RollMedian(RowWise, order);
end;

function TDoubleMatrix.RollMedianI(RowWise: boolean; order: integer): IMatrix;
begin
     Result := RollMedian(RowWise, order);
end;

procedure TDoubleMatrix.RollMedianInPlace(RowWise: boolean; order: integer);
begin
     CheckAndRaiseError( (Width > 0) and (Height > 0), 'No data assigned');
     MatrixRollMedian(StartElement, LineWidth, Width, Height, order, RowWise);
end;

function TDoubleMatrix.Sort(RowWise: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     Result.SortInPlace(RowWise);
end;

function TDoubleMatrix.SortI(RowWise: boolean): IMatrix;
begin
     Result := Sort(RowWise);
end;

procedure TDoubleMatrix.SortInPlace(RowWise: boolean);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixSort(StartElement, LineWidth, width, Height, RowWise, nil);
end;

function TDoubleMatrix.Std(RowWise: boolean; unbiased : boolean = True): TDoubleMatrix;
begin
     Result := Variance(RowWise, unbiased);
     Result.SQRTInPlace;
end;

function TDoubleMatrix.StdD(RowWise, unbiased: boolean): IDoubleMatrix;
begin
     Result := Std(RowWise, unbiased);
end;

function TDoubleMatrix.StdI(RowWise, unbiased: boolean): IMatrix;
begin
     Result := Std(RowWise, unbiased);
end;

procedure TDoubleMatrix.StdInPlace(RowWise: boolean; unbiased : boolean = True);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Std(RowWise, unbiased);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.Variance(RowWise: boolean; unbiased : boolean = True): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          MatrixVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          MatrixVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end;
end;

function TDoubleMatrix.VarianceD(RowWise, unbiased: boolean): IDoubleMatrix;
begin
     Result := Variance(RowWise, Unbiased);
end;

function TDoubleMatrix.VarianceI(RowWise, unbiased: boolean): IMatrix;
begin
     Result := Variance(RowWise, unbiased);
end;

procedure TDoubleMatrix.VarianceInPlace(RowWise: boolean; unbiased : boolean = True);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Variance(RowWise, unbiased);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.Min: double;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := MatrixMin(StartElement, fSubWidth, fSubHeight, LineWidth);
end;

function TDoubleMatrix.Mult(Value: TDoubleMatrix): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubWidth, fSubHeight);

     MatrixMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth);
end;

function TDoubleMatrix.MultD(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := Mult(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.MultInPlace(Value: TDoubleMatrix);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');
     CheckAndRaiseError((fSubWidth = Value.fSubHeight), 'Dimension error');

     dl := Mult(Value);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

procedure TDoubleMatrix.MultInPlaceD(Value: IDoubleMatrix);
begin
     MultInPlace(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.MultInPlaceT1(Value: TDoubleMatrix);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');

     dl := MultT1(Value);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

procedure TDoubleMatrix.MultInPlaceT1D(Value: IDoubleMatrix);
begin
     MultInPlaceT1(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.MultInPlaceT2(Value: TDoubleMatrix);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');

     dl := MultT2(Value);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.MultT1(Value: TDoubleMatrix): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubWidth, fSubWidth);

     MatrixMultT1Ex(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth, BlockMatrixCacheSize, doNone, nil);
end;

function TDoubleMatrix.MultT1D(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := MultT1(Value.GetObjRef as TDoubleMatrix);
end;

function TDoubleMatrix.MultT2(Value: TDoubleMatrix): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubHeight, fSubHeight);

     MatrixMultT2Ex(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth, BlockMatrixCacheSize, doNone, nil);
end;

function TDoubleMatrix.MultT2D(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := MultT2(Value.GetObjRef as TDoubleMatrix);
end;

function TDoubleMatrix.Scale(const Value: double): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     MatrixAddAndScale(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, 0, Value);
end;

function TDoubleMatrix.ScaleI(const Value: double): IMatrix;
begin
     Result := ScaleAndAdd(0, Value);
end;

function TDoubleMatrix.ScaleAndAdd(const aOffset, aScale: double): TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.AssignEx(self, True);

     Result.ScaleAndAddInPlace(aOffset, aScale);
end;

function TDoubleMatrix.ScaleAndAddD(const aOffset,
  aScale: double): IDoubleMatrix;
begin
     Result := ScaleAndAdd(aOffset, aScale);
end;

function TDoubleMatrix.ScaleAndAddI(const aOffset, aScale: double): IMatrix;
begin
     Result := ScaleAndAdd( aOffset, aScale );
end;

procedure TDoubleMatrix.ScaleAndAddInPlace(const aOffset, aScale: double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixScaleAndAdd(StartElement, LineWidth, fSubWidth, fSubHeight, aOffset, aScale);
end;

function TDoubleMatrix.ScaleD(const Value: double): IDoubleMatrix;
begin
     Result := Scale(Value);
end;

procedure TDoubleMatrix.ScaleInPlace(const Value: Double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'Dimension error');

     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, 0, Value);
end;

procedure TDoubleMatrix.Normalize(RowWise: boolean);
var dt : TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');

     dt := ResultClass.Create(Width, Height);
     try
        MatrixNormalize(dt.StartElement, dt.LineWidth, StartElement, LineWidth, Width, Height, RowWise);
        TakeOver(dt);
     finally
            dt.Free;
     end;
end;

procedure TDoubleMatrix.NormZeroMeanVarOne(RowWise : boolean);
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');
     MatrixNormalizeMeanVar(StartElement, LineWidth, width, height, RowWise);
end;

class function TDoubleMatrix.NumberType: TNumberType;
begin
     Result := ntDouble;
end;

function TDoubleMatrix.PseudoInversion(out mtx : TDoubleMatrix) : TSVDResult;
var tmp : TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');

     mtx := ResultClass.Create( height, width );
     tmp := ResultClass.Create;
     try
        tmp.Assign(self);
        
        Result := MatrixPseudoinverse2Ex( mtx.StartElement, mtx.LineWidth, tmp.StartElement, tmp.LineWidth, Width, Height, 
                                        StartElement, LineWidth, fLinEQProgress );

        if Result <> srOk then
           FreeAndNil(mtx);

        FreeAndNil(tmp);
           
        mtx.fLinEQProgress := nil;
     except
           FreeAndNil(mtx);
           FreeAndNil(tmp);

           raise;
     end;
end;

function TDoubleMatrix.PseudoInversion(out Mtx: IDoubleMatrix): TSVDResult;
var outMtx : TDoubleMatrix;
begin
     Result := PseudoInversion(outMtx);
     Mtx := outMtx;
end;

function TDoubleMatrix.PseudoInversionInPlace: TSVDResult;
var mtx : TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');

     mtx := ResultClass.Create(height, width);
     try
        Result := MatrixPseudoinverse2(mtx.StartElement, mtx.LineWidth, StartElement, LineWidth, Width, Height, fLinEQProgress);

        if Result = srOk then
           TakeOver(mtx);
     finally
            mtx.Free;
     end;
end;

procedure TDoubleMatrix.QR(out R: IDoubleMatrix);
var outR : TDoubleMatrix;
begin
     QR(outR);
     R := outR;
end;

procedure TDoubleMatrix.QR(out R: TDoubleMatrix);
var tmp : TDoubleMatrix;
    i, j : integer;
begin
     QR(R, tmp);
     tmp.Free;

     R.Resize( Width, Math.Min(Width, Height) );
     // clear lower triangular matrix of R
     for i := 1 to R.Height - 1 do
          for j := 0 to i - 1 do
              R[j, i] := 0;
end;

procedure TDoubleMatrix.QR(out ecosizeR: IDoubleMatrix; out tau : IDoubleMatrix);
var outR : TDoubleMatrix;
    outTau : TDoubleMatrix;
begin
     QR(outR, outTau);
     ecosizeR := outR;
     tau := outTau;
end;

procedure TDoubleMatrix.QRFull(out Q, R: TDoubleMatrix);
var tau : TDoubleMatrix;
    tmp : TDoubleMatrix;
    x, y : integer;
    pdata : PConstDoubleArr;
begin
     Q := nil;
     R := nil;
     tau := nil;

     // ###########################################
     // #### First create a compact QR decomposition
     QR(R, Tau);

     // ###########################################
     // #### Calculation Q from the previous operation
     tmp := ResultClass.Create;
     try
        tmp.Assign(R);
     
        MatrixQFromQRDecomp(tmp.StartElement, tmp.LineWidth, Width, Height, tau.StartElement, fLinEQProgress);

        // now assign only the relevant parts of Q (or just copy it if we have a square matrix)
        if Width = height 
        then
            Q := tmp
        else
        begin
             // economy size Q:
             tmp.SetSubMatrix(0, 0, Math.min(tmp.Width, tmp.Height), tmp.Height);
          
             Q := ResultClass.Create;
             Q.AssignEx(tmp, True);
             tmp.Free;
             
             tmp := R;
             tmp.SetSubMatrix(0, 0, tmp.Width, Math.Min(tmp.Width, tmp.Height));
             R := ResultClass.Create;
             R.AssignEx(tmp, True);
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

procedure TDoubleMatrix.QRFull(out Q, R: IDoubleMatrix);
var outR, outQ : TDoubleMatrix;
begin
     QRFull(outQ, outR);
     Q := outQ;
     R := outR;
end;

procedure TDoubleMatrix.QR(out ecosizeR: TDoubleMatrix; out tau : TDoubleMatrix);
begin
     // note: the QR decomposition here does not update the resulting matrix size -> it is further used in the Q decomposition and that memory
     // is needed there. So in any subsequent call one needs to adjust the size self. Also the lower triangular data is not destroyed and set to 0!
     ecosizeR := Clone;
     tau := ResultClass.Create(width, 1);
     try
        MatrixQRDecompInPlace2(ecosizeR.StartElement, ecosizeR.LineWidth, width, height, tau.StartElement, fLinEQProgress);
     except
           FreeAndNil(ecosizeR);
           FreeAndNil(tau);

           raise;
     end;
end;

// ###########################################
// #### Hessenberg decomponsition
// ###########################################

procedure TDoubleMatrix.Hess( out h : TDoubleMatrix );
var tau : TDoubleMatrix;
    x, y : integer;
    pData : PConstDoubleArr;
begin
     CheckAndRaiseError( Width = Height, 'Hessenberg decomposition only allowed on square matrices');

     Hess( h, tau );

     tau.Free;
     // clear h so we only have an upper (+1) triangle matrix
     // zero out the parts occupied by Q
     for y := 2 to h.Height - 1 do
     begin
          pData := PConstDoubleArr( h.StartElement );
          inc(PByte(pData), y*h.LineWidth);
          for x := 0 to y - 2 do
              pData^[x] := 0;
     end;
end;

procedure TDoubleMatrix.Hess( out h : IDoubleMatrix );
var tmp, tau : TDoubleMatrix;
    x, y : integer;
    pData : PConstDoubleArr;
begin
     CheckAndRaiseError( Width = Height, 'Hessenberg decomposition only allowed on square matrices');

     Hess( tmp, tau );
     tau.Free;
     h := tmp;

     // clear h so we only have an upper (+1) triangle matrix
     // zero out the parts occupied by Q
     for y := 2 to h.Height - 1 do
     begin
          pData := PConstDoubleArr( h.StartElement );
          inc(PByte(pData), y*h.LineWidth);
          for x := 0 to y - 2 do
              pData^[x] := 0;
     end;
end;

// for the full transformation: Q*H*Q' = A
procedure TDoubleMatrix.HessFull(out Q, h : TDoubleMatrix);
var tau : TDoubleMatrix;
    x, y : integer;
    pdata : PConstDoubleArr;
begin
     CheckAndRaiseError( Width = Height, 'Hessenberg decomposition only allowed on square matrices');
     
     Q := nil;
     h := nil;
     Tau := nil;

     // ###########################################
     // #### First create a compact QR decomposition
     Hess(h, Tau);

     // ###########################################
     // #### Calculation Q from the previous operation
     Q := h.Clone;
     try
        MatrixQFromHessenbergDecomp(Q.StartElement, Q.LineWidth, Width, tau.StartElement, fLinEQProgress);
        
        // clear h so we only have an upper (+1) triangle matrix
        // zero out the parts occupied by Q
        for y := 2 to h.Height - 1 do
        begin
             pData := PConstDoubleArr( h.StartElement );
             inc(PByte(pData), y*h.LineWidth);
             for x := 0 to y - 2 do
                 pData^[x] := 0;
        end;
     finally
            Tau.Free;
     end;
end;

procedure TDoubleMatrix.Hess(out h, tau: TDoubleMatrix);
begin
     tau := ResultClass.Create( width, 1 );
     h := Clone;
     try
        MatrixHessenberg2InPlace( h.StartElement, h.LineWidth, Width, tau.StartElement, HessBlockSize );
     except
           FreeAndNil(tau);
           FreeAndNil(h);
           
           raise;
     end;
end;

procedure TDoubleMatrix.HessFull(out Q, h : IDoubleMatrix);
var temp1, temp2 : TDoubleMatrix;
begin
     HessFull(temp1, temp2);
     Q := temp1;
     h := temp2;
end;

function TDoubleMatrix.RepeatMatrix(numX, numy : integer): TDoubleMatrix;
begin
     Result := Clone;
     Result.RepeatMatrixInPlace(numX, numY);
end;

function TDoubleMatrix.RepeatMatrixD(numX, numY: integer): IDoubleMatrix;
begin
     Result := RepeatMatrix(numX, numY);
end;

function TDoubleMatrix.RepeatMatrixI(numX, numY: integer): IMatrix;
begin
     Result := RepeatMatrix(numX, numY);
end;

procedure TDoubleMatrix.RepeatMatrixInPlace(numX, numY: integer);
var origW, origH : integer;
    aSubMtx : IDoubleMatrix;
    x, y : Integer;
begin
     origW := Width;
     origH := Height;

     aSubMtx := Clone;

     SetWidthHeight( Width*numX, Height*numY );

     for y := 0 to numY - 1 do
         for x := 0 to numX - 1 do
             AssignSubMatrix(aSubMtx.GetObjRef as TDoubleMatrix, x*origW, y*origH);
end;

function TDoubleMatrix.Reshape(newWidth, newHeight: integer; ColMajor : boolean = False) : TDoubleMatrix;
var xOld, yOld : integer;
    x, y : Integer;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Error operation not allowed on empty matrices');
     CheckAndRaiseError((newWidth*newHeight) = (fSubWidth*fSubHeight), 'Error new dimension does not fit into the old one');

     Result := ResultClass.Create(newWidth, newHeight);
     try
        if ColMajor then
        begin
             // reshape along columns not rows...
             xOld := 0;
             yOld := 0;
             for x := 0 to newWidth - 1 do
             begin
                  for y := 0 to newHeight - 1 do
                  begin
                       Result[x, y] := Items[xOld, yOld];

                       inc(yOld);
                       if yOld = Height then
                       begin                       
                            inc(xOld);
                            yOld := 0;
                       end;
                  end;
             end;
        end
        else
        begin
             // reshape along rows not columns...
             xOld := 0;
             yOld := 0;
             for y := 0 to newheight - 1 do
             begin
                  for x := 0 to newWidth - 1 do
                  begin
                       Result[x, y] := Items[xOld, yOld];

                       inc(xOld);
                       if xOld = Width then
                       begin                       
                            inc(yOld);
                            xOld := 0;
                       end;
                  end;
             end;
        end;
     except
           FreeAndNil(Result);

           raise;
     end;
end;

function TDoubleMatrix.ReshapeD(newWidth, newHeight: integer;
  RowMajor: boolean): IDoubleMatrix;
begin
     Result := Reshape(newWidth, newHeight, RowMajor);
end;

function TDoubleMatrix.ReshapeI(newWidth, newHeight: integer;
  RowMajor: boolean): IMatrix;
begin
     Result := Reshape( newWidth, newHeight, RowMajor);
end;

procedure TDoubleMatrix.ReshapeInPlace(newWidth, newHeight: integer; ColMajor : boolean = False);
var res : TDoubleMatrix;
begin
     CheckAndRaiseError((fWidth > 0) and (fHeight > 0), 'Error operation not allowed on empty matrices');
     CheckAndRaiseError((fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');
     CheckAndRaiseError((newWidth*newHeight) = (fWidth*fHeight), 'Error new dimension does not fit into the old one');

     // check if the line width fits the old width -> then we only have to adjust the
     // line width parameter, otherwise copy. 
     if (fWidth = fSubWidth) and (fHeight = fSubHeight) and (fLineWidth = fWidth*sizeof(double)) and not ColMajor
     then
         fLineWidth := newWidth*sizeof(double)
     else
     begin
          res := Reshape(newWidth, newHeight, ColMajor);
          TakeOver(res);
          res.Free;
     end;

     fWidth := newWidth;
     fHeight := newHeight;
     fSubWidth := newWidth;
     fSubHeight := newHeight;
     fOffsetX := 0;
     fOffsetY := 0;
end;

class function TDoubleMatrix.ResultClass: TDoubleMatrixClass;
begin
     Result := TDoubleMatrix;
end;

procedure TDoubleMatrix.SetColumn(col : integer; Values: TDoubleMatrix; ValCols : integer);
begin
     CheckAndRaiseError(Values.fSubHeight = fSubHeight, 'Dimension error');
     CheckAndRaiseError((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

     MatrixCopy( GenPtr( StartElement, col, 0, LineWidth ), LineWidth,
                 GenPtr( Values.StartElement, ValCols, 0, Values.LineWidth ), values.LineWidth,
                 1, fSubHeight);
end;

procedure TDoubleMatrix.SetColumnI(col: integer; Values: IMatrix;
  ValCols: integer);
begin
     CheckForRealMtx(Values);

     SetColumn(col, Values.GetObjRef as TDoubleMatrix, ValCols);
end;

procedure TDoubleMatrix.SetData(data: PDouble; srcLineWidth, width,
  height: integer);
begin
     ReserveMem(width, height);

     MatrixCopy(StartElement, LineWidth, data, srcLineWidth, width, height);
end;

procedure TDoubleMatrix.SetColumn(col : integer; const Values: array of Double);
begin
     CheckAndRaiseError(fSubHeight = Length(Values), 'Dimension error');
     CheckAndRaiseError((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

     MatrixCopy( GenPtr( StartElement, col, 0, LineWidth ), LineWidth,
                 @Values[0], sizeof(double),
                 1, fsubHeight );
end;

procedure TDoubleMatrix.SetRow(row : integer; Values: TDoubleMatrix; ValRow : integer);
begin
     CheckAndRaiseError(Values.Width = fSubWidth, 'Dimension Error');
     CheckAndRaiseError((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     MatrixCopy( GenPtr( StartElement, 0, row, LineWidth), LineWidth,
                 GenPtr(Values.StartElement, 0, ValRow, Values.LineWidth), VAlues.LineWidth,
                 fSubWidth, 1 );
end;

procedure TDoubleMatrix.SetRow(row : integer; const Values: array of Double);
begin
     CheckAndRaiseError(Length(Values) = fSubWidth, 'Dimension Error');
     CheckAndRaiseError((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     MatrixCopy( GenPtr( StartElement, 0, row, LineWidth), LineWidth,
                 @Values[0], fSubWidth*sizeof(double),
                 fSubWidth, 1 );
end;

procedure TDoubleMatrix.SetValue(const initVal: double);
begin
     if (Width > 0) and (height > 0) then
        MatrixInit( StartElement, LineWidth, width, height, initVal);
end;

procedure TDoubleMatrix.SetValueSubMtx(const initVal : double; x, y, aWidth, aHeight : NativeInt);
begin
     CheckAndRaiseError( Width >= x + aWidth, 'Dimension X Error');
     CheckAndRaiseError( Height >= y + aHeight, 'Dimension Y Error');

     MatrixInit( GenPtr( StartElement, x, y, LineWidth ), LineWidth, aWidth, aHeight, initVal);
end;

procedure TDoubleMatrix.InitRandom(method : TRandomAlgorithm; var seed : LongInt);
begin
     fObj := TRandomGenerator.Create;
     TRandomGenerator(fObj).RandMethod := method;
     TRandomGenerator(fObj).Init(seed);
     MatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, {$IFDEF FPC}@{$ENDIF}MtxRandWithEng);
     seed := TRandomGenerator(fObj).RandInt;
     FreeAndNil(fObj);
end;

function TDoubleMatrix.Sub(Value: TDoubleMatrix): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(fSubWidth, fSubHeight);
     MatrixSub(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.SubInPlace(Value: TDoubleMatrix);
begin
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');
     MatrixSub(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.SubInPlaceD(Value: IDoubleMatrix);
begin
     SubInPlace(Value.GetObjRef as TDoubleMatrix);
end;

procedure TDoubleMatrix.SubVecInPlace(Value: TDoubleMatrix; rowWise : Boolean);
var incX : NativeInt;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     if rowWise
     then
         CheckAndRaiseError((Value.VecLen = Width), 'Arguments dimension error')
     else
         CheckAndRaiseError((Value.VecLen = Height), 'Arguments dimension error');

     incX := sizeof(double);

     if value.Width = 1 then
        incX := value.LineWidth;

     MatrixSubVec(StartElement, LineWidth, Value.StartElement, incX, Width, Height, RowWise);
end;

function TDoubleMatrix.SubVec(aVec: TDoubleMatrix; rowWise : Boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     if rowWise
     then
         CheckAndRaiseError((aVec.VecLen = Width), 'Arguments dimension error')
     else
         CheckAndRaiseError((aVec.VecLen = Height), 'Arguments dimension error');

     Result := Clone;
     Result.SubVecInPlace(aVec, rowWise);
end;

function TDoubleMatrix.SubVecD(aVec: IDoubleMatrix;
  rowWise: Boolean): IDoubleMatrix;
begin
     Result := SubVec(aVec.GetObjRef as TDoubleMatrix, rowWise);
end;

//function TDoubleMatrix.SubVecI(aVec: IMatrix; rowWise: Boolean): IMatrix;
//begin
//     CheckForRealMtx(aVec);
//
//     Result := SubVec(aVec.GetObjRef as TDoubleMatrix, rowWise);
//end;
//
procedure TDoubleMatrix.SubVecInPlaceD(Value: IDoubleMatrix; rowWise: Boolean);
begin
     SubVecInPlace(Value.GetObjRef as TDoubleMatrix, rowWise);
end;

function TDoubleMatrix.SubMatrix: TDoubleDynArray;
begin
     Result := nil;
     if fSubWidth*fSubHeight = 0 then
        exit;

     SetLength(Result, fSubWidth*fSubHeight);
     MatrixCopy( @Result[0], fSubWidth*sizeof(double), StartElement, LineWidth, fSubWidth, fSubHeight);
end;

//function TDoubleMatrix.SubI(Value: IMatrix): IMatrix;
//begin
//     CheckForRealMtx(Value);
//
//     Result := Sub(Value.GetObjRef as TDoubleMatrix);
//end;

function TDoubleMatrix.SubMtxI(fromColIdx, ToColIdx, fromRowIdx,
  ToRowIdx: integer): IMatrix;
begin
     Result := SubMtx( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx);
end;

function TDoubleMatrix.SubMtxIdxI(colIdx, rowIdx: TIntegerDynArray): IMatrix;
begin
     Result := SubMtxIdx(colIdx, rowIdx);
end;

function TDoubleMatrix.SubColMtx(fromIdx, toIdx: integer): TDoubleMatrix;
var colIdx : TIntegerDynArray;
    i: Integer;
begin
     CheckAndRaiseError( (fromIdx >= 0) and (fromIdx < width), 'Column Index out of bounds');
     CheckAndRaiseError( (ToIdx >= 0) and (toIdx < width), 'Column Index out of bounds');

     if toIdx >= fromIdx then
     begin
          SetLength(colIdx, toIdx - fromIdx + 1);
          for i := 0 to Length(colIdx) - 1 do
              colIdx[i] := fromIdx + i;
     end
     else
     begin
          SetLength(colIdx, fromIdx - toIdx + 1);
          for i := 0 to Length(colIdx) - 1 do
              colIdx[i] := fromIdx - i;
     end;
     Result := SubMtxIdx(colIdx, nil );
end;

function TDoubleMatrix.SubColMtxD(fromIdx, toIdx: integer): IDoubleMatrix;
begin
     Result := SubColMtx(fromIdx, toIdx);
end;

function TDoubleMatrix.SubRowMtx(fromIdx, toIdx: integer): TDoubleMatrix;
var rowIdx : TIntegerDynArray;
    i: Integer;
begin
     CheckAndRaiseError( (fromIdx >= 0) and (fromIdx < Height), 'Row Index out of bounds');
     CheckAndRaiseError( (fromIdx >= 0) and (fromIdx < Height), 'Row Index out of bounds');

     if toIdx >= fromIdx then
     begin
          SetLength(rowIdx, toIdx - fromIdx + 1);
          for i := 0 to Length(rowIdx) - 1 do
              rowIdx[i] := fromIdx + i;
     end
     else
     begin
          SetLength(rowIdx, fromIdx - toIdx + 1);
          for i := 0 to Length(rowIdx) - 1 do
              rowIdx[i] := fromIdx - i;
     end;
     Result := SubMtxIdx(nil, rowIdx);
end;


function TDoubleMatrix.SubRowMtxD(fromIdx, toIdx: integer): IDoubleMatrix;
begin
     Result := SubRowMtx(fromIdx, toIdx);
end;

function TDoubleMatrix.SubMtx(fromColIdx, ToColIdx, fromRowIdx,
  ToRowIdx: integer): TDoubleMatrix;
var colIdx : TIntegerDynArray;
    rowIdx : TIntegerDynArray;
    increment : integer;
    i : integer;
begin
     CheckAndRaiseError( (fromColIdx >= 0) and (fromColIdx < width), 'Column Index out of bounds');
     CheckAndRaiseError( (ToColIdx >= 0) and (toColIdx < width), 'Column Index out of bounds');
     CheckAndRaiseError( (fromRowIdx >= 0) and (fromRowIdx < Height), 'Row Index out of bounds');
     CheckAndRaiseError( (fromRowIdx >= 0) and (fromRowIdx < Height), 'Row Index out of bounds');

     SetLength(colIdx, System.Abs(ToColIdx - fromColIdx) + 1);
     SetLength(rowIdx, System.Abs(ToRowIdx - fromRowIdx) + 1);

     increment := 1;
     if ToColIdx < fromColIdx then
        increment := -1;
     for i := 0 to Length(colIdx) - 1 do
         colIdx[i] := fromColIdx + i*increment;

     increment := 1;
     if ToRowIdx < fromRowIdx then
        increment := -1;
     for i := 0 to Length(rowIdx) - 1 do
         rowIdx[i] := fromRowIdx + i*increment;

     Result := SubMtxIdx(colIdx, rowIdx);
end;

function TDoubleMatrix.SubMtxD(fromColIdx, ToColIdx, fromRowIdx,
  ToRowIdx: integer): IDoubleMatrix;
begin
     Result := SubMtx(fromColIdx, ToColIdx, fromRowIdx, ToRowIdx);
end;

function TDoubleMatrix.SubRowMtxIdxI(rowIdx: TIntegerDynArray): IMatrix;
begin
     Result := SubRowMtxIdx(rowIdx);
end;

function TDoubleMatrix.SubRowMtxI(fromIdx, toIdx: integer): IMatrix;
begin
     Result := SubRowMtx(fromIdx, toIdx);
end;

function TDoubleMatrix.SubColMtxIdx(colIdx: TIntegerDynArray): TDoubleMatrix;
begin
     Result := SubMtxIdx(colIdx, nil);
end;

function TDoubleMatrix.SubColMtxIdxD(colIdx: TIntegerDynArray): IDoubleMatrix;
begin
     Result := SubColMtxIdx(colIdx);
end;

function TDoubleMatrix.SubColMtxIdxI(colIdx: TIntegerDynArray): IMatrix;
begin
     Result := SubColMtxIdx(colIdx);
end;

function TDoubleMatrix.SubD(Value: IDoubleMatrix): IDoubleMatrix;
begin
     Result := Sub( Value.GetObjRef as TDoubleMatrix );
end;

function TDoubleMatrix.SubMtxIdx(colIdx, rowIdx: TIntegerDynArray): TDoubleMatrix;
var i : integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'Not allowed on an empty matrix');
     // ###########################################
     // #### A nil value in the parameter actually select all
     if colIdx = nil then
     begin
          SetLength( colIdx, Width );
          for i := 0 to Length(colIdx) - 1 do
              colIdx[i] := i;
     end;
     if rowIdx = nil then
     begin
          SetLength( rowIdx, Height );
          for i := 0 to Length(rowIdx) - 1 do
              rowIdx[i] := i;
     end;

     // ###########################################
     // #### Check if the index is out of bounds
     for i := 0 to Length(colIdx) - 1 do
         CheckAndRaiseError( (colIdx[i] >= 0) and (colIdx[i] < Width), 'Column index out of bounds');
     for i := 0 to Length(rowIdx) - 1 do
         CheckAndRaiseError( (rowIdx[i] >= 0) and (rowIdx[i] < Height), 'Column index out of bounds');

     Result := ResultClass.Create( Length(colIdx), Length(rowIdx));

     MatrixIndex(Result.StartElement, Result.LineWidth, StartElement, LineWidth, colIdx, rowIdx);
end;

function TDoubleMatrix.SubMtxIdxD(colIdx,
  rowIdx: TIntegerDynArray): IDoubleMatrix;
begin
     Result := SubMtxIdx(colIdx, rowIdx);
end;

function TDoubleMatrix.SubRowMtxIdx(rowIdx: TIntegerDynArray): TDoubleMatrix;
begin
     Result := SubMtxIdx( nil, rowIdx );
end;

function TDoubleMatrix.SubRowMtxIdxD(rowIdx: TIntegerDynArray): IDoubleMatrix;
begin
     Result := SubRowMtxIdx(rowIdx);
end;

function TDoubleMatrix.Sum(RowWise: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          MatrixSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          MatrixSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end;
end;

function TDoubleMatrix.SumD(RowWise: boolean): IDoubleMatrix;
begin
     Result := Sum(RowWise);
end;

function TDoubleMatrix.SumAll: double;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := MatrixSum(StartElement, LineWidth, width, height);
end;

function TDoubleMatrix.SumI(RowWise: boolean): IMatrix;
begin
     Result := Sum(RowWise);
end;

procedure TDoubleMatrix.SumInPlace(RowWise, keepMemory: boolean);
begin
     if not keepMemory 
     then
         SumInPlace(RowWise)
     else
     begin
          CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

          // inplace sum + do not change the 
          if RowWise then
          begin
               MatrixSum(PDouble(fData), LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
               fOffsetX := 0;
               fSubWidth := 1;
          end
          else
          begin
               MatrixSum(PDouble(fData), LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
               fOffsetY := 0;
               fSubHeight := 1;
          end;
     end;
end;

procedure TDoubleMatrix.SumInPlace(RowWise: boolean);
var dl : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Sum(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TDoubleMatrix.SVD(out U, V, W: IDoubleMatrix; onlyDiagElements: boolean): TSVDResult;
var uObj, vObj, wObj : TDoubleMatrix;
begin
     Result := SVD(uObj, vObj, wObj, onlyDiagElements);
     U := uObj;
     V := vObj;
     W := wObj;
end;

procedure TDoubleMatrix.SwapColumn(idx1, idx2: integer);
begin
     CheckAndRaiseError((idx1 >= 0) and (idx1 < Width), 'Dimension error');
     CheckAndRaiseError((idx2 >= 0) and (idx2 < Width), 'Dimension error');

     if idx1 <> idx2 then
        MatrixColSwap( GenPtr( StartElement, idx1, 0, LineWidth ),
                       GenPtr( StartElement, idx2, 0, LineWidth ),
                       LineWidth,
                       Height
                     );

end;

procedure TDoubleMatrix.SwapRow(idx1, idx2: integer);
begin
     CheckAndRaiseError((idx1 >= 0) and (idx1 < Height), 'Dimension error');
     CheckAndRaiseError((idx2 >= 0) and (idx2 < Height), 'Dimension error');

     if idx1 <> idx2 then
        MatrixRowSwap( GenPtr( StartElement, 0, idx1, LineWidth ),
                       GenPtr( StartElement, 0, idx2, LineWidth ),
                       Width
                     );
end;

function TDoubleMatrix.SVD(out U, V, W: TDoubleMatrix; onlyDiagElements : boolean): TSVDResult;
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

             Result := MatrixSVDInPlace2(V.StartElement, V.LineWidth, Height, Width, pW, U.StartElement, U.LineWidth,
                                         SVDBlockSize, fLinEQProgress );

             // we need a final transposition on the matrix U and V
             U.TransposeInPlace;
             V.TransposeInPlace;
        end
        else
        begin
             U := Clone;
             V := ResultClass.Create(fSubWidth, fSubWidth);

             Result := MatrixSVDInPlace2Ex(U.StartElement, U.LineWidth, Width, Height, pW, V.StartElement, V.LineWidth,
                                         StartElement, LineWidth,
                                         SVDBlockSize, fLinEQProgress);
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

function TDoubleMatrix.SymEig(out EigVals: TDoubleMatrix): TEigenvalueConvergence;
var dt : TDoubleMatrix;
    vecs : TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubWidth = fSubHeight), 'Eigenvalue calculation only allowed on square matrices');

     EigVals := nil;

     dt := ResultClass.Create(1, fSubHeight);
     vecs := ResultClass.Create(fSubWidth, fSubWidth);
     try
        vecs.AssignEx(self, True);
        Result := MatrixEigUpperSymmetricMatrixInPlace2(vecs.StartElement, vecs.LineWidth, fSubWidth,
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

function TDoubleMatrix.SymEig(out EigVals,
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
        vecs.AssignEx(self, True);
        Result := MatrixEigUpperSymmetricMatrixInPlace2(vecs.StartElement, vecs.LineWidth, fSubWidth,
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

procedure TDoubleMatrix.TakeOver(Value: TDoubleMatrix);
begin
     DoTakeOver(Value);
end;

procedure TDoubleMatrix.TakeOverD(Value: IDoubleMatrix);
begin
     TakeOver(Value.GetObjRef as TDoubleMatrix);
end;

function TDoubleMatrix.Trace: double;
var i : Integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Width = Height), 'Trace only defined on square matrices');

     Result := 0;
     for i := 0 to Width - 1 do
         Result := Result + Items[i, i];
end;

function TDoubleMatrix.Transpose : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := ResultClass.Create(fSubHeight, fSubWidth);
     MatrixTranspose(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TDoubleMatrix.TransposeD: IDoubleMatrix;
begin
     Result := Transpose;
end;

function TDoubleMatrix.TransposeI: IMatrix;
begin
     Result := Transpose;
end;

procedure TDoubleMatrix.TransposeInPlace;
var dt : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     // transpose is only allowed on full matrix
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fSubWidth = fWidth) and (fSubHeight = fHeight), 'Operation only allowed on full matrices');

     // in case of vectors we can fasten things up a bit
     if (fwidth = 1) and (LineWidth = sizeof(double)) then
     begin
          fWidth := fHeight;
          fSubWidth := fSubHeight;
          fHeight := 1;
          fSubHeight := 1;
          fLineWidth := fWidth*sizeof(double);
          exit;
     end;

     if fHeight = 1 then
     begin
          fHeight := fWidth;
          fSubHeight := fSubWidth;
          fWidth := 1;
          fSubWidth := 1;
          fLineWidth := sizeof(double);
          exit;
     end;

     // ###########################################
     // #### Standard transpose
     dt := Transpose;
     try
        TakeOver(dt);
     finally
            dt.Free;
     end;
end;

// ###########################################################
// #### Persistence functions
procedure TDoubleMatrix.DefineProps;
var origSubWidth, origSubHeight, origOffsetX, origOffsetY : integer;
begin
     origSubWidth := fSubWidth;
     origSubHeight := fSubHeight;
     origOffsetX := fOffsetX;
     origOffsetY := fOffsetY;

     inherited;

     // now store the data
     UseFullMatrix;
     AddDoubleArr('data', SubMatrix);
     SetSubMatrix(origOffsetX, origOffsetY, origSubWidth, origSubHeight);
end;

function TDoubleMatrix.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, 'data') = 0
     then
         Result := ptDouble
     else
         Result := inherited PropTypeOfName(Name);
end;


procedure TDoubleMatrix.OnLoadDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     if (CompareText(Name, 'data') = 0) and (Length(Value) > 0) then
     begin
          CheckAndRaiseError(Length(value) = fWidth*fHeight, 'The loaded array does not fit the expected size.');
          Assign(Value, fWidth, fHeight);
     end;
end;

class procedure TDoubleMatrix.CheckForRealMtx(mtx: IMatrix);
begin
     if not (mtx.GetObjRef is TDoubleMatrix) then
        raise EMatrixTypeException.Create('Operation only allowed on real matrices');
end;

function TDoubleMatrix.Cholesky(out Chol: IDoubleMatrix): TCholeskyResult;
var outChol : TDoubleMatrix;
begin
     Result := Cholesky(outChol);
     Chol := outChol;
end;

class function TDoubleMatrix.ClassIdentifier: String;
begin
     Result := 'MTX';
end;

function TDoubleMatrix.CloneI: IMatrix;
begin
     Result := Clone;
end;

function TDoubleMatrix.Clone: TDoubleMatrix;
begin
     Result := ResultClass.Create(Width, Height);
     Result.AssignEx(Self, True);
end;

function TDoubleMatrix.CloneD: IDoubleMatrix;
begin
     Result := Clone;
end;

procedure TDoubleMatrix.SetRowI(row: integer; Values: IMatrix; ValRow: integer);
begin
     CheckForRealMtx(Values);

     SetRow( row, Values.GetObjRef as TDoubleMatrix, ValRow );
end;

//function TDoubleMatrix.MultI(Value: IMatrix): IMatrix;
//begin
//     CheckForRealMtx(Value);
//
//     Result := Mult(value.GetObjRef as TDoubleMatrix);
//end;
//
//procedure TDoubleMatrix.MultInPlaceI(Value: IMatrix);
//begin
//     CheckForRealMtx(Value);
//
//     MultInPlace(Value as TDoubleMatrix);
//end;

//procedure TDoubleMatrix.MultInPlaceT1D(Value: IDoubleMatrix);
//begin
//     MultInPlaceT1(Value.GetObjRef as TDoubleMatrix);
//end;

procedure TDoubleMatrix.MultInPlaceT2D(Value: IDoubleMatrix);
begin
     MultInPlaceT2(Value.GetObjRef as TDoubleMatrix);
end;

//function TDoubleMatrix.MultT1D(Value: IDoubleMatrix): IDoubleMatrix;
//begin
//     Result := MultT1(Value.GetObjRef as TDoubleMatrix);
//end;

//function TDoubleMatrix.MultT2D(Value: IDoubleMatrix): IDoubleMatrix;
//begin
//     Result := MultT2(Value.GetObjRef as TDoubleMatrix);
//end;

function TDoubleMatrix.SubColMtxI(fromIdx, toIdx: integer): IMatrix;
begin
     Result := SubColMtx(fromIdx, toIdx);
end;

//procedure TDoubleMatrix.SubInPlaceI(Value: IMatrix);
//begin
//     CheckForRealMtx(Value);
//
//     SubInPlace(value.GetObjRef as TDoubleMatrix);
//end;

function TDoubleMatrix.SymEig(out EigVals, EigVect: IDoubleMatrix): TEigenvalueConvergence;
var outEigVals, outEigVect : TDoubleMatrix;
begin
     Result := SymEig(outEigVals, outEigVect);
     EigVals := outEigVals;
     EigVect := outEigVect;
end;

function TDoubleMatrix.SymEig(out EigVals: IDoubleMatrix): TEigenvalueConvergence;
var outEigVals : TDoubleMatrix;
begin
     Result := SymEig(outEigVals);
     EigVals := outEigVals;
end;

// ###########################################
// #### Utility Functions to read and write simple textual based matrices
// ###########################################

procedure WriteBinary(const fn : string; mtx : IDoubleMatrix);
var vec : IDoubleMatrix;
begin
     vec := mtx.AsVector(True) as IDoubleMatrix;
     with TFileStream.Create(fn, fmOpenWrite or fmCreate) do
     try
        WriteBuffer( vec.SubMatrix[0], sizeof(double)*vec.VecLen);
     finally
            Free;
     end;
end;

procedure MatrixToTxtFile(const fileName : string; const mtx : TDoubleMatrix; prec : integer = 8);
var s : UTF8String;
    x, y : integer;
    ft : TFormatSettings;    
begin
     ft := GetLocalFMTSet;

     ft.DecimalSeparator := '.';

     with TFileStream.Create(fileName, fmCreate or fmOpenWrite) do
     try
        for y := 0 to mtx.Height - 1 do
        begin
             s := '';
             for x := 0 to mtx.width - 1 do
                 s := s + UTF8String(Format('%.*f,', [prec, mtx[x, y]], ft));

             s[length(s)] := #13;
             s := s + #10;

             WriteBuffer(s[1], Length(s));
        end;
     finally
            Free;
     end;
end;

function MatrixFromTxtFile( fileName : string ) : TDoubleMatrix; overload;
begin
     Result := MatrixFromTxtFile( fileName, TDoubleMatrix );
end;

function TDoubleMatrix.AsVector( ColMajor : boolean = False ): TDoubleMatrix;
begin
     if Height = 1
     then
         Result := Clone
     else
         Result := Reshape(width*height, 1, ColMajor);
end;

function TDoubleMatrix.AsVectorD(ColMajor: boolean): IDoubleMatrix;
begin
     Result := AsVector(ColMajor);
end;

{$Warnings off}
function MatrixFromTxtFile( fileName : string; mtxClass : TDoubleMatrixClass ) : TDoubleMatrix;
var fmt : TFormatSettings;
    w, h : integer;
    arr : TDoubleDynArray;
    numElem : integer;
    i : integer;
    slLine : TStringList;
    aFile : TStringList;
    cnt: Integer;
begin
     fmt := GetLocalFMTSet;
     fmt.DecimalSeparator := '.';

     numElem := 0;

     aFile := TStringList.Create;
     try
        aFile.LoadFromFile(filename);
        w := 0;
        h := aFile.Count;
        
        slLine := TStringList.Create;
        slLine.Delimiter := ',';
        try
           for cnt := 0 to h - 1 do
           begin
                slLine.DelimitedText := aFile[cnt];

                if w = 0 then
                begin
                     w := slLine.Count;
                     SetLength(arr, w*h);
                end;

                if w <> slLine.Count then
                   raise Exception.Create('Number of columns do not match');

                for i := 0 to slLine.Count - 1 do
                begin 
                     arr[numElem] := StrToFloat( slLine[i], fmt );
                     inc(numElem);
                end;
           end;
        finally
               slLine.Free;
        end;

        // ###########################################
        // #### Build resulting matrix
        SetLength(arr, numElem);
        Result := mtxClass.Create(arr, w, h );
     finally
            aFile.Free;
     end;
end;

// ###########################################
// #### Base matrix class
// ###########################################

function TMatrixClass.GetMtxClass: TDoubleMatrixClass;
begin
     Result := fMatrixClass;
     if Result = nil then
        Result := DefMatrixClass;
end;

procedure TMatrixClass.SetMtxClass(const Value: TDoubleMatrixClass);
begin
     fMatrixClass := Value;
end;

procedure TMatrixClass.DefineProps;
begin
     // do nothing here
end;

initialization
   TMatrixClass.DefMatrixClass := TDoubleMatrix;
   RegisterMathIO(TDoubleMatrix);
   RegisterCastClass(IDoubleMatrix, TDoubleMatrix);

end.
