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

uses SysUtils, Classes, Types, MatrixConst, BaseMathPersistence, RandomEng;

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

    function GetVecItem(idx: integer): double; register;
    procedure SetVecItem(idx: integer; const Value: double); register;

    function GetSubWidth : integer;
    function GetSubHeight : integer;
    procedure SetWidth(const Value : integer);
    procedure SetHeight(const Value : integer);
    procedure SetWidthHeight(const Width, Height : integer);
    function GetVecLen : integer;

    property Width : integer read GetSubWidth write SetWidth;
    property Height : integer read GetSubHeight write SetHeight;

    property LinEQProgress : TLinEquProgress read GetLinEQProgress write SetLinEQProgress;

    // general access
    property Items[x, y : integer] : double read GetItems write SetItems; default;
    property Vec[idx : integer] : double read GetVecItem write SetVecItem; // matrix as vector
    property VecLen : integer read GetVecLen;   

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

    procedure SetValue(const initVal : double);

    function Reshape(newWidth, newHeight : integer; RowMajor : boolean = False) : TDoubleMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer; RowMajor : boolean = False);

    // ###################################################
    // #### Simple matrix utility functions
    function Max : double;
    function Min : double;

    function Abs : TDoubleMatrix;
    procedure AbsInPlace;

    procedure DiagInPlace(createDiagMtx : boolean);
    function Diag(createDiagMtx : boolean) : TDoubleMatrix;
    function Trace : double;

    procedure Normalize(RowWise : boolean);
    function ElementwiseNorm2(doSqrt : boolean = True) : double;   

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

    // multT1: dest = mt1' * mt2     mt1' = mt1.transpose
    procedure MultInPlaceT1(Value : TDoubleMatrix); overload;
    function MultT1(Value : TDoubleMatrix) : TDoubleMatrix; overload;

    // multT2: dest = mt1 * mt2'     mt2 = mt2.transpose
    procedure MultInPlaceT2(Value : TDoubleMatrix); overload;
    function MultT2(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure AddInplace(Value : IMatrix); overload;
    function Add(Value : IMatrix) : TDoubleMatrix; overload;
    procedure SubInPlace(Value : IMatrix); overload;
    function Sub(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlace(Value : IMatrix); overload;
    function Mult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlaceT1(Value : IMatrix); overload;
    function MultT1(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlaceT2(Value : IMatrix); overload;
    function MultT2(Value : IMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : TDoubleMatrix); overload;
    function ElementWiseMult(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : IMatrix); overload;
    function ElementWiseMult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseDivInPlace(Value : TDoubleMatrix); overload;
    function ElementWiseDiv(Value : TDoubleMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseDivInPlace(Value : IMatrix); overload;
    function ElementWiseDiv(Value : IMatrix) : TDoubleMatrix; overload;


    procedure AddAndScaleInPlace(const Offset, Scale : double);
    function AddAndScale(const Offset, Scale : double) : TDoubleMatrix;

    function Mean(RowWise : boolean) : TDoubleMatrix;
    procedure MeanInPlace(RowWise : boolean);
    function Variance(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure VarianceInPlace(RowWise : boolean; unbiased : boolean = True);
    function Std(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure StdInPlace(RowWise : boolean; unbiased : boolean = True);

    // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure MeanVarianceInPlace(RowWise : boolean; unbiased : boolean = True);
    
    
    function Median(RowWise : boolean) : TDoubleMatrix;
    procedure MedianInPlace(RowWise : boolean);

    procedure SortInPlace(RowWise : boolean);
    function Sort(RowWise : boolean) : TDoubleMatrix;

    function Diff(RowWise : boolean) : TDoubleMatrix;
    procedure DiffInPlace(RowWise : boolean);
    function Sum(RowWise : boolean) : TDoubleMatrix;
    procedure SumInPlace(RowWise : boolean);
    function CumulativeSum(RowWise : boolean) : TDoubleMatrix;
    procedure CumulativeSumInPlace(RowWise : boolean);

    function Add(const Value : double) : TDoubleMatrix; overload;
    procedure AddInPlace(const Value : double); overload;
    function Scale(const Value : double) : TDoubleMatrix;
    procedure ScaleInPlace(const Value : Double);
    function ScaleAndAdd(const aOffset, aScale : double) : TDoubleMatrix;
    procedure ScaleAndAddInPlace(const aOffset, aScale : double);
    function SQRT : TDoubleMatrix;
    procedure SQRTInPlace;

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

    // solves least sqaures A*x = b using the QR decomposition
    function SolveLeastSquaresInPlace(out x : TDoubleMatrix; b : TDoubleMatrix) : TQRResult; overload; 
    function SolveLeastSquaresInPlace(out x : IMatrix; b : IMatrix) : TQRResult; overload; 
    function SolveLeastSquares(out x : TDoubleMatrix; b : TDoubleMatrix) : TQRResult; overload; 
    function SolveLeastSquares(out x : IMatrix; b : IMatrix) : TQRResult; overload;

    // matrix inversion (based on LU decomposition)    
    function InvertInPlace : TLinEquResult;
    function Invert : TDoubleMatrix;
    function PseudoInversionInPlace : TSVDResult;
    function PseudoInversion(out Mtx : TDoubleMatrix) : TSVDResult; overload;
    function PseudoInversion(out Mtx : IMatrix) : TSVDResult; overload;

    function Determinant : double;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : double);
    procedure RepeatMatrixInPlace(numX, numY : integer);
    function RepeatMatrix(numX, numY : integer) : TDoubleMatrix;

    // ###################################################
    // #### Matrix transformations
    function SVD(out U, V, W : TDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SVD(out U, V, W : IMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SymEig(out EigVals : TDoubleMatrix; out EigVect : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix; out EigVect : IMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix; out EigVect : TDoubleMatrix; normEigVecs : boolean = False)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix; out EigVect : IMatrix; normEigVecs : boolean = False) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix)  : TEigenvalueConvergence; overload;
    function Cholesky(out Chol : TDoubleMatrix) : TCholeskyResult; overload;
    function Cholesky(out Chol : IMatrix) : TCholeskyResult; overload;
    function QR(out R : TDoubleMatrix; out tau : TDoubleMatrix) : TQRResult; overload;
    function QR(out R : IMatrix; out tau : IMatrix) : TQRResult; overload;
    function QRFull(out Q, R : TDoubleMatrix) : TQRResult; overload;
    function QRFull(out Q, R : IMatrix) : TQRResult; overload;

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(Value : TDoubleMatrix); overload;
    procedure Assign(Value : IMatrix); overload;
    procedure Assign(Value : TDoubleMatrix; OnlySubElements : boolean); overload;
    procedure Assign(Value : IMatrix; OnlySubElements : boolean); overload;
    procedure Assign(const Mtx : Array of double; W, H : integer); overload;
    procedure AssignSubMatrix(Value : TDoubleMatrix; X : integer = 0; Y : integer = 0); overload;
    procedure AssignSubMatrix(Value : IMatrix; X : integer = 0; Y : integer = 0); overload;

    // moves data from Value to self and clears original object
    procedure TakeOver(Value : TDoubleMatrix); overload;
    procedure TakeOver(Value : IMatrix); overload;

    function Clone : TDoubleMatrix;
  end;

// #################################################
// #### Builds base matrix operations
  TDoubleMatrixClass = class of TDoubleMatrix;
  TDoubleMatrix = class(TBaseMathPersistence, IMatrix)
  private
    type
      TLocConstDoubleArr = Array[0..MaxInt div sizeof(double) - 1] of double;
      PLocConstDoubleArr = ^TLocConstDoubleArr;
  protected
    // used to determine which class type to use as a result
    // e.g. the threaded class does not override all standard functions but
    // the resulting class type shall always be the threaded class!
    class function ResultClass : TDoubleMatrixClass; virtual;

    function GetItems(x, y: integer): double; register; inline;
    procedure SetItems(x, y: integer; const Value: double); register; inline;
    // Access as vector -> same as GetItem(idx mod width, idx div width)
    function GetVecItem(idx: integer): double; register; 
    procedure SetVecItem(idx: integer; const Value: double); register; 

    // matrix persistence functions
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    class function ClassIdentifier : String; override;
    procedure OnLoadStringProperty(const Name : String; const Value : String); overload; override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); overload; override;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); override;

    procedure SetLinEQProgress(value : TLinEquProgress);
    function GetLinEQProgress : TLinEquProgress;

    procedure InternalSetWidthHeight(const aWidth, aHeight : integer; AssignMem : boolean = True);
    procedure ReserveMem(width, height: integer);
  private
    fMemory : Pointer;
    fData : PLocConstDoubleArr;       // 16 byte aligned pointer:
    fLineWidth : TASMNativeInt;
    fWidth : integer;
    fObj : TObject;                // arbitrary object

    procedure MtxRandWithEng(var value : double);
    procedure MtxRand(var value : double);
  protected
    fHeight : integer;
    fName : String;
    fSubWidth : integer;
    fSubHeight : integer;
    fOffsetX : integer;
    fOffsetY : integer;

    fLinEQProgress : TLinEquProgress;

    procedure CheckAndRaiseError(assertionVal : boolean; const msg : string);

    procedure SetData(data : PDouble; srcLineWidth, width, height : integer);
    procedure Clear;
    procedure SetWidth(const Value : integer);
    procedure SetHeight(const Value : integer);
    function GetSubWidth : integer;
    function GetSubHeight : integer;
    function GetVecLen : integer;
    function GetObjRef : TDoubleMatrix;
  public
    property Width : integer read GetSubWidth write SetWidth;
    property Height : integer read GetSubHeight write SetHeight;
    procedure SetWidthHeight(const aWidth, aHeight : integer);

    property Name : string read fName write fName;

    property LineEQProgress : TLinEquProgress read GetLinEQProgress write SetLinEQProgress;

    // general access

    // direct access functionality (use only when you know what you are doing!)
    function StartElement : PDouble; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}
    function LineWidth : integer; {$IFNDEF FPC} {$IF CompilerVersion >= 17.0} inline; {$IFEND} {$ENDIF}

    property Items[x, y : integer] : double read GetItems write SetItems; default;
    property Vec[idx : integer] : double read GetVecItem write SetVecItem; // matrix as vector
    property VecLen : integer read GetVecLen;  // width*height
    function SubMatrix : TDoubleDynArray;
    procedure SetSubMatrix(x, y, Subwidth, Subheight : integer);
    procedure UseFullMatrix;

    procedure SetRow(row : integer; const Values : Array of Double); overload;
    procedure SetRow(row : integer; Values : TDoubleMatrix; ValRow : integer = 0); overload;
    procedure SetRow(row : integer; Values : IMatrix; ValRow : integer = 0); overload;
    procedure SetColumn(col : integer; const Values : Array of Double); overload;
    procedure SetColumn(col : integer; Values : TDoubleMatrix; ValCols : integer = 0); overload;
    procedure SetColumn(col : integer; Values : IMatrix; ValCols : integer = 0); overload;

    procedure SetValue(const initVal : double);

    function Reshape(newWidth, newHeight : integer; RowMajor : boolean = False) : TDoubleMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer; RowMajor : boolean = False);

    // ###################################################
    // #### Simple matrix utility functions
    function Max : double;
    function Min : double;

    function Abs : TDoubleMatrix;
    procedure AbsInPlace;

    procedure DiagInPlace(createDiagMtx : boolean);
    function Diag(createDiagMtx : boolean) : TDoubleMatrix;
    function Trace : double;

    procedure Normalize(RowWise : boolean);
    function ElementwiseNorm2(doSqrt : boolean = True) : double;

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
    procedure MultInPlaceT1(Value : TDoubleMatrix); overload; virtual;
    function MultT1(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure MultInPlaceT2(Value : TDoubleMatrix); overload; virtual;
    function MultT2(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure AddInplace(Value : IMatrix); overload;
    function Add(Value : IMatrix) : TDoubleMatrix; overload;
    procedure SubInPlace(Value : IMatrix); overload;
    function Sub(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlace(Value : IMatrix); overload;
    function Mult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlaceT1(Value : IMatrix); overload;
    function MultT1(Value : IMatrix) : TDoubleMatrix; overload;
    procedure MultInPlaceT2(Value : IMatrix); overload;
    function MultT2(Value : IMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : IMatrix); overload;
    function ElementWiseMult(Value : IMatrix) : TDoubleMatrix; overload;
    procedure ElementWiseMultInPlace(Value : TDoubleMatrix); overload; virtual;
    function ElementWiseMult(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;

    procedure ElementWiseDivInPlace(Value : TDoubleMatrix); overload; virtual;
    function ElementWiseDiv(Value : TDoubleMatrix) : TDoubleMatrix; overload; virtual;
    procedure ElementWiseDivInPlace(Value : IMatrix); overload; virtual;
    function ElementWiseDiv(Value : IMatrix) : TDoubleMatrix; overload; virtual;

    procedure AddAndScaleInPlace(const Offset, Scale : double); virtual;
    function AddAndScale(const Offset, Scale : double) : TDoubleMatrix; virtual;

    function Mean(RowWise : boolean) : TDoubleMatrix;
    procedure MeanInPlace(RowWise : boolean);
    function Variance(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure VarianceInPlace(RowWise : boolean; unbiased : boolean = True);
    function Std(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure StdInPlace(RowWise : boolean; unbiased : boolean = True);
    
     // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : TDoubleMatrix;
    procedure MeanVarianceInPlace(RowWise : boolean; unbiased : boolean = True);

    function Median(RowWise : boolean) : TDoubleMatrix; virtual;
    procedure MedianInPlace(RowWise : boolean);

    procedure SortInPlace(RowWise : boolean); virtual;
    function Sort(RowWise : boolean) : TDoubleMatrix;

    function Diff(RowWise : boolean) : TDoubleMatrix;
    procedure DiffInPlace(RowWise : boolean);
    function Sum(RowWise : boolean) : TDoubleMatrix;
    procedure SumInPlace(RowWise : boolean);
    function CumulativeSum(RowWise : boolean) : TDoubleMatrix;
    procedure CumulativeSumInPlace(RowWise : boolean);

    function Add(const Value : double) : TDoubleMatrix; overload;
    procedure AddInPlace(const Value : double); overload;
    function Scale(const Value : double) : TDoubleMatrix;
    procedure ScaleInPlace(const Value : Double);
    function ScaleAndAdd(const aOffset, aScale : double) : TDoubleMatrix;
    procedure ScaleAndAddInPlace(const aOffset, aScale : double);
    function SQRT : TDoubleMatrix;
    procedure SQRTInPlace;

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
    
    // solves least sqaures A*x = b using the QR decomposition
    function SolveLeastSquaresInPlace(out x : TDoubleMatrix; b : TDoubleMatrix) : TQRResult; overload; virtual;
    function SolveLeastSquaresInPlace(out x : IMatrix; b : IMatrix) : TQRResult; overload; 
    function SolveLeastSquares(out x : TDoubleMatrix; b : TDoubleMatrix) : TQRResult; overload; virtual;
    function SolveLeastSquares(out x : IMatrix; b : IMatrix) : TQRResult; overload;

    // Matrix inversion (via LU decomposition)
    function InvertInPlace : TLinEquResult; virtual;
    function Invert : TDoubleMatrix; virtual;
    function Determinant : double; virtual;

    // pseudoinversion via svd
    function PseudoInversionInPlace : TSVDResult;
    function PseudoInversion(out Mtx : TDoubleMatrix) : TSVDResult; overload;
    function PseudoInversion(out Mtx : IMatrix) : TSVDResult; overload;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : double);
    procedure RepeatMatrixInPlace(numX, numY : integer);
    function RepeatMatrix(numX, numY : integer) : TDoubleMatrix;

    // ###################################################
    // #### Matrix transformations
    function SVD(out U, V, W : TDoubleMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload; virtual;
    function SVD(out U, V, W : IMatrix; onlyDiagElements : boolean = False) : TSVDResult; overload;
    function SymEig(out EigVals : TDoubleMatrix; out EigVect : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : TDoubleMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix; out EigVect : IMatrix) : TEigenvalueConvergence; overload;
    function SymEig(out EigVals : IMatrix) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix; out EigVect : TDoubleMatrix; normEigVecs : boolean = False)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : TDoublematrix)  : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix; out EigVect : IMatrix; normEigVecs : boolean = False) : TEigenvalueConvergence; overload;
    function Eig(out EigVals : IMatrix)  : TEigenvalueConvergence; overload;
    function Cholesky(out Chol : TDoubleMatrix) : TCholeskyResult; overload; virtual;
    function Cholesky(out Chol : IMatrix) : TCholeskyResult; overload;
    function QR(out ecosizeR : TDoubleMatrix; out tau : TDoubleMatrix) : TQRResult; overload; virtual;
    function QR(out ecosizeR : IMatrix; out tau : IMatrix) : TQRResult; overload;
    function QRFull(out Q, R : TDoubleMatrix) : TQRResult; overload; virtual;
    function QRFull(out Q, R : IMatrix) : TQRResult; overload;

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(Value : TDoubleMatrix); overload;
    procedure Assign(Value : IMatrix); overload;
    procedure Assign(Value : TDoubleMatrix; OnlySubElements : boolean); overload;
    procedure Assign(Value : IMatrix; OnlySubElements : boolean); overload;
    procedure Assign(const Mtx : Array of double; W, H : integer); overload;
    procedure AssignSubMatrix(Value : TDoubleMatrix; X : integer = 0; Y : integer = 0); overload;
    procedure AssignSubMatrix(Value : IMatrix; X : integer = 0; Y : integer = 0); overload;

    // actually the same as assign but it takes over the data and leaves an empty matrix object behind.
    procedure TakeOver(Value : TDoubleMatrix); overload;
    procedure TakeOver(Value : IMatrix); overload;


    function Clone : TDoubleMatrix;

    constructor Create; overload;
    constructor Create(aWidth, aHeight : integer; const initVal : double = 0); overload;
    constructor CreateEye(aWidth : integer);
    constructor Create(data : PDouble; aLineWidth : integer; aWidth, aHeight : integer); overload;
    constructor CreateDyn(const Data : TDoubleDynArray; aWidth, aHeight : integer); overload;
    constructor CreateDyn(const Data : TDoubleDynArray; fromDataIdx : integer; aWidth, aHeight : integer); overload;
    constructor Create(const Mtx : Array of double; W, H : integer); overload; 
    constructor CreateRand(aWidth, aHeight : integer; method : TRandomAlgorithm; seed : LongInt); overload; // uses random engine
    constructor CreateRand(aWidth, aHeight : integer); overload; // uses system default random
    constructor CreateLinSpace(aVecLen : integer; const StartVal : double; const EndVal : double);
    destructor Destroy; override;
  end;

type
  TDoubleMatrixDynArr = Array of TDoubleMatrix;
  IMatrixDynArr = Array of IMatrix;

// default class used in derrived methods like in the global subspace methdos:
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


implementation

uses Math, OptimizedFuncs, Eigensystems, LinAlgSVD, LinAlgQR, BlockSizeSetup, LinAlgCholesky,
     LinAlgLU;


{$IFNDEF CPUX64}
type
  NativeUInt = Cardinal;
{$ENDIF}


// ###########################################
// #### Inline functions (need to be first)
// ###########################################

procedure TDoubleMatrix.SetItems(x, y: integer; const Value: double);
var pData : PLocConstDoubleArr;
begin
     CheckAndRaiseError((x >= 0) and (x < fSubWidth), 'Dimension error');
     CheckAndRaiseError((y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := fData;
     inc(PByte(pData), (fOffsetY + y)*fLineWidth);

     pData^[fOffsetX + x] := Value;
end;

function TDoubleMatrix.GetItems(x, y: integer): double;
var pData : PLocConstDoubleArr;
begin
     CheckAndRaiseError((x >= 0) and (x < fSubWidth) and (y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := fData;
     inc(PByte(pData), (fOffsetY + y)*fLineWidth);

     Result := pData^[fOffsetX + x];
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
     Result.Assign(Self, True);

     MatrixAbs(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

procedure TDoubleMatrix.AbsInPlace;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAbs(StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TDoubleMatrix.Add(Value: IMatrix): TDoubleMatrix;
begin
     Result := Add(Value.GetObjRef);
end;

function TDoubleMatrix.Add(const Value: double): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.Assign(Self, True);

     MatrixAddAndScale(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, Value, 1);
end;

function TDoubleMatrix.AddAndScale(const Offset, Scale: double): TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self, True);

     Result.AddAndScaleInPlace(Offset, Scale);
end;

procedure TDoubleMatrix.AddAndScaleInPlace(const Offset, Scale: double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Offset, Scale);
end;

procedure TDoubleMatrix.AddInplace(const Value: double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     MatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Value, 1);
end;

procedure TDoubleMatrix.AddInplace(Value: IMatrix);
begin
     AddInplace(Value.GetObjRef);
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
     CheckAndRaiseError((Value.Width <= Width) and (Value.Height <= Height), 'Dimension error');
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
          fSubWidth := Value.fSubWidth;
          fSubHeight := Value.fSubHeight;
     end;
end;

function TDoubleMatrix.Add(Value : TDoubleMatrix) : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(fSubWidth, fSubHeight);
     MatrixAdd(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.Assign(Value: TDoubleMatrix);
begin
     Assign(Value, False);
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

constructor TDoubleMatrix.Create(data: PDouble; aLineWidth, aWidth,
  aHeight: integer);
begin
     inherited Create;

     fWidth := aWidth;
     fHeight := aHeight;
     fMemory := data;
     fData := PLocConstDoubleArr(data);
     fLineWidth := aLineWidth;

     fOffsetX := 0;
     fOffsetY := 0;
     fSubWidth := fWidth;
     fSubHeight := fHeight;
     CheckAndRaiseError(width*sizeof(double) <= LineWidth, 'Dimension error');
end;

constructor TDoubleMatrix.Create(aWidth, aHeight: integer; const initVal : double);
var pData : PConstDoubleArr;
    x, y : integer;
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     if initVal <> 0 then
     begin
          for y := 0 to height - 1 do
          begin
               pData := PConstDoubleArr( StartElement );
               inc(PByte(pData), y*LineWidth);

               for x := 0 to Width - 1 do
                   pData^[x] := initVal;
          end;
     end;
end;

constructor TDoubleMatrix.CreateDyn(const Data : TDoubleDynArray; aWidth, aHeight : integer);
begin
     inherited Create;

     Assign(Data, aWidth, aHeight);
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
  method: TRandomAlgorithm; seed: LongInt);
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     fObj := TRandomGenerator.Create;
     TRandomGenerator(fObj).RandMethod := method;
     TRandomGenerator(fObj).Init(seed);
     ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}MtxRandWithEng);
     FreeAndNil(fObj);
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
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);
     ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}MtxRand);
end;

function TDoubleMatrix.CumulativeSum(RowWise: boolean): TDoubleMatrix;
begin
     Result := TDoubleMatrix.Create(Width, Height);
     MatrixCumulativeSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, width, height, RowWise);
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
               Result := TDoubleMatrix.Create(height, height);
               for x := 0 to Height - 1 do
                   Result[x, x] := Vec[x];
          end
          else if height = 1 then
          begin
               Result := TDoubleMatrix.Create(width, width);
               for x := 0 to width - 1 do
                   Result[x, x] := Vec[x];
          end
          else
          begin
               Result := TDoubleMatrix.Create(Math.Min(Width, Height), Math.Min(Width, Height));
               for x := 0 to Math.Min(Width, Height) - 1 do
                   Result[x, x] := Items[x, x];
          end;
     end
     else
     begin
          Result := TDoubleMatrix.Create(1, Math.Min(Width, Height));
          for x := 0 to Result.Height - 1 do
              Result[Math.Min(x, Result.width - 1), x] := Items[x, x];
     end;
end;

procedure TDoubleMatrix.DiagInPlace(createDiagMtx : boolean);
var dl : IMatrix;
begin
     dl := Diag(createDiagMtx);

     TakeOver(dl);
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

procedure TDoubleMatrix.DiffInPlace(RowWise: boolean);
begin
     if (width = 0) or (height = 0) then
        exit;

     MatrixDiff(StartElement, LineWidth, StartElement, LineWidth, Width, height, RowWise);

     if RowWise then
     begin
          dec(fSubWidth);
          fWidth := fSubWidth;
     end
     else
     begin
          dec(fSubHeight);
          fHeight := fHeight;
     end;
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

function TDoubleMatrix.SolveLeastSquares(out x : TDoubleMatrix; b : TDoubleMatrix): TQRResult;
var tmpA : TDoubleMatrix;
begin
     CheckAndRaiseError( width <= Height, 'Height must be at least width');
     
     tmpA := Clone;
     try
        x := ResultClass.Create( 1, Width );

        Result := MatrixQRSolve( x.StartElement, x.LineWidth, tmpA.StartElement, tmpA.LineWidth, b.StartElement, b.LineWidth, width, height);

        if Result <> qrOK then
           FreeAndNil(x);
     finally
            tmpA.Free;
     end;
end;

function TDoubleMatrix.SolveLeastSquares(out x : IMatrix; b: IMatrix): TQRResult;
var tmp : TDoubleMatrix;
begin
     Result := SolveLeastSquares(tmp, b.GetObjRef);

     x := tmp;
end;

function TDoubleMatrix.SolveLeastSquaresInPlace(out x: TDoubleMatrix;
  b: TDoubleMatrix): TQRResult;
begin
     CheckAndRaiseError( width <= Height, 'Height must be at least width');
     
     x := ResultClass.Create( 1, Width );

     Result := MatrixQRSolve( x.StartElement, x.LineWidth, StartElement, LineWidth, b.StartElement, b.LineWidth, width, height);

     if Result <> qrOK then
        FreeAndNil(x);
end;

function TDoubleMatrix.SolveLeastSquaresInPlace(out x: IMatrix;
  b: IMatrix): TQRResult;
var tmp : TDoubleMatrix;
begin
     Result := SolveLeastSquaresInPlace(tmp, b.GetObjRef);

     x := tmp;
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

function TDoubleMatrix.SQRT: TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(Self, True);

     MatrixSQRT(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

procedure TDoubleMatrix.SQRTInPlace;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     MatrixSQRT(StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TDoubleMatrix.Eig(out EigVals, EigVect: IMatrix; normEigVecs : boolean = False): TEigenvalueConvergence;
var outEigVals, outEigVect : TDoubleMatrix;
begin
     Result := Eig(outEigVals, outEigVect, normEigVecs);
     EigVals := outEigVals;
     EigVect := outEigVect;
end;

function TDoubleMatrix.Eig(out EigVals: IMatrix): TEigenvalueConvergence;
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

function TDoubleMatrix.Eig(out EigVals, EigVect: TDoubleMatrix; normEigVecs : boolean = False): TEigenvalueConvergence;
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
        dummy := TDoubleMatrix.Create;
        try
           dummy.Assign(self);
           Result := MatrixUnsymEigVecInPlace(dummy.StartElement, dummy.LineWidth,
                                              fSubWidth, 
                                              pReal, dt.LineWidth, pImag, dt.LineWidth, 
                                              vecs.StartElement, vecs.LineWidth,
                                              True);
        finally
               dummy.Free;
        end;

        if Result = qlOk then
        begin
             if normEigVecs then
                MatrixNormEivecInPlace(vecs.StartElement, vecs.LineWidth, fSubWidth, pImag, dt.LineWidth);
             
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

function TDoubleMatrix.ElementWiseMult(Value: TDoubleMatrix) : TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     Result := ResultClass.Create;
     Result.Assign(self);
     MatrixElemMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
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

procedure TDoubleMatrix.ElementWiseDivInPlace(Value: TDoubleMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     MatrixElemDiv(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TDoubleMatrix.ElementWiseDivInPlace(Value : IMatrix);
begin
     ElementWiseDivInPlace(Value.GetObjRef);
end;

function TDoubleMatrix.ElementWiseDiv(Value : IMatrix) : TDoubleMatrix;
begin
     Result := ElementWiseDiv(Value.GetObjRef);
end;

function TDoubleMatrix.ElementwiseNorm2(doSqrt : boolean = True): double;
begin
     Result := 0;

     if (Width > 0) and (Height > 0) then
        Result := MatrixElementwiseNorm2(StartElement, LineWidth, Width, Height, doSqrt);
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

function TDoubleMatrix.GetVecLen: integer;
begin
     // treat matrix as vector
     Result := fSubWidth*fSubHeight;
end;

function TDoubleMatrix.Invert: TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     Result := ResultClass.Create;
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
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     dt := ResultClass.Create(Width, Height);
     try
        dt.Assign(self, True);
        Result := MatrixInverseInPlace(dt.StartElement, dt.LineWidth, fSubWidth, fLinEQProgress);
        if Result = leOk then
           TakeOver(dt);

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

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          MatrixMean(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          MatrixMean(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end;
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

function TDoubleMatrix.Sort(RowWise: boolean): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     Result.SortInPlace(RowWise);
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

function TDoubleMatrix.MultT2(Value: TDoubleMatrix): TDoubleMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubHeight, fSubHeight);

     MatrixMultT2Ex(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth, BlockMatrixCacheSize, doNone, nil);
end;

function TDoubleMatrix.Scale(const Value: double): TDoubleMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.Assign(Self, True);

     MatrixAddAndScale(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, 0, Value);
end;

function TDoubleMatrix.ScaleAndAdd(const aOffset, aScale: double): TDoubleMatrix;
begin
     CheckAndRaiseError((width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self, True);

     Result.ScaleAndAddInPlace(aOffset, aScale);
end;

procedure TDoubleMatrix.ScaleAndAddInPlace(const aOffset, aScale: double);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     
     MatrixScaleAndAdd(StartElement, LineWidth, fSubWidth, fSubHeight, aOffset, aScale);
end;

procedure TDoubleMatrix.ScaleInPlace(const Value: Double);
begin
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

function TDoubleMatrix.PseudoInversion(out mtx : TDoubleMatrix) : TSVDResult;
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');

     mtx := ResultClass.Create;
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

function TDoubleMatrix.QR(out ecosizeR: IMatrix; out tau : IMatrix): TQRResult;
var outR : TDoubleMatrix;
    outTau : TDoubleMatrix;
begin
     Result := QR(outR, outTau);
     ecosizeR := outR;
     tau := outTau;
end;

function TDoubleMatrix.QRFull(out Q, R: TDoubleMatrix): TQRResult;
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

function TDoubleMatrix.QRFull(out Q, R: IMatrix): TQRResult;
var outR, outQ : TDoubleMatrix;
begin
     Result := QRFull(outQ, outR);
     Q := outQ;
     R := outR;
end;

function TDoubleMatrix.QR(out ecosizeR: TDoubleMatrix; out tau : TDoubleMatrix): TQRResult;
begin
     ecosizeR := ResultClass.Create;
     tau := ResultClass.Create(width, 1);
     try
        ecosizeR.Assign(self);
        Result := MatrixQRDecompInPlace2(ecosizeR.StartElement, ecosizeR.LineWidth, width, height, tau.StartElement, fLinEQProgress);
     except
           FreeAndNil(ecosizeR);
           FreeAndNil(tau);

           raise;
     end;
end;

function TDoubleMatrix.RepeatMatrix(numX, numy : integer): TDoubleMatrix;
begin
     Result := Clone;
     Result.RepeatMatrixInPlace(numY, numX);
end;

procedure TDoubleMatrix.RepeatMatrixInPlace(numX, numY: integer);
var origW, origH : integer;
    subMtx : IMatrix;
    x, y : Integer;
begin
     origW := Width;
     origH := Height;

     subMtx := Clone;

     SetWidthHeight( Width*numX, Height*numY );

     for y := 0 to numY - 1 do
         for x := 0 to numX - 1 do
             AssignSubMatrix(subMtx, x*origW, y*origH);
end;

procedure TDoubleMatrix.ReserveMem(width, height : integer);
begin
     // check if we need to reserve memory or if we already have a matrix with this size
     if (fWidth = width) and (fHeight = height) and (width > 0) and (height > 0) and Assigned(fMemory) then
        exit;
     
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
          if width > 1 then
          begin
               // get an AVX friendly linewidth
               fData := MtxAllocAlign(width, height, fLineWidth, fMemory);
          end
          else
          begin
               fMemory := MtxAlloc($20 + height*width*sizeof(double));
               fData := PLocConstDoubleArr( TASMNativeUint( fMemory ) + $20 - TASMNativeUint( fMemory ) and $1F );
               fLineWidth := sizeof(double);
          end;
     end;
end;

function TDoubleMatrix.Reshape(newWidth, newHeight: integer; RowMajor : boolean = False) : TDoubleMatrix;
var xOld, yOld : integer;
    x, y : Integer;
begin
     CheckAndRaiseError((fWidth > 0) and (fHeight > 0), 'Error operation not allowed on empty matrices');
     CheckAndRaiseError((newWidth*newHeight) = (fWidth*fHeight), 'Error new dimension does not fit into the old one');

     Result := ResultClass.Create(newWidth, newHeight);
     try
        if RowMajor then
        begin
             // reshape along rows not columns...
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

procedure TDoubleMatrix.ReshapeInPlace(newWidth, newHeight: integer; RowMajor : boolean = False);
var res : TDoubleMatrix;
begin
     CheckAndRaiseError((fWidth > 0) and (fHeight > 0), 'Error operation not allowed on empty matrices');
     CheckAndRaiseError((fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');
     CheckAndRaiseError((newWidth*newHeight) = (fWidth*fHeight), 'Error new dimension does not fit into the old one');

     // check if the line width fits the old width -> then we only have to adjust the
     // line width parameter, otherwise copy. 
     if (fWidth = fSubWidth) and (fHeight = fSubHeight) and (fLineWidth = fWidth*sizeof(double)) and not RowMajor
     then
         fLineWidth := newWidth*sizeof(double)
     else
     begin
          res := Reshape(newWidth, newHeight, RowMajor);
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
var pVal1 : PDouble;
    pVal2 : PDouble;
    y : integer;
begin
     CheckAndRaiseError(Values.fSubHeight = fSubHeight, 'Dimension error');
     CheckAndRaiseError((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

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
     CheckAndRaiseError(fSubHeight = Length(Values), 'Dimension error');
     CheckAndRaiseError((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

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

procedure TDoubleMatrix.SetLinEQProgress(value: TLinEquProgress);
begin
     fLinEQProgress := value;
end;

procedure TDoubleMatrix.SetRow(row : integer; Values: TDoubleMatrix; ValRow : integer);
var pVal1 : PDouble;
    pVal2 : PDouble;
begin
     CheckAndRaiseError(Values.Width = fSubWidth, 'Dimension Error');
     CheckAndRaiseError((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     pVal1 := StartElement;
     inc(PByte(pVal1), row*LineWidth);

     pVal2 := Values.StartElement;
     inc(PByte(pVal2), Values.LineWidth*ValRow);

     Move(pVal2^, pVal1^, fSubWidth*sizeof(double));
end;

procedure TDoubleMatrix.SetRow(row : integer; const Values: array of Double);
var pVal1 : PDouble;
begin
     CheckAndRaiseError(Length(Values) = fSubWidth, 'Dimension Error');
     CheckAndRaiseError((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     pVal1 := StartElement;
     inc(PByte(pVal1), row*LineWidth);

     Move(Values[0], pVal1^, fSubWidth*sizeof(double));
end;

procedure TDoubleMatrix.SetSubMatrix(x, y, Subwidth, Subheight: integer);
begin
     CheckAndRaiseError((x >= 0) and (x + SubWidth <= fWidth), 'Dimension x error');
     CheckAndRaiseError((y >= 0) and (y + SubHeight <= fHeight), 'Dimension y error');

     fOffsetX := x;
     fOffsetY := y;
     fSubWidth := Subwidth;
     fSubHeight := Subheight;
end;

procedure TDoubleMatrix.SetValue(const initVal: double);
var x, y : integer;
    pData : PConstDoubleArr;
begin
     // ###########################################
     // #### Initialize the matrix with the given value
     for y := 0 to height - 1 do
     begin
          pData := PConstDoubleArr( StartElement );
          inc(PByte(pData), y*LineWidth);

          for x := 0 to Width - 1 do
              pData^[x] := initVal;
     end;
end;

procedure TDoubleMatrix.SetWidth(const Value: integer);
begin
     SetWidthHeight(Value, fHeight);
end;

procedure TDoubleMatrix.SetWidthHeight(const aWidth, aHeight: integer);
begin
     InternalSetWidthHeight(aWidth, aHeight, True);
end;

procedure TDoubleMatrix.InternalSetWidthHeight(const aWidth, aHeight: integer; AssignMem : boolean);
begin
     CheckAndRaiseError((aWidth > 0) and (aHeight > 0), 'Dimension error');

     if AssignMem
     then
         ReserveMem(aWidth, aHeight)
     else
         ReserveMem(0, 0);

     fWidth := aWidth;
     fHeight := aHeight;
     fSubWidth := aWidth;
     fSubHeight := aHeight;
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

function TDoubleMatrix.SVD(out U, V, W: IMatrix; onlyDiagElements: boolean): TSVDResult;
var uObj, vObj, wObj : TDoubleMatrix;
begin
     Result := SVD(uObj, vObj, wObj, onlyDiagElements);
     U := uObj;
     V := vObj;
     W := wObj;
end;

function TDoubleMatrix.SVD(out U, V, W: TDoubleMatrix; onlyDiagElements : boolean): TSVDResult;
var pW : PConstDoubleArr;
    wArr : PByte;
    minWH : TASMNativeInt;
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

             Result := MatrixSVDInPlace2(V.StartElement, V.LineWidth, Height, Width, pW, U.StartElement, U.LineWidth, SVDBlockSize, fLinEQProgress );

             // we need a final transposition on the matrix U and V
             U.TransposeInPlace;
             V.TransposeInPlace;
        end
        else
        begin
             U := Clone;
             V := ResultClass.Create(fSubWidth, fSubWidth);

             Result := MatrixSVDInPlace2(U.StartElement, U.LineWidth, Width, Height, pW, V.StartElement, V.LineWidth, SVDBlockSize, fLinEQProgress);
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
        Result := MatrixEigTridiagonalMatrix(vecs.StartElement, vecs.LineWidth, StartElement, LineWidth, fSubWidth, dt.StartElement, dt.LineWidth);
        if Result = qlOk then
        begin
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

     dt := ResultClass.Create(1, fSubHeight);
     vecs := ResultClass.Create(fSubWidth, fSubWidth);
     try
        Result := MatrixEigTridiagonalMatrix(vecs.StartElement, vecs.LineWidth, StartElement, LineWidth, fSubWidth, dt.StartElement, dt.LineWidth);
        if Result = qlOk then
        begin
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
     // free memory from self
     Clear;

     // move properties to self
     fSubWidth := Value.fSubWidth;
     fSubHeight := Value.fSubHeight;
     fMemory := Value.fMemory;
     fData := Value.fData;
     fLineWidth := Value.fLineWidth;
     fWidth := Value.fWidth;
     fHeight := Value.fHeight;
     fOffsetX := Value.fOffsetX;
     fOffsetY := Value.fOffsetY;

     // since this is a hostile takeover we need to clear the other object
     value.fSubWidth := 0;
     value.fSubHeight := 0;
     Value.fMemory := nil;
     value.fData := nil;
     value.fLineWidth := 0;
     value.fWidth := 0;
     value.fHeight := 0;
     value.fOffsetX := 0;
     value.fOffsetY := 0;
end;

procedure TDoubleMatrix.TakeOver(Value: IMatrix);
begin
     TakeOver(Value.GetObjRef);
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

function TDoubleMatrix.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, 'Name') = 0
     then
         Result := ptString
     else if CompareText(Name, 'Width') = 0
     then
         Result := ptInteger
     else if CompareText(Name, 'Height') = 0
     then
         Result := ptInteger
     else if CompareText(Name, 'data') = 0
     then
         Result := ptDouble
     else if CompareText(Name, 'SubWidth') = 0
     then
         Result := ptInteger
     else if CompareText(Name, 'SubHeight') = 0
     then
         Result := ptInteger
     else if CompareText(Name, 'OffsetX') = 0
     then
         Result := ptInteger
     else if CompareText(Name, 'OffsetY') = 0
     then
         Result := ptInteger
     else
         Result := inherited PropTypeOfName(Name);
end;


destructor TDoubleMatrix.Destroy;
begin
     Clear;

     inherited;
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

procedure TDoubleMatrix.CheckAndRaiseError(assertionVal: boolean; const msg: string);
begin
     if not assertionVal then
        raise EBaseMatrixException.Create(msg);
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

function TDoubleMatrix.Clone: TDoubleMatrix;
begin
     Result := ResultClass.Create(Width, Height);
     Result.Assign(Self, True);
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

procedure TDoubleMatrix.MultInPlaceT1(Value: IMatrix);
begin
     MultInPlaceT1(Value.GetObjRef);
end;

procedure TDoubleMatrix.MultInPlaceT2(Value: IMatrix);
begin
     MultInPlaceT2(Value.GetObjRef);
end;

function TDoubleMatrix.MultT1(Value: IMatrix): TDoubleMatrix;
begin
     Result := MultT1(Value.GetObjRef);
end;

function TDoubleMatrix.MultT2(Value: IMatrix): TDoubleMatrix;
begin
     Result := MultT2(VAlue.GetObjRef);
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

end.
