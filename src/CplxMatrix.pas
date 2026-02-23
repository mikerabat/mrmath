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

unit CplxMatrix;

interface

uses SysUtils, Classes, Types, MatrixConst, BaseMathPersistence,
     RandomEng, Matrix;

type
  TCplxMatrix = class;

  ICplxMatrix = interface(IMatrix)
   ['{37F58AED-ECE0-4E17-9CCE-3FCB0B86520C}']
    function SubMatrix : TComplexDynArray;

    procedure SetRow(row : integer; const Values : Array of TComplex); overload;
    procedure SetColumn(col : integer; const Values : Array of TComplex); overload;

    procedure SetValue(const initVal : TComplex); overload;
    procedure SetValueSubMtx(const initVal : TComplex; x, y, aWidth, aHeight : NativeInt); overload;  // sets the value in a subsection of the matrix

    function GetItemsC(x, y: integer): TComplex;
    procedure SetItemsC(x, y: integer; const Value: TComplex);

    function GetVecItemC(idx: integer): TComplex;
    procedure SetVecItemC(idx: integer; const Value: TComplex);

    // general access
    property ItemsC[x, y : integer] : TComplex read GetItemsC write SetItemsC; default;
    property VecC[idx : integer] : TComplex read GetVecItemC write SetVecItemC; // matrix as vector


    // ###################################################
    // #### Simple matrix utility functions
    function Max : TComplex;
    function Min : TComplex;
    function SumAll : TComplex; overload;

    function Trace : TComplex;

    // ###########################################
    // #### Reintroduced functions for complex matrices
    function Mult(Value : ICplxMatrix) : ICplxMatrix;
    procedure MultInPlace(Value : ICplxMatrix);
    function MultT1(Value : ICplxMatrix) : ICplxMatrix;
    function MultT2(Value : ICplxMatrix) : ICplxMatrix;
    procedure MultInPlaceT1(Value : ICplxMatrix);
    procedure MultInPlaceT2(Value : ICplxMatrix);

    function Add(Value : ICplxMatrix) : ICplxMatrix;
    procedure AddInPlace(Value : ICplxMatrix);
    function AddVec(aVec : ICplxMatrix; rowWise : Boolean) : ICplxMatrix;
    procedure AddVecInPlace(Value : ICplxMatrix; rowWise : Boolean);
    function Sub(Value : ICplxMatrix) : ICplxMatrix;
    procedure SubInplace(Value : ICplxMatrix);
    function SubVec(aVec : ICplxMatrix; rowWise : Boolean) : ICplxMatrix;
    procedure SubVecInPlace(Value : ICplxMatrix; rowWise : Boolean);
    function Transpose : ICplxMatrix;
    function TransposeNonConj : ICplxMatrix;
    function ElementWiseMult(Value : ICplxMatrix) : ICplxMatrix;
    function ElementWiseDiv(Value : ICplxMatrix) : ICplxMatrix;
    function AddAndScaleC(const Offset, Scale : TComplex) : ICplxMatrix;
    function AddAndScale(const Offset, Scale : double) : ICplxMatrix;
    procedure AddAndScaleInPlaceC(const Offset, Scale : TComplex);

    function AddConst(const Value : TComplex) : ICplxMatrix;
    function Scale(const Value : TComplex) : ICplxMatrix;
    function SQRT : ICplxMatrix;
    function ScaleAndAdd(const aOffset, aScale : TComplex) : ICplxMatrix;
    function Mean(RowWise : boolean) : ICplxMatrix;
    function Variance(RowWise : boolean; unbiased : boolean = True) : ICplxMatrix;
    function Std(RowWise : boolean; unbiased : boolean = True) : ICplxMatrix;
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : ICplxMatrix;
    procedure Assign(Value : ICplxMatrix); overload;
    procedure AssignEx(Value : ICplxMatrix; OnlySubElements : boolean); overload;
    procedure AssignSubMatrix(Value : ICplxMatrix; X : integer = 0; Y : integer = 0);
    procedure Append(Value : ICplxMatrix; appendColumns : boolean); overload;
    function Reshape(newWidth, newHeight : integer; RowMajor : boolean = False) : ICplxMatrix;
    function Sum(RowWise : boolean) : ICplxMatrix; overload;
    procedure TakeOver(Value : ICplxMatrix);
    function Clone : ICplxMatrix;
    function Diff(RowWise : boolean) : ICplxMatrix;
    function RepeatMatrix(numX, numY : integer) : ICplxMatrix;
    function AsVector( ColMajor : boolean = False ) : ICplxMatrix;
    function SubColMtxIdx( colIdx : TIntegerDynArray ) : ICplxMatrix; overload;
    function SubRowMtxIdx( rowIdx : TIntegerDynArray ) : ICplxMatrix; overload;
    function SubMtxIdx( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : ICplxMatrix; overload;
    function SubColMtx( fromIdx, toIdx : integer ) : ICplxMatrix; overload;
    function SubRowMtx( fromIdx, toIdx : integer ) : ICplxMatrix; overload;
    function SubMtx( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : ICplxMatrix; overload;
    function Median(RowWise : boolean) : ICplxMatrix;
    function RollMedian(RowWise : boolean; order : integer) : ICplxMatrix;

    procedure ElementWiseMultInPlace(Value : ICplxMatrix);
    procedure ElementWiseDivInPlace(Value : ICplxMatrix);

    // ###################################################
    // #### Base Matrix operations that extend the base class
    procedure ElementwiseFuncInPlace(func : TCplxMatrixFunc); overload;
    function ElementwiseFunc(func : TCplxMatrixFunc) : TCplxMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixObjFunc); overload;
    function ElementwiseFunc(func : TCplxMatrixObjFunc) : TCplxMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixMtxRefFunc); overload;
    function ElementwiseFunc(func : TCplxMatrixMtxRefFunc) : TCplxMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixMtxRefObjFunc); overload;
    function ElementwiseFunc(func : TCplxMatrixMtxRefObjFunc) : TCplxMatrix; overload;

    {$IFDEF ANONMETHODS}
    procedure ElementwiseFuncInPlace(func : TCplxMatrixFuncRef); overload;
    function ElementwiseFunc(func : TCplxMatrixFuncRef) : TCplMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixMtxRefFuncRef); overload;
    function ElementwiseFunc(func : TCplxMatrixMtxRefFuncRef) : TCplMatrix; overload;
    {$ENDIF}

    // ###################################################
    // #### Linear System solver A*x = B -> use A.SolveLinEQ(B) -> x

    function Invert : ICplxMatrix; overload;
    function InvertEx( out InvMtx : ICplxMatrix ) : TLinEquResult; overload;

    // solves least sqaures A*x = b using the QR decomposition
    function Determinant : TComplex;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : TComplex);

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(const Mtx : Array of TComplex; W, H : integer); overload;
    procedure AssignDyn(Value : TComplexDynArray; W, H : integer);
  end;

  // #################################################
// #### Builds base matrix operations
  TCplxMatrixClass = class of TCplxMatrix;
  TCplxMatrix = class(TBaseMatrix, ICplxMatrix)
  private
    type
      TLocConstComplexArr = Array[0..MaxInt div sizeof(TComplex) - 1] of TComplex;
      PLocConstComplexArr = ^TLocConstComplexArr;
  protected
    // used to determine which class type to use as a result
    // e.g. the threaded class does not override all standard functions but
    // the resulting class type shall always be the threaded class!
    class function ResultClass : TCplxMatrixClass; virtual;
    class function NumberType : TNumberType; override;

    function StartElement : PComplex;

    // base class -> convert to double
    function GetItems(x, y: integer): double; override;
    procedure SetItems(x, y: integer; const Value: double); override;
    // Access as vector -> same as GetItem(idx mod width, idx div width)
    function GetVecItem(idx: integer): double; override;
    procedure SetVecItem(idx: integer; const Value: double); override;


    // matrix persistence functions
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;
    procedure OnLoadCmplxArr(const Name : String; const Value : TComplexDynArray); override;

    class function ClassIdentifier : String; override;
    class function GetItemSize : integer; override;
  private
    fObj : TObject;                // arbitrary object

    procedure MtxRandWithEng(var value : TComplex);
    procedure MtxRand(var value : TComplex);
  public
    // ###########################################
    // #### Reintroduced functions
    function ICplxMatrix.Mult = MultC;
    function ICplxMatrix.Transpose = TransposeC;
    function ICplxMatrix.Add = AddC;
    procedure ICplxMatrix.AddInPlace = AddInPlaceC;
    function ICplxMatrix.Sub = SubC;
    procedure ICplxMatrix.SubInPlace = SubInPlaceC;
    function ICplxMatrix.MultT1 = MultT1C;
    function ICplxMatrix.AddVec = AddVecC;
    function ICplxMatrix.SubVec = SubVecC;
    function ICplxMatrix.ElementWiseMult = ElementWiseMultC;
    function ICplxMatrix.ElementWiseDiv = ElementWiseDivC;
    function ICplxMatrix.AddConst = AddConstC;
    function ICplxMatrix.Scale = ScaleC;
    function ICplxMatrix.SQRT = SQRTC;
    function ICplxMatrix.ScaleAndAdd = ScaleAndAddC;
    function ICplxMatrix.AddAndScaleC = AddAndScaleC;
    function ICplxMatrix.Mean = MeanC;
    function ICplxMatrix.Variance = VarianceC;
    function ICplxMatrix.Std = StdC;
    function ICplxMatrix.MeanVariance = MeanVarianceC;
    procedure ICplxMatrix.MultInPlace = MultInPlaceC;
    procedure ICplxMatrix.MultInPlaceT1 = MultInPlaceT1C;
    procedure ICplxMatrix.MultInPlaceT2 = MultInPlaceT2C;
    function ICplxMatrix.MultT2 = MultT2C;
    procedure ICplxMatrix.Assign = AssignC;
    procedure ICplxMatrix.AssignEx = AssignExC;
    procedure ICplxMatrix.AssignSubMatrix = AssignSubMatrixC;
    procedure ICplxMatrix.Append = AppendC;
    function ICplxMatrix.Reshape = ReshapeC;
    function ICplxMatrix.Invert = InvertC;
    function ICplxMatrix.InvertEx = InvertExC;
    function ICplxMatrix.Sum = SumC;
    procedure ICplxMatrix.TakeOver = TakeOverC;
    function ICplxMatrix.Clone = CloneC;
    function ICplxMatrix.Diff = DiffC;
    function ICplxMatrix.RepeatMatrix = RepeatMatrixC;
    function ICplxMatrix.AsVector = AsVectorC;
    function ICplxMatrix.SubColMtxIdx = SubColMtxIdxC;
    function ICplxMatrix.SubRowMtxIdx = SubRowMtxIdxC;
    function ICplxMatrix.SubMtxIdx = SubMtxIdxC;
    function ICplxMatrix.SubColMtx = SubColMtxC;
    function ICplxMatrix.SubRowMtx = SubRowMtxC;
    function ICplxMatrix.SubMtx = SubMtxC;
    function ICplxMatrix.Median = MedianC;
    function ICplxMatrix.RollMedian = RollMedianC;
    function ICplxMatrix.TransposeNonConj = TransposeNonConjC;
    procedure ICplxMatrix.AddVecInPlace = AddVecInPlaceC;
    procedure ICplxMatrix.SubVecInPlace = SubVecInPlaceC;
    procedure ICplxMatrix.ElementWiseMultInPlace = ElementWiseMultInPlaceC;
    procedure ICplxMatrix.ElementWiseDivInPlace = ElementWiseDivInPlaceC;
    function ICplxMatrix.AddAndScale = AddAndScaleD;
    procedure ICplxMatrix.AddAndScaleInPlaceC = AddAndScaleInPlaceC;

    function MultC(Value : ICplxMatrix) : ICplxMatrix;
    function TransposeC : ICplxMatrix;
    function TransposeNonConjC : ICplxMatrix;
    function AddC(Value : ICplxMatrix) : ICplxMatrix;
    procedure AddInPlaceC(Value : ICplxMatrix);
    function SubC(Value : ICplxMatrix) : ICplxMatrix;
    procedure SubInplaceC(Value : ICplxMatrix);
    procedure MultInPlaceT1C(Value : ICplxMatrix);
    procedure MultInPlaceC(Value : ICplxMatrix);
    procedure MultInPlaceT2C(Value : ICplxMatrix);
    function AddVecC(aVec : ICplxMatrix; rowWise : Boolean) : ICplxMatrix;
    procedure AddVecInPlaceC(Value : ICplxMatrix; rowWise : Boolean);
    function SubVecC(aVec : ICplxMatrix; rowWise : Boolean) : ICplxMatrix;
    procedure SubVecInPlaceC(Value : ICplxMatrix; rowWise : Boolean);
    function ElementWiseMultC(Value : ICplxMatrix) : ICplxMatrix;
    function ElementWiseDivC(Value : ICplxMatrix) : ICplxMatrix;
    function AddConstC(const Value : TComplex) : ICplxMatrix;
    function ScaleC(const Value : TComplex) : ICplxMatrix;
    function SQRTC : ICplxMatrix;
    function ScaleAndAddC(const aOffset, aScale : TComplex) : ICplxMatrix;
    function AddAndScaleC(const Offset, Scale : TComplex) : ICplxMatrix;
    function MeanC(RowWise : boolean) : ICplxMatrix;
    function VarianceC(RowWise : boolean; unbiased : boolean = True) : ICplxMatrix;
    function StdC(RowWise : boolean; unbiased : boolean = True) : ICplxMatrix;
    function MeanVarianceC(RowWise : boolean; unbiased : boolean = True) : ICplxMatrix;
    procedure AssignC(Value : ICplxMatrix); overload;
    procedure AssignExC(Value : ICplxMatrix; OnlySubElements : boolean); overload;
    procedure AssignSubMatrixC(Value : ICplxMatrix; X : integer = 0; Y : integer = 0);
    procedure AppendC(Value : ICplxMatrix; appendColumns : boolean); overload;
    function ReshapeC(newWidth, newHeight : integer; RowMajor : boolean = False) : ICplxMatrix;
    function InvertC : ICplxMatrix; overload;
    function InvertExC( out InvMtx : ICplxMatrix ) : TLinEquResult; overload;
    function SumC(RowWise : boolean) : ICplxMatrix; overload;
    procedure TakeOverC(Value : ICplxMatrix);
    function CloneC : ICplxMatrix;
    function DiffC(RowWise : boolean) : ICplxMatrix;
    function RepeatMatrixC(numX, numY : integer) : ICplxMatrix;
    function AsVectorC( ColMajor : boolean = False ) : ICplxMatrix;
    function SubColMtxIdxC( colIdx : TIntegerDynArray ) : ICplxMatrix; overload;
    function SubRowMtxIdxC( rowIdx : TIntegerDynArray ) : ICplxMatrix; overload;
    function SubMtxIdxC( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : ICplxMatrix; overload;
    function SubColMtxC( fromIdx, toIdx : integer ) : ICplxMatrix; overload;
    function SubRowMtxC( fromIdx, toIdx : integer ) : ICplxMatrix; overload;
    function SubMtxC( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : ICplxMatrix; overload;
    function MedianC(RowWise : boolean) : ICplxMatrix;
    function RollMedianC(RowWise : boolean; order : integer) : ICplxMatrix;

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

    // multT1: dest = mt1' * mt2     mt1' = mt1.transpose
    function MultT1C(Value : ICplxMatrix) : ICplxMatrix;
    function MultT2C(Value : ICplxMatrix) : ICplxMatrix;

    procedure ElementWiseMultInPlaceC(Value : ICplxMatrix);
    procedure ElementWiseDivInPlaceC(Value : ICplxMatrix);

    function AddAndScaleD(const Offset, Scale : double) : ICplxMatrix;

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

    // ###########################################
    // #### Linear systems

    // matrix inversion (based on LU decomposition)
    function InvertI : IMatrix; overload; override;
    function InvertExI( out InvMtx : IMatrix ) : TLinEquResult; overload; override;

    // moves data from Value to self and clears original object
    function CloneI : IMatrix; override;

    // ###################################################
    // #### Special functions
    function RepeatMatrixI(numX, numY : integer) : IMatrix; override;

  public
    // general access
    function GetItemsC(x, y: integer): TComplex;
    procedure SetItemsC(x, y: integer; const Value: TComplex);

    function GetVecItemC(idx: integer): TComplex;
    procedure SetVecItemC(idx: integer; const Value: TComplex);

    property ItemsC[x, y : integer] : TComplex read GetItemsC write SetItemsC; default;
    property VecC[idx : integer] : TComplex read GetVecItemC write SetVecItemC; // matrix as vector

    function SubMatrix : TComplexDynArray;

    procedure InitRandom(method : TRandomAlgorithm; var seed : LongInt); override;

    function Reshape(newWidth, newHeight : integer; ColMajor : boolean = False) : TCplxMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer; ColMajor : boolean = False); override;

    function AsVector(ColMajor: boolean = False) :  TCplxMatrix;

    function GetType : TMatrixType; override;

    procedure SetRow(row : integer; const Values : Array of TComplex); overload;
    procedure SetRow(row : integer; Values : TCplxMatrix; ValRow : integer = 0); overload;
    procedure SetColumn(col : integer; const Values : Array of TComplex); overload;
    procedure SetColumn(col : integer; Values : TCplxMatrix; ValCols : integer = 0); overload;

    procedure SetValue(const initVal : TComplex); overload;
    procedure SetValueSubMtx(const initVal : TComplex; x, y, aWidth, aHeight : NativeInt); overload;  // sets the value in a subsection of the matrix

    procedure SetData(data : PComplex; srcLineWidth, width, height : integer);

    procedure SwapRow( idx1, idx2 : integer ); override;
    procedure SwapColumn( idx1, idx2 : integer ); override;

    procedure Normalize(RowWise : boolean); override;
    procedure NormZeroMeanVarOne(RowWise : boolean); override;
    function ElementwiseNorm2(doSqrt : boolean = True) : double; override;

    function AddConst(const Value : TComplex) : TCplxMatrix; overload;
    procedure AddAndScaleInPlace(const Offset, Scale : double); overload; override;
    procedure AddAndScaleInPlaceC(const Offset, Scale : TComplex); overload;

    function SubColMtxIdx( colIdx : TIntegerDynArray ) : TCplxMatrix; overload;
    function SubRowMtxIdx( rowIdx : TIntegerDynArray ) : TCplxMatrix; overload;
    function SubMtxIdx( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : TCplxMatrix; overload;
    function SubColMtx( fromIdx, toIdx : integer ) : TCplxMatrix; overload;
    function SubRowMtx( fromIdx, toIdx : integer ) : TCplxMatrix; overload;
    function SubMtx( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : TCplxMatrix; overload;

    procedure RepeatMatrixInPlace(numX, numY : integer); override;

    // ###################################################
    // #### Simple matrix utility functions
    function Max : TComplex;
    function Min : TComplex;
    function SumAll : TComplex; overload;

    procedure AbsInPlace; override;
    procedure DiagInPlace(createDiagMtx : boolean); override;

    function Abs: TCplxMatrix; overload;

    function Diag(createDiagMtx : boolean) : TCplxMatrix; overload;
    function DiagFromOffset(K : integer) : TCplxMatrix; overload; // returns the diagonal on offset k
    function Trace : TComplex;

    // ###################################################
    // #### Base Matrix operations
    function Transpose : TCplxMatrix;
    function TransposeNonConj : TCplxMatrix;
    procedure TransposeInPlace; override;

    procedure AddInplace(Value : TCplxMatrix); overload; virtual;
    function Add(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;
    procedure AddVecInPlace(Value : TCplxMatrix; rowWise : Boolean); overload;
    function AddVec(aVec : TCplxMatrix; rowWise : Boolean) : TCplxMatrix; overload;

    procedure SubInPlace(Value : TCplxMatrix); overload; virtual;
    function Sub(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;

    procedure SubVecInPlace(Value : TCplxMatrix; rowWise : Boolean); overload;
    function SubVec(aVec : TCplxMatrix; rowWise : Boolean) : TCplxMatrix; overload;

    procedure MultInPlace(Value : TCplxMatrix); overload; virtual;
    function Mult(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;
    procedure MultInPlaceT1(Value : TCplxMatrix); overload; virtual;
    function MultT1(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;
    procedure MultInPlaceT2(Value : TCplxMatrix); overload; virtual;
    function MultT2(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;
    procedure ElementWiseMultInPlace(Value : TCplxMatrix); overload; virtual;
    function ElementWiseMult(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;

    procedure ElementWiseDivInPlace(Value : TCplxMatrix); overload; virtual;
    function ElementWiseDiv(Value : TCplxMatrix) : TCplxMatrix; overload; virtual;

    function AddAndScale(const Offset, Scale : TComplex) : TCplxMatrix; virtual;

    function Mean(RowWise : boolean) : TCplxMatrix;
    procedure MeanInPlace(RowWise : boolean); override;
    function Variance(RowWise : boolean; unbiased : boolean = True) : TCplxMatrix;
    function Std(RowWise : boolean; unbiased : boolean = True) : TCplxMatrix;
    procedure VarianceInPlace(RowWise : boolean; unbiased : boolean = True); override;
    procedure StdInPlace(RowWise : boolean; unbiased : boolean = True); override;

    procedure MeanVarianceInPlace(RowWise : boolean; unbiased : boolean = True); override;

     // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : TCplxMatrix;

    function Median(RowWise : boolean) : TCplxMatrix; virtual;
    procedure MedianInPlace(RowWise : boolean); override;
    function RollMedian(RowWise : boolean; order : integer) : TCplxMatrix;
    procedure RollMedianInPlace(RowWise : boolean; order : integer); override;

    function Sort(RowWise : boolean) : TCplxMatrix;
    procedure SortInPlace(RowWise : boolean); override;

    function Diff(RowWise : boolean) : TCplxMatrix;
    procedure DiffInPlace(RowWise : boolean); override;
    function Sum(RowWise : boolean) : TCplxMatrix; overload;
    procedure SumInPlace(RowWise : boolean); overload; override;
    procedure SumInPlace(RowWise : boolean; keepMemory : boolean); overload; override;

    function CumulativeSum(RowWise : boolean) : TCplxMatrix;
    procedure CumulativeSumInPlace(RowWise : boolean); override;

    procedure AddConstInPlace(const Value : double); overload; override;
    procedure AddConstInPlace(const Value : TComplex); overload;
    function Scale(const Value : TComplex) : TCplxMatrix;
    function ScaleAndAdd(const aOffset, aScale : TComplex) : TCplxMatrix;
    procedure ScaleInPlace(const Value : double); overload; override;
    procedure ScaleInPlace(const Value : TComplex); reintroduce; overload;
    procedure ScaleAndAddInPlace(const aOffset, aScale : TComplex); reintroduce; overload;
    procedure ScaleAndAddInPlace(const aOffset, aScale : double); overload; override;
    procedure SQRTInPlace; override;

    function SQRT : TCplxMatrix;

    procedure ElementwiseFuncInPlace(func : TCplxMatrixFunc); overload; virtual;
    function ElementwiseFunc(func : TCplxMatrixFunc) : TCplxMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixObjFunc); overload; virtual;
    function ElementwiseFunc(func : TCplxMatrixObjFunc) : TCplxMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixMtxRefFunc); overload; virtual;
    function ElementwiseFunc(func : TCplxMatrixMtxRefFunc) : TCplxMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixMtxRefObjFunc); overload; virtual;
    function ElementwiseFunc(func : TCplxMatrixMtxRefObjFunc) : TCplxMatrix; overload; virtual;

    {$IFDEF ANONMETHODS}
    procedure ElementwiseFuncInPlace(func : TCplxMatrixFuncRef); overload; virtual;
    function ElementwiseFunc(func : TCplxMatrixFuncRef) : TCplxMatrix; overload; virtual;
    procedure ElementwiseFuncInPlace(func : TCplxMatrixMtxRefFuncRef); overload; virtual;
    function ElementwiseFunc(func : TCplxMatrixMtxRefFuncRef) : TCplxMatrix; overload; virtual;
    {$ENDIF}


    // ###################################################
    // #### Linear System solver A*x = B -> use A.SolveLinEQ(B) -> x
    function SolveLinEQ(Value : TCplxMatrix; numRefinements : integer = 0) : TCplxMatrix; overload; virtual;
    procedure SolveLinEQInPlace(Value : TCplxMatrix; numRefinements : integer = 0); overload; virtual;

    // Matrix inversion (via LU decomposition)
    function Invert : TCplxMatrix; overload; virtual;
    function InvertEx( out InvMtx : TCplxMatrix ) : TLinEquResult; overload; virtual;
    function InvertInPlace : TLinEquResult; override;
    function Determinant : TComplex; virtual;

    // ###################################################
    // #### Special functions
    procedure MaskedSetValue(const Mask : Array of boolean; const newVal : TComplex);
    function RepeatMatrix(numX, numY : integer) : TCplxMatrix; overload;

    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(const Mtx : Array of TComplex; W, H : integer); overload;
    procedure AssignDbl( data : PDouble; aLineWidth : NativeInt; W, H : integer);
    procedure AssignDyn(Value : TComplexDynArray; W, H : integer); overload;
    procedure AssignDyn(Value : TDoubleDynArray; W, H : integer); overload;
    procedure Assign(Value : TCplxMatrix); overload;
    procedure AssignEx(Value : TCplxMatrix; OnlySubElements : boolean);

    procedure AssignSubMatrix(Value : TCplxMatrix; X : integer = 0; Y : integer = 0); overload;

    procedure Append(Value : TCplxMatrix; appendColumns : boolean); overload;

    // actually the same as assign but it takes over the data and leaves an empty matrix object behind.
    procedure TakeOver(Value : TCplxMatrix); overload;

    function Clone : TCplxMatrix;

    constructor Create(aWidth, aHeight : integer; NoLineWidthGap : boolean); overload;
    constructor Create; overload;
    constructor CreateVec( aLen : integer ); overload;
    constructor CreateVec( aLen : integer; const initVal : TComplex ); overload;
    constructor Create(aWidth, aHeight : integer); overload;
    constructor Create(aWidth, aHeight : integer; const initVal : TComplex ); overload;
    constructor CreateEye(aWidth : integer);
    constructor Create(data : PComplex; aLineWidth : NativeInt; aWidth, aHeight : integer); overload;
    constructor CreateCpy( aWidth, aHeight : integer; data : PComplex; aLineWidth : integer);   // different params than in create since delphi complains
    constructor CreateDyn(const Data : TComplexDynArray; aWidth, aHeight : integer); overload;
    constructor CreateDyn(const Data : TComplexDynArray; fromDataIdx : integer; aWidth, aHeight : integer); overload;
    constructor CreateFromDbl(const Data : TDoubleDynArray; aWidth, aHeight : integer); overload;
    constructor Create(const Mtx : Array of TComplex; W, H : integer); overload;
    constructor CreateRand(aWidth, aHeight : integer; method : TRandomAlgorithm; var seed : LongInt); overload; // uses random engine
    constructor CreateRand(aWidth, aHeight : integer); overload; // uses system default random
    constructor CreateLinSpace(aVecLen : integer; const StartVal : TComplex; const endVal : TComplex);

    class procedure CheckForComplexMtx( mtx : IMatrix );
  end;


implementation

uses CplxSimpleMatrixOperations, Math, LinAlgLU, BlockedMult, BlockSizeSetup;

{ TCplxMatrix }

function TCplxMatrix.Abs: TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     CplxGenericMtxAbs(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;

function TCplxMatrix.AbsI: IMatrix;
begin
     Result := Abs;
end;

procedure TCplxMatrix.AbsInPlace;
begin
     CplxGenericMtxAbs(StartElement, LineWidth, Width, Height);
end;

function TCplxMatrix.Add(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(fSubWidth, fSubHeight);
     CplxGenericMtxAdd(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TCplxMatrix.AddAndScale(const Offset, Scale: TComplex): TCplxMatrix;
begin
     CheckAndRaiseError((width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.AssignEx(self, True);

     Result.AddAndScaleInPlaceC(Offset, Scale);
end;


function TCplxMatrix.AddAndScaleC(const Offset, Scale: TComplex): ICplxMatrix;
begin
     Result := AddAndScale(Offset, Scale);
end;

function TCplxMatrix.AddAndScaleD(const Offset, Scale : double) : ICplxMatrix;
begin
     Result := AddAndScaleC(InitComplex(Offset, 0), InitComplex(Scale, 0));
end;

procedure TCplxMatrix.AddAndScaleInPlace(const Offset, Scale: double);
begin
     AddAndScaleInPlaceC( InitComplex(offset, 0), Initcomplex(Scale, 0) );
end;

procedure TCplxMatrix.AddAndScaleInPlaceC(const Offset, Scale: TComplex);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     CplxGenericMtxAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Offset, Scale);
end;

function TCplxMatrix.AddC(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := Add(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.AddConst(const Value: TComplex): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := Clone;
     Result.AddConstInPlace(Value);
end;

function TCplxMatrix.AddConstC(const Value: TComplex): ICplxMatrix;
begin
     Result := AddConst(Value);
end;

function TCplxMatrix.AddConstI(const Value: double): IMatrix;
begin
     Result := AddConst(InitComplex(Value, 0));
end;

procedure TCplxMatrix.AddConstInPlace(const Value: double);
begin
     AddConstInPlace(InitComplex(Value, 0));
end;

procedure TCplxMatrix.AddConstInPlace(const Value: TComplex);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     CplxGenericMtxElemAdd(StartElement, LineWidth, fSubWidth, fSubHeight, Value);
end;

//function TCplxMatrix.AddI(Value: IMatrix): IMatrix;
//begin
//     Result := Add( Value.GetObjRef as TCplxMatrix );
//end;

procedure TCplxMatrix.AddInplace(Value: TCplxMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     // inplace matrix addition
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Matrix dimensions do not match');
     CplxGenericMtxAdd(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TCplxMatrix.AddInPlaceC(Value: ICplxMatrix);
begin
     AddInPlace( Value.GetObjRef as TCplxMatrix );
end;

function TCplxMatrix.AddVec(aVec: TCplxMatrix; rowWise: Boolean): TCplxMatrix;
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

function TCplxMatrix.AddVecC(aVec: ICplxMatrix; rowWise: Boolean): ICplxMatrix;
begin
     Result := AddVec(aVec.GetObjRef as TCplxMatrix, rowWise);
end;

procedure TCplxMatrix.AddVecInPlace(Value: TCplxMatrix; rowWise: Boolean);
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

     CplxGenericAddVec(StartElement, LineWidth, Value.StartElement, incX, Width, Height, RowWise);
end;

procedure TCplxMatrix.AddVecInPlaceC(Value: ICplxMatrix; rowWise: Boolean);
begin
     AddVecInPlace(Value.GetObjRef as TCplxMatrix, rowWise);
end;

procedure TCplxMatrix.Append(Value: TCplxMatrix; appendColumns: boolean);
var pStart : PComplex;
begin
     if AppendColumns then
     begin
          CheckAndRaiseError(Value.Height = fSubHeight, 'Dimension error, number of rows are not equal');
          Resize(fSubWidth + Value.Width, fSubHeight);
          pStart := StartElement;
          inc(pStart, fSubWidth - Value.Width);
          CplxGenericMtxCopy(pStart, LineWidth, Value.StartElement, Value.LineWidth, Value.Width, Value.Height );
     end
     else
     begin
          CheckAndRaiseError(Value.Width = fSubWidth, 'Dimension error, number of columns are not equal');
          Resize(fSubWidth, Value.Height + fSubHeight);
          pStart := CplxGenPtr(StartElement, 0, fSubHeight - Value.Height, LineWidth);
          CplxGenericMtxCopy(pStart, LineWidth, Value.StartElement, Value.LineWidth, Value.Width, Value.Height );
     end;
end;

procedure TCplxMatrix.AppendC(Value: ICplxMatrix; appendColumns: boolean);
begin
     Append(Value.GetObjRef as TCplxMatrix, appendColumns);
end;

procedure TCplxMatrix.Assign(Value: TCplxMatrix);
begin
     AssignEx(Value, False);
end;

procedure TCplxMatrix.Assign(const Mtx: array of TComplex; W, H: integer);
begin
     CheckAndRaiseError((W*H > 0) and (Length(Mtx) = W*H), 'Dimension error');

     SetWidthHeight(W, H);
     CplxGenericMtxCopy(StartElement, LineWidth, @Mtx[0], W*sizeof(TComplex), W, H);
end;


procedure TCplxMatrix.AssignC(Value: ICplxMatrix);
begin
     AssignEx( Value.GetObjRef as TCplxMatrix, False);
end;

procedure TCplxMatrix.AssignDbl(data: PDouble; aLineWidth: NativeInt; W,
  H: integer);
var y: Integer;
    x : integer;
    pCplx : PConstComplexArr;
    pDbl : PConstDoubleArr;
begin
     CheckAndRaiseError((W*H > 0) and (w*sizeof(double) <= aLineWidth), 'Dimension error');

     // ###########################################
     // #### Conver the input real values to complex ones
     SetWidthHeight(w, h);
     pDbl := PConstDoubleArr(data);
     pCplx := PConstComplexArr(StartElement);

     for y := 0 to h - 1 do
     begin
          for x := 0 to w - 1 do
              pCplx^[x].real := pDbl^[x];

          inc(PByte(pCplx), LineWidth);
          inc(PByte(pDbl), aLineWidth);
     end;
end;


procedure TCplxMatrix.AssignDyn(Value: TDoubleDynArray; W, H: integer);
var y: Integer;
    x, idx : integer;
    pCplx : PConstComplexArr;
begin
     CheckAndRaiseError((W*H > 0) and (Length(Value) = W*H), 'Dimension error');

     // ###########################################
     // #### Conver the input real values to complex ones
     SetWidthHeight(w, h);
     idx := 0;
     for y := 0 to h - 1 do
     begin
          pCplx := CplxGenPtrArr(StartElement, 0, y, LineWidth);
          for x := 0 to w - 1 do
          begin
               pCplx^[x].real := Value[idx];
               inc(idx);
          end;
     end;
end;

procedure TCplxMatrix.AssignDyn(Value: TComplexDynArray; W, H: integer);
begin
     CheckAndRaiseError((W*H > 0) and (Length(Value) = W*H), 'Dimension error');

     SetWidthHeight(W, H);
     CplxGenericMtxCopy(StartElement, LineWidth, @Value[0], W*sizeof(double), W, H);
end;


procedure TCplxMatrix.AssignEx(Value: TCplxMatrix; OnlySubElements: boolean);
begin
     fName := Value.Name;

     if OnlySubElements then
     begin
          SetWidthHeight(Value.Width, Value.Height);
          CplxGenericMtxCopy(StartElement, LineWidth, Value.StartElement, Value.LineWidth, Value.Width, Value.Height);
     end
     else
     begin
          SetWidthHeight(Value.fWidth, Value.fHeight);
          CplxGenericMtxCopy(StartElement, LineWidth, PComplex(Value.fData), Value.LineWidth, Value.fWidth, Value.fHeight);
          fSubWidth := Value.fSubWidth;
          fSubHeight := Value.fSubHeight;
     end;
end;


procedure TCplxMatrix.AssignExC(Value: ICplxMatrix; OnlySubElements: boolean);
begin
     AssignEx(Value.GetObjRef as TCplxMatrix, OnlySubElements);
end;

procedure TCplxMatrix.AssignSubMatrix(Value: TCplxMatrix; X, Y: integer);
var pSelf, pValue : PComplex;
begin
     CheckAndRaiseError((Value.Width <= Width + x) and (Value.Height <= Height + y), 'Dimension error');
     if (Value.Width = 0) or (Value.Height = 0) then
        exit;

     pSelf := CplxGenPtr(StartElement, x, y, LineWidth);
     pValue := Value.StartElement;

     CplxGenericMtxCopy(pSelf, LineWidth, pValue, Value.LineWidth, Value.Width, Value.Height);
end;

procedure TCplxMatrix.AssignSubMatrixC(Value: ICplxMatrix; X, Y: integer);
begin
     AssignSubMatrix(Value.GetObjRef as TCplxMatrix, X, Y);
end;

function TCplxMatrix.AsVector(ColMajor: boolean): TCplxMatrix;
begin
     if Height = 1
     then
         Result := Clone
     else
         Result := Reshape(width*height, 1, ColMajor);
end;


function TCplxMatrix.AsVectorC(ColMajor: boolean): ICplxMatrix;
begin
     Result := AsVector(ColMajor);
end;

function TCplxMatrix.AsVectorI(ColMajor: boolean): IMatrix;
begin
     Result := AsVector(ColMajor);
end;

class procedure TCplxMatrix.CheckForComplexMtx(mtx: IMatrix);
begin
     if not (mtx.GetObjRef is TCplxMatrix) then
        raise EMatrixTypeException.Create('Operation only allowed on complex matrices');
end;

class function TCplxMatrix.ClassIdentifier: String;
begin
     Result := 'CMTX';
end;

function TCplxMatrix.Clone: TCplxMatrix;
begin
     Result := ResultClass.Create(Width, Height);
     Result.AssignEx(Self, True);
end;

function TCplxMatrix.CloneC: ICplxMatrix;
begin
     Result := Clone;
end;

function TCplxMatrix.CloneI: IMatrix;
begin
     Result := Clone;
end;

constructor TCplxMatrix.Create(aWidth, aHeight: integer;
  NoLineWidthGap: boolean);
begin
     CheckAndRaiseError((aWidth > 0) and (aHeight > 0), 'Dimension error');

     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     // just shorten the linewidth -> so we get continous element access
     if NoLineWidthGap then
        fLineWidth := aWidth*SizeOf(TComplex);
end;


constructor TCplxMatrix.Create(data: PComplex; aLineWidth: NativeInt; aWidth,
  aHeight: integer);
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (aLineWidth >= aWidth*sizeof(TComplex)), 'Dimension error');

     inherited Create;

     if (data <> nil) then
     begin
          SetWidthHeight(aWidth, aHeight);
          CplxGenericMtxCopy( StartElement, LineWidth, data, aLineWidth, aWidth, aHeight);
     end;
end;


constructor TCplxMatrix.Create(const Mtx: array of TComplex; W, H: integer);
begin
     CheckAndRaiseError((W*H > 0) and (Length(Mtx) = W*H), 'Dimension error');

     inherited Create;

     SetWidthHeight(W, H);
     CplxGenericMtxCopy(StartElement, LineWidth, @Mtx[0], W*sizeof(TComplex), W, H);
end;

constructor TCplxMatrix.Create(aWidth, aHeight: integer);
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);
end;

constructor TCplxMatrix.Create;
begin
     inherited Create;

     SetWidthHeight(1, 1);
end;

constructor TCplxMatrix.Create(aWidth, aHeight: integer;
  const initVal: TComplex);
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     if CCmp(initVal, cCplxZero) <> 0 then
        SetValue(initVal);
end;

constructor TCplxMatrix.CreateCpy(aWidth, aHeight: integer; data: PComplex;
  aLineWidth: integer);
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (aLineWidth >= aWidth*sizeof(TComplex)), 'Dimension error');

     inherited Create;

     if (data <> nil) then
     begin
          SetWidthHeight(aWidth, aHeight);
          CplxGenericMtxCopy( StartElement, LineWidth, data, aLineWidth, aWidth, aHeight);
     end;
end;

constructor TCplxMatrix.CreateFromDbl(const Data: TDoubleDynArray; aWidth,
  aHeight: integer);
var idx, x, y : integer;
    pCData : PConstComplexArr;
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (Length(Data) >= aWidth*aHeight), 'Dimension error');

     inherited Create;

     if (data <> nil) then
     begin
          idx := 0;
          SetWidthHeight(aWidth, aHeight);
          for y := 0 to aHeight - 1 do
          begin
               pCData := CplxGenPtrArr(StartElement, 0, y, LineWidth);

               for x := 0 to aWidth - 1 do
               begin
                    pCData^[x].real := Data[idx];
                    inc(idx);
               end;
          end;
     end;
end;

constructor TCplxMatrix.CreateDyn(const Data: TComplexDynArray; fromDataIdx,
  aWidth, aHeight: integer);
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (Length(Data) >= aWidth*aHeight + fromDataIdx), 'Dimension error');

     inherited Create;

     if (data <> nil) then
     begin
          SetWidthHeight(aWidth, aHeight);
          CplxGenericMtxCopy( StartElement, LineWidth, @Data[fromDataIdx], aWidth*sizeof(TComplex), aWidth, aHeight);
     end;
end;

constructor TCplxMatrix.CreateDyn(const Data: TComplexDynArray; aWidth,
  aHeight: integer);
begin
     CheckAndRaiseError((aWidth*aHeight >= 0) and (Length(Data) >= aWidth*aHeight), 'Dimension error');

     inherited Create;

     if (data <> nil) then
     begin
          SetWidthHeight(aWidth, aHeight);
          CplxGenericMtxCopy( StartElement, LineWidth, @Data[0], aWidth*sizeof(TComplex), aWidth, aHeight);
     end;
end;

constructor TCplxMatrix.CreateEye(aWidth: integer);
var i : integer;
begin
     inherited Create;

     SetWidthHeight(aWidth, aWidth);

     for i := 0 to width - 1 do
         ItemsC[i, i] := cCplxReal1;
end;


constructor TCplxMatrix.CreateLinSpace(aVecLen: integer; const StartVal,
  endVal: TComplex);
var value : TComplex;
    pVec : PComplex;
    counter: Integer;
    dx : TComplex;
begin
     assert(vecLen >= 0, 'Error: initialized by negative len');

     inherited Create;

     SetWidthHeight(1, aVecLen);

     if aVecLen = 0 then
        exit;

     dx := RCMul( 1/Math.Max(1, aVecLen - 1), CSub(EndVal, StartVal) );

     pVec := StartElement;
     value := startVal;
     for counter := 0 to aVecLen - 1 do
     begin
          pVec^ := value;
          Value := CAdd(value, dx);
          inc(PByte(pVec), LineWidth);
     end;

     // account for small accumulated errors - so at least the last
     // value is as expected
     if aVecLen > 1 then
        VecC[vecLen - 1] := EndVal;
end;


constructor TCplxMatrix.CreateRand(aWidth, aHeight: integer;
  method: TRandomAlgorithm; var seed: Integer);
begin
     inherited Create;

     SetWidthHeight(aWidth, aHeight);

     InitRandom(method, seed);
end;


constructor TCplxMatrix.CreateRand(aWidth, aHeight: integer);
var tmp : TCplxMatrix;
begin
     inherited Create;

     // note: since the random function depends on internal states of the random engine
     // we need to make sure this is NEVER called in threads -> internal states
     // could be reused and the result is not what we expect from random numbers
     // (first surfaced in threaded cholesky decomposition on my 32core machine)
     tmp := TCplxMatrix.Create( aWidth, aHeight );
     tmp.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}MtxRand);
     TakeOver(tmp);

     tmp.Free;
end;

constructor TCplxMatrix.CreateVec(aLen: integer);
begin
     inherited Create;

     SetWidthHeight(aLen, 1);
end;

constructor TCplxMatrix.CreateVec(aLen: integer; const initVal: TComplex);
begin
     inherited Create;

     SetWidthHeight(aLen, 1);
     if CCmp(initVal, cCplxZero) <> 0 then
        SetValue(initVal);
end;

function TCplxMatrix.CumulativeSum(RowWise: boolean): TCplxMatrix;
begin
     Result := ResultClass.Create(Width, Height);
     CplxGenericMtxCumulativeSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, width, height, RowWise);
end;

function TCplxMatrix.CumulativeSumI(RowWise: boolean): IMatrix;
begin
     Result := CumulativeSum(RowWise);
end;

procedure TCplxMatrix.CumulativeSumInPlace(RowWise: boolean);
begin
     CplxGenericMtxCumulativeSum(StartElement, LineWidth,
                                 StartElement, LineWidth, width, height, RowWise);
end;

// ###########################################################
// #### Persistence functions
procedure TCplxMatrix.DefineProps;
var origSubWidth, origSubHeight, origOffsetX, origOffsetY : integer;
begin
     origSubWidth := fSubWidth;
     origSubHeight := fSubHeight;
     origOffsetX := fOffsetX;
     origOffsetY := fOffsetY;

     inherited;

     // now store the data
     UseFullMatrix;
     AddCplxArr('data', SubMatrix);
     SetSubMatrix(origOffsetX, origOffsetY, origSubWidth, origSubHeight);
end;

function TCplxMatrix.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, 'data') = 0
     then
         Result := ptComplex
     else
         Result := inherited PropTypeOfName(Name);
end;

function TCplxMatrix.Determinant: TComplex;
begin
     CheckAndRaiseError((Width > 0) and (Height = Width), 'Determinant only allowed on square matrices');
     Result := CplxMatrixDeterminant(StartElement, LineWidth, fSubWidth);
end;

function TCplxMatrix.Diag(createDiagMtx: boolean): TCplxMatrix;
var x : integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if createDiagMtx then
     begin
          if (width = 1) then
          begin
               Result := ResultClass.Create(height, height);
               for x := 0 to Height - 1 do
                   Result[x, x] := VecC[x];
          end
          else if height = 1 then
          begin
               Result := ResultClass.Create(width, width);
               for x := 0 to width - 1 do
                   Result[x, x] := VecC[x];
          end
          else
          begin
               Result := ResultClass.Create(Math.Min(Width, Height), Math.Min(Width, Height));
               for x := 0 to Math.Min(Width, Height) - 1 do
                   Result[x, x] := ItemsC[x, x];
          end;
     end
     else
     begin
          Result := ResultClass.Create(1, Math.Min(Width, Height));
          for x := 0 to Result.Height - 1 do
              Result[Math.Min(x, Result.width - 1), x] := ItemsC[x, x];
     end;
end;


function TCplxMatrix.DiagFromOffset(K: integer): TCplxMatrix;
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
                   Result[x, x - k] := VecC[x];
          end
          else
          begin
               Result := ResultClass.Create(VecLen + System.abs(k), VecLen);

               for x := 0 to VecLen - 1 do
                   Result[x + k, x] := VecC[x];
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


function TCplxMatrix.DiagFromOffsetI(K: integer): IMatrix;
begin
     Result := DiagFromOffset(K);
end;

function TCplxMatrix.DiagI(createDiagMtx: boolean): IMatrix;
begin
     Result := Diag(createDiagMtx);
end;

procedure TCplxMatrix.DiagInPlace(createDiagMtx: boolean);
var dl : IMatrix;
begin
     dl := Diag(createDiagMtx);

     TakeOver(dl.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.Diff(RowWise: boolean): TCplxMatrix;
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
     CplxGenericMtxDifferentiate(Result.StartElement, Result.LineWidth, StartElement, LineWidth, Width, Height, RowWise);
end;


function TCplxMatrix.DiffC(RowWise: boolean): ICplxMatrix;
begin
     Result := Diff(RowWise);
end;

function TCplxMatrix.DiffI(RowWise: boolean): IMatrix;
begin
     Result := Diff(RowWise);
end;

procedure TCplxMatrix.DiffInPlace(RowWise: boolean);
begin
     if (width = 0) or (height = 0) then
        exit;

     CplxGenericMtxDifferentiate(StartElement, LineWidth, StartElement, LineWidth, Width, height, RowWise);

     // just reduce the sub width
     if RowWise
     then
         dec(fSubWidth)
     else
         dec(fSubHeight);
end;

function TCplxMatrix.ElementWiseDiv(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     Result := ResultClass.Create;
     Result.Assign(self);
     CplxGenericMtxElemDiv(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;


function TCplxMatrix.ElementWiseDivC(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := ElementWiseDiv(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.ElementWiseDivInPlace(Value: TCplxMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     CplxGenericMtxElemDiv(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TCplxMatrix.ElementWiseDivInPlaceC(Value: ICplxMatrix);
begin
     ElementWiseDivInPlace(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.ElementwiseFunc(func: TCplxMatrixMtxRefFunc): TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     CplxGenericMtxFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;


function TCplxMatrix.ElementwiseFunc(
  func: TCplxMatrixMtxRefObjFunc): TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     CplxGenericMtxFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;


function TCplxMatrix.ElementwiseFunc(func: TCplxMatrixFunc): TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     CplxGenericMtxFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;


function TCplxMatrix.ElementwiseFunc(func: TCplxMatrixObjFunc): TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     CplxGenericMtxFunc(Result.StartElement, Result.LineWidth, Result.Width, Result.Height, func);
end;


procedure TCplxMatrix.ElementwiseFuncInPlace(func: TCplxMatrixMtxRefFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     CplxGenericMtxFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;


procedure TCplxMatrix.ElementwiseFuncInPlace(func: TCplxMatrixMtxRefObjFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     CplxGenericMtxFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

procedure TCplxMatrix.ElementwiseFuncInPlace(func: TCplxMatrixFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     CplxGenericMtxFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

procedure TCplxMatrix.ElementwiseFuncInPlace(func: TCplxMatrixObjFunc);
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     CplxGenericMtxFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TCplxMatrix.ElementWiseMult(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');
     Result := ResultClass.Create;
     Result.Assign(self);
     CplxGenericMtxElemMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;


function TCplxMatrix.ElementWiseMultC(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := ElementWiseMult(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.ElementWiseMultInPlace(Value: TCplxMatrix);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = Value.fSubWidth) and (fSubHeight = Value.fSubHeight), 'Dimension error');

     CplxGenericMtxElemMult(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

procedure TCplxMatrix.ElementWiseMultInPlaceC(Value: ICplxMatrix);
begin
     ElementWiseMultInPlace(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.ElementwiseNorm2(doSqrt: boolean): double;
begin
     Result := 0;

     if (Width > 0) and (Height > 0) then
        Result := CplxGenericMtxElementwiseNorm2(StartElement, LineWidth, Width, Height, doSqrt);
end;

function TCplxMatrix.GetItems(x, y: integer): double;
begin
     Result := CAbs( GetItemsC(x, y) );
end;

function TCplxMatrix.GetItemsC(x, y: integer): TComplex;
var pData : PLocConstComplexArr;
begin
     CheckAndRaiseError((x >= 0) and (x < fSubWidth) and (y >= 0) and (y < fSubHeight), 'Dimension error');
     pData := PLocConstComplexArr(StartElement);
     inc(PByte(pData), y*fLineWidth);

     Result := pData^[x];
end;

class function TCplxMatrix.GetItemSize: integer;
begin
     Result := sizeof(TComplex);
end;

function TCplxMatrix.GetType: TMatrixType;
begin
     Result := mtComplex;
end;

function TCplxMatrix.GetVecItem(idx: integer): double;
begin
     Result := CAbs(GetVecItemc(idx));
end;

function TCplxMatrix.GetVecItemC(idx: integer): TComplex;
begin
     if idx < fSubWidth
     then
         Result := GetItemsC(idx, 0)
     else
         Result := GetItemsC(idx mod fSubWidth, idx div fSubWidth);
end;

procedure TCplxMatrix.InitRandom(method: TRandomAlgorithm; var seed: Integer);
begin
     fObj := TRandomGenerator.Create;
     TRandomGenerator(fObj).RandMethod := method;
     TRandomGenerator(fObj).Init(seed);
     CplxGenericMtxFunc(StartElement, LineWidth, fSubWidth, fSubHeight, {$IFDEF FPC}@{$ENDIF}MtxRandWithEng);
     seed := TRandomGenerator(fObj).RandInt;
     FreeAndNil(fObj);
end;


function TCplxMatrix.Invert: TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     Result := ResultClass.Create;
     try
        Result.AssignEx(Self, True);

        if CplxMatrixInverseInPlace(Result.StartElement, Result.LineWidth, fSubWidth, fLinEQProgress) = leSingular then
           raise ELinEQSingularException.Create('Singular matrix');
     except
           Result.Free;
           raise;
     end;
end;

function TCplxMatrix.InvertC: ICplxMatrix;
begin
     Result := Invert;
end;

function TCplxMatrix.InvertEx(out InvMtx: TCplxMatrix): TLinEquResult;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     InvMtx := ResultClass.Create;
     InvMtx.AssignEx(Self, True);

     Result := CplxMatrixInverseInPlace(InvMtx.StartElement, InvMtx.LineWidth, fSubWidth, fLinEQProgress);
     if Result <> leOk then
        FreeAndNil(invMtx);
end;


function TCplxMatrix.InvertExC(out InvMtx: ICplxMatrix): TLinEquResult;
var res : TCplxMatrix;
begin
     Result := InvertEx(res);
     InvMtx := res;
end;

function TCplxMatrix.InvertExI(out InvMtx: IMatrix): TLinEquResult;
var res : TCplxMatrix;
begin
     Result := InvertEx(res);
     InvMtx := res;
end;

function TCplxMatrix.InvertI: IMatrix;
begin
     Result := Invert;
end;

function TCplxMatrix.InvertInPlace: TLinEquResult;
var dt : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError(fSubWidth = fSubHeight, 'Operation only allowed on square matrices');

     dt := ResultClass.Create(Width, Height);
     try
        dt.AssignEx(self, True);
        Result := CplxMatrixInverseInPlace(dt.StartElement, dt.LineWidth, fSubWidth, fLinEQProgress);
        if Result = leOk then
           TakeOver(dt);

        FreeAndNil(dt);
     except
           dt.Free;
           raise;
     end;
end;

procedure TCplxMatrix.MaskedSetValue(const Mask: array of boolean;
  const newVal: TComplex);
var y : integer;
    x : integer;
    maskIdx : integer;
    pValue2 : PConstComplexArr;
    pValue1 : PByte;
begin
     CheckAndRaiseError(fSubWidth*fSubHeight = Length(Mask), 'Error number of mask elements must be the same as the matrix dimension');
     pValue1 := PByte(StartElement);
     maskIdx := 0;
     for y := 0 to fsubHeight - 1 do
     begin
          pValue2 := PConstComplexArr(pValue1);

          for x := 0 to fSubWidth - 1 do
          begin
               if not mask[maskidx] then
                  pValue2^[x] := newVal;

               inc(maskidx);
          end;
          inc(pValue1, LineWidth);
     end;
end;


function TCplxMatrix.Max: TComplex;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := CplxGenericMtxMax(StartElement, fSubWidth, fSubHeight, LineWidth);
end;

function TCplxMatrix.Mean(RowWise: boolean): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise
     then
         Result := ResultClass.Create(1, fSubHeight)
     else
         Result := ResultClass.Create(fSubWidth, 1);

     CplxGenericMtxMean(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
end;

function TCplxMatrix.MeanC(RowWise: boolean): ICplxMatrix;
begin
     Result := Mean(RowWise);
end;

function TCplxMatrix.MeanI(RowWise: boolean): IMatrix;
begin
     Result := Mean(RowWise);
end;

procedure TCplxMatrix.MeanInPlace(RowWise: boolean);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Mean(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TCplxMatrix.MeanVariance(RowWise, unbiased: boolean): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(2, fSubHeight);

          CplxGenericMtxVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 2);

          CplxGenericMtxVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end;
end;

function TCplxMatrix.MeanVarianceC(RowWise, unbiased: boolean): ICplxMatrix;
begin
     Result := MeanVariance(RowWise, unbiased);
end;

function TCplxMatrix.MeanVarianceI(RowWise, unbiased: boolean): IMatrix;
begin
     Result := MeanVariance(RowWise, unbiased);
end;

procedure TCplxMatrix.MeanVarianceInPlace(RowWise, unbiased: boolean);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := MeanVariance(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TCplxMatrix.Median(RowWise: boolean): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          CplxGenericMtxMedian(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, nil);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          CplxGenericMtxMedian(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, nil);
     end;
end;

function TCplxMatrix.MedianC(RowWise: boolean): ICplxMatrix;
begin
     Result := Median(RowWise);
end;

function TCplxMatrix.MedianI(RowWise: boolean): IMatrix;
begin
     Result := Median(RowWise);
end;

procedure TCplxMatrix.MedianInPlace(RowWise: boolean);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Median(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TCplxMatrix.Min: TComplex;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := CplxGenericMtxMin(StartElement, fSubWidth, fSubHeight, LineWidth);
end;

procedure TCplxMatrix.MtxRandWithEng(var value: TComplex);
begin
     value.real := TRandomGenerator(fObj).Random;
     value.imag := TRandomGenerator(fObj).Random;
end;

procedure TCplxMatrix.MtxRand(var value: TComplex);
begin
     value.real := Random;
     value.imag := Random;
end;

function TCplxMatrix.Mult(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubWidth, fSubHeight);

     CplxGenericMtxMult(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth);
end;

function TCplxMatrix.MultC(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := Mult(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.MultInPlace(Value: TCplxMatrix);
var dl : TCplxMatrix;
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


procedure TCplxMatrix.MultInPlaceC(Value: ICplxMatrix);
begin
     MultInPlace(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.MultInPlaceT1(Value: TCplxMatrix);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fWidth = fSubWidth) and
                        (fHeight = fSubHeight), 'Operation only allowed on full matrices');

     dl := MultT1(Value);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;


procedure TCplxMatrix.MultInPlaceT1C(Value: ICplxMatrix);
begin
     MultInPlaceT1(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.MultInPlaceT2(Value: TCplxMatrix);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fWidth = fSubWidth) and
                        (fHeight = fSubHeight), 'Operation only allowed on full matrices');

     dl := MultT2(Value);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

procedure TCplxMatrix.MultInPlaceT2C(Value: ICplxMatrix);
begin
     CheckForComplexMtx(Value);

     MultInPlaceT2(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.MultT1(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubWidth, fSubWidth);

     CplxGenericBlockMatrixMultiplicationT1(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth, BlockMatrixCacheSize, doNone, nil);
end;


function TCplxMatrix.MultT1C(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := MultT1(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.MultT2(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create(Value.fSubHeight, fSubHeight);

     CplxGenericBlockMatrixMultiplicationT2(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight,
                Value.fSubWidth, Value.fSubHeight, LineWidth, Value.LineWidth, BlockMatrixCacheSize, doNone, nil);
end;

function TCplxMatrix.MultT2C(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := MultT2(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.Normalize(RowWise: boolean);
var dt : TCplxMatrix;
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');

     dt := ResultClass.Create(Width, Height);
     try
        CplxGenericMtxNormalize(dt.StartElement, dt.LineWidth, StartElement, LineWidth, Width, Height, RowWise);
        TakeOver(dt);
     finally
            dt.Free;
     end;
end;

procedure TCplxMatrix.NormZeroMeanVarOne(RowWise: boolean);
begin
     CheckAndRaiseError((width > 0) and (height > 0), 'Dimension error');
     CplxGenericMtxNormalizeMeanVar(StartElement, LineWidth, width, height, RowWise);
end;


class function TCplxMatrix.NumberType: TNumberType;
begin
     Result := ntComplex;
end;

procedure TCplxMatrix.OnLoadCmplxArr(const Name: String;
  const Value: TComplexDynArray);
begin
     if (CompareText(Name, 'data') = 0) and (Length(Value) > 0) then
     begin
          CheckAndRaiseError(Length(value) = fWidth*fHeight, 'The loaded array does not fit the expected size.');
          Assign(Value, fWidth, fHeight);
     end;
end;

function TCplxMatrix.RepeatMatrix(numX, numY: integer): TCplxMatrix;
begin
     Result := Clone;
     Result.RepeatMatrixInPlace(numX, numY);
end;

function TCplxMatrix.RepeatMatrixC(numX, numY: integer): ICplxMatrix;
begin
     Result := RepeatMatrix(numX, numY);
end;

function TCplxMatrix.RepeatMatrixI(numX, numY: integer): IMatrix;
begin
     Result := RepeatMatrix(numX, numY);
end;

procedure TCplxMatrix.RepeatMatrixInPlace(numX, numY: integer);
var origW, origH : integer;
    aSubMtx : ICplxMatrix;
    x, y : Integer;
begin
     origW := Width;
     origH := Height;

     aSubMtx := Clone;

     SetWidthHeight( Width*numX, Height*numY );

     for y := 0 to numY - 1 do
         for x := 0 to numX - 1 do
             AssignSubMatrixI(aSubMtx, x*origW, y*origH);
end;


function TCplxMatrix.Reshape(newWidth, newHeight: integer;
  ColMajor: boolean): TCplxMatrix;
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
                       Result.ItemsC[x, y] := ItemsC[xOld, yOld];

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
                       Result.ItemsC[x, y] := ItemsC[xOld, yOld];

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


function TCplxMatrix.ReshapeC(newWidth, newHeight: integer;
  RowMajor: boolean): ICplxMatrix;
begin
     Result := Reshape(newWidth, newHeight);
end;

function TCplxMatrix.ReshapeI(newWidth, newHeight: integer;
  RowMajor: boolean): IMatrix;
begin
     Result := Reshape(newWidth, newHeight);
end;

procedure TCplxMatrix.ReshapeInPlace(newWidth, newHeight: integer;
  ColMajor: boolean);
var res : TCplxMatrix;
begin
     CheckAndRaiseError((fWidth > 0) and (fHeight > 0), 'Error operation not allowed on empty matrices');
     CheckAndRaiseError((fWidth = fSubWidth) and (fHeight = fSubHeight), 'Operation only allowed on full matrices');
     CheckAndRaiseError((newWidth*newHeight) = (fWidth*fHeight), 'Error new dimension does not fit into the old one');

     // check if the line width fits the old width -> then we only have to adjust the
     // line width parameter, otherwise copy.
     if (fWidth = fSubWidth) and (fHeight = fSubHeight) and (fLineWidth = fWidth*sizeof(TComplex))
         and not ColMajor
     then
         fLineWidth := newWidth*sizeof(TComplex)
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


class function TCplxMatrix.ResultClass: TCplxMatrixClass;
begin
     Result := TCplxMatrix;
end;

function TCplxMatrix.RollMedian(RowWise: boolean; order: integer): TCplxMatrix;
begin
     Result := Clone;
     Result.RollMedianInPlace(RowWise, order);
end;

function TCplxMatrix.RollMedianC(RowWise: boolean; order: integer): ICplxMatrix;
begin
     Result := RollMedian(RowWise, order);
end;

function TCplxMatrix.RollMedianI(RowWise: boolean; order: integer): IMatrix;
begin
     Result := RollMedian(RowWise, order);
end;

procedure TCplxMatrix.RollMedianInPlace(RowWise: boolean; order: integer);
begin
     CheckAndRaiseError( (Width > 0) and (Height > 0), 'No data assigned');
     CplxGenericMtxRollMedian(StartElement, LineWidth, Width, Height, order, RowWise);
end;

function TCplxMatrix.Scale(const Value: TComplex): TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     CplxGenericMtxAddAndScale(Result.StartElement, Result.LineWidth,
                               Result.Width, Result.Height, cCplxZero, Value);
end;


function TCplxMatrix.ScaleAndAdd(const aOffset, aScale: TComplex): TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'Dimension Error');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     CplxGenericMtxScaleAndAdd(Result.StartElement, Result.LineWidth,
                               Result.Width, Result.Height, aOffset, aScale);
end;

function TCplxMatrix.ScaleAndAddC(const aOffset, aScale: TComplex): ICplxMatrix;
begin
     Result := ScaleAndAdd(aOffset, aScale);
end;

function TCplxMatrix.ScaleAndAddI(const aOffset, aScale: double): IMatrix;
begin
     Result := ScaleAndAdd( InitComplex(aOffset, 0), InitComplex(aScale, 0));
end;

procedure TCplxMatrix.ScaleAndAddInPlace(const aOffset, aScale: double);
begin
     ScaleAndAddInPlace(InitComplex(aOffset, 0), InitComplex(aScale, 0));
end;

procedure TCplxMatrix.ScaleAndAddInPlace(const aOffset, aScale: TComplex);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     CplxGenericMtxScaleAndAdd(StartElement, LineWidth, fSubWidth, fSubHeight, aOffset, aScale);
end;

function TCplxMatrix.ScaleC(const Value: TComplex): ICplxMatrix;
begin
     Result := Scale(Value);
end;

function TCplxMatrix.ScaleI(const Value: double): IMatrix;
begin
     Result := Scale(InitComplex(Value, 0));
end;

procedure TCplxMatrix.ScaleInPlace(const Value: double);
begin
     ScaleInPlace(InitComplex(Value, 0));
end;

procedure TCplxMatrix.ScaleInPlace(const Value: TComplex);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     CplxGenericMtxScaleAndAdd(StartElement, LineWidth, fSubWidth, fSubHeight, cCplxZero, Value);
end;

procedure TCplxMatrix.SetColumn(col: integer; const Values: array of TComplex);
begin
     CheckAndRaiseError(Length(Values) = fSubHeight, 'Dimension error');
     CheckAndRaiseError((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

     CplxGenericMtxCopy( CplxGenPtr(StartElement, col, 0, LineWidth), LineWidth,
                         @Values[0], sizeof(TComplex), 1, fSubHeight );
end;

procedure TCplxMatrix.SetColumn(col: integer; Values: TCplxMatrix;
  ValCols: integer);
begin
     CheckAndRaiseError(Values.fSubHeight = fSubHeight, 'Dimension error');
     CheckAndRaiseError((col >= 0) and (col < fSubWidth), 'Error index out of bounds');

     CplxGenericMtxCopy( CplxGenPtr( StartElement, col, 0, LineWidth ), LineWidth,
                         CplxGenPtr( Values.StartElement, ValCols, 0, Values.LineWidth ),
                         values.LineWidth,
                         1, fSubHeight);
end;

procedure TCplxMatrix.SetColumnI(col: integer; Values: IMatrix;
  ValCols: integer);
begin
     CheckForComplexMtx(Values);

     SetColumn(col, Values.GetObjRef as TCplxMatrix, ValCols);
end;

procedure TCplxMatrix.SetData(data: PComplex; srcLineWidth, width,
  height: integer);
begin
     ReserveMem(width, height);

     CplxGenericMtxCopy(StartElement, LineWidth, data, srcLineWidth, width, height);
end;


procedure TCplxMatrix.SetItems(x, y: integer; const Value: double);
begin
     SetItemsC(x, y, InitComplex(Value, 0));
end;

procedure TCplxMatrix.SetItemsC(x, y: integer; const Value: TComplex);
begin
     CheckAndRaiseError((x >= 0) and (x < fSubWidth), 'Dimension error');
     CheckAndRaiseError((y >= 0) and (y < fSubHeight), 'Dimension error');
     CplxGenPtr(StartElement, x, y, LineWidth)^ := Value;
end;

procedure TCplxMatrix.SetRow(row: integer; Values: TCplxMatrix;
  ValRow: integer);
begin
     CheckAndRaiseError(Values.Width = fSubWidth, 'Dimension Error');
     CheckAndRaiseError((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     CplxGenericMtxCopy( CplxGenPtr( StartElement, 0, row, LineWidth), LineWidth,
                         CplxGenPtr(Values.StartElement, 0, ValRow, Values.LineWidth), VAlues.LineWidth,
                         fSubWidth, 1 );
end;

procedure TCplxMatrix.SetRow(row: integer; const Values: array of TComplex);
begin
     CheckAndRaiseError(Length(Values) = fSubWidth, 'Dimension Error');
     CheckAndRaiseError((row >= 0) and (row < fSubHeight), 'Error index out of bounds');

     CplxGenericMtxCopy( CplxGenPtr( StartElement, 0, row, LineWidth), LineWidth,
                         @Values[0], Length(Values)*sizeof(TComplex),
                         fSubWidth, 1 );
end;

procedure TCplxMatrix.SetRowI(row: integer; Values: IMatrix; ValRow: integer);
begin
     CheckForComplexMtx(Values);

     SetRow(row, Values.GetObjRef as TCplxMatrix, ValRow);
end;

procedure TCplxMatrix.SetValue(const initVal: TComplex);
begin
     if (Width > 0) and (height > 0) then
        CplxGenericMtxInit( StartElement, LineWidth, width, height, initVal);
end;

procedure TCplxMatrix.SetValueSubMtx(const initVal: TComplex; x, y, aWidth,
  aHeight: NativeInt);
begin
     CheckAndRaiseError( Width >= x + aWidth, 'Dimension X Error');
     CheckAndRaiseError( Height >= y + aHeight, 'Dimension Y Error');

     CplxGenericMtxInit( CplxGenPtr( StartElement, x, y, LineWidth ), LineWidth,
                         aWidth, aHeight, initVal);
end;

procedure TCplxMatrix.SetVecItem(idx: integer; const Value: double);
begin
     SetVecItemC(idx, InitComplex(Value, 0));
end;

procedure TCplxMatrix.SetVecItemC(idx: integer; const Value: TComplex);
begin
     if idx < fSubWidth
     then
         SetItemsC(idx, 0, Value)
     else
         SetItemsC(idx mod fSubWidth, idx div fSubWidth, Value);
end;

function TCplxMatrix.SolveLinEQ(Value: TCplxMatrix;
  numRefinements: integer): TCplxMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = fSubHeight) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(Value.fSubWidth, fSubHeight);
     try
        if CplxMatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement, Value.LineWidth, Result.StartElement,
                                Result.LineWidth, Value.fSubWidth, numRefinements, fLinEQProgress) = leSingular
     then
         raise ELinEQSingularException.Create('Matrix is singular');
     except
           FreeAndNil(Result);
           raise;
     end;
end;

procedure TCplxMatrix.SolveLinEQInPlace(Value: TCplxMatrix;
  numRefinements: integer);
var dt : TCplxMatrix;
begin
     // solves the System: A * x = b
     // whereas A is the matrix stored in self, and be is the matrix in Value
     // The result is a matrix having the size of Value.
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((fSubWidth = fSubHeight) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     dt := ResultClass.Create(Value.fSubWidth, fSubHeight);
     try
        if CplxMatrixLinEQSolve(StartElement, LineWidth, fSubWidth, Value.StartElement, Value.LineWidth, dt.StartElement,
                            dt.LineWidth, Value.fSubWidth, numRefinements, fLinEQProgress) = leSingular
        then
            raise ELinEQSingularException.Create('Matrix is singular');

        TakeOver(dt);
     finally
            dt.Free;
     end;
end;

function TCplxMatrix.Sort(RowWise: boolean): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.Assign(self);

     Result.SortInPlace(RowWise);
end;

function TCplxMatrix.SortI(RowWise: boolean): IMatrix;
begin
     Result := Sort(RowWise);
end;

procedure TCplxMatrix.SortInPlace(RowWise: boolean);
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     CplxGenericMtxSort(StartElement, LineWidth, width, Height, RowWise, nil);
end;

function TCplxMatrix.SQRT: TCplxMatrix;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := ResultClass.Create;
     Result.AssignEx(Self, True);

     CplxGenericMtxSqrt(Result.StartElement, Result.LineWidth, Result.Width, Result.Height);
end;


function TCplxMatrix.SQRTC: ICplxMatrix;
begin
     Result := SQRT;
end;

function TCplxMatrix.SQRTI: IMatrix;
begin
     Result := SQRT;
end;

procedure TCplxMatrix.SQRTInPlace;
begin
     CheckAndRaiseError((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     CplxGenericMtxSqrt(StartElement, LineWidth, Width, Height);
end;

function TCplxMatrix.StartElement: PComplex;
begin
     Result := PComplex(inherited StartElement);
end;

function TCplxMatrix.Std(RowWise, unbiased: boolean): TCplxMatrix;
begin
     Result := Variance(RowWise, unbiased);
     Result.SQRTInPlace;
end;

function TCplxMatrix.StdC(RowWise, unbiased: boolean): ICplxMatrix;
begin
     Result := Std(RowWise, unbiased);
end;

function TCplxMatrix.StdI(RowWise, unbiased: boolean): IMatrix;
begin
     Result := Std(RowWise, unbiased);
end;

procedure TCplxMatrix.StdInPlace(RowWise, unbiased: boolean);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Std(RowWise, unbiased);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

function TCplxMatrix.Sub(Value: TCplxMatrix): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');

     Result := ResultClass.Create(fSubWidth, fSubHeight);
     CplxGenericMtxSub(Result.StartElement, Result.LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;


function TCplxMatrix.SubC(Value: ICplxMatrix): ICplxMatrix;
begin
     Result := Sub(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.SubColMtx(fromIdx, toIdx: integer): TCplxMatrix;
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

function TCplxMatrix.SubColMtxC(fromIdx, toIdx: integer): ICplxMatrix;
begin
     Result := SubColMtx(fromIdx, toIdx);
end;

function TCplxMatrix.SubColMtxI(fromIdx, toIdx: integer): IMatrix;
begin
     Result := SubColMtx(fromIdx, toIdx);
end;

function TCplxMatrix.SubColMtxIdx(colIdx: TIntegerDynArray): TCplxMatrix;
begin
     Result := SubMtxIdx(colIdx, nil);
end;

function TCplxMatrix.SubColMtxIdxC(colIdx: TIntegerDynArray): ICplxMatrix;
begin
     Result := SubMtxIdx(colIdx, nil);
end;

function TCplxMatrix.SubColMtxIdxI(colIdx: TIntegerDynArray): IMatrix;
begin
     Result := SubMtxIdx(colIdx, nil);
end;

procedure TCplxMatrix.SubInplaceC(Value: ICplxMatrix);
begin
     SubInPlace(Value.GetObjRef as TCplxMatrix);
end;

procedure TCplxMatrix.SubInPlace(Value: TCplxMatrix);
begin
     CheckAndRaiseError((Value.fSubWidth = fSubWidth) and (Value.fSubHeight = fSubHeight), 'Dimension error');
     CplxGenericMtxSub(StartElement, LineWidth, StartElement, Value.StartElement, fSubWidth, fSubHeight, LineWidth, Value.LineWidth);
end;

function TCplxMatrix.SubMatrix: TComplexDynArray;
begin
     Result := nil;
     if fSubWidth*fSubHeight = 0 then
        exit;

     SetLength(Result, fSubWidth*fSubHeight);
     CplxGenericMtxCopy( @Result[0], fSubWidth*sizeof(TComplex), StartElement, LineWidth, fSubWidth, fSubHeight);
end;


function TCplxMatrix.SubMtx(fromColIdx, ToColIdx, fromRowIdx,
  ToRowIdx: integer): TCplxMatrix;
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


function TCplxMatrix.SubMtxC(fromColIdx, ToColIdx, fromRowIdx,
  ToRowIdx: integer): ICplxMatrix;
begin
     Result := SubMtx(fromColIdx, ToColIdx, fromRowIdx, ToRowIdx);
end;

function TCplxMatrix.SubMtxI(fromColIdx, ToColIdx, fromRowIdx,
  ToRowIdx: integer): IMatrix;
begin
     Result := SubMtx(fromColIdx, ToColIdx, fromRowIdx, ToRowIdx);
end;

function TCplxMatrix.SubMtxIdx(colIdx, rowIdx: TIntegerDynArray): TCplxMatrix;
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

     CplxGenericMtxIndex(Result.StartElement, Result.LineWidth, StartElement, LineWidth, colIdx, rowIdx);
end;


function TCplxMatrix.SubMtxIdxC(colIdx, rowIdx: TIntegerDynArray): ICplxMatrix;
begin
     Result := SubMtxIdx(colIdx, rowIdx);
end;

function TCplxMatrix.SubMtxIdxI(colIdx, rowIdx: TIntegerDynArray): IMatrix;
begin
     Result := SubMtxIdx(colIdx, rowIdx);
end;

function TCplxMatrix.SubRowMtx(fromIdx, toIdx: integer): TCplxMatrix;
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


function TCplxMatrix.SubRowMtxC(fromIdx, toIdx: integer): ICplxMatrix;
begin
     Result := SubRowMtx(fromIdx, toIdx);
end;

function TCplxMatrix.SubRowMtxI(fromIdx, toIdx: integer): IMatrix;
begin
     Result := SubRowMtx(fromIdx, toIdx);
end;

function TCplxMatrix.SubRowMtxIdx(rowIdx: TIntegerDynArray): TCplxMatrix;
begin
     Result := SubMtxIdx( nil, rowIdx );
end;

function TCplxMatrix.SubRowMtxIdxC(rowIdx: TIntegerDynArray): ICplxMatrix;
begin
     Result := SubMtxIdx(nil, rowIdx);
end;

function TCplxMatrix.SubRowMtxIdxI(rowIdx: TIntegerDynArray): IMatrix;
begin
     Result := SubRowMtxIdx(rowIdx);
end;

function TCplxMatrix.SubVec(aVec: TCplxMatrix; rowWise: Boolean): TCplxMatrix;
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


function TCplxMatrix.SubVecC(aVec: ICplxMatrix; rowWise: Boolean): ICplxMatrix;
begin
     Result := SubVec(aVec.GetObjRef as TCplxMatrix, rowWise);
end;

//function TCplxMatrix.SubVecI(aVec: IMatrix; rowWise: Boolean): IMatrix;
//begin
//     CheckForComplexMtx(aVec);
//     Result := SubVec(aVec.GetObjRef as TCplxMatrix, rowWise);
//end;

procedure TCplxMatrix.SubVecInPlace(Value: TCplxMatrix; rowWise: Boolean);
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

     CplxGenericSubVec(StartElement, LineWidth, Value.StartElement, incX, Width, Height, RowWise);
end;


procedure TCplxMatrix.SubVecInPlaceC(Value: ICplxMatrix; rowWise: Boolean);
begin
     SubVecInPlace(Value.GetObjRef as TCplxMatrix, rowWise);
end;

function TCplxMatrix.Sum(RowWise: boolean): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          CplxGenericMtxSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          CplxGenericMtxSum(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
     end;
end;


function TCplxMatrix.SumAll: TComplex;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := CplxGenericMtxSumSum(StartElement, LineWidth, width, height);
end;

function TCplxMatrix.SumC(RowWise: boolean): ICplxMatrix;
begin
     Result := Sum(RowWise);
end;

function TCplxMatrix.SumI(RowWise: boolean): IMatrix;
begin
     Result := Sum(RowWise);
end;

procedure TCplxMatrix.SumInPlace(RowWise: boolean);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Sum(RowWise);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

procedure TCplxMatrix.SumInPlace(RowWise, keepMemory: boolean);
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
               CplxGenericMtxSum(PComplex(fData), LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
               fOffsetX := 0;
               fSubWidth := 1;
          end
          else
          begin
               CplxGenericMtxSum(PComplex(fData), LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise);
               fOffsetY := 0;
               fSubHeight := 1;
          end;
     end;
end;


procedure TCplxMatrix.SwapColumn(idx1, idx2: integer);
begin
     CheckAndRaiseError((idx1 >= 0) and (idx1 < Width), 'Dimension error');
     CheckAndRaiseError((idx2 >= 0) and (idx2 < Width), 'Dimension error');

     if idx1 <> idx2 then
        CplxGenericColSwap( CplxGenPtr( StartElement, idx1, 0, LineWidth ),
                            CplxGenPtr( StartElement, idx2, 0, LineWidth ),
                            LineWidth,
                            Height
                          );

end;


procedure TCplxMatrix.SwapRow(idx1, idx2: integer);
begin
     CheckAndRaiseError((idx1 >= 0) and (idx1 < Height), 'Dimension error');
     CheckAndRaiseError((idx2 >= 0) and (idx2 < Height), 'Dimension error');

     if idx1 <> idx2 then
        CplxGenericRowSwap( CplxGenPtr( StartElement, 0, idx1, LineWidth ),
                            CplxGenPtr( StartElement, 0, idx2, LineWidth ),
                            Width
                          );

end;

procedure TCplxMatrix.TakeOver(Value: TCplxMatrix);
begin
     DoTakeOver(Value);
end;

procedure TCplxMatrix.TakeOverC(Value: ICplxMatrix);
begin
     TakeOver(Value.GetObjRef as TCplxMatrix);
end;

function TCplxMatrix.Trace: TComplex;
var i : Integer;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     CheckAndRaiseError((Width = Height), 'Trace only defined on square matrices');

     Result := cCplxZero;
     for i := 0 to Width - 1 do
         CInc(Result, ItemsC[i, i]);
end;

function TCplxMatrix.Transpose: TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := ResultClass.Create(fSubHeight, fSubWidth);
     CplxGenericMtxTranspose(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight);
end;

function TCplxMatrix.TransposeC: ICplxMatrix;
begin
     Result := Transpose;
end;

function TCplxMatrix.TransposeI: IMatrix;
begin
     Result := Transpose;
end;

procedure TCplxMatrix.TransposeInPlace;
var dt : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');
     // transpose is only allowed on full matrix
     CheckAndRaiseError((fOffsetX = 0) and (fOffsetY = 0) and (fSubWidth = fWidth) and (fSubHeight = fHeight), 'Operation only allowed on full matrices');

     // in case of vectors we can fasten things up a bit
     if (fwidth = 1) and (LineWidth = sizeof(TComplex)) then
     begin
          fWidth := fHeight;
          fSubWidth := fSubHeight;
          fHeight := 1;
          fSubHeight := 1;
          fLineWidth := fWidth*sizeof(TComplex);
          exit;
     end;

     if fHeight = 1 then
     begin
          fHeight := fWidth;
          fSubHeight := fSubWidth;
          fWidth := 1;
          fSubWidth := 1;
          fLineWidth := sizeof(TComplex);
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

function TCplxMatrix.TransposeNonConj: TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     Result := ResultClass.Create(fSubHeight, fSubWidth);
     CplxGenericMtxTransposeNoConj(Result.StartElement, Result.LineWidth, StartElement,
                                   LineWidth, fSubWidth, fSubHeight);
end;

function TCplxMatrix.TransposeNonConjC: ICplxMatrix;
begin
     Result := TransposeNonConj;
end;

function TCplxMatrix.Variance(RowWise, unbiased: boolean): TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     if RowWise then
     begin
          Result := ResultClass.Create(1, fSubHeight);

          CplxGenericMtxVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end
     else
     begin
          Result := ResultClass.Create(fSubWidth, 1);

          CplxGenericMtxVar(Result.StartElement, Result.LineWidth, StartElement, LineWidth, fSubWidth, fSubHeight, RowWise, unbiased);
     end;
end;


function TCplxMatrix.VarianceC(RowWise, unbiased: boolean): ICplxMatrix;
begin
     Result := Variance(RowWise, unbiased);
end;

function TCplxMatrix.VarianceI(RowWise, unbiased: boolean): IMatrix;
begin
     Result := Variance(RowWise, unbiased);
end;

procedure TCplxMatrix.VarianceInPlace(RowWise, unbiased: boolean);
var dl : TCplxMatrix;
begin
     CheckAndRaiseError((Width > 0) and (Height > 0), 'No data assigned');

     dl := Variance(RowWise, unbiased);
     try
        TakeOver(dl);
     finally
            dl.Free;
     end;
end;

initialization
  RegisterCastClass(ICplxMatrix, TCplxMatrix);
  RegisterMathIO(TCplxMatrix);


end.
