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

unit Matrix;

interface

uses SysUtils, Classes, BaseMathPersistence, MatrixConst, Types,
     RandomEng;

// ###########################################
// #### Abstract base class. The memory management base class for various matrix classes
// is implemented here. Every other method is abstract
type
  EBaseMatrixException = class(Exception);
  EMatrixTypeException = class(EBaseMatrixException);
  ELinEQSingularException = class(EBaseMatrixException);

  TMatrixType = (mtReal, mtComplex);

  TBaseMatrix = class;
  IMatrix = interface(IMathPersistence)
  ['{71AE57A2-6389-4234-A09D-5552D0DFB4E2}']
    procedure Clear;
    procedure SetWidth(const Value : integer);
    procedure SetHeight(const Value : integer);
    function GetSubWidth : integer;
    function GetSubHeight : integer;
    function GetVecLen : integer;
    function GetType : TMatrixType;

    function GetItems(x, y: integer): double;
    procedure SetItems(x, y: integer; const Value: double);

    function GetVecItem(idx: integer): double;
    procedure SetVecItem(idx: integer; const Value: double);

    // general access
    property Items[x, y : integer] : double read GetItems write SetItems; default;
    property Vec[idx : integer] : double read GetVecItem write SetVecItem; // matrix as vector

    function GetObjRef : TBaseMatrix;
    function Cast( const IID : TGuid ) : IMatrix; // cast and clone to double or complex matrix

    procedure SetLinEQProgress(value : TLinEquProgress);
    function GetLinEQProgress : TLinEquProgress;

    property Width : integer read GetSubWidth write SetWidth;
    property Height : integer read GetSubHeight write SetHeight;
    property LinEQProgress : TLinEquProgress read GetLinEQProgress write SetLinEQProgress;

    procedure SetWidthHeight(const aWidth, aHeight : integer);
    procedure Resize(aNewWidth, aNewHeight : Integer);           // setwidthheight with data preserve

    // direct access functionality (use only when you know what you are doing!)
    function StartElement : PDouble;
    function LineWidth : NativeInt;

    procedure SetSubMatrix(x, y, Subwidth, Subheight : integer);
    procedure UseFullMatrix;

    // ###########################################
    // #### common matrix functions shared by all matrix classes
    procedure SetRow(row : integer; Values : IMatrix; ValRow : integer = 0); overload;
    procedure SetColumn(col : integer; Values : IMatrix; ValCols : integer = 0); overload;

    procedure SwapRow( idx1, idx2 : integer );
    procedure SwapColumn( idx1, idx2 : integer );

    function SubColMtxIdx( colIdx : TIntegerDynArray ) : IMatrix; overload;
    function SubRowMtxIdx( rowIdx : TIntegerDynArray ) : IMatrix; overload;
    function SubMtxIdx( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : IMatrix; overload;
    function SubColMtx( fromIdx, toIdx : integer ) : IMatrix; overload;
    function SubRowMtx( fromIdx, toIdx : integer ) : IMatrix; overload;
    function SubMtx( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : IMatrix; overload;

    function Reshape(newWidth, newHeight : integer; RowMajor : boolean = False) : IMatrix;
    procedure ReshapeInPlace(newWidth, newHeight : integer; ColMajor : boolean = False);

    function AsVector( ColMajor : boolean = False ) : IMatrix;

    // ###################################################
    // #### Simple matrix utility functions
    function Abs : IMatrix;
    procedure AbsInPlace;

    procedure DiagInPlace(createDiagMtx : boolean);
    function Diag(createDiagMtx : boolean) : IMatrix;
    function DiagFromOffset(K : integer) : IMatrix;  // returns the diagonal on offset k

    procedure Normalize(RowWise : boolean);
    procedure NormZeroMeanVarOne(RowWise : boolean);
    function ElementwiseNorm2(doSqrt : boolean = True) : double;

    // ###################################################
    // #### Base Matrix operations
    function Transpose : IMatrix;
    procedure TransposeInPlace;

    procedure AddInplace(Value : IMatrix);
    function Add(Value : IMatrix) : IMatrix;
    procedure AddVecInPlace(Value : IMatrix; rowWise : Boolean);
    function AddVec(aVec : IMatrix; rowWise : Boolean) : IMatrix;

    procedure SubInPlace(Value : IMatrix);
    function Sub(Value : IMatrix) : IMatrix;
    procedure SubVecInPlace(Value : IMatrix; rowWise : Boolean);
    function SubVec(aVec : IMatrix; rowWise : Boolean) : IMatrix;

    procedure MultInPlace(Value : IMatrix);
    function Mult(Value : IMatrix) : IMatrix;

    // multT1: dest = mt1' * mt2     mt1' = mt1.transpose
    procedure MultInPlaceT1(Value : IMatrix);
    function MultT1(Value : IMatrix) : IMatrix;

    procedure MultInPlaceT2(Value : IMatrix);
    function MultT2(Value : IMatrix) : IMatrix;

    procedure ElementWiseMultInPlace(Value : IMatrix);
    function ElementWiseMult(Value : IMatrix) : IMatrix;
    procedure ElementWiseDivInPlace(Value : IMatrix);
    function ElementWiseDiv(Value : IMatrix) : IMatrix;

    procedure AddAndScaleInPlace(const Offset, Scale : double);
    function AddAndScale(const Offset, Scale : double) : IMatrix;

    function Mean(RowWise : boolean) : IMatrix;
    procedure MeanInPlace(RowWise : boolean);
    function Variance(RowWise : boolean; unbiased : boolean = True) : IMatrix;
    procedure VarianceInPlace(RowWise : boolean; unbiased : boolean = True);
    function Std(RowWise : boolean; unbiased : boolean = True) : IMatrix;
    procedure StdInPlace(RowWise : boolean; unbiased : boolean = True);

    // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVariance(RowWise : boolean; unbiased : boolean = True) : IMatrix;
    procedure MeanVarianceInPlace(RowWise : boolean; unbiased : boolean = True);

    function Median(RowWise : boolean) : IMatrix;
    procedure MedianInPlace(RowWise : boolean);
    function RollMedian(RowWise : boolean; order : integer) : IMatrix;
    procedure RollMedianInPlace(RowWise : boolean; order : integer);

    procedure SortInPlace(RowWise : boolean);
    function Sort(RowWise : boolean) : IMatrix;

    function Diff(RowWise : boolean) : IMatrix;
    procedure DiffInPlace(RowWise : boolean);
    function Sum(RowWise : boolean) : IMatrix; overload;
    procedure SumInPlace(RowWise : boolean); overload;
    procedure SumInPlace(RowWide : boolean; keepMemory : boolean); overload;
    function CumulativeSum(RowWise : boolean) : IMatrix;
    procedure CumulativeSumInPlace(RowWise : boolean);

    function AddConst(const Value : double) : IMatrix;
    procedure AddConstInPlace(const Value : double);
    function Scale(const Value : double) : IMatrix;
    procedure ScaleInPlace(const Value : Double);
    function ScaleAndAdd(const aOffset, aScale : double) : IMatrix;
    procedure ScaleAndAddInPlace(const aOffset, aScale : double);
    function SQRT : IMatrix;
    procedure SQRTInPlace;


    // ###################################################
    // #### Matrix assignment operations
    procedure Assign(Value : IMatrix); overload;
    procedure AssignEx(Value : IMatrix; OnlySubElements : boolean); overload;
    procedure AssignSubMatrix(Value : IMatrix; X : integer = 0; Y : integer = 0);
    procedure Append(Value : IMatrix; appendColumns : boolean); overload;

    procedure InitRandom(method : TRandomAlgorithm; var seed : LongInt);

    // ###########################################
    // #### Linear systems
    function SolveLinEQ(Value : IMatrix; numRefinements : integer = 0) : IMatrix;
    procedure SolveLinEQInPlace(Value : IMatrix; numRefinements : integer = 0);

    // matrix inversion (based on LU decomposition)
    function InvertInPlace : TLinEquResult;
    function Invert : IMatrix; overload;
    function InvertEx( out InvMtx : IMatrix ) : TLinEquResult; overload;


    // moves data from Value to self and clears original object
    procedure TakeOver(Value : IMatrix);
    function Clone : IMatrix;

    // ###################################################
    // #### Special functions
    procedure RepeatMatrixInPlace(numX, numY : integer);
    function RepeatMatrix(numX, numY : integer) : IMatrix;

    property VecLen : integer read GetVecLen;
  end;
  IMatrixDynArr = Array of IMatrix;

// ###########################################
// #### Memory handling of for the (for now) 2 matrix classes bundled in one base class
  TBaseMatrixClass = class of TBaseMatrix;
  TBaseMatrix = class(TBaseMathPersistence, IMatrix)
  protected
    fMemory : Pointer;             // unaligned memory block
    fData : PByte;                 // 32 byte aligned data
    fItemSize : integer;           // 8 = double, 16 = complex
    fLineWidth : NativeInt;

    fHeight : integer;
    fWidth : integer;
    fSubWidth : integer;
    fSubHeight : integer;
    fOffsetX : integer;
    fOffsetY : integer;

    fLinEQProgress : TLinEquProgress;

    fName: string;

    class function NumberType : TNumberType; virtual; abstract;
    procedure CheckAndRaiseError(assertionVal : boolean; const msg : string); inline;

    procedure Clear;
    procedure SetWidth(const Value : integer);
    procedure SetHeight(const Value : integer);
    function GetSubWidth : integer;
    function GetSubHeight : integer;
    function GetVecLen : integer;

    procedure SetLinEQProgress(value : TLinEquProgress);
    function GetLinEQProgress : TLinEquProgress;

    procedure InternalSetWidthHeight(const aWidth, aHeight : integer; AssignMem : boolean = True);
    procedure ReserveMem(width, height: integer);

    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    function GetObjRef : TBaseMatrix;

    procedure OnLoadIntProperty(const Name : String; Value : integer); overload; override;
    procedure OnLoadStringProperty(const Name : String; const Value : String); overload; override;

    class function GetItemSize : integer; virtual; abstract;
  protected
    // ###########################################
    // #### Interface mapping - we cannot define a different matrix return value without this trick
    procedure IMatrix.SetRow = SetRowI;
    procedure IMatrix.SetColumn = SetColumnI;
    function IMatrix.SubColMtxIdx = SubColMtxIdxI;
    function IMatrix.SubRowMtxIdx = SubRowMtxIdxI;
    function IMatrix.SubMtxIdx = SubMtxIdxI;
    function IMatrix.SubColMtx = SubColMtxI;
    function IMatrix.SubRowMtx = SubRowMtxI;
    function IMatrix.SubMtx = SubMtxI;
    function IMatrix.Reshape = ReshapeI;
    function IMatrix.AsVector = AsVectorI;
    function IMatrix.Abs = AbsI;

    function IMatrix.Diag = DiagI;
    function IMatrix.DiagFromOffset = DiagFromOffsetI;

    // ###################################################
    // #### Base Matrix operations
    function IMatrix.Transpose = TransposeI;

    procedure IMatrix.AddInplace = AddInplaceI;
    function IMatrix.Add = AddI;
    procedure IMatrix.AddVecInPlace = AddVecInPlaceI;
    function IMatrix.AddVec = AddVecI;

    procedure IMatrix.SubInPlace = SubInPlaceI;
    function IMatrix.Sub = SubI;
    procedure IMatrix.SubVecInPlace = SubVecInPlaceI;
    function IMatrix.SubVec = SubVecI;

    procedure IMatrix.MultInPlace = MultInPlaceI;
    function IMatrix.Mult = MultI;

    // multT1: dest = mt1' * mt2     mt1' = mt1.transpose
    procedure IMatrix.MultInPlaceT1 = MultInPlaceT1I;
    function IMatrix.MultT1 = MultT1I;

    procedure IMatrix.MultInPlaceT2 = MultInPlaceT2I;
    function IMatrix.MultT2 = MultT2I;

    procedure IMatrix.ElementWiseMultInPlace = ElementWiseMultInPlaceI;
    function IMatrix.ElementWiseMult = ElementWiseMultI;
    procedure IMatrix.ElementWiseDivInPlace = ElementWiseDivInPlaceI;
    function IMatrix.ElementWiseDiv = ElementWiseDivI;

    function IMatrix.AddAndScale = AddAndScaleI;

    function IMatrix.Mean = MeanI;
    function IMatrix.Variance = VarianceI;
    function IMatrix.Std = StdI;
    function IMatrix.MeanVariance = MeanVarianceI;

    function IMatrix.Median = MedianI;
    function IMatrix.RollMedian = RollMedianI;

    function IMatrix.Sort = SortI;

    function IMatrix.Diff = DiffI;
    function IMatrix.Sum = SumI;
    function IMatrix.CumulativeSum = CumulativeSumI;

    function IMatrix.AddConst = AddConstI;
    function IMatrix.Scale = ScaleI;
    function IMatrix.ScaleAndAdd = ScaleAndAddI;
    function IMatrix.SQRT = SQRTI;

    // ###################################################
    // #### Matrix assignment operations
    procedure IMatrix.Assign = AssignI;
    procedure IMatrix.AssignEx = AssignExI;
    procedure IMatrix.AssignSubMatrix = AssignSubMatrixI;
    procedure IMatrix.Append = AppendI;
    function IMatrix.Cast = CastI;

    // ###########################################
    // #### Linear systems
    function IMatrix.SolveLinEQ = SolveLinEQI;
    procedure IMatrix.SolveLinEQInPlace = SolveLinEQInPlaceI;

    // matrix inversion (based on LU decomposition)
    function IMatrix.Invert = InvertI;
    function IMatrix.InvertEx = InvertExI;

    // moves data from Value to self and clears original object
    procedure IMatrix.TakeOver = TakeOverI;
    function IMatrix.Clone = CloneI;

    // ###################################################
    // #### Special functions
    function IMatrix.RepeatMatrix = RepeatMatrixI;

  protected
    // ###########################################
    // #### Interface functions - all are abstract and implemented by the derrived classes
    function GetItems(x, y: integer): double; virtual; abstract;
    procedure SetItems(x, y: integer; const Value: double); virtual; abstract;

    function GetVecItem(idx: integer): double; virtual; abstract;
    procedure SetVecItem(idx: integer; const Value: double); virtual; abstract;

    function GetType : TMatrixType; virtual; abstract;
    procedure SetRowI(row : integer; Values : IMatrix; ValRow : integer = 0); virtual; abstract;
    procedure SetColumnI(col : integer; Values : IMatrix; ValCols : integer = 0); virtual; abstract;

    procedure SwapRow( idx1, idx2 : integer ); virtual; abstract;
    procedure SwapColumn( idx1, idx2 : integer ); virtual; abstract;

    function SubColMtxIdxI( colIdx : TIntegerDynArray ) : IMatrix; overload; virtual; abstract;
    function SubRowMtxIdxI( rowIdx : TIntegerDynArray ) : IMatrix; overload; virtual; abstract;
    function SubMtxIdxI( colIdx : TIntegerDynArray; rowIdx : TIntegerDynArray ) : IMatrix; overload; virtual; abstract;
    function SubColMtxI( fromIdx, toIdx : integer ) : IMatrix; overload; virtual; abstract;
    function SubRowMtxI( fromIdx, toIdx : integer ) : IMatrix; overload; virtual; abstract;
    function SubMtxI( fromColIdx, ToColIdx, fromRowIdx, ToRowIdx : integer ) : IMatrix; overload; virtual; abstract;

    function ReshapeI(newWidth, newHeight : integer; ColMajor : boolean = False) : IMatrix; virtual; abstract;
    procedure ReshapeInPlace(newWidth, newHeight : integer; ColMajor : boolean = False); virtual; abstract;

    function AsVectorI( ColMajor : boolean = False ) : IMatrix; virtual; abstract;

    // ###################################################
    // #### Simple matrix utility functions
    function AbsI : IMatrix; virtual; abstract;
    procedure AbsInPlace; virtual; abstract;

    procedure DiagInPlace(createDiagMtx : boolean); virtual; abstract;
    function DiagI(createDiagMtx : boolean) : IMatrix; overload; virtual; abstract;
    function DiagFromOffsetI(K : integer) : IMatrix; overload; virtual; abstract; // returns the diagonal on o ffset k

    procedure Normalize(RowWise : boolean); virtual; abstract;
    procedure NormZeroMeanVarOne(RowWise : boolean); virtual; abstract;
    function ElementwiseNorm2(doSqrt : boolean = True) : double; virtual; abstract;

    // ###################################################
    // #### Base Matrix operations
    function TransposeI : IMatrix; virtual; abstract;
    procedure TransposeInPlace; virtual; abstract;

    procedure AddInplaceI(Value : IMatrix); overload;
    function AddI(Value : IMatrix) : IMatrix; overload;
    procedure AddVecInPlaceI(Value : IMatrix; rowWise : Boolean);
    function AddVecI(aVec : IMatrix; rowWise : Boolean) : IMatrix;

    procedure SubInPlaceI(Value : IMatrix);
    function SubI(Value : IMatrix) : IMatrix;
    procedure SubVecInPlaceI(Value : IMatrix; rowWise : Boolean);
    function SubVecI(aVec : IMatrix; rowWise : Boolean) : IMatrix;

    procedure MultInPlaceI(Value : IMatrix);
    function MultI(Value : IMatrix) : IMatrix;

    // multT1: dest = mt1' * mt2     mt1' = mt1.transpose
    procedure MultInPlaceT1I(Value : IMatrix);
    function MultT1I(Value : IMatrix) : IMatrix;

    procedure MultInPlaceT2I(Value : IMatrix);
    function MultT2I(Value : IMatrix) : IMatrix;

    procedure ElementWiseMultInPlaceI(Value : IMatrix);
    function ElementWiseMultI(Value : IMatrix) : IMatrix;
    procedure ElementWiseDivInPlaceI(Value : IMatrix);
    function ElementWiseDivI(Value : IMatrix) : IMatrix;

    procedure AddAndScaleInPlace(const Offset, Scale : double); virtual; abstract;
    function AddAndScaleI(const Offset, Scale : double) : IMatrix;

    function MeanI(RowWise : boolean) : IMatrix; virtual; abstract;
    procedure MeanInPlace(RowWise : boolean); virtual; abstract;
    function VarianceI(RowWise : boolean; unbiased : boolean = True) : IMatrix; virtual; abstract;
    procedure VarianceInPlace(RowWise : boolean; unbiased : boolean = True); virtual; abstract;
    function StdI(RowWise : boolean; unbiased : boolean = True) : IMatrix; virtual; abstract;
    procedure StdInPlace(RowWise : boolean; unbiased : boolean = True); virtual; abstract;

    // calculates mean and variance in one step and stores the mean in the first row, the variance in the second
    // if RowWise is selected. If rowwise is false it's vice versa
    function MeanVarianceI(RowWise : boolean; unbiased : boolean = True) : IMatrix; virtual; abstract;
    procedure MeanVarianceInPlace(RowWise : boolean; unbiased : boolean = True); virtual; abstract;

    function MedianI(RowWise : boolean) : IMatrix; virtual; abstract;
    procedure MedianInPlace(RowWise : boolean); virtual; abstract;
    function RollMedianI(RowWise : boolean; order : integer) : IMatrix; virtual; abstract;
    procedure RollMedianInPlace(RowWise : boolean; order : integer); virtual; abstract;

    procedure SortInPlace(RowWise : boolean); virtual; abstract;
    function SortI(RowWise : boolean) : IMatrix; virtual; abstract;

    function DiffI(RowWise : boolean) : IMatrix; virtual; abstract;
    procedure DiffInPlace(RowWise : boolean); virtual; abstract;
    function SumI(RowWise : boolean) : IMatrix; virtual; abstract;
    procedure SumInPlace(RowWise : boolean); overload; virtual; abstract;
    procedure SumInPlace(RowWide : boolean; keepMemory : boolean); overload; virtual; abstract;
    function CumulativeSumI(RowWise : boolean) : IMatrix; virtual; abstract;
    procedure CumulativeSumInPlace(RowWise : boolean); virtual; abstract;

    function AddConstI(const Value : double) : IMatrix; overload; virtual; abstract;
    procedure AddConstInPlace(const Value : double); overload; virtual; abstract;
    function ScaleI(const Value : double) : IMatrix; virtual; abstract;
    procedure ScaleInPlace(const Value : Double); virtual; abstract;
    function ScaleAndAddI(const aOffset, aScale : double) : IMatrix; virtual; abstract;
    procedure ScaleAndAddInPlace(const aOffset, aScale : double); virtual; abstract;
    function SQRTI : IMatrix; virtual; abstract;
    procedure SQRTInPlace; virtual; abstract;


    // ###################################################
    // #### Matrix assignment operations
    procedure AssignI(Value : IMatrix); overload;
    procedure AssignExI(Value : IMatrix; OnlySubElements : boolean); overload;
    procedure AssignSubMatrixI(Value : IMatrix; X : integer = 0; Y : integer = 0);
    procedure AppendI(Value : IMatrix; appendColumns : boolean);
    procedure InitRandom(method : TRandomAlgorithm; var seed : LongInt); virtual; abstract;

    // ###########################################
    // #### Linear systems
    function SolveLinEQI(Value : IMatrix; numRefinements : integer = 0) : IMatrix;
    procedure SolveLinEQInPlaceI(Value : IMatrix; numRefinements : integer = 0);

    // matrix inversion (based on LU decomposition)
    function InvertInPlace : TLinEquResult; virtual; abstract;
    function InvertI : IMatrix; overload; virtual; abstract;
    function InvertExI( out InvMtx : IMatrix ) : TLinEquResult; overload; virtual; abstract;


    // moves data from Value to self and clears original object
    procedure TakeOverI(Value : IMatrix);
    function CloneI : IMatrix; virtual; abstract;

    // ###################################################
    // #### Special functions
    procedure RepeatMatrixInPlace(numX, numY : integer); virtual; abstract;
    function RepeatMatrixI(numX, numY : integer) : IMatrix; virtual; abstract;

    // ###########################################
    // #### Genereal assignments
    procedure DoTakeOver( Value : TBaseMatrix );

    // changes the actual class type e.g from TDoubleMatrx to TCplxMatrix
    // -> this is a bit dangerous in case there are references out there that
    // use interfaces. Use only with IMatrix or the base class
    // current matrix is destroyed and a self is assigned a new type
    procedure ChangeClass( cl : TBaseMatrixClass );
  public
    property Width : integer read GetSubWidth write SetWidth;
    property Height : integer read GetSubHeight write SetHeight;
    property VecLen : integer read GetVecLen;

    // general access - real elements are available in all types of matrices
    property Items[x, y : integer] : double read GetItems write SetItems; default;
    property Vec[idx : integer] : double read GetVecItem write SetVecItem; // matrix as vector

    property LinEQProgress : TLinEquProgress read GetLinEQProgress write SetLinEQProgress;

    property Name : string read fName write fName;

    procedure SetWidthHeight(const aWidth, aHeight : integer);
    procedure Resize(aNewWidth, aNewHeight : Integer);           // setwidthheight with data preserve

    // direct access functionality (use only when you know what you are doing!)
    function StartElement : PDouble; {$IFNDEF FPC} inline; {$ENDIF}
    function LineWidth : NativeInt; {$IFNDEF FPC} inline; {$ENDIF}

    procedure SetSubMatrix(x, y, Subwidth, Subheight : integer);
    procedure UseFullMatrix;

    // cast the content of the class - result is then from given matrix class
    function CastI( const IID : TGuid ) : IMatrix;
    function Cast( toClass : TBaseMatrixClass ) : TBaseMatrix;

    procedure AfterConstruction; override;

    constructor Create; overload;
    constructor Create( data : PByte; aLineWidth : NativeInt; aWidth, aHeight : integer); overload;
    destructor Destroy; override;
  end;

procedure RegisterCastClass( const aIntf : TGuid; cl : TBaseMatrixClass );

implementation

uses MatrixASMStubSwitch, Math, DblMatrix, CplxMatrix, MathUtilFunc, TypInfo;

// ###########################################
// #### Register a container class
// ###########################################

type
  TMatrixClIntf = record
    IID : TGuid;
    cl : TBaseMatrixClass;
  end;

var locMatrixIntf : Array of TMatrixClIntf;
    locNumMatrixIntf : integer = 0;

procedure RegisterCastClass( const aIntf : TGuid; cl : TBaseMatrixClass );
begin
     if Length(locMatrixIntf) <= locNumMatrixIntf then
        SetLength(locMatrixIntf, locNumMatrixIntf + 10);

     locMatrixIntf[locNumMatrixIntf].IID := aIntf;
     locMatrixIntf[locNumMatrixIntf].cl := cl;
     inc(locNumMatrixIntf);
end;

// ###########################################
// #### Resulting matix functions - bring both matrix interfaces on the compatible
// #### form: dbl dbl -> dbl, cplx cplx -> cplx, dbl cplx -> cplx

type
  TValidMatrixType = (vmDouble, vmComplex);

function GetCommonMatrix( inp1, inp2 : IMatrix; out outDbl1, outDbl2 : IDoubleMatrix;
  out cplx1, cplx2 : ICplxMatrix) : TValidMatrixType;
var b1, b2 : boolean;
begin
     cplx1 := nil;
     cplx2 := nil;
     outDbl1 := nil;
     outDbl2 := nil;

     Result := vmDouble;
     b1 := Supports(inp1, IDoubleMatrix, outDbl1);
     b2 := Supports(inp2, IDoubleMatrix, outDbl2);
     if b1 and b2 then
        exit;

     Result := vmComplex;
     b1 := Supports(inp1, ICplxMatrix, cplx1);
     b2 := Supports(inp2, ICplxMatrix, cplx2);
     if b1 and b2 then
        exit;

     if outDbl1 <> nil then
        cplx1 := TCplxMatrix.CreateFromDbl( outDbl1.SubMatrix, outDbl1.Width, outDbl1.Height );
     if outDbl2 <> nil then
        cplx2 := TCplxMatrix.CreateFromDbl( outDbl2.SubMatrix, outDbl2.Width, outDbl2.Height );
end;

function GetCommonMtxConvInput( inp1 : TBaseMatrix; inp2 : IMatrix; out outDbl1, outDbl2 : IDoubleMatrix;
  out cplx1, cplx2 : ICplxMatrix) : TValidMatrixType;
var b1, b2 : boolean;
    cl : IDoubleMatrix;
begin
     cplx1 := nil;
     cplx2 := nil;
     outDbl1 := nil;
     outDbl2 := nil;

     Result := vmDouble;
     b1 := Supports(inp1, IDoubleMatrix, outDbl1);
     b2 := Supports(inp2, IDoubleMatrix, outDbl2);
     if b1 and b2 then
        exit;

     Result := vmComplex;
     b1 := Supports(inp1, ICplxMatrix, cplx1);
     b2 := Supports(inp2, ICplxMatrix, cplx2);
     if b1 and b2 then
        exit;

     if outDbl1 <> nil then
     begin
          // ###########################################
          // #### Try to change class type of the input...
          cl := outDbl1.Clone;
          outDbl1 := nil; // remove reference...
          (inp1.GetObjRef).ChangeClass(TCplxMatrix);
          (inp1.GetObjRef as TCplxMatrix).AssignDbl(cl.StartElement, cl.LineWidth, cl.Width, cl.Height);

          if not Supports(inp1, ICplxMatrix, cplx1) then
             raise EBaseMatrixException.Create('Failed to change class type');
     end;
     if outDbl2 <> nil then
        cplx2 := TCplxMatrix.CreateFromDbl( outDbl2.SubMatrix, outDbl2.Width, outDbl2.Height );
end;


function TBaseMatrix.AddAndScaleI(const Offset, Scale: double): IMatrix;
var d1 : IDoubleMatrix;
    c1 : ICplxMatrix;
begin
     if Supports(self, IDoubleMatrix, d1) then
     begin
          d1.AddAndScale(Offset, Scale);
          exit;
     end;
     if Supports(self, ICplxMatrix, c1) then
     begin
          c1.AddAndScale(Offset, Scale);
          exit;
     end;
end;

function TBaseMatrix.AddI(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.Add(d2);
       vmComplex: Result := c1.Add(c2);
     end;
end;

procedure TBaseMatrix.AddInplaceI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.AddInPlace(d2);
       vmComplex: c1.AddInPlace(c2);
     end;
end;

function TBaseMatrix.AddVecI(aVec: IMatrix; rowWise: Boolean): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, aVec, d1, d2, c1, c2) of
       vmDouble: Result := d1.AddVec(d2, rowWise);
       vmComplex: Result := c1.AddVec(c2, rowWise);
     end;
end;

procedure TBaseMatrix.AddVecInPlaceI(Value: IMatrix; rowWise: Boolean);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.AddVecInPlace(d2, rowWise);
       vmComplex: c1.AddVecInPlace(c2, rowWise);
     end;
end;

procedure TBaseMatrix.AfterConstruction;
begin
     if fItemSize = 0 then
        fItemSize := GetItemSize;

     inherited;
end;

procedure TBaseMatrix.AppendI(Value: IMatrix; appendColumns: boolean);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.Append(d2, appendColumns);
       vmComplex: c1.Append(c2, appendColumns);
     end;
end;

procedure TBaseMatrix.AssignExI(Value: IMatrix; OnlySubElements: boolean);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.AssignEx(d2, OnlySubElements);
       vmComplex: c1.AssignEx(c2, OnlySubElements);
     end;
end;


procedure TBaseMatrix.AssignI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.Assign(d2);
       vmComplex: c1.Assign(c2);
     end;
end;

procedure TBaseMatrix.AssignSubMatrixI(Value: IMatrix; X, Y: integer);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.AssignSubMatrix(d2, X, Y);
       vmComplex: c1.AssignSubMatrix(c2, X, Y);
     end;
end;


function TBaseMatrix.Cast(toClass: TBaseMatrixClass): TBaseMatrix;
var pLine : PDouble;
    pOutline : PDOuble;
    y: Integer;
    mtx : IMatrix;
    convFunc : TNumConvFunc;
    x: Integer;
begin
     // ###########################################
     // #### Call assignments
     if toClass.NumberType <> NumberType then
     begin
          Result := toClass.Create;
          Result.SetWidthHeight(Width, Height);

          // convert from one number type to the next one
          // -> complex to double using CABs operator
          // -> double to complex InitComplex(value, 0);

          convFunc := GetConvFunc(NumberType, toClass.NumberType);
          for y := 0 to Height - 1 do
          begin
               pLine := GenPtr(StartElement, 0, y, LineWidth);
               pOutline := GenPtr(Result.StartElement, 0, y, Result.LineWidth);

               for x := 0 to Width - 1 do
               begin
                    convFunc( PByte(pLine), PByte(pOutline));
                    inc(PByte(pLine), GetItemSize);
                    inc(PByte(pOutline), Result.GetItemSize);
               end;
          end;
     end
     else
     begin
          // no cast to the same classe -> use the clone operation
          mtx := CloneI;
          Result := toClass.Create;
          Result.TakeOverI(mtx);
     end;
end;

function TBaseMatrix.CastI(const IID: TGuid): IMatrix;
var i: Integer;
begin
     for i := 0 to locNumMatrixIntf - 1 do
     begin
          // maybe move that to a global interface container list
          if IsEqualGUID( IID, locMatrixIntf[i].IID ) then
          begin
               Result := Cast(locMatrixIntf[i].cl);
               exit;
          end;
     end;

     raise EMatrixTypeException.Create('Cast to unknown interface');
end;

procedure TBaseMatrix.ChangeClass(cl: TBaseMatrixClass);
var refCnt : integer;
begin
     // all references may only be IMatrix - so... this should work then
     // basically we exchange the pointer self with another class...
     // note the data is lost only the class changes -> if you need it to be filled
     // up again you nee to do this outside this routine

     // -> note: this is not 100% thread safe but the class itself was never to be
     // built to be used over multiple threads.
     refCnt := self.RefCount;
     self.FRefCount := 1;  // the last release will free "self"

     // ###########################################
     // #### Recreate self the new class -> data is assigned "outside"
     self._Release; // runs implicitly free
     self := cl.Create;
     self.fRefCount := refCnt;
end;

procedure TBaseMatrix.CheckAndRaiseError(assertionVal: boolean; const msg: string);
begin
     if not assertionVal then
        raise EBaseMatrixException.Create(msg);
end;


procedure TBaseMatrix.Clear;
begin
     ReserveMem(0, 0);

     fWidth := 0;
     fHeight := 0;
     fSubWidth := 0;
     fSubHeight := 0;
end;

constructor TBaseMatrix.Create(data: PByte; aLineWidth: NativeInt; aWidth,
  aHeight: integer);
begin
     inherited Create;

     fItemSize := GetItemSize;

     fWidth := aWidth;
     fHeight := aHeight;
     fMemory := data;
     fData := data;
     fLineWidth := aLineWidth;

     fOffsetX := 0;
     fOffsetY := 0;
     fSubWidth := fWidth;
     fSubHeight := fHeight;
     CheckAndRaiseError(width*fItemSize <= LineWidth, 'Dimension error');
end;

constructor TBaseMatrix.Create;
begin
     inherited Create;

     fItemSize := GetItemSize;
end;

procedure TBaseMatrix.DefineProps;
begin
     inherited;

     AddStringProperty('Name', fName);

     AddIntProperty('Width', fWidth);
     AddIntProperty('Height', fHeight);

     AddIntProperty('SubWidth', fSubWidth);
     AddIntProperty('SubHeight', fSubHeight);
     AddIntProperty('OffsetX', fOffsetX);
     AddIntProperty('OffsetY', fOffsetY);
end;

destructor TBaseMatrix.Destroy;
begin
     Clear;

     inherited;
end;

procedure TBaseMatrix.DoTakeOver(Value: TBaseMatrix);
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

function TBaseMatrix.ElementWiseDivI(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.ElementWiseDiv(d2);
       vmComplex: Result := c1.ElementWiseDiv(c2);
     end;
end;

procedure TBaseMatrix.ElementWiseDivInPlaceI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.ElementWiseDivInPlace(d2);
       vmComplex: c1.ElementWiseDivInPlace(c2);
     end;
end;

function TBaseMatrix.ElementWiseMultI(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.ElementWiseMult(d2);
       vmComplex: Result := c1.ElementWiseMult(c2);
     end;
end;

procedure TBaseMatrix.ElementWiseMultInPlaceI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.ElementWiseMultInPlace(d2);
       vmComplex: c1.ElementWiseMultInPlace(c2);
     end;
end;

function TBaseMatrix.GetLinEQProgress: TLinEquProgress;
begin
     Result := fLinEQProgress;
end;

function TBaseMatrix.GetObjRef: TBaseMatrix;
begin
     Result := self;
end;

function TBaseMatrix.GetSubHeight: integer;
begin
     Result := fSubHeight;
end;

function TBaseMatrix.GetSubWidth: integer;
begin
     Result := fSubWidth;
end;

function TBaseMatrix.GetVecLen: integer;
begin
     // treat matrix as vector
     Result := fSubWidth*fSubHeight;
end;

procedure TBaseMatrix.InternalSetWidthHeight(const aWidth,
  aHeight: integer; AssignMem: boolean);
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

function TBaseMatrix.LineWidth: NativeInt;
begin
     Result := fLineWidth;
end;

function TBaseMatrix.MultI(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.Mult(d2);
       vmComplex: Result := c1.Mult(c2);
     end;
end;

procedure TBaseMatrix.MultInPlaceI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.MultInPlace(d2);
       vmComplex: c1.MultInPlace(c2);
     end;
end;

procedure TBaseMatrix.MultInPlaceT1I(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.MultInPlaceT1(d2);
       vmComplex: c1.MultInPlaceT1(c2);
     end;
end;


procedure TBaseMatrix.MultInPlaceT2I(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.MultInPlaceT2(d2);
       vmComplex: c1.MultInPlaceT2(c2);
     end;
end;

function TBaseMatrix.MultT1I(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.MultT1(d2);
       vmComplex: Result := c1.MultT1(c2);
     end;
end;

function TBaseMatrix.MultT2I(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.MultT2(d2);
       vmComplex: Result := c1.MultT2(c2);
     end;
end;


procedure TBaseMatrix.OnLoadIntProperty(const Name: String; Value: integer);
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
         inherited;
end;

procedure TBaseMatrix.OnLoadStringProperty(const Name, Value: String);
begin
     if CompareText(Name, 'Name') = 0
     then
         fName := Value
     else
         inherited;
end;

function TBaseMatrix.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, 'Width') = 0
     then
         Result := ptInteger
     else if CompareText(Name, 'Height') = 0
     then
         Result := ptInteger
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
     else if CompareText(Name, 'Name') = 0
     then
         Result := ptString
     else
         Result := inherited PropTypeOfName( Name );
end;

procedure TBaseMatrix.ReserveMem(width, height: integer);
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
               fData := MtxAllocAlign(width*(fItemSize div 8), height, fLineWidth, fMemory);
          end
          else
          begin
               // aligned "vector" - make sure the items are placed next to each other
               // -> some functions like Matrix vector multiplication like it
               // better to have the memory aligned without "holes"
               fMemory := MtxAlloc($20 + height*width*fItemSize);
               fData := AlignPtr32( fMemory );
               fLineWidth := fItemSize;
          end;

          CheckAndRaiseError(fMemory <> nil, 'Failed to allocate memory');
     end;
end;

procedure TBaseMatrix.Resize(aNewWidth, aNewHeight: Integer);
var origSubWidth,
    origSubHeight : integer;
    origMemory : Pointer;
    origLineWidth : NativeInt;
    origStart : PDouble;
begin
     CheckAndRaiseError( (aNewWidth > 0) and (aNewHeight > 0), 'Width and height need to be greater than 0');
     origSubWidth := fSubWidth;
     origSubHeight := fSubHeight;
     origMemory := fMemory;
     origLineWidth := fLineWidth;

     origStart := StartElement;

     fMemory := nil;
     fData := nil;

     InternalSetWidthHeight(aNewWidth, aNewHeight, True);

     // #######################################
     // #### copy old memory and free
     // note only the submatrix stuff is copied
     MatrixCopy( StartElement, LineWidth, origStart, origLineWidth,
                 (1 + Integer(fItemSize > sizeof(double)))*Math.Min(fWidth, origSubWidth), Math.Min( fHeight, origSubHeight ) );

     FreeMem(origMemory);
end;


procedure TBaseMatrix.SetHeight(const Value: integer);
begin
     SetWidthHeight(fWidth, Value);
end;

procedure TBaseMatrix.SetLinEQProgress(value: TLinEquProgress);
begin
     fLinEQProgress := value;
end;

procedure TBaseMatrix.SetSubMatrix(x, y, Subwidth, Subheight: integer);
begin
     CheckAndRaiseError((x >= 0) and (x + SubWidth <= fWidth), 'Dimension x error');
     CheckAndRaiseError((y >= 0) and (y + SubHeight <= fHeight), 'Dimension y error');

     fOffsetX := x;
     fOffsetY := y;
     fSubWidth := Subwidth;
     fSubHeight := Subheight;
end;

procedure TBaseMatrix.SetWidth(const Value: integer);
begin
     SetWidthHeight(Value, fHeight);
end;

procedure TBaseMatrix.SetWidthHeight(const aWidth, aHeight: integer);
begin
     InternalSetWidthHeight(aWidth, aHeight, True);
end;

function TBaseMatrix.SolveLinEQI(Value: IMatrix;
  numRefinements: integer): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.SolveLinEQ(d2, numRefinements);
       vmComplex: Result := c1.SolveLinEQ(c2, numRefinements);
     end;
end;

procedure TBaseMatrix.SolveLinEQInPlaceI(Value: IMatrix;
  numRefinements: integer);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.SolveLinEQInPlace(d2, numRefinements);
       vmComplex: c1.SolveLinEQInPlace(c2, numRefinements);
     end;
end;

function TBaseMatrix.StartElement: PDouble;
begin
     if (fWidth <> 0) and (fHeight <> 0)
     then
         Result := PDouble(NativeUInt((NativeUInt(fData) + NativeUInt(fItemSize*fOffsetX)) +
                           NativeUInt(fOffsetY*fLineWidth)))
     else
         Result := nil;

end;

function TBaseMatrix.SubI(Value: IMatrix): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, Value, d1, d2, c1, c2) of
       vmDouble: Result := d1.Sub(d2);
       vmComplex: Result := c1.Sub(c2);
     end;
end;

procedure TBaseMatrix.SubInPlaceI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.SubInPlace(d2);
       vmComplex: c1.SubInPlace(c2);
     end;
end;


function TBaseMatrix.SubVecI(aVec: IMatrix; rowWise: Boolean): IMatrix;
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     case GetCommonMatrix(self, aVec, d1, d2, c1, c2) of
       vmDouble: Result := d1.SubVec(d2, rowWise);
       vmComplex: Result := c1.SubVec(c2, rowWise);
     end;
end;


procedure TBaseMatrix.SubVecInPlaceI(Value: IMatrix; rowWise: Boolean);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.SubVecInPlace(d2, rowWise);
       vmComplex: c1.SubVecInPlace(c2, rowWise);
     end;
end;

procedure TBaseMatrix.TakeOverI(Value: IMatrix);
var d1, d2 : IDoubleMatrix;
    c1, c2 : ICplxMatrix;
begin
     // note: the function raises an exception if self is double and value is complex!
     // -> we cannot change the class type
     case GetCommonMtxConvInput(self, Value, d1, d2, c1, c2) of
       vmDouble: d1.TakeOver(d2);
       vmComplex: c1.TakeOver(c2);
     end;
end;

procedure TBaseMatrix.UseFullMatrix;
begin
     fOffsetX := 0;
     fOffsetY := 0;
     fSubWidth := fWidth;
     fSubHeight := fHeight;
end;

end.
