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
    class function ClassIdentifier : String; override;
  public
    procedure MultInPlace(Value : TDoubleMatrix);
    function Mult(Value : TDoubleMatrix) : TDoubleMatrix;

    procedure AddAndScaleInPlace(const Offset, Scale : double);
    function AddAndScale(const Offset, Scale : double) : TDoubleMatrix;

    procedure ElementwiseFuncInPlace(func : TMatrixFunc); overload;
    function ElementwiseFunc(func : TMatrixFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixObjFunc); overload;
    function ElementwiseFunc(func : TMatrixObjFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefFunc); overload;
    function ElementwiseFunc(func : TMatrixMtxRefFunc) : TDoubleMatrix; overload;
    procedure ElementwiseFuncInPlace(func : TMatrixMtxRefObjFunc); overload;
    function ElementwiseFunc(func : TMatrixMtxRefObjFunc) : TDoubleMatrix; overload;

    // note: due to the multi threaded offset the add and sub functions seem to be
    // faster if implemented single threaded
    procedure AddInplace(Value : TDoubleMatrix);
    function Add(Value : TDoubleMatrix) : TDoubleMatrix;
    procedure SubInPlace(Value : TDoubleMatrix);
    function Sub(Value : TDoubleMatrix) : TDoubleMatrix;

    class procedure InitThreadPool;
    class procedure FinalizeThreadPool;
  end;

implementation

uses ThreadedMatrixOperations, Types, MtxThreadPool;

// never realy understood why I can't access protected members from the base class if
// I want to access a member in from a parameter. So I need this "hack" class definition.
type
  THackMtx = class(TDoubleMatrix);

{ TThreadedMatrix }

function TThreadedMatrix.Add(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((THackMtx(Value).fSubWidth = fSubWidth) and (THackMtx(Value).fSubHeight = fSubHeight), 'Dimension error');

     Result := TThreadedMatrix.Create;
     THackMtx(Result).SetWidthHeight(fSubWidth, fSubHeight, True);
     ThrMatrixAdd(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, StartElement, THackMtx(Value).StartElement, fSubWidth, fSubHeight, LineWidth, THackMtx(Value).LineWidth);
end;

function TThreadedMatrix.AddAndScale(const Offset,
  Scale: double): TDoubleMatrix;
begin
     assert((width > 0) and (Height > 0), 'No data assigned');
     Result := TThreadedMatrix.Create;
     Result.Assign(self, True);

     Result.AddAndScaleInPlace(Offset, Scale);
end;

procedure TThreadedMatrix.AddAndScaleInPlace(const Offset, Scale: double);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');

     ThrMatrixAddAndScale(StartElement, LineWidth, fSubWidth, fSubHeight, Offset, Scale);
end;

procedure TThreadedMatrix.AddInplace(Value: TDoubleMatrix);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     // inplace matrix addition
     assert((fSubWidth = THackMtx(Value).fSubWidth) and (fSubHeight = THackMtx(Value).fSubHeight), 'Matrix dimensions do not match');
     ThrMatrixAdd(StartElement, LineWidth, StartElement, THackMtx(Value).StartElement, fSubWidth, fSubHeight, LineWidth, THackMtx(Value).LineWidth);
end;

class function TThreadedMatrix.ClassIdentifier: String;
begin
     Result := 'TMTX';
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     THackMtx(Result).Assign(self);

     ThrMatrixFunc(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, THackMtx(Result).Width, THackMtx(Result).Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixObjFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     THackMtx(Result).Assign(self);

     ThrMatrixFunc(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, THackMtx(Result).Width, THackMtx(Result).Height, func);
end;

procedure TThreadedMatrix.ElementwiseFuncInPlace(func: TMatrixObjFunc);
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');

     ThrMatrixFunc(StartElement, LineWidth, fSubWidth, fSubHeight, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixMtxRefFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     THackMtx(Result).Assign(self);

     ThrMatrixFunc(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, THackMtx(Result).Width, THackMtx(Result).Height, func);
end;

function TThreadedMatrix.ElementwiseFunc(func: TMatrixMtxRefObjFunc): TDoubleMatrix;
begin
     assert((fSubWidth > 0) and (fSubHeight > 0), 'No data assigned');
     Result := TDoubleMatrix.Create;
     THackMtx(Result).Assign(self);

     ThrMatrixFunc(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, THackMtx(Result).Width, THackMtx(Result).Height, func);
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

function TThreadedMatrix.Mult(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     Result := TThreadedMatrix.Create(Value.Width, fSubHeight);

     if (Value.Width = 1) and (THackMtx(Value).LineWidth = sizeof(double))
     then
         ThrMatrixVecMult(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, StartElement, THackMtx(Value).StartElement, Width, Height, Value.Height, LineWidth)
     else
         ThrMatrixMult(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, StartElement, THackMtx(Value).StartElement, Width, Height, Value.Width, Value.Height, LineWidth, THackMtx(Value).LineWidth);
end;

procedure TThreadedMatrix.MultInPlace(Value: TDoubleMatrix);
var res : TThreadedMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((fOffsetX = 0) and (fOffsetY = 0) and (Width = fSubWidth) and (Height = fSubHeight), 'Operation only allowed on full matrices');
     assert((fSubWidth = THackMtx(Value).fSubHeight), 'Dimension error');

     res := TThreadedMatrix.Create(Value.Width, Height);

     try
        if (Value.Width = 1) and (THackMtx(Value).LineWidth = sizeof(double))
        then
            ThrMatrixVecMult(res.StartElement, res.LineWidth, StartElement, THackMtx(Value).StartElement, Width, Height, Value.Height, LineWidth)
        else
            ThrMatrixMult(res.StartElement, res.LineWidth, StartElement, THackMtx(Value).StartElement, Width, Height, Value.Width, Value.Height, LineWidth, THackMtx(Value).LineWidth);

        Assign(res);
     except
           res.Free;
           raise;
     end;
end;

function TThreadedMatrix.Sub(Value: TDoubleMatrix): TDoubleMatrix;
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     assert((THackMtx(Value).fSubWidth = fSubWidth) and (THackMtx(Value).fSubHeight = fSubHeight), 'Dimension error');

     Result := TThreadedMatrix.Create(fSubWidth, fSubHeight);
     ThrMatrixSub(THackMtx(Result).StartElement, THackMtx(Result).LineWidth, StartElement, THackMtx(Value).StartElement, fSubWidth, fSubHeight,
                  Width*sizeof(double), THackMtx(Value).LineWidth);
end;

procedure TThreadedMatrix.SubInPlace(Value: TDoubleMatrix);
begin
     assert((Width > 0) and (Height > 0), 'No data assigned');
     
     // inplace matrix substraction
     assert((fSubWidth = Value.Width) and (fSubHeight = Value.Height), 'Matrix dimensions do not match');
     ThrMatrixSub(StartElement, LineWidth, StartElement, THackMtx(Value).StartElement, fSubWidth, fSubHeight, LineWidth, THackMtx(Value).LineWidth);
end;

end.
