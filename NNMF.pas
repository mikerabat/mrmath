unit NNMF;

// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2014, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

interface

// ###########################################
// #### Non-Negative Matrix factorization
// ###########################################

uses SysUtils, Classes, Matrix, MatrixConst, BaseMathPersistence;

type
  TNNMFPropsMethod = (nnmfDivergence, nnmfEukledian, nnmfAlternateLeastSquare);
  TNNMFProps = record
    MaxIter : integer;
    tolUpdate : double;           // algorithm converges if reconstruction error 
                                  // does not change by this tolerance. If set to 0 the Maximum iterations are used
    method : TNNMFPropsMethod;     // if false use euclidian update
    RankOfBasis : integer;
    UseLastResIfFail : boolean;   // in case we have problems with the floating point precission use the last valid result
    DoUpdateWithEPSMtx : boolean; // before division the matrix a matrix close to floating point precission is added
  end;

type
  ENMFException = class(EBaseMatrixException);
  TNMFProgress = procedure(Sender : TObject; progress : integer) of Object;
  TNMFRes = (nmOk, nmFloatingPointPrecReached, nmFailed);
  TNNMF = class(TMatrixClass)
  private
    fProps : TNNMFProps;
    fH: TDoubleMatrix;
    fW: TDoubleMatrix;
    fWInv : TDoubleMatrix;
    
    fOnProgress: TNMFProgress;
    procedure Clear;
    
    // procedures applied to each element of a matrix:
    procedure epsFunc(var value: double);
    procedure maxFunc(var value : double);
  protected
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;

    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    procedure OnLoadDoubleProperty(const Name : String; const Value : double); override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
  public
    property OnProgress : TNMFProgress read fOnProgress write fOnProgress;

    property H : TDoubleMatrix read fH;
    property W : TDoubleMatrix read fW;
    property WInv : TDoubleMatrix read fWInv;

    function CalcNMF(V : TDoubleMatrix) : TNMFRes;
    procedure InitProjectionmatrix; // calculates pseudoinverse of W

    function ProjectToFeatureSpace(Example: TDoubleMatrix): TDoubleMatrix;
    function Reconstruct(Features: TDoubleMatrix): TDoubleMatrix;

    procedure SetProperties(const props : TNNMFProps);
    
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Math, MathUtilFunc;

const cNNMFH = 'NNMFH';
      cNNMFW = 'NNMFW';
      cNNMFWInv = 'NNMFWinf';
      cNNMFMaxIter = 'NNMFMaxIter';
      cNNMFTolerance = 'NNMFTolerance';
      cNNMFMethod = 'NNMFMethod';
      cNNMFRank = 'NNMFRank';
      cNNMFUpdateEPS = 'NNMFEpsUpdate';
      cNNMFUseLastRes = 'NNMFUseLastRes';

{ TNMF }

// ###########################################
// #### Create Free
// ###########################################

procedure TNNMF.Clear;
begin
     FreeAndNil(fH);
     FreeAndNil(fW);
     FreeAndNil(fWInv);
end;

constructor TNNMF.Create;
begin
     fProps.MaxIter := 100;
     fProps.method := nnmfDivergence;
     fProps.RankOfBasis := -1; // use all
     fProps.tolUpdate := 1e-4; // use a tolerance
     fProps.UseLastResIfFail := True;
     fProps.DoUpdateWithEPSMtx := True;
     
     inherited Create;
end;

destructor TNNMF.Destroy;
begin
     Clear;
     
     inherited;
end;

// ###########################################
// #### NNMF algorithms
// ###########################################

function TNNMF.CalcNMF(V: TDoubleMatrix) : TNMFRes;
var lenOfEigVec : integer;
    NumOfEigVec : integer;
    RankOfBasis : integer;
    sumW : IMatrix;
    counter: Integer;
    wh : IMatrix; 
    hht : IMatrix;
    temp1 : IMatrix;
    temp2 : IMatrix;
    iter : integer;
    nm : integer;
    dnorm0, dnorm : double;
    lastW, lastH : TDoubleMatrix;
    epsMtx : IMatrix;
    progress, lastProgress : integer;
begin
     Result := nmOk;
     Clear;
     
     // ###########################################
     // #### Preprocess params
     lenOfEigVec := V.Height;
     NumOfEigVec := V.Width;
     RankOfBasis := V.Width;
     if fProps.RankOfBasis > 0 then
        RankOfBasis := Min(RankOfBasis, fProps.RankOfBasis);

     dnorm0 := 0;
     nm := V.Height*V.Width;
        
     lastW := nil;
     lastH := nil;
        
     // ###########################################
     // #### Randomly initialize matrices
     try
        fW := MatrixClass.CreateRand(RankOfBasis, lenOfEigVec);

        sumW := fW.Sum(False);
        for counter := 0 to fW.Height - 1 do
        begin
             fW.SetSubMatrix(0, counter, fW.Width, 1);
             fW.ElementWiseDivInPlace(sumW);
        end;
        fW.UseFullMatrix;
        sumW := nil;

        fH := MatrixClass.CreateRand(NumOfEigVec, RankOfBasis);

        if ( fProps.UseLastResIfFail ) and (fProps.method <> nnmfAlternateLeastSquare ) then
        begin
             lastW := fW.Clone;
             lastH := fH.Clone;
        end;

        lastProgress := -1;
        // ###########################################
        // #### Iterative update of NMF matrix
        for iter := 0 to fProps.MaxIter - 1 do
        begin
             if fProps.method = nnmfDivergence then
             begin
                  // implements:
                  //  H = H.*(W'*(V./(W*H)));
                  wh := fW.Mult(fH);
                  if fProps.DoUpdateWithEPSMtx then
                  begin
                       epsMtx := wh.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}epsFunc);
                       wh.AddInPlace(epsMtx);
                  end;
                  temp1 := V.ElementWiseDiv(wh);
                  temp2 := fW.MultT1(temp1);
                  
                  fH.ElementWiseMultInPlace(temp2);

                  //  W = W.*((V./(W*H))*H');
                  wh := fW.Mult(fH);
                  if fProps.DoUpdateWithEPSMtx then
                  begin
                       epsMtx := wh.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}epsFunc);
                       wh.AddInPlace(epsMtx);
                  end;
                  temp1 := V.ElementWiseDiv(wh);
                  temp2 := temp1.MultT2(fH);
                  fW.ElementWiseMultInPlace(temp2);

                  //  W = W./(ones(LenOfEigVec,1)*sum(W));
                  sumW := fW.Sum(False);
                  for counter := 0 to fW.Height - 1 do
                  begin
                       fW.SetSubMatrix(0, counter, fW.Width, 1);
                       fW.ElementWiseDivInPlace(sumW);
                  end;
                  fW.UseFullMatrix;
             end
             else if fProps.method = nnmfEukledian then
             begin
                  // euclidian update

                  // implements 
                  // W = W.*(V*H')./(W*(H*H'));
                  hht := fH.MultT2(fH);
                  wh := fW.Mult(hht);
                  temp1 := V.MultT2(fH);
                  if fProps.DoUpdateWithEPSMtx then
                  begin
                       epsMtx := temp1.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}epsFunc);
                       wh.AddInplace(epsMtx);
                  end;
                  
                  temp1.ElementWiseDivInPlace(wh);
                  fW.ElementWiseMultInPlace(temp1);
                  
                  // H = H.*(W'*V)./((W'*W)*H);
                  hht := fW.MultT1(fW);
                  wh := hht.Mult(fH);
                  temp1 := fW.MultT1(V);
                  if fProps.DoUpdateWithEPSMtx then
                  begin
                       epsMtx := temp1.ElementwiseFunc({$IFDEF FPC}@{$ENDIF}epsFunc);
                       wh.AddInplace(epsMtx);
                  end;
                  temp1.ElementWiseDivInPlace(wh);
                  fH.ElementWiseMultInPlace(temp1);
             end
             else 
             begin
                  // alternating Least Squares
                  // h = max(0, w0\a);
                  // w = max(0, a/h);

                  if fW.PseudoInversion(temp1) = srNoConvergence then
                  begin
                       Result := nmFloatingPointPrecReached;
                       exit;
                  end;
                  temp1.MultInPlace(V);
                  temp1.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}maxFunc);

                  if temp1.PseudoInversion(temp2) = srNoConvergence then
                  begin
                       Result := nmFloatingPointPrecReached;
                       exit;
                  end;

                  temp2 := V.Mult(temp2);
                  temp2.ElementwiseFuncInPlace({$IFDEF FPC}@{$ENDIF}maxFunc);
                  
                  fH.Assign(temp1);
                  fW.Assign(temp2);
             end;
                  

             // ###########################################
             // #### check for convergence (very simple one!)
             // checks if the change in the reconstruction error is smaller than the given tolerance
             if fProps.tolUpdate > 0 then
             begin
                  temp2 := fW.Mult(fH);
                  temp2.SubInPlace(V);
                  dNorm := sqrt( sqr(temp2.ElementwiseNorm2) / nm );
                  
                  if iter > 0 then
                  begin
                       // check if change in norm is smaller than the given tolerance
                       if Abs(dnorm0 - dnorm) < fProps.tolUpdate then
                       begin
                            if Assigned(fOnProgress) then
                               fOnProgress(self, 100);

                            break;
                       end;
                  end;

                  dNorm0 := dNorm;
             end;
             
             // make clones in case an update fails due to floating point precission
             // problems -> use the last valid matrix
             if (fProps.UseLastResIfFail) and (fProps.method <> nnmfAlternateLeastSquare) then
             begin
                  lastW.Free;
                  lastH.Free;
                  lastW := fW.Clone;
                  lastH := fH.Clone;
             end;
             
             progress := (iter + 1)*100 div fProps.MaxIter;
             
             if Assigned(fOnProgress) and (lastProgress <> progress) then
             begin
                  lastProgress := progress;
                  fOnProgress(self, progress);
             end;
        end;

        lastW.Free;
        lastH.Free;
     except              
           // if update is divergent (or made too often)
           on E : EInvalidOp do
           begin 
                Clear; 

                if fProps.method = nnmfAlternateLeastSquare 
                then
                    Result := nmFailed
                else
                begin
                     Result := nmFloatingPointPrecReached;
                
                     if fProps.UseLastResIfFail then
                     begin
                          fW := lastW;
                          fH := lastH;
                     end
                     else
                         Result := nmFailed; // in this case we don't have a valid result
                end;
           end;
     else
         Clear;

         Result := nmFailed;
     end;
end;

// ###########################################
// #### Projection and reconstruction
// ###########################################

procedure TNNMF.InitProjectionmatrix;
begin
     if not Assigned(fW) then
        raise ENMFException.Create('NMF object not initialized');

     if not Assigned(fWInv) then
     begin
          if fW.PseudoInversion(fWInv) <> srOk then
             raise ENMFException.Create('Error initilaizing projection matrix - no convergence');
     end;
end;

function TNNMF.ProjectToFeatureSpace(
  Example: TDoubleMatrix): TDoubleMatrix;
var exmplTransp : IMatrix;
begin
     if not Assigned(fW) then
        raise ENMFException.Create('NMF object not initialized');

     InitProjectionmatrix;

     // check dimensions -> transpose if necessary
     if (Example.Width = fW.Height) and (Example.Height = 1) then
     begin
          exmplTransp := Example.Transpose;
          Result := fWInv.Mult(exmplTransp);
     end
     else
         Result := fWInv.Mult(Example);
end;

function TNNMF.Reconstruct(Features: TDoubleMatrix): TDoubleMatrix;
begin
     if not Assigned(fW) then
        raise ENMFException.Create('NMF object not initialized');

     Result := fW.Mult(features);
end;

// ###########################################
// #### Functions applied to the matrix
// ###########################################

procedure TNNMF.epsFunc(var value : double);
begin
     value := eps(value);
end;

procedure TNNMF.maxFunc(var value: double);
begin
     if Value < 0 then
        Value := 0;
end;

// ###########################################
// #### Persistence
// ###########################################

procedure TNNMF.DefineProps;
begin
     AddIntProperty(cNNMFMaxIter, fProps.MaxIter);
     AddDoubleProperty(cNNMFTolerance, fProps.tolUpdate);
     AddIntProperty(cNNMFMethod, Integer(fProps.method));
     AddIntProperty(cNNMFRank, fProps.RankOfBasis);
     AddIntProperty(cNNMFUpdateEPS, Integer(fProps.DoUpdateWithEPSMtx));
     AddIntProperty(cNNMFUseLastRes, Integer(fProps.UseLastResIfFail));

     if Assigned(fW) then
        AddObject(cNNMFW, fW);
     if Assigned(fH) then
        AddObject(cNNMFH, fH);
     if Assigned(fWInv) then
        AddObject(cNNMFWInv, fWInv);
end;

function TNNMF.PropTypeOfName(const Name: string): TPropType;
begin
     if (CompareText(Name, cNNMFMaxIter) = 0) or (CompareText(Name, cNNMFMethod) = 0) or
        (CompareText(Name, cNNMFRank) = 0) or (CompareText(Name, cNNMFUpdateEPS) = 0) or
        (CompareText(Name, cNNMFUseLastRes) = 0)
     then
         Result := ptInteger
     else if CompareText(Name, cNNMFTolerance) = 0
     then
         Result := ptDouble
     else if (CompareText(Name, cNNMFW) = 0) or (CompareText(Name, cNNMFH) = 0) or
             (CompareText(Name, cNNMFWInv) = 0)
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;


procedure TNNMF.OnLoadDoubleProperty(const Name: String; const Value: double);
begin
     if SameText(Name, cNNMFTolerance)
     then
         fProps.tolUpdate := Value
     else
         inherited OnLoadDoubleProperty(Name, Value);
end;

procedure TNNMF.OnLoadIntProperty(const Name: String; Value: integer);
begin
     if SameText(Name, cNNMFMaxIter)
     then
         fProps.MaxIter := Value
     else if SameText(Name, cNNMFMethod) 
     then
         fProps.method := TNNMFPropsMethod( Value )
     else if SameText(Name, cNNMFRank) 
     then
         fProps.RankOfBasis := Value
     else if SameText(Name, cNNMFUpdateEPS) 
     then
         fProps.DoUpdateWithEPSMtx := Value <> 0
     else if SameText(Name, cNNMFUseLastRes) 
     then
         fProps.UseLastResIfFail := Value <> 0
     else 
         inherited OnLoadIntProperty(Name, Value); 
end;

function TNNMF.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if SameText(Name, cNNMFH) 
     then
         fH := obj as TDoubleMatrix
     else if SameText(Name, cNNMFW) 
     then
         fW := obj as TDoubleMatrix
     else if SameText(Name, cNNMFWInv) 
     then
         fWInv := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, obj);
end;

procedure TNNMF.SetProperties(const props: TNNMFProps);
begin
     fProps := props;
end;

initialization
   RegisterMathIO(TNNMF);
end.
