// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2018, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit ufrmEMTest;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ExtCtrls, Types, Matrix;

type
  TfrmEMTest = class(TForm)
    pnlSettings: TPanel;
    grpCenters: TGroupBox;
    rad2C: TRadioButton;
    rad3C: TRadioButton;
    rad4C: TRadioButton;
    scrVarC1: TScrollBar;
    lblCapVarC1: TLabel;
    scrVarC2: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    scrVarC3: TScrollBar;
    Label3: TLabel;
    scrVarC4: TScrollBar;
    Button1: TButton;
    pbEM: TPaintBox;
    timAnimate: TTimer;
    Label4: TLabel;
    lblIter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure pbEMPaint(Sender: TObject);
    procedure rad2CClick(Sender: TObject);
    procedure scrVarC1Change(Sender: TObject);
    procedure timAnimateTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    fNumCenters : integer;
    fXY : IMatrix;

    fResW : IMatrix;
    fResV : IMatrixDynArr;
    fResM : IMatrix;
    fResE : IMatrix;

    fDispW : IMatrix;
    fDispV : IMatrixDynArr;
    fDispM : IMatrix;
    fDispE : IMatrix;
    fDispDistMtx : IMatrix;  // distances to the cneters

    fIdx : Integer;
    fNumIter : Integer;
    fIterW : IMatrixDynArr;
    fIterM : IMatrixDynArr;
    fIterE : IMatrixDynArr;
    fIterV : Array of IMatrixDynArr;


    fDispSigmaRot : Array[0..3] of integer;    // in degree
    fDispSigmas : Array[0..1, 0..3] of double; // x, y sigma

    fSigmaRot : Array[0..3] of integer;    // in degree
    fSigmas : Array[0..1, 0..3] of double; // x, y sigma

    fIterSigmaRot : Array of Array[0..3] of integer;    // in degree
    fIterSigmas : Array of Array[0..1, 0..3] of double; // x, y sigma

    fMinX, fMaxX : double;
    fMinY, fMaxY : double;

    fCentCol : Array[0..3] of TColor;

    procedure ExecEM;

    procedure CalcDist;
    procedure OnEMUpdate( Sender : TObject; iter : integer; W, M : IMatrix; V : IMatrixDynArr; E : IMatrix);
  public
    { Public-Deklarationen }
  end;

var frmEMTest: TfrmEMTest;

implementation

uses EM, RandomEng, Math, MatrixConst, Dist;

{$R *.dfm}

{ TfrmEMTest }

//
const cMeanX : Array[0..3] of double = (-1.5, -0.1, 1.5, 1.2);
      cMeanY : Array[0..3] of double = (-1.5, -0.1, 1.5, -1.2);

      cVarX : Array[0..3] of double = (0.3, 0.8, 0.4, 0.5);
      cVarY : Array[0..3] of double = (0.8, 0.03, 1.2, 0.5);

      // rotation angle in degree
      cAngleXY : Array[0..3] of double = (0, 45, 90, 0);

      cPtsPerCent = 50;

procedure TfrmEMTest.CalcDist;
var i: integer;
    sigInv : IMatrix;
    aVec : IMatrix;
    mDist : IMatrix;
begin
     if Assigned(fDispM) then
     begin
          fDispDistMtx := TDoubleMatrix.Create(fDispM.Height, fXY.Height);
          with TDistance.Create do
          try
             for i := 0 to fDispM.Height - 1 do
             begin
                  fDispM.SetSubMatrix(0, i, 2, 1);

                  Init(fDispM, fDispV[i]);
                  mDist := MahalDist(fXY);

                  fDispDistMtx.SetColumn(i, mdist);
             end;

             fDispM.UseFullMatrix;
          finally
                 Free;
          end;
     end;
end;

procedure TfrmEMTest.ExecEM;
var cntCnt: Integer;
    idx : Integer;
    diff : double;
    rnd : TRandomGenerator;
    sinA, cosA : double;
    x, y : double;
    evect, evals : IMatrix;
    M, tmp : IMatrix;
    emObj : TExpectationMax;
begin
     // ###########################################
     // #### Prepare point cloud
     rnd := TRandomGenerator.Create( raMersenneTwister );
     rnd.Init;

     fMinx := 0;
     fMaxX := 0;
     fMinY := 0;
     fMaxY := 0;

     fNumCenters := 2;
     if rad3C.Checked then
        fNumCenters := 3;
     if rad4C.Checked then
        fNumCenters := 4;

     fXY := TDoubleMatrix.Create( 2, fNumCenters*cPtsPerCent);
     M := TDoubleMatrix.Create(2, fNumCenters);

     for cntCnt := 0 to fNumCenters - 1 do
     begin
          sinA := sin(cAngleXY[cntCnt]*pi/180);
          cosA := cos(cAngleXY[cntCnt]*pi/180);
          for idx := cntCnt*cPtsPerCent to (cntCnt + 1)*cPtsPerCent - 1 do
          begin
               x := rnd.RandGauss*cVarX[cntCnt];
               y := rnd.RandGauss*cVarY[cntCnt];

               fXY[0, idx] := 10 + cMeanX[cntCnt] + (x*cosA - y*sinA);
               fXY[1, idx] := cMeanY[cntCnt] + (x*sinA + y*cosA);
          end;

          // calc an init M
          fXY.SetSubMatrix(0, cntCnt*cPtsPerCent, 2, 3);
          tmp := fXY.Mean(False);
          M.SetRow( cntCnt, tmp);
          fXY.UseFullMatrix;
     end;

     fXY.SetSubMatrix(0, 0, 1, fXY.Height);
     fMinX := fXY.Min;
     fMaxX := fXY.Max;

     fXY.SetSubMatrix(1, 0, 1, fXY.Height);
     fMinY := fXY.Min;
     fMaxY := fXY.Max;

     fXY.UseFullMatrix;

     diff := (fMaxX - fMinX);
     fMaxX := fMaxX + 0.1*diff;
     fMinX := fMinX - 0.1*diff;

     diff := (fMaxY - fMinY);
     fMaxY := fMaxY + 0.1*diff;
     fMiny := fMinY - 0.1*diff;

     // ########################################
     // #### Execute expectation maximization
     FillChar(fSigmas, sizeof(fSigmas), 0);
     fIterW := nil;
     fIterM := nil;
     fIterV := nil;
     fIterE := nil;
     fDispW := nil;
     fDispV := nil;
     fDispM := nil;
     fDispE := nil;
     SetLength(fIterW, 1000);
     SetLength(fIterM, 1000);
     SetLength(fIterV, 1000);
     SetLength(fIterE, 1000);
     SetLength(fIterSigmaRot, 1000);
     SetLength(fIterSigmas, 1000);
     fNumIter := 0;

     emObj := TExpectationMax.Create(1000);
     try
        emObj.OnUpdate := OnEMUpdate;

        emObj.Init(M);
        if not emObj.Estimate(fXY, fNumCenters, fResW, fResM, fResV) then
        begin
             fResW := nil;
             fResM := nil;
             fResV := nil;
             fResE := nil;

             fNumIter := 0;

             lblIter.Caption := IntToStr(fNumIter);
             MessageDlg('No estimation found or an estimated center orphaned', mtWarning, [mbOk], -1);
        end
        else
        begin
             for cntCnt := 0 to fNumCenters - 1 do
             begin
                  if fResV[cntCnt].SymEig( evals, evect ) = qlOk then
                  begin
                       if evals.Vec[0] > evals.Vec[1] then
                       begin
                            fSigmas[1, cntCnt] := sqrt(evals.Vec[0]);
                            fSigmas[0, cntCnt] := sqrt(evals.Vec[1]);

                            fSigmaRot[cntCnt] := Round( ArcTan2( evect[0, 0], evect[0, 1] ) * 180/pi);
                       end
                       else
                       begin
                            fSigmas[0, cntCnt] := sqrt(evals.Vec[0]);
                            fSigmas[1, cntCnt] := sqrt(evals.Vec[1]);

                            fSigmaRot[cntCnt] := Round( ArcTan2( evect[1, 0], evect[1, 1] ) * 180/pi)
                       end;
                  end;
             end;

             Move(fSigmas, fDispSigmas, sizeof(fSigmas));
             Move(fSigmaRot, fDispSigmaRot, sizeof(fSigmaRot));

             fResE := emObj.E;
        end;
     finally
            emObj.Free;
     end;

     fDispW := fResW;
     fDispM := fResM;
     fDispV := fResV;
     fDispE := fResE;

     CalcDist;
     pbEM.Invalidate;
end;

procedure TfrmEMTest.FormCreate(Sender: TObject);
begin
     fCentCol[0] := RGB( 200, 0, 0 );
     fCentCol[1] := RGB( 0, 128, 0 );
     fCentCol[2] := RGB( 255, 128, 64 );
     fCentCol[3] := RGB( 64, 128, 128 );

     ExecEM;
end;

procedure TfrmEMTest.pbEMPaint(Sender: TObject);
var cntIdx : integer;
    idx : integer;
    x, y : integer;
    xFact, yFact : double;
    centRc : TRect;
procedure RotatePts(var pts: array of TPoint;
  origin: TPoint; radians: single);
var i,x,y: integer;
    cosAng, sinAng: single;
begin
     cosAng := cos(radians);
     sinAng := sin(radians);
     for i := low(pts) to high(pts) do
     begin
          x := pts[i].X - origin.X;
          y := pts[i].Y - origin.Y;
          pts[i].X := round((x * cosAng) - (y * sinAng)) + origin.X;
          pts[i].Y := round((x * sinAng) + (y * cosAng)) + origin.Y;
     end;
end;

procedure DrawRotatedEllipse(canvas: TCanvas;
  rec: TRect; degrees: integer);
const
  //Magic constant = 2/3*(sqrt(2)-1)
  offset: single = 0.27614237;
var midx, midy, offx, offy: integer;
    pts: array [0..12] of TPoint;
    radians: single;
    i: integer;
begin
     degrees := degrees mod 360;
     if degrees < 0 then
        inc(degrees, 360);
     radians := degrees *pi / 180;

     //if there's no rotation, use the standard Windows function
     if radians = 0
     then
         canvas.Ellipse(rec)
     else
     begin
          with rec do
          begin
               dec(right); dec(bottom); //empirically this seems better
               midx := (right + left) div 2;
               midy := (bottom + top) div 2;
               offx := round((right - left) * offset);
               offy := round((bottom - top) * offset);
               pts[0]  := Point(left, midy);
               pts[1]  := Point(left, midy - offy);
               pts[2]  := Point(midx - offx, top);
               pts[3]  := Point(midx, top);
               pts[4]  := Point(midx + offx, top);
               pts[5]  := Point(right, midy - offy);
               pts[6]  := Point(right, midy);
               pts[7]  := Point(right, midy + offy);
               pts[8]  := Point(midx + offx, bottom);
               pts[9]  := Point(midx, bottom);
               pts[10] := Point(midx - offx, bottom);
               pts[11] := Point(left, midy + offy);
               pts[12] := pts[0];
               //rotate all points about the ellipse center ...
               RotatePts(pts, Point(midx,midy), radians);
          end;
          with canvas do
          begin
               // scale again -> take different x, y factors into account
               for i := 0 to Length(pts) - 1 do
               begin
                    pts[i].X := Round((pts[i].X - midx)/1000*xFact + midx);
                    pts[i].Y := Round((pts[i].Y - midy)/1000*yFact + midy);
               end;

               beginPath(Handle);
               canvas.PolyBezier(pts);
               EndPath(Handle);
               if canvas.Brush.Style = bsClear
               then
                   StrokePath(Handle)
               else
                   StrokeAndFillPath(Handle);
          end;
     end;
end;

var minIdx : integer;
    i : integer;
begin
     if not Assigned(fXY) and ( (fMaxX - fMinX) > 0 ) and ((fMaxY - fMinY) > 0) then
        exit;

     if (pbEM.Width = 0) or (pbEM.Height = 0) then
        exit;

     xFact := pbEM.Width/(fMaxX - fMinX);
     yFact := pbEM.Height/(fMaxY - fMinY);

     for cntIdx := 0 to fXY.Height div cPtsPerCent - 1 do
     begin
          for idx := cntIdx*cPtsPerCent to (cntIdx + 1)*cPtsPerCent - 1 do
          begin
               pbEM.Canvas.Brush.Color := fCentCol[cntIdx];
               pbEM.Canvas.Brush.Style := bsSolid;
               x := Round( (fXY[0, idx] - fMinX)*xFact);
               y := Round( pbEM.Height - (fXY[1, idx] - fMinY)*yFact);

               pbEM.Canvas.Pen.Color := clBlack;
               pbEM.Canvas.Ellipse( x - 2, y - 2, x + 2, y + 2);

               // calc closes center
               if Assigned(fDispDistMtx) then
               begin
                    minIdx := 0;
                    for I := 1 to fDispDistMtx.Width - 1 do
                        if fDispDistMtx[minIdx, idx] > fDispDistMtx[i, idx] then
                           minIdx := i;

                    pbEM.Canvas.Brush.Style := bsClear;
                    pbEM.Canvas.Pen.Color := fCentCol[minIdx];
                    pbEM.Canvas.Ellipse( x - 4, y - 4, x + 4, y + 4);
               end;
          end;
     end;

     // paint centers...
     if Assigned(fDispM) then
     begin
          for cntIdx := 0 to fDispM.Height - 1 do
          begin
               x := Round( (fDispM[0, cntIdx] - fMinX)*xFact);
               y := Round( pbEM.Height - (fDispM[1, cntIdx] - fMinY)*yFact);

               pbEM.Canvas.Brush.Style := bsSolid;
               pbEM.Canvas.Brush.Color := clRed;
               pbEM.Canvas.Ellipse( x - 4, y - 4, x + 4, y + 4);

               pbEM.Canvas.Brush.Style := bsClear;
               centRc.Left := x - Round( 1000 * fDispSigmas[0, cntIdx] );
               centRc.Right := x + Round( 1000 * fDispSigmas[0, cntIdx] );

               centRc.Top := y - Round( 1000 * fDispSigmas[1, cntIdx] );
               centRc.Bottom := y + Round( 1000 * fDispSigmas[1, cntIdx] );

               DrawRotatedEllipse(pbEm.Canvas, centRc, fDispSigmaRot[cntIdx]);
          end;
     end;

end;

procedure TfrmEMTest.rad2CClick(Sender: TObject);
begin
     ExecEM;
end;

procedure TfrmEMTest.scrVarC1Change(Sender: TObject);
begin
     ExecEM;
end;

procedure TfrmEMTest.OnEMUpdate(Sender: TObject; iter: integer; W, M: IMatrix;
  V: IMatrixDynArr; E : IMatrix);
var counter: Integer;
    cntCnt : integer;
    evals, evect : IMatrix;
begin
     fIterW[fNumIter] := W.Clone;
     fIterM[fNumIter] := M.Clone;
     SetLength(fIterV[fNumIter], Length(V));
     fIterE[fNumIter] := nil;
     if Assigned(E) then
        fIterE[fNumIter] := E.Clone;

     FillChar( fIterSigmaRot[fNumIter], sizeof(fIterSigmaRot[fNumIter]), 0);
     FillChar( fIterSigmas[fNumIter], sizeof(fIterSigmas[fNumIter]), 0);

     for counter := 0 to length(V) - 1 do
         fIterV[fNumIter][counter] := V[counter].Clone;

     for cntCnt := 0 to fNumCenters - 1 do
     begin
          if V[cntCnt].SymEig( evals, evect ) = qlOk then
          begin
               if evals.Vec[0] > evals.Vec[1] then
               begin
                    fIterSigmas[fNumIter][1, cntCnt] := sqrt(evals.Vec[0]);
                    fIterSigmas[fNumIter][0, cntCnt] := sqrt(evals.Vec[1]);

                    fIterSigmaRot[fNumIter][cntCnt] := Round( ArcTan2( evect[0, 0], evect[0, 1] ) * 180/pi);
               end
               else
               begin
                    fIterSigmas[fNumIter][0, cntCnt] := sqrt(evals.Vec[0]);
                    fIterSigmas[fNumIter][1, cntCnt] := sqrt(evals.Vec[1]);

                    fIterSigmaRot[fNumIter][cntCnt] := Round( ArcTan2( evect[1, 0], evect[1, 1] ) * 180/pi)
               end;
          end;
     end;

     inc(fNumIter);
     lblIter.Caption := IntToStr(fNumIter);
end;

procedure TfrmEMTest.timAnimateTimer(Sender: TObject);
begin
     inc(fIdx);

     if fIdx >= fNumIter then
     begin
          timAnimate.Enabled := False;
          fDispM := fResM;
          fDispV := fResV;
          fDispW := fResW;
          fDispE := fResE;
          Move(fSigmas, fDispSigmas, sizeof(fSigmas));
          Move(fSigmaRot, fDispSigmaRot, sizeof(fSigmaRot));
     end
     else
     begin
          fDispM := fIterM[fIdx];
          fDispV := fIterV[fIdx];
          fDispW := fIterW[fIdx];
          fDispE := fIterE[fIdx];

          Move(fIterSigmas[fIdx], fDispSigmas, sizeof(fDispSigmas));
          Move(fIterSigmaRot[fIdx], fDispSigmaRot, sizeof(fDispSigmaRot));
     end;
     pbEM.Invalidate;
end;

procedure TfrmEMTest.Button1Click(Sender: TObject);
begin
     fIdx := -1;
     timAnimate.Enabled := True;
end;

end.
