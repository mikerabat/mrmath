unit mrMatrixPlotOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMtxPlotOptions = class(TForm)
    lblCapCurXAxis: TLabel;
    lblCapCurYAxis: TLabel;
    lblCurXAxis: TLabel;
    lblCurYAxis: TLabel;
    lblCapXAxis: TLabel;
    lblCapNewYAxis: TLabel;
    edMinXAxis: TEdit;
    edMaxXAxis: TEdit;
    edMinYAxis: TEdit;
    edMaxYAxis: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    lblCapDataXVals: TLabel;
    lblDataX: TLabel;
    lblCapDataYVals: TLabel;
    lblDataY: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmMtxPlotOptions: TfrmMtxPlotOptions;

implementation

{$R *.dfm}

end.
