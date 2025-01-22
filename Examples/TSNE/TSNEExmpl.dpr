program TSNEExmpl;

uses
  Forms,
  ufrmTSNE in 'ufrmTSNE.pas' {frmTSNE},
  tSNE in '..\..\tSNE.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTSNE, frmTSNE);
  Application.Run;
end.
