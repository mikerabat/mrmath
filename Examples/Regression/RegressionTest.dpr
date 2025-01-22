program RegressionTest;

uses
  Forms,
  ufrmRegression in 'ufrmRegression.pas' {frmRegression};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRegression, frmRegression);
  Application.Run;
end.
