program TableModuleRTTI;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uObjectDataSet in 'uObjectDataSet.pas',
  uData.Customers in 'uData.Customers.pas',
  uData.Cities in 'uData.Cities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
