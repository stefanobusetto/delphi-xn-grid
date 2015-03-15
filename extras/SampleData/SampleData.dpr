program SampleData;

uses
  Vcl.Forms,
  main in 'main.pas' {FormMain},
  xn.grid.common in '..\..\sources\xn.grid.common.pas',
  cSampleData in '..\..\cSampleData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
