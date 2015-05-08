program TestObserver;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  cSampleDataset in 'cSampleDataset.pas',
  cDatasource in 'cDatasource.pas',
  xn.dataset.cache in 'xn.dataset.cache.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
