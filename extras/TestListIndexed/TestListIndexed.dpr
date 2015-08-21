program TestListIndexed;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  xn.list in '..\..\sources\xn.list.pas',
  xn.grid.data in '..\..\sources\xn.grid.data.pas',
  xn.list.indexed in 'xn.list.indexed.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
