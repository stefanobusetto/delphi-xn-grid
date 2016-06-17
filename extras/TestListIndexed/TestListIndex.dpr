program TestListIndex;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  xn.list in '..\..\sources\xn.list.pas',
  xn.grid.data in '..\..\sources\xn.grid.data.pas',
  xn.list.index in 'xn.list.index.pas',
  helpers in 'helpers.pas',
  helpers.integer in 'helpers.integer.pas',
  helpers.myrecord in 'helpers.myrecord.pas',
  helpers.myobject in 'helpers.myobject.pas',
  xn.list.observable in '..\..\sources\xn.list.observable.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
