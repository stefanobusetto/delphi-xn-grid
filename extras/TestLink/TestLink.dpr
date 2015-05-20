program TestLink;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  uBitmaps in 'uBitmaps.pas',
  xn.grid.link.sample in 'xn.grid.link.sample.pas',
  uCommands in 'uCommands.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Luna');
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
