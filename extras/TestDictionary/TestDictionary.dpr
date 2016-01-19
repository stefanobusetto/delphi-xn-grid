program TestDictionary;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1} ,
  xn.dictionary in '..\..\sources\xn.dictionary.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
