program SampleData;

uses
  Vcl.Forms,
  main in 'main.pas' {FormMain},
  xn.grid.link in '..\..\sources\xn.grid.link.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
