program QuickSort;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain},
  cSampleData in 'D:\svn\delphi-xn-grid\cSampleData.pas',
  SortIndex in 'SortIndex.pas',
  SortArray in 'SortArray.pas',
  xn.grid.link in 'D:\svn\delphi-xn-grid\sources\xn.grid.link.pas',
  xn.grid.common in 'D:\svn\delphi-xn-grid\sources\xn.grid.common.pas',
  xn.grid.data in 'D:\svn\delphi-xn-grid\sources\xn.grid.data.pas',
  xn.list in 'D:\svn\delphi-xn-grid\sources\xn.list.pas',
  xn.timers in 'D:\svn\delphi-xn-grid\sources\xn.timers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
