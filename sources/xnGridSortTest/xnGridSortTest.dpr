program xnGridSortTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
  DUnitTestRunner,
  xn.grid.sort.test in 'xn.grid.sort.test.pas',
  cSampleData in '..\..\cSampleData.pas',
  xn.list in '..\xn.list.pas',
  xn.grid.common in '..\xn.grid.common.pas',
  xn.grid.data in '..\xn.grid.data.pas',
  xn.grid.link in '..\xn.grid.link.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;

end.
