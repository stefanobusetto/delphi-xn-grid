program xnGridSortTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
  DUnitTestRunner,
  xn.grid.sort in '..\xn.grid.sort.pas',
  xn.grid.link in '..\xn.grid.link.pas',
  xn.grid.sort.test in 'xn.grid.sort.test.pas',
  cSampleData in '..\..\cSampleData.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;

end.
