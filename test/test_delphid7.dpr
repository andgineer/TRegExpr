program test_delphid7;

uses
  FastMM4 in '..\..\..\fastMM\FastMM4.pas',
  SysUtils,
  TestFramework,
  TestExtensions,
  Forms,
  GUITestRunner,
  TextTestRunner,
  GUITesting,
  tests in 'tests.pas',
  regexpr in '..\..\Regexpr.pas';

{$R *.res}


begin
  if FindCmdLineSwitch('text-mode', ['-','/'], true) then
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures)
  else
  begin
    Application.Initialize;
    Application.Title := 'DUnit Tests';
    // RunRegisteredTests class methods are recommended
    TGUITestRunner.RunRegisteredTests;
  end;
end.
