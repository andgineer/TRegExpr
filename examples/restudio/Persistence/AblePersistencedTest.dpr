program AblePersistencedTest;

uses
  Forms,
  AblePersistencedTestMain in 'AblePersistencedTestMain.pas' {Form1},
  tynList in 'tynList.pas',
  ansoRTTI in 'ansoRTTI.pas',
  ansoRTTIHook in 'ansoRTTIHook.pas',
  ansoStrings in 'ansoStrings.pas',
  RETestCases in '..\Demo\RETestCases.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
