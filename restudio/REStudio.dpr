{$I REStudio_inc.pas}
program REStudio;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
 Visual debugger for regular expressions

 (c) 1999-2004 Andrey V. Sorokin
  Saint Petersburg, Russia
  https://sorokin.engineer/
  andrey@sorokin.engineer
}

uses
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  REStudioMain in 'REStudioMain.pas' {fmREStudioMain},
  PCode in 'PCode.pas' {fmPseudoCodeViewer},
  StopWatch in 'StopWatch\StopWatch.pas',
  regexpr in '..\src\regexpr.pas',
  ansoRTTI in '..\Persistence\ansoRTTI.pas',
  ansoRTTIHook in '..\Persistence\ansoRTTIHook.pas',
  ansoStrings in '..\Persistence\ansoStrings.pas',
  tynList in '..\Persistence\tynList.pas';

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmREDebuggerMain, fmREDebuggerMain);
  // Application.CreateForm(TfmTestCases, fmTestCases);
  Application.Run;
end.

