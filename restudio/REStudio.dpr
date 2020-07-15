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
  Forms, Interfaces,
  REStudioMain in 'REStudioMain.pas' {fmREStudioMain},
  PCode in 'PCode.pas' {fmPseudoCodeViewer},
  FileViewer in 'FileViewer.pas' {fmFileViewer},
  regexpr in '..\regexpr.pas',
  StopWatch in 'StopWatch\StopWatch.pas',
  tynList in 'Persistence\tynList.pas',
  ansoRTTIHook in 'Persistence\ansoRTTIHook.pas',
  ansoStrings in 'Persistence\ansoStrings.pas';

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmREDebuggerMain, fmREDebuggerMain);
  //Application.CreateForm(TfmTestCases, fmTestCases);
  Application.Run;
end.

