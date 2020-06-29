program TRegExprRoutins;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  TRegExprRoutinesMain in 'TRegExprRoutinesMain.pas' {fmTRegExprRoutines},
  regexpr in '..\..\Src\regexpr.pas';

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmTRegExprRoutines, fmTRegExprRoutines);
  Application.Run;
end.
