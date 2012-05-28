program TRegExprRoutins;

uses
  Forms,
  TRegExprRoutinesMain in 'TRegExprRoutinesMain.pas' {fmTRegExprRoutines},
  RegExpr in '..\..\Src\RegExpr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmTRegExprRoutines, fmTRegExprRoutines);
  Application.Run;
end.
