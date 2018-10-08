program TRegExprClass;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  TRegExprClassMain in 'TRegExprClassMain.pas' {fmTRegExprClassMain},
  RegExpr in '..\..\Src\RegExpr.pas';

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmTRegExprClassMain, fmTRegExprClassMain);
  Application.Run;
end.
