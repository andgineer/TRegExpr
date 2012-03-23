program TRegExprClass;

uses
  Forms,
  TRegExprClassMain in 'TRegExprClassMain.pas' {fmTRegExprClassMain},
  RegExpr in '..\..\Source\RegExpr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmTRegExprClassMain, fmTRegExprClassMain);
  Application.Run;
end.
