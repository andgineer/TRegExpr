program tregexpr_test;

{$mode objfpc}{$H+}

uses
  Classes, Interfaces, consoletestrunner, tregexpr_tests, RegExpr;

type

  TRegExprRunner = class(TTestRunner)
  protected
  end;

var
  Application: TRegExprRunner;

begin
  Application := TRegExprRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
