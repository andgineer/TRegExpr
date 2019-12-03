program test_fpc;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tests;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='test_fpc';
  Application.Run;
  Application.Free;
end.

