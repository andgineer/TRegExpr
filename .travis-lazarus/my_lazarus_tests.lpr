program my_lazarus_tests;

{$mode objfpc}{$H+}

uses
  Classes, Interfaces, consoletestrunner, my_lazarus_test;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
    // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
