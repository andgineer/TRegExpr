unit my_lazarus_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TAwesomeTestCase= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

uses
  LCLIntf;

procedure TAwesomeTestCase.TestHookUp;
begin
  AssertTrue('This shouldn''t fail...', True);
  AssertTrue('GetTickCount', GetTickCount() >= 0);
  AssertEquals('RValue', $01, GetRValue($10FF01));
  AssertEquals('RValue', $FF, GetGValue($10FF01));
  AssertEquals('RValue', $10, GetBValue($10FF01));
end;

initialization
  RegisterTest(TAwesomeTestCase);
end.

