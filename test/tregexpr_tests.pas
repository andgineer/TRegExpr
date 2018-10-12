unit tregexpr_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TRegExprTestCase= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

uses
  LCLIntf, RegExpr;

procedure TRegExprTestCase.TestHookUp;
  var
     r : TRegExpr;

  procedure Check (ASubExprMatchCount, APos, ALen : integer);
   begin
    writeln ('Expression "', r.Expression, '"'#$d#$a,
       'Modifiers "', r.ModifierStr, '"'#$d#$a,
       'Input text "', r.InputString, '"'#$d#$a,
       'Actual/expected results: '#$d#$a,
       '  Sub-expressions matched: ', r.SubExprMatchCount, ' / ', ASubExprMatchCount, #$d#$a,
       '  Expression match starting position: ', r.MatchPos [0], ' / ', APos, #$d#$a,
       '  Expression match length: ', r.MatchLen [0], ' / ', ALen);
    writeln ('P-Code:'#$d#$a, r.Dump);

    AssertEquals(r.SubExprMatchCount, ASubExprMatchCount);
    AssertEquals(r.MatchPos [0], APos);
    AssertEquals(r.MatchLen [0], ALen);
   end;

  procedure CheckReplace (AExpr, AText, ASubs, AExpected : string);
   var
      ActualResult: string;
      c: char;
   begin
    ActualResult := ReplaceRegExpr(AExpr, AText, ASubs, True);
    writeln ('Error. '#$d#$a'Expression "', AExpr, '"'#$d#$a,
     'Input text "', AText, '"'#$d#$a,
     'Pattern "', ASubs, '"'#$d#$a,
     'Actual/expected results: ', ActualResult, ' / ', AExpected);
    write('Actual result bytes: ');
    for c in ActualResult do
        write(IntToHex(ord(c), 2), ' ');
    writeln;
    AssertEquals(ActualResult, AExpected);
   end;

  begin
     r := TRegExpr.Create;

     r.Expression := '[A-Z]';
     r.Exec ('234578923457823659GHJK38');
     Check (0, 19, 1);

     r.Expression := '[A-Z]*?';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 1, 0);

     r.Expression := '[A-Z]+';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 5);

     r.Expression := '[A-Z][A-Z]*';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 5);

     r.Expression := '[A-Z][A-Z]?';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 2);

     r.Expression := '[^\d]+';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 5);

     { test chaining }

     r.Expression := '[A-Z][A-Z]?[A-Z]';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 3);

     r.Expression := '[A-Z][A-Z]*[0-9]';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 6);

     r.Expression := '[A-Z]+[0-9]';
     r.Exec ('234578923457823659ARTZU38');
     Check (0, 19, 6);

     { case insensitive: }
     r.ModifierI := True;

     r.Expression := '[A-Z]';
     r.Exec ('234578923457823659a38');
     Check (0, 19, 1);

     { case insensitive: }
     r.Expression := '[a-z]';
     r.Exec ('234578923457823659A38');
     Check (0, 19, 1);

     r.ModifierI := False;

     { with parenthsis }
     r.Expression := '(foo)1234';
     r.Exec ('1234   foo1234XXXX');
     Check (1, 8, 7);

     r.Expression := '(((foo)))1234';
     r.Exec ('1234   foo1234XXXX');
     Check (3, 8, 7);

     r.Expression := '(foo)(1234)';
     r.Exec ('1234   foo1234XXXX');
     Check (2, 8, 7);

     { test real backtracking }

     r.Expression := 'nofoo|foo';
     r.Exec ('1234   foo1234XXXX');
     Check (0, 8, 3);

     r.Expression := '(nofoo|foo)1234';
     r.Exec ('1234   nofoo1234XXXX');
     Check (1, 8, 9);

     r.Expression := '(nofoo|foo|anotherfoo)1234';
     r.Exec ('1234   nofoo1234XXXX');
     Check (1, 8, 9);

     r.Expression := 'nofoo1234|foo1234';
     r.Exec ('1234   foo1234XXXX');
     Check (0, 8, 7);

     r.Expression := '(\w*)';
     r.Exec ('name.ext');
     Check (1, 1, 4);

     CheckReplace('(\w*)', 'name.ext', '$1.new', 'name.new.new.ext.new.new');

     r.Expression := '\r(\n)';
     r.Exec (#$d#$a);
     Check (1, 1, 2);

     CheckReplace(#$d'('#$a')', 'word'#$d#$a, '$1', 'word'#$a);
     CheckReplace('(word)', 'word', '\U$1\\r', 'WORD\r');
     CheckReplace('(word)', 'word', '$1\n', 'word'#$d#$a);
end;

initialization
  RegisterTest(TRegExprTestCase);
end.
