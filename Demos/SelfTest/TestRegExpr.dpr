program TestRegExpr;

{
  Very simple test of Delphi Regular Expression library
  (www.RegExpStudio.com) for Delphi/FreePascal compilers.
  This is also self-test (very basic one) for the library.
  Adapted from FreePascal test application
  \source\packages\base\regexpr\testreg1.pp
  by Andrey V. Sorokin.

  2004.01.04 v.1.0
  -=- Just first version

}

{$IFDEF FPC}
 {$MODE DELPHI} // Delphi-compatible mode in FreePascal
{$ENDIF}

{$IFDEF FPC}
 Please, do not forget to set correct path to RegExpr.pas unit
 (because embeded into FreePascal simple r.e. unit has the same
  name as one from www.RegExpStudio.com).
 The simplest way - just place RegExpr.pas into the same directory
 with TestRegExpr.dpr.
 And remove this comment ;)
{$ENDIF}


uses
   RegExpr;

var
   r : TRegExpr;

procedure Check (ASubExprMatchCount, APos, ALen : integer);
 begin
  if (r.SubExprMatchCount <> ASubExprMatchCount)
     or (r.MatchPos [0] <> APos) or (r.MatchLen [0] <> ALen) then begin
    writeln ('Error. '#$d#$a'Expression "', r.Expression, '"'#$d#$a,
     'Modifiers "', r.ModifierStr, '"'#$d#$a,
     'Input text "', r.InputString, '"'#$d#$a,
     'Actual/expected results: '#$d#$a,
     '  Sub-expressions matched: ', r.SubExprMatchCount, ' / ', ASubExprMatchCount, #$d#$a,
     '  Expression match starting position: ', r.MatchPos [0], ' / ', APos, #$d#$a,
     '  Expression match length: ', r.MatchLen [0], ' / ', ALen);
    writeln ('P-Code:'#$d#$a, r.Dump);
    halt (1);
   end;
 end;

begin
   writeln ('*** Testing library RegExpr (www.RegExpStudio.com) ***');

   { basic tests }

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


   writeln ('*** The test have been successfully finished ***');

end.
