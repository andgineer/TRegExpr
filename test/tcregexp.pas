unit tcregexp;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{ $DEFINE DUMPTESTS} //define this to dump a

{$IFDEF VER130} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D5
{$IFDEF VER140} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D6
{$IFDEF VER150} {$DEFINE D7} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D7
{$IFDEF D5} {$DEFINE OverMeth} {$ENDIF}
{$IFDEF FPC} {$DEFINE OverMeth} {$ENDIF}

interface

uses
  {$IFDEF FPC}
  fpcunit, testregistry,
    {$IFDEF VER3}
    fpwidestring, //required in FPC to use WideChar uppercase/lowercase
    {$ENDIF}
  {$ELSE}
  TestFramework,
  {$ENDIF}
  Classes, SysUtils,
  RegExpr {$IFDEF FPC}in '../src/RegExpr.pas'{$ENDIF};

type

  { TTestRegexpr }

  TTestRegexpr= class(TTestCase)
  private
    RE: TRegExpr;
  protected
    class function PrintableString(AString: string): string;
    Procedure RunRETest(aIndex : Integer);
    procedure CompileRE(AExpression: string);
    procedure IsNotNull(ArrorMessage: string; AObjectToCheck: TObject);
    procedure IsTrue(ArrorMessage: string; AConditionToCheck: boolean);
    procedure IsFalse(ArrorMessage: string; AConditionToCheck: boolean);
    procedure AreEqual(ArrorMessage: string; s1,s2: string); overload;
    procedure AreEqual(ArrorMessage: string; i1,i2: integer); overload;
  published
    procedure TestEmpty;
    procedure TestNotFound;
    {$IFDEF OverMeth}
    procedure TestReplaceOverload;
    {$ENDIF}
    Procedure RunTest1;
    Procedure RunTest2;
    Procedure RunTest3;
    Procedure RunTest4;
    Procedure RunTest5;
    Procedure RunTest6;
    Procedure RunTest7;
    Procedure RunTest8;
    Procedure RunTest9;
    Procedure RunTest10;
    Procedure RunTest11;
    Procedure RunTest12;
    Procedure RunTest13;
    Procedure RunTest14;
    Procedure RunTest15;
    Procedure RunTest16;
    Procedure RunTest17;
    Procedure RunTest18;
    Procedure RunTest19;
    Procedure RunTest20;
    Procedure RunTest21;
    Procedure RunTest22;
    Procedure RunTest23;
    Procedure RunTest24;
    Procedure RunTest25;
    Procedure RunTest26;
    Procedure RunTest27;
    Procedure RunTest28;
    Procedure RunTest29;
    Procedure RunTest30;
    Procedure RunTest31;
    Procedure RunTest32;
    Procedure RunTest33;
    Procedure RunTest34;
    Procedure RunTestGrp;
  end;

implementation

Type
  TRegExTest = record
    Expression: RegExprString;
    InputText: RegExprString;
    SubstitutionText: RegExprString;
    ExpectedResult: RegExprString;
    MatchStart: integer;
  end;

const
  testCases: array [1..34] of TRegExTest = (
    (
    expression: '\nd';
    inputText: 'abc'#13#10'def';
    substitutionText: '\n\x{10}\r\\';
    expectedResult: 'abc'#13#10#16#13'\ef';
    MatchStart: 0
    ),
    (
    expression: '(\w*)';
    inputText: 'name.ext';
    substitutionText: '$1.new';
    expectedResult: 'name.new.new.ext.new.new';
    MatchStart: 0
    ),
    (
    expression: #$d'('#$a')';
    inputText: 'word'#$d#$a;
    substitutionText: '$1';
    expectedResult: 'word'#$a;
    MatchStart: 0
    ),
    (
    expression: '(word)';
    inputText: 'word';
    substitutionText: '\U$1\\r';
    expectedResult: 'WORD\r';
    MatchStart: 0
    ),
    (
    expression: '(word)';
    inputText: 'word';
    substitutionText: '$1\n';
    expectedResult: 'word'#$a;
    MatchStart: 0
    ),
    (
    expression: '[A-Z]';
    inputText: '234578923457823659GHJK38';
    substitutionText: '';
    expectedResult: 'G';
    matchStart: 19;
    ),
    (
    expression: '[A-Z]*?';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: '';
    matchStart: 1
    ),
    (
    expression: '[A-Z]+';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU';
    matchStart: 19
    ),
    (
    expression: '[A-Z][A-Z]*';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU';
    matchStart: 19
    ),
    (
    expression: '[A-Z][A-Z]?';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'AR';
    matchStart: 19
    ),
    (
    expression: '[^\d]+';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU';
    matchStart: 19
    ),
    (
    expression: '[A-Z][A-Z]?[A-Z]';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ART';
    matchStart: 19
    ),
    (
    expression: '[A-Z][A-Z]*[0-9]';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU3';
    matchStart: 19
    ),
    (
    expression: '[A-Z]+[0-9]';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU3';
    matchStart: 19
    ),
    (
    expression: '(?i)[A-Z]';
    inputText: '234578923457823659a38';
    substitutionText: '';
    expectedResult: 'a';
    matchStart: 19
    ),
    (
    expression: '(?i)[a-z]';
    inputText: '234578923457823659A38';
    substitutionText: '';
    expectedResult: 'A';
    matchStart: 19
    ),
    (
    expression: '(foo)1234';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    (
    expression: '(((foo)))1234';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    (
    expression: '(foo)(1234)';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    (
    expression: 'nofoo|foo';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo';
    matchStart: 8
    ),
    (
    expression: '(nofoo|foo)1234';
    inputText: '1234   nofoo1234XXXX';
    substitutionText: '';
    expectedResult: 'nofoo1234';
    matchStart: 8
    ),
    (
    expression: '(nofoo|foo|anotherfoo)1234';
    inputText: '1234   nofoo1234XXXX';
    substitutionText: '';
    expectedResult: 'nofoo1234';
    matchStart: 8
    ),
    (
    expression: 'nofoo1234|foo1234';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    (
    expression: '(\w*)';
    inputText: 'name.ext';
    substitutionText: '';
    expectedResult: 'name';
    matchStart: 1
    ),
    (
    expression: '\r(\n)';
    inputText: #$d#$a;
    substitutionText: '';
    expectedResult: #$d#$a;
    matchStart: 1
    ),
    (
    expression: '\r(\n)';
    inputText: #$d#$a;
    substitutionText: '\n';
    expectedResult: #$a;
    matchStart: 1
    ),
    (
    expression: '(?m)Test:\s*(.*?)\s;';
    inputText: 'Test: hel'#$d#$a'lo ;';
    substitutionText: '';
    expectedResult: 'Test: hel'#$d#$a'lo ;';
    matchStart: 1
    ),
    (
    expression: '(?:\w+)=\w+;(\w+)=\w+;(?:\w+)=\w+;(\w+)=\w+;';
    inputText: 'skip1=11;needed1=22;skip2=33;needed2=44;';
    substitutionText: '$1 $2';
    expectedResult: 'needed1 needed2';
    matchStart: 0
    ),
    (
    expression: '.*?\b(https?|ftp)://(?:\w+)\.(?:\w+)\.(\w+)';
    inputText: '>>ftp://www.name.com';
    substitutionText: '$1 $2';
    expectedResult: 'ftp com';
    matchStart: 0
    ),
    (
    expression: '\v';
    inputText: 'aaa'#10'bbb'#13'ccc'#$c'ddd'#$b'eee';
    substitutionText: '-';
    expectedResult: 'aaa-bbb-ccc-ddd-eee';
    matchStart: 0
    ),
    (
    expression: '\h+';
    inputText: #9'aaa  bbb '#9' ccc  '#$A0#9;
    substitutionText: '-';
    expectedResult: '-aaa-bbb-ccc-';
    matchStart: 0
    ),
    (
    expression: '\w+';
    inputText: 'abc XY 12.,';
    substitutionText: '\L$0';
    expectedResult: 'abc xy 12.,';
    matchStart: 0
    ),
    (
    expression: '\w+';
    inputText: 'abc XY 12.,';
    substitutionText: '\U$0';
    expectedResult: 'ABC XY 12.,';
    matchStart: 0
    ),
    ( // NULL chars in InputString
    expression: #0+'?[2-5]+(\s+)([xyz\$\#]{3,})\1'+#0+'+.+';
    inputText: '.:'+#0+'ab'+#0+'_34  z$x  '+#0+'end';
    substitutionText: '';
    expectedResult: '34  z$x  '+#0+'end';
    matchStart: 8
    )
  );

procedure TTestRegexpr.IsFalse(ArrorMessage: string; AConditionToCheck: boolean);
begin
  IsTrue(ArrorMessage, not AConditionToCheck)
end;

procedure TTestRegexpr.IsTrue(ArrorMessage: string; AConditionToCheck: boolean);
begin
  {$IFDEF FPC}
  AssertTrue(ArrorMessage, AConditionToCheck);
  {$ELSE}
  CheckTrue(AConditionToCheck, ArrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.IsNotNull(ArrorMessage: string; AObjectToCheck: TObject);
begin
  {$IFDEF FPC}
  AssertNotNull(ArrorMessage, AObjectToCheck);
  {$ELSE}
  CheckNotNull(AObjectToCheck, ArrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.AreEqual(ArrorMessage: string; s1,s2: string);
begin
  {$IFDEF FPC}
  AssertEquals(ArrorMessage, s1,s2);
  {$ELSE}
  CheckEquals(s1,s2, ArrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.AreEqual(ArrorMessage: string; i1,i2: integer);
begin
  {$IFDEF FPC}
  AssertEquals(ArrorMessage, i1,i2);
  {$ELSE}
  CheckEquals(i1,i2, ArrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.TestEmpty;
begin
  CompileRE('1'); // just to create RE object
  IsFalse('UseOsLineEndOnReplace correctly set', RE.UseOsLineEndOnReplace);
end;

procedure TTestRegexpr.TestNotFound;
var
  N: integer;
begin
  CompileRE('w{2,}');
  RE.InputString:= 'tst';
  IsFalse('Exec must give False', RE.Exec(1));
  N:= RE.MatchPos[0];
  IsTrue('MatchPos[0] must be -1, but it is '+IntToStr(N), N=-1);
  N:=RE.MatchLen[0];
  IsTrue('MatchLen[0] must be -1, but it is '+IntToStr(N), N=-1);
end;

{$IFDEF OverMeth}
procedure TTestRegexpr.TestReplaceOverload;

var
  act : String;

begin
    CompileRE('A\r(\n)'); // just to print compiled re - it will be recompiled below
    act:=ReplaceRegExpr('A\r(\n)', 'a'#$d#$a, '\n', [rroModifierI, rroUseSubstitution]);
    AssertEquals('Replace failed', PrintableString(#$a), PrintableString(Act))
end;
{$ENDIF}

procedure TTestRegexpr.RunTest1;
begin
  RunRETest(1);
end;

procedure TTestRegexpr.RunTest2;
begin
  RunRETest(2);
end;

procedure TTestRegexpr.RunTest3;
begin
  RunRETest(3);
end;

procedure TTestRegexpr.RunTest4;
begin
  RunRETest(4);
end;

procedure TTestRegexpr.RunTest5;
begin
  RunRETest(5);
end;

procedure TTestRegexpr.RunTest6;
begin
  RunRETest(6);
end;

procedure TTestRegexpr.RunTest7;
begin
  RunRETest(7);
end;

procedure TTestRegexpr.RunTest8;
begin
  RunRETest(8);
end;

procedure TTestRegexpr.RunTest9;
begin
  RunRETest(9);
end;

procedure TTestRegexpr.RunTest10;
begin
  RunRETest(10);
end;

procedure TTestRegexpr.RunTest11;
begin
  RunRETest(11);
end;

procedure TTestRegexpr.RunTest12;
begin
  RunRETest(12);
end;

procedure TTestRegexpr.RunTest13;
begin
  RunRETest(13);
end;

procedure TTestRegexpr.RunTest14;
begin
  RunRETest(14);
end;

procedure TTestRegexpr.RunTest15;
begin
  RunRETest(15);
end;

procedure TTestRegexpr.RunTest16;
begin
  RunRETest(16);
end;

procedure TTestRegexpr.RunTest17;
begin
  RunRETest(17);
end;

procedure TTestRegexpr.RunTest18;
begin
  RunRETest(18);
end;

procedure TTestRegexpr.RunTest19;
begin
  RunRETest(19);
end;

procedure TTestRegexpr.RunTest20;
begin
  RunRETest(20);
end;

procedure TTestRegexpr.RunTest21;
begin
  RunRETest(21);
end;

procedure TTestRegexpr.RunTest22;
begin
  RunRETest(22);
end;

procedure TTestRegexpr.RunTest23;
begin
  RunRETest(23);
end;

procedure TTestRegexpr.RunTest24;
begin
  RunRETest(24);
end;

procedure TTestRegexpr.RunTest25;
begin
  RunRETest(25);
end;

procedure TTestRegexpr.RunTest26;
begin
  RunRETest(26);
end;

procedure TTestRegexpr.RunTest27;
begin
  RunRETest(27);
end;

procedure TTestRegexpr.RunTest28;
begin
  RunRETest(28);
end;

procedure TTestRegexpr.RunTest29;
begin
  RunRETest(29);
end;

procedure TTestRegexpr.RunTest30;
begin
  RunRETest(30);
end;

procedure TTestRegexpr.RunTest31;
begin
  RunRETest(31);
end;

procedure TTestRegexpr.RunTest32;
begin
  RunRETest(32);
end;

procedure TTestRegexpr.RunTest33;
begin
  RunRETest(33);
end;

procedure TTestRegexpr.RunTest34;
begin
  RunRETest(34);
end;

procedure TTestRegexpr.RunTestGrp;
var
  R: TRegExpr;
begin
  R:= TRegExpr.Create;
  try
    R.Expression:= '(\w+) (?:\w+) (\w+) (?:\w+) (\d+)';
    R.InputString:= 'abc wall dirt wert 234';
    R.ExecPos(1);
    AreEqual('Group finder failed', 1, R.MatchPos[0]);
    AreEqual('Group counter failed', 3, R.SubExprMatchCount);
  finally
    FreeAndNil(R);
  end;
end;

Class function TTestRegexpr.PrintableString(AString: string): string;

var
    ch: Char;

begin
  Result := '';
  for ch in AString do
    if ch < #31 then
      Result := Result + '#' + IntToStr(Ord(ch))
    else
      Result := Result + ch;
end;

procedure TTestRegexpr.CompileRE(AExpression: string);
begin
  if (RE = Nil) then begin
    RE := TRegExpr.Create;
    RE.UseOsLineEndOnReplace:=False;
  end;
  RE.Expression:=AExpression;
  RE.Compile;
{$IFDEF DUMPTESTS}
  writeln('  Modifiers "', RE.ModifierStr, '"');
  writeln('  Regular expression: ', T.Expression,' ,');
  writeln('  compiled into p-code: ');
  writeln('  ', RE.Dump);
  writeln('  Input text: "', PrintableString(T.inputText), '"');
  if (T.substitutionText <> '')  then
    Writeln('  Substitution text: "', PrintableString(T.substitutionText), '"');
{$ENDIF}
end;

procedure TTestRegexpr.RunRETest(aIndex: Integer);


var
  T: TRegExTest;
  act : String;

begin
  T:=testCases[aIndex];
{$IFDEF DUMPTESTS}
  Writeln('Test: ',TestName);
{$ENDIF}
  CompileRE(T.Expression);
  if (T.SubstitutionText <> '') then
    begin
    act:=RE.Replace(T.InputText,T.SubstitutionText,True);
    AreEqual('Replace failed', PrintableString(T.ExpectedResult),PrintableString(Act))
    end
  else
    begin
    RE.Exec(T.inputText);
    AreEqual('Search position',T.MatchStart,RE.MatchPos[0]);
    AreEqual('Matched text',PrintableString(T.ExpectedResult),PrintableString(RE.Match[0]));
    end;
end;

initialization

{$IFDEF FPC}
  RegisterTest(TTestRegexpr);
{$ENDIF}
end.

