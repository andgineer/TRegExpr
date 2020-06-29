unit tests;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{ $DEFINE DUMPTESTS} //define this to dump a

{$IFDEF VER130} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D5
{$IFDEF VER140} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D6
{$IFDEF VER150} {$DEFINE D7} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D7
{$IFDEF D5} {$DEFINE OverMeth} {$ENDIF}
{$IFDEF FPC} {$DEFINE OverMeth} {$ENDIF}

{off $DEFINE Unicode}

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
  regexpr {$IFDEF FPC}in '../src/regexpr.pas'{$ENDIF};

type

  { TTestRegexpr }

  TTestRegexpr= class(TTestCase)
  private
    RE: TRegExpr;
  protected
    Procedure RunRETest(aIndex : Integer);
    procedure CompileRE(AExpression: string);
    procedure IsNotNull(AErrorMessage: string; AObjectToCheck: TObject);
    procedure IsTrue(AErrorMessage: string; AConditionToCheck: boolean);
    procedure IsFalse(AErrorMessage: string; AConditionToCheck: boolean);
    procedure AreEqual(AErrorMessage: string; s1,s2: string); overload;
    procedure AreEqual(AErrorMessage: string; i1,i2: integer); overload;
    procedure TestBadRegex(const AErrorMessage, AExpression: string);
  published
    procedure TestEmpty;
    procedure TestNotFound;
    procedure TestBads;
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
    Procedure RunTest35;
    Procedure RunTest36;
    Procedure RunTest37;
    Procedure RunTest38;
    Procedure RunTest39;
    Procedure RunTest40;
    Procedure RunTest41;
    Procedure RunTest42;
    Procedure RunTest43;
    Procedure RunTest44;
    Procedure TestGroups;
    {$IFDEF Unicode}
    procedure TestUnicode1;
    {$ENDIF}
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

function PrintableString(const S: string): string;
var
  ch: char;
  i: integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    ch := S[i];
    if Ord(ch) < 31 then
      Result := Result + '#' + IntToStr(Ord(ch))
    else
      Result := Result + ch;
  end;
end;


const
  testCases: array [1..44] of TRegExTest = (
    // 1
    (
    expression: '\nd';
    inputText: 'abc'#13#10'def';
    substitutionText: '\n\x{10}\r\\';
    expectedResult: 'abc'#13#10#16#13'\ef';
    MatchStart: 0
    ),
    // 2
    (
    expression: '(\w*)';
    inputText: 'name.ext';
    substitutionText: '$1.new';
    expectedResult: 'name.new.new.ext.new.new';
    MatchStart: 0
    ),
    // 3
    (
    expression: #$d'('#$a')';
    inputText: 'word'#$d#$a;
    substitutionText: '$1';
    expectedResult: 'word'#$a;
    MatchStart: 0
    ),
    // 4
    (
    expression: '(word)';
    inputText: 'word';
    substitutionText: '\U$1\\r';
    expectedResult: 'WORD\r';
    MatchStart: 0
    ),
    // 5
    (
    expression: '(word)';
    inputText: 'word';
    substitutionText: '$1\n';
    expectedResult: 'word'#$a;
    MatchStart: 0
    ),
    // 6
    (
    expression: '[A-Z]';
    inputText: '234578923457823659GHJK38';
    substitutionText: '';
    expectedResult: 'G';
    matchStart: 19;
    ),
    // 7
    (
    expression: '[A-Z]*?';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: '';
    matchStart: 1
    ),
    // 8
    (
    expression: '[A-Z]+';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU';
    matchStart: 19
    ),
    // 9
    (
    expression: '[A-Z][A-Z]*';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU';
    matchStart: 19
    ),
    // 10
    (
    expression: '[A-Z][A-Z]?';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'AR';
    matchStart: 19
    ),
    // 11
    (
    expression: '[^\d]+';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU';
    matchStart: 19
    ),
    // 12
    (
    expression: '[A-Z][A-Z]?[A-Z]';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ART';
    matchStart: 19
    ),
    // 13
    (
    expression: '[A-Z][A-Z]*[0-9]';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU3';
    matchStart: 19
    ),
    // 14
    (
    expression: '[A-Z]+[0-9]';
    inputText: '234578923457823659ARTZU38';
    substitutionText: '';
    expectedResult: 'ARTZU3';
    matchStart: 19
    ),
    // 15
    (
    expression: '(?i)[A-Z]';
    inputText: '234578923457823659a38';
    substitutionText: '';
    expectedResult: 'a';
    matchStart: 19
    ),
    // 16
    (
    expression: '(?i)[a-z]';
    inputText: '234578923457823659A38';
    substitutionText: '';
    expectedResult: 'A';
    matchStart: 19
    ),
    // 17
    (
    expression: '(foo)1234';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    // 18
    (
    expression: '(((foo)))1234';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    // 19
    (
    expression: '(foo)(1234)';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    // 20
    (
    expression: 'nofoo|foo';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo';
    matchStart: 8
    ),
    // 21
    (
    expression: '(nofoo|foo)1234';
    inputText: '1234   nofoo1234XXXX';
    substitutionText: '';
    expectedResult: 'nofoo1234';
    matchStart: 8
    ),
    // 22
    (
    expression: '(nofoo|foo|anotherfoo)1234';
    inputText: '1234   nofoo1234XXXX';
    substitutionText: '';
    expectedResult: 'nofoo1234';
    matchStart: 8
    ),
    // 23
    (
    expression: 'nofoo1234|foo1234';
    inputText: '1234   foo1234XXXX';
    substitutionText: '';
    expectedResult: 'foo1234';
    matchStart: 8
    ),
    // 24
    (
    expression: '(\w*)';
    inputText: 'name.ext';
    substitutionText: '';
    expectedResult: 'name';
    matchStart: 1
    ),
    // 25
    (
    expression: '\r(\n)';
    inputText: #$d#$a;
    substitutionText: '';
    expectedResult: #$d#$a;
    matchStart: 1
    ),
    // 26
    (
    expression: '\r(\n)';
    inputText: #$d#$a;
    substitutionText: '\n';
    expectedResult: #$a;
    matchStart: 1
    ),
    // 27
    (
    expression: '(?m)Test:\s*(.*?)\s;';
    inputText: 'Test: hel'#$d#$a'lo ;';
    substitutionText: '';
    expectedResult: 'Test: hel'#$d#$a'lo ;';
    matchStart: 1
    ),
    // 28
    (
    expression: '(?:\w+)=\w+;(\w+)=\w+;(?:\w+)=\w+;(\w+)=\w+;';
    inputText: 'skip1=11;needed1=22;skip2=33;needed2=44;';
    substitutionText: '$1 $2';
    expectedResult: 'needed1 needed2';
    matchStart: 0
    ),
    // 29
    (
    expression: '.*?\b(https?|ftp)://(?:\w+)\.(?:\w+)\.(\w+)';
    inputText: '>>ftp://www.name.com';
    substitutionText: '$1 $2';
    expectedResult: 'ftp com';
    matchStart: 0
    ),
    // 30
    (
    expression: '\v';
    inputText: 'aaa'#10'bbb'#13'ccc'#$c'ddd'#$b'eee';
    substitutionText: '-';
    expectedResult: 'aaa-bbb-ccc-ddd-eee';
    matchStart: 0
    ),
    // 31
    (
    expression: '\h+';
    inputText: #9'aaa  bbb '#9' ccc  '#$A0#9;
    substitutionText: '-';
    expectedResult: '-aaa-bbb-ccc-';
    matchStart: 0
    ),
    // 32
    (
    expression: '\w+';
    inputText: 'abc XY 12.,';
    substitutionText: '\L$0';
    expectedResult: 'abc xy 12.,';
    matchStart: 0
    ),
    // 33
    (
    expression: '\w+';
    inputText: 'abc XY 12.,';
    substitutionText: '\U$0';
    expectedResult: 'ABC XY 12.,';
    matchStart: 0
    ),
    // 34
    ( // NULL chars in InputString
    expression: #0+'?[2-5]+(\s+)([xyz\$\#]{3,})\1'+#0+'+.+';
    inputText: '.:'+#0+'ab'+#0+'_34  z$x  '+#0+'end';
    substitutionText: '';
    expectedResult: '34  z$x  '+#0+'end';
    matchStart: 8
    ),
    // 35
    (
    expression: '\w\cA\cz\cb\w';
    inputText: '..abc'#1#26#2'test';
    substitutionText: '';
    expectedResult: 'c'#1#26#2't';
    matchStart: 5
    ),
    // 36
    (
    expression: '\V+';
    inputText: '.,,'#10'aB2'#13'cc()'#$c'$%'#$b'[]';
    substitutionText: '-';
    expectedResult: '-'#10'-'#13'-'#$c'-'#$b'-';
    matchStart: 0
    ),
    // 37
    (
    expression: '\H+';
    inputText: #9'.,;  aB2 '#9' ^&()  '#$A0#9;
    substitutionText: '-';
    expectedResult: #9'-  - '#9' -  '#$A0#9;
    matchStart: 0
    ),
    // 38
    ( // brackets just after [
    expression: '[[\w]+ []\w]+';
    inputText: '  ww[ww w]www';
    substitutionText: '';
    expectedResult: 'ww[ww w]www';
    matchStart: 3
    ),
    // 39
    ( // NULL in expression, negative \W \S \D in []
    expression: '([\x00\d]+ )+ [\W]+ [\S\x00-\x10]+ [\D]+';
    inputText: '  22'#0'33 '#0'33  .& w#'#5#0' w#';
    substitutionText: '';
    expectedResult: '22'#0'33 '#0'33  .& w#'#5#0' w#';
    matchStart: 3
    ),
    // 40
    ( // find 1+ simple chars
    expression: 'd+';
    inputText: '  ddddee  ';
    substitutionText: '';
    expectedResult: 'dddd';
    matchStart: 3
    ),
    // 41
    ( // find {N,M} spaces
    expression: ' {4,}';
    inputText: 'dd      dd';
    substitutionText: '';
    expectedResult: '      ';
    matchStart: 3
    ),
    // 42
    ( // valid regex set [.-]
    expression: '\w+([.-])\d+([.-])\w+([.-])\w+';
    inputText: 'Pictures-2018-Spain.Madrid';
    substitutionText: '$1 $2 $3';
    expectedResult: '- - .';
    matchStart: 0
    ),
    // 43
    ( // valid regex set combinaton if escaping
    expression: '\w+([.\-])\d+([\.-])\w+([\.\-])\w+';
    inputText: 'Pictures-2018.Spain-Madrid';
    substitutionText: '$1 $2 $3';
    expectedResult: '- . -';
    matchStart: 0
    ),
    // 44
    ( // valid regex set
    expression: '.*?([.-]Test[.-])';
    inputText: 'This.Is.A_Test_1234.Test.abc';
    substitutionText: '$1';
    expectedResult: '.Test.abc';
    matchStart: 0
    )
  );

procedure TTestRegexpr.IsFalse(AErrorMessage: string; AConditionToCheck: boolean);
begin
  IsTrue(AErrorMessage, not AConditionToCheck)
end;

procedure TTestRegexpr.IsTrue(AErrorMessage: string; AConditionToCheck: boolean);
begin
  {$IFDEF FPC}
  AssertTrue(AErrorMessage, AConditionToCheck);
  {$ELSE}
  CheckTrue(AConditionToCheck, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.IsNotNull(AErrorMessage: string; AObjectToCheck: TObject
  );
begin
  {$IFDEF FPC}
  AssertNotNull(AErrorMessage, AObjectToCheck);
  {$ELSE}
  CheckNotNull(AObjectToCheck, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.AreEqual(AErrorMessage: string; s1, s2: string);
begin
  {$IFDEF FPC}
  AssertEquals(AErrorMessage, s1,s2);
  {$ELSE}
  CheckEquals(s1,s2, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.AreEqual(AErrorMessage: string; i1, i2: integer);
begin
  {$IFDEF FPC}
  AssertEquals(AErrorMessage, i1,i2);
  {$ELSE}
  CheckEquals(i1,i2, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.TestBadRegex(const AErrorMessage, AExpression: string);
var
  ok: boolean;
begin
  try
    CompileRE(AExpression);
    ok := False;
  except
    ok := True;
  end;
  IsTrue(AErrorMessage, ok);
end;

procedure TTestRegexpr.TestEmpty;
begin
  CompileRE('1'); // just to create RE object
  IsFalse('UseOsLineEndOnReplace correctly set', RE.UseOsLineEndOnReplace);
end;

procedure TTestRegexpr.TestNotFound;
begin
  CompileRE('w{2,}');
  RE.InputString:= 'tst';
  IsFalse('Exec must give False', RE.Exec(1));
  AreEqual('MatchPos[0] must fail', -1, RE.MatchPos[0]);
  AreEqual('MatchLen[0] must fail', -1, RE.MatchLen[0]);
  AreEqual('SubExprCount must be -1', -1, RE.SubExprMatchCount);
end;

{$IFDEF OverMeth}
procedure TTestRegexpr.TestReplaceOverload;
var
  act: string;
begin
  CompileRE('A\r(\n)'); // just to print compiled re - it will be recompiled below
  act:=ReplaceRegExpr('A\r(\n)', 'a'#$d#$a, '\n', [rroModifierI, rroUseSubstitution]);
  AssertEquals('Replace failed', PrintableString(#$a), PrintableString(Act))
end;
{$ENDIF}

procedure TTestRegexpr.TestBads;
begin
  //TestBadRegex('No Error for bad braces', 'd{');
  //TestBadRegex('No Error for bad braces', 'd{22');
  //TestBadRegex('No Error for bad braces', 'd{}');
end;

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

procedure TTestRegexpr.RunTest35;
begin
  RunRETest(35);
end;

procedure TTestRegexpr.RunTest36;
begin
  RunRETest(36);
end;

procedure TTestRegexpr.RunTest37;
begin
  RunRETest(37);
end;

procedure TTestRegexpr.RunTest38;
begin
  RunRETest(38);
end;

procedure TTestRegexpr.RunTest39;
begin
  RunRETest(39);
end;

procedure TTestRegexpr.RunTest40;
begin
  RunRETest(40);
end;

procedure TTestRegexpr.RunTest41;
begin
  RunRETest(41);
end;

procedure TTestRegexpr.RunTest42;
begin
  RunRETest(42);
end;

procedure TTestRegexpr.RunTest43;
begin
  RunRETest(43);
end;

procedure TTestRegexpr.RunTest44;
begin
  RunRETest(44);
end;

procedure TTestRegexpr.TestGroups;
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

{$IFDEF Unicode}
procedure TTestRegexpr.TestUnicode1;
const
  sTest: string = 'пપ2 ϦϨ3';
var
  R: TRegExpr;
begin
  R:= TRegExpr.Create;
  try
    R.ModifierR:= True;
    R.Expression:= '\w+ \w+';
    R.InputString:= UTF8Decode(string(sTest));
    IsTrue('Unicode find failed', R.ExecPos(1));
    AreEqual('Unicode pos failed', 1, R.MatchPos[0]);
    AreEqual('Unicode len failed', 7, R.MatchLen[0]);
  finally
    FreeAndNil(R);
  end;
end;
{$ENDIF}

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

