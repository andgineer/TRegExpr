unit tests;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{ $DEFINE DUMPTESTS} //define this to dump results to console

{$IFDEF VER130} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D5
{$IFDEF VER140} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D6
{$IFDEF VER150} {$DEFINE D7} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D7
{$IFDEF D5} {$DEFINE OverMeth} {$ENDIF}
{$IFDEF FPC} {$DEFINE OverMeth} {$ENDIF}

{$DEFINE Unicode}

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

  { TTestableRegExpr }

  TTestableRegExpr = class(TRegExpr)
  private
    FTestLastError: integer;
    FTestErrorCatching: boolean;
  protected
    procedure Error(AErrorID: integer); override;
  public
    procedure TestStartErrorCatching;
    procedure TestEndErrorCatching;
    procedure TestClearError;
    property TestLastError: integer read FTestLastError;
  end;
  { TTestRegexpr }

  TTestRegexpr= class(TTestCase)
  private
    RE: TTestableRegExpr;
  protected
    procedure RunRETest(aIndex: Integer);
    procedure CompileRE(const AExpression: RegExprString);
    procedure IsNotNull(AErrorMessage: string; AObjectToCheck: TObject);
    procedure IsTrue(AErrorMessage: string; AConditionToCheck: boolean);
    procedure IsFalse(AErrorMessage: string; AConditionToCheck: boolean);
    procedure AreEqual(AErrorMessage: string; s1, s2: string); overload;
    procedure AreEqual(AErrorMessage: string; i1, i2: integer); overload;
    procedure TestBadRegex(const AErrorMessage: string; const AExpression: RegExprString; ExpErrorId: Integer = 0);
    // CheckMatches: returns error message
    procedure IsMatching(AErrorMessage: String; ARegEx, AInput: RegExprString; AExpectStartLenPairs: array of Integer);
    procedure IsNotMatching(AErrorMessage: String; ARegEx, AInput: RegExprString);
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestNotFound;
    procedure TestBads;
    procedure TestContinueAnchor;
    procedure TestRegMustExist;
    procedure TestAtomic;
    procedure TestLoop;
    procedure TestIsFixedLength;
    procedure TestAnchor;
    {$IFDEF OverMeth}
    procedure TestReplaceOverload;
    {$ENDIF}
    procedure RunTest1;
    procedure RunTest2;
    procedure RunTest3;
    procedure RunTest4;
    procedure RunTest5;
    procedure RunTest6;
    procedure RunTest7;
    procedure RunTest8;
    procedure RunTest9;
    procedure RunTest10;
    procedure RunTest11;
    procedure RunTest12;
    procedure RunTest13;
    procedure RunTest14;
    procedure RunTest15;
    procedure RunTest16;
    procedure RunTest17;
    procedure RunTest18;
    procedure RunTest19;
    procedure RunTest20;
    procedure RunTest21;
    procedure RunTest22;
    procedure RunTest23;
    procedure RunTest24;
    procedure RunTest25;
    procedure RunTest26;
    procedure RunTest27;
    procedure RunTest28;
    procedure RunTest29;
    procedure RunTest30;
    procedure RunTest31;
    procedure RunTest32;
    procedure RunTest33;
    procedure RunTest34;
    procedure RunTest35;
    procedure RunTest36;
    procedure RunTest37;
    procedure RunTest38;
    procedure RunTest39;
    procedure RunTest40;
    procedure RunTest41;
    procedure RunTest42;
    procedure RunTest43;
    procedure RunTest44;
    procedure RunTest45;
    procedure RunTest46;
    procedure RunTest47;
    procedure RunTest48;
    procedure RunTest49;
    procedure RunTest50;
    procedure TestGroups;
    {$IFDEF UnicodeRE}
    procedure RunTest51unicode;
    procedure RunTest52unicode;
    procedure RunTest70russian;
    {$ENDIF}
    procedure RunTest53;
    procedure RunTest54;
    procedure RunTest55;
    procedure RunTest56;
    procedure RunTest57;
    procedure RunTest58;
    procedure RunTest59;
    procedure RunTest60;
    procedure RunTest61;
    procedure RunTest62;
    procedure RunTest63;
    procedure RunTest64;
    procedure RunTest65;
    procedure RunTest66;
    procedure RunTest67;
    procedure RunTest68;
    procedure RunTest69;
    procedure RunTest70;
    procedure RunTest71;
    procedure RunTest72;
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

function PrintableString(const S: RegExprString): string;
var
  buf: string;
  ch: char;
  i: integer;
begin
  Result := '';
  buf := UTF8Encode(S);
  for i := 1 to Length(buf) do
  begin
    ch := buf[i];
    if Ord(ch) < 31 then
      Result := Result + '#' + IntToStr(Ord(ch))
    else
      Result := Result + ch;
  end;
end;


const
  testCases: array [1 .. 72] of TRegExTest = (
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
    substitutionText: '${1}';
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
    expression: '.*?\b(https?|ftp)\b://(?:\w+)\.(?:\w+)\.(\w\B\w\B\w)';
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
    ),
    // 45
    ( // comments and modifier-strings
    expression: '(?#zzz)(?i)aA(?#zz).*(?-i)aA(?#zzz)';
    inputText: '_a_aaaAAAaaaAAAaaa__';
    substitutionText: '';
    expectedResult: 'aaaAAAaaaA';
    matchStart: 4
    ),
    // 46
    ( // named groups
    expression: '(?P<quote>[''"])\w+(?P=quote).*(?:\w+).*(?P<q>")\w+(?P=q)';
    inputText: 'aa "bb? "ok" a ''b "ok" eeee';
    substitutionText: '';
    expectedResult: '"ok" a ''b "ok"';
    matchStart: 9
    ),
    // 47
    ( // lookbehind. it also has group refs \1 \2.
    expression: '(?<=foo)(=)(\w)\w+\2\1';
    inputText: '..=tat=..=tat=..foo=tabt=..';
    substitutionText: '';
    expectedResult: '=tabt=';
    matchStart: 20
    ),
    // 48
    ( // lookahead
    expression: '(=)\w+\1(?=bar)';
    inputText: '..=taat=..=tddt=bar..';
    substitutionText: '';
    expectedResult: '=tddt=';
    matchStart: 11
    ),
    // 49
    ( // lookahead+lookbehind
    expression: '(?<=[a-z]+)(\d+)[a-z]+\1(?=[a-z]+)';
    inputText: '..2tt2..foo23test23bar..';
    substitutionText: '';
    expectedResult: '23test23';
    matchStart: 12
    ),
    // 50
    ( // replace with named groups
    expression: '\s+(?P<aa>[f-h]+)\s+(?P<bb>[o-r]+)\s+';
    inputText: '<  fg   oppo  >';
    substitutionText: '{${bb},${aa}}';
    expectedResult: '<{oppo,fg}>';
    matchStart: 1
    ),
    // 51, unicode!
    (
    expression: '\pL \p{Lu}{3,} \PL+ \P{Lu}+';
    inputText: ',,wew ABDEF 345 weUPend';
    substitutionText: '';
    expectedResult: 'w ABDEF 345 we';
    matchStart: 5
    ),
    // 52, unicode!
    (
    expression: '[\p{Ll}\p{N}%]{5,} [\P{L}]+';
    inputText: ',,NOPE%400 @_ ok%200 @_end';
    substitutionText: '';
    expectedResult: 'ok%200 @_';
    matchStart: 15
    ),
    // 53, lookahead aa(?!bb)
    (
    expression: 'a+(?!\w)';
    inputText: 'aabaaddaaazaaa=aau';
    substitutionText: '';
    expectedResult: 'aaa';
    matchStart: 12
    ),
    // 54, lookahead aa(?!bb)
    (
    expression: '(?:\s+)\w{2,}\.(?!com|org|net)';
    inputText: '  www.com  www.org  www.ok  www.net';
    substitutionText: '';
    expectedResult: '  www.';
    matchStart: 19
    ),
    // 55, atomic groups
    (
    expression: 'a(?>bc|b)c';
    inputText: ' abc abcc abc abcc ';
    substitutionText: '_';
    expectedResult: ' abc _ abc _ ';
    matchStart: 1
    ),
    // 56, a++
    (
    expression: '\d++e\d++';
    inputText: ' 20ed2 100e20 2e34 ';
    substitutionText: '_';
    expectedResult: ' 20ed2 _ _ ';
    matchStart: 1
    ),
    // 57, a*+, must fail
    (
    expression: '".*+"';
    inputText: 'dd "abc" ee';
    substitutionText: '';
    expectedResult: '';
    matchStart: -1
    ),
    // 58, recursion
    (
    expression: 'a(?R)?b';
    inputText: '__aaaabbbbbbbb__';
    substitutionText: '';
    expectedResult: 'aaaabbbb';
    matchStart: 3
    ),
    // 59, recursion, generic regex 1 - https://regular-expressions.mobi/recurse.html?wlr=1
    (
    expression: 'b(?:m|(?R))*e';
    inputText: '_bbfee_bbbmeee__';
    substitutionText: '';
    expectedResult: 'bbbmeee';
    matchStart: 8
    ),
    // 60, recursion, generic regex 2 - https://regular-expressions.mobi/recurse.html?wlr=1
    (
    expression: 'b(?R)*e|m';
    inputText: '__bbbmeee__bme__m__';
    substitutionText: '@';
    expectedResult: '__@__@__@__';
    matchStart: 1
    ),
    // 61, recursion, balanced set of parentheses - https://regular-expressions.mobi/recurse.html?wlr=1
    (
    expression: '\((?>[^()]|(?0))*\)';
    inputText: '__(((dd)dd))__(dd)__(((dd)f)f)__';
    substitutionText: '@';
    expectedResult: '__@__@__@__';
    matchStart: 1
    ),
    // 62, subroutine call (?3) + non-capturing groups + atomic group
    (
    expression: '(rr)(qq)(?:t)(?:t)(\[(?>m|(?3))*\])';
    inputText: '__rrqqtt[[[mmm]mm]m]m]m]m]m]__';
    substitutionText: '';
    expectedResult: 'rrqqtt[[[mmm]mm]m]';
    matchStart: 3
    ),
    // 63, subroutine call (?P>name)
    (
    expression: '(?P<name>[abc])(?1)(?P>name)';
    inputText: '__bcabcadef__';
    substitutionText: '';
    expectedResult: 'bca';
    matchStart: 3
    ),
    // 64
    ( // named groups with Perl syntax
    expression: '(?''quote''[''"])\w+(?&quote).*(?:\w+).*(?''q''")\w+(?&q)';
    inputText: 'aa "bb? "ok" a ''b "ok" eeee';
    substitutionText: '';
    expectedResult: '"ok" a ''b "ok"';
    matchStart: 9
    ),
    // 65
    ( // \A and \z
    expression: '(?s)\A.+\z';
    inputText: 'some'#10'text'#10;
    substitutionText: '-';
    expectedResult: '-';
    matchStart: 1
    ),
    // 66
    ( // \A and \Z
    expression: '(?s)\A.+\w\Z';
    inputText: 'some'#13#10'text'#13#10;
    substitutionText: '-';
    expectedResult: '-'#13#10;
    matchStart: 1
    ),
    // 67
    ( // (?<!foo)bar
    expression: '(?<!foo)bar';
    inputText: 'foobar foobar zzbar';
    substitutionText: '';
    expectedResult: 'bar';
    matchStart: 17
    ),
    // 68
    ( // (?<!foo)bar
    expression: '(?<![a-o]\d)bar';
    inputText: 'a2bar o3bar __bar';
    substitutionText: '';
    expectedResult: 'bar';
    matchStart: 15
    ),
    // 69
    ( // empty str
    expression: '^ *$';
    inputText: '';
    substitutionText: '';
    expectedResult: '';
    matchStart: 1
    ),
    // 70
    (
    expression: '(b)';
    inputText: 'abc';
    substitutionText: '$1$2$9';
    expectedResult: 'abc';
    matchStart: 1
    ),
    // 71
    ( // sub-routine
    expression: '(\w+)_(?1)';
    inputText: '==abc_de==';
    substitutionText: '$1';
    expectedResult: '==abc==';
    matchStart: 3
    ),
    // 72
    ( // recursion with back-reference
    expression: '(\w+)(?R)?\1';
    inputText: '=abcba=12344321=';
    substitutionText: '';
    expectedResult: '12344321';
    matchStart: 8
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

procedure TTestRegexpr.TestBadRegex(const AErrorMessage: string;
  const AExpression: RegExprString; ExpErrorId: Integer);
begin
  try
    RE.TestStartErrorCatching;
    CompileRE(AExpression);
    if ExpErrorId <> 0 then
      AreEqual(AErrorMessage, ExpErrorId, RE.TestLastError)
    else
      IsTrue(AErrorMessage, RE.TestLastError <> 0);
  finally
    RE.TestEndErrorCatching;
  end;
end;

procedure TTestRegexpr.IsMatching(AErrorMessage: String; ARegEx,
  AInput: RegExprString; AExpectStartLenPairs: array of Integer);
var
  i: Integer;
  L: SizeInt;
begin
  CompileRE(ARegEx);
  RE.InputString:= AInput;

  IsTrue(AErrorMessage + ' Exec must give True', RE.Exec);

  L := Length(AExpectStartLenPairs) div 2;
  AreEqual(AErrorMessage + ': MatchCount', L - 1, RE.SubExprMatchCount);
  for i := 0 to L - 1 do begin
    AreEqual(AErrorMessage + ': MatchPos['+inttostr(i)+']', AExpectStartLenPairs[i*2], RE.MatchPos[i]);
    AreEqual(AErrorMessage + ': MatchLen['+inttostr(i)+']', AExpectStartLenPairs[i*2+1], RE.MatchLen[i]);
  end;
end;

procedure TTestRegexpr.IsNotMatching(AErrorMessage: String; ARegEx,
  AInput: RegExprString);
begin
  CompileRE(ARegEx);
  RE.InputString:= AInput;

  IsFalse(AErrorMessage + ': Exec must give False', RE.Exec);
end;

procedure TTestRegexpr.SetUp;
begin
  inherited SetUp;
  if (RE = Nil) then
  begin
    RE := TTestableRegExpr.Create;
    RE.ReplaceLineEnd := #10;
  end;
end;

procedure TTestRegexpr.TearDown;
begin
  inherited TearDown;
  FreeAndNil(RE);
end;

procedure TTestRegexpr.TestEmpty;
begin
  CompileRE('1'); // just to create RE object
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
  TestBadRegex('Error for matching zero width {}', '(a{0,2})*', 115);
  TestBadRegex('No Error for bad braces', 'd{');
  TestBadRegex('No Error for bad braces', 'd{22');
  TestBadRegex('No Error for bad braces', 'd{}');
end;

procedure TTestRegexpr.TestContinueAnchor;
  procedure AssertMatch(AName: String; AStart, ALen: Integer);
  begin
    AreEqual(AName + 'MatchCount', 1, RE.SubExprMatchCount);
    AreEqual(AName + 'MatchPos[1]', AStart, RE.MatchPos[1]);
    AreEqual(AName + 'MatchLen[1]', ALen, RE.MatchLen[1]);
  end;
begin
  // Without \G MatchNext will skip
  CompileRE('(A)');
  RE.InputString:= 'AABA';

  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('"A" match 1 at 1', 1, 1);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('"A" match 2 at 2', 2, 1);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('"A" match 3 at 4', 4, 1);


  // With \G MatchNext will fail instead of skip
  CompileRE('\G(A)');
  RE.InputString:= 'AABA';

  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('"A" match 1 at 1', 1, 1);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('"A" match 2 at 2', 2, 1);
  IsFalse('Exec must give False "\G(A)"', RE.ExecNext);


  // Without \G  chars will be matched before the capture
  CompileRE('[^A]*([^A]*?A)');
  RE.InputString:= '123A345A67890A--';

  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('Zero-len * - match 1 at 4', 4, 1);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('Zero-len * - match 2 at 8', 8, 1);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('Zero-len * - match 3 at 14', 14, 1);

  IsTrue('Exec must give True', RE.Exec(2));
  AssertMatch('Zero-len * - Exec(2) at 4', 4, 1);

  // Without \G  chars will be matched in the capture
  CompileRE('[^A]*(\G[^A]*?A)');
  RE.InputString:= '123A345A67890A--';

  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('\G match * - match 1 at 1', 1, 4);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('\G match * - match 2 at 5', 5, 4);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('\G match * - match 3 at 9', 9, 6);

  IsTrue('Exec must give True', RE.Exec(2));
  AssertMatch('\C Zero-len * - Exec(2) at 2', 2, 3);

  // Without \G  chars will be matched in the capture
  CompileRE('[^A]*\G([^A]*?A)');
  RE.InputString:= '123A345A67890A--';

  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('\G match * - match 1 at 1', 1, 4);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('\G match * - match 2 at 5', 5, 4);
  IsTrue('Exec must give True', RE.ExecNext);
  AssertMatch('\G match * - match 3 at 9', 9, 6);

  IsTrue('Exec must give True', RE.Exec(2));
  AssertMatch('\C Zero-len * - Exec(2) at 2', 2, 3);


  CompileRE('(A|B)');
  RE.InputString:= 'xBxA';
  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('(A|B)  xBxA', 2, 1);

  CompileRE('(A|\GB)');
  RE.InputString:= 'xBxA';
  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('(A|\GB)  xBxA', 4, 1);

  CompileRE('(A|\GB)');
  RE.InputString:= 'xBxA';
  IsTrue('Exec must give True', RE.Exec(2));
  AssertMatch('(A|\GB)  xBxA offset 2', 2, 1);

  // in look behind
  CompileRE('(?<=\GA.*)(X)');
  RE.InputString:= '123X3';
  IsFalse('Exec must give True', RE.Exec);
  // in look behind
  CompileRE('(?<=\GA.*)(X)');
  RE.InputString:= 'A123X3';
  IsTrue('Exec must give True', RE.Exec);
  AssertMatch('(?<=\GA.*)(X)  A123X3', 5, 1);

  // in look behind
  CompileRE('(?<=\GA.*)(X)');
  RE.InputString:= 'A123X3';
  IsFalse('Exec must give False', RE.Exec(2));

  // in look behind
  CompileRE('(?<=\GA.*)(X)');
  RE.InputString:= '_A123X3';
  IsTrue('Exec must give True', RE.Exec(2));
  AssertMatch('(?<=\GA.*)(X)  _A123X3 offset 2 ', 6, 1);

//  CompileRE('(?<=^.\GA...)(X)');
//  CompileRE('(?<=^.\GA...)(X)');
//  RE.InputString:= '_A123X3';
//  IsTrue('Exec must give True', RE.Exec(2));
//  AssertMatch('(?<=^.\GA...)(X)  _A123X3 offset 2 ', 6, 1);
end;

procedure TTestRegexpr.TestRegMustExist;
begin
  CompileRE('\w*abcd');
  RE.InputString:= StringOfChar('.', 3000) + 'abcd';
  RE.SlowChecksSizeMax := 5000;
  IsTrue('Exec must give True', RE.Exec);
end;

procedure TTestRegexpr.TestAtomic;
begin
  IsNotMatching('Match is not changed, if pattern fails after atomic',
                'a(?>b.*?c|.)x..8', '1ab__cx__9cx__8...56Yb');
  IsMatching('Atomic backtrace until match is found ',
             'a(?>b.*?cx..8|.)',
             '1ab__cx__9cx__8_...56Yb', [2,14]);

  IsMatching('Bactrace to before start of atomic',
             '(.)(?>Y|X)a', '1234Xa...56Yb', [4,3,  4,1]);
  IsMatching('Bactrace to before start of atomic - atomic on 2nd pos',
             '(.)(?>X|Y)b',
             '1234Xa...56Yb', [11,3,  11,1]);
  IsMatching('Bactrace to before start of atomic - atomic on 2nd pos',
             '(.)(?>Y|X)b',
             '1234Xa...56Yb', [11,3,  11,1]);

  IsMatching('Bactrace to before start of atomic - 2nd pos - forget capture in 1st pos',
             '(.)(?>(X)|Y)b',
             '1234Xa...56Yb', [11,3,  11,1, -1,-1]);
  IsMatching('Bactrace to before start of atomic - 2nd pos - forget capture in 1st pos',
             '(.)(?>Y|(X))b',
             '1234Xa...56Yb', [11,3,  11,1, -1,-1]);

  // Nested
  IsNotMatching('NESTED: Match is not changed, if pattern fails after atomic',
                'a(?>(b.*?c)|.)x..8', '1ab__cx__9cx__8...56Yb');
  IsMatching('NESTED: Atomic backtrace until match is found ',
             'a(?>((?>b.*?cx..8))|.)',
             '1ab__cx__9cx__8_...56Yb', [2,14,  3,13]);
  // The outer atomic must not backtrace, the inner group must not undo the flag for the outer
  IsNotMatching('NESTED: Atomic backtrace inner and outer',
             'a(?>((?>b.*?cx..8))|.)_',
             '1ab__cx__9cx__8...56Yb');
  IsMatching('NESTED: Atomic backtrace inner, outer can still try and find ',
             'a(?>((?>b.*?cx..8|.)_)|.)',
             '1ab__cx__9cx__8...56Yb',  [2,2,  -1,-1]);

  IsNotMatching('NESTED 2: Match is not changed, if pattern fails after atomic',
                'a(?>(b.*?c)|.)x..8', '1ab__cx__9cx__8...56Yb');
  IsMatching('NESTED 2: Atomic backtrace until match is found ',
             'a(?>((?>b.*?cx..8))|.)',
             '1ab__cx__9cx__8_...56Yb', [2,14,  3,13]);
end;

procedure TTestRegexpr.TestLoop;
begin
  // The patterns below **should** use OP_LOOP[ng]

  (* 'Aa(x|y(x|y(x|y){3,4}){3,4}){3,4}',
      The most inner () matches minimum  3 y each run
      The 2nd inner ()  matches minimum  3 times 1 y + 3y   = 12 y
      The outer ()      matches minimum  3 times 1 y + 12 y = 3*13 = 39 y
  *)

  IsMatching('nested {} greedy ',
             'Aa(x|y(x|y){3,4}){3,4}',
             'Aayyyyyyyyyyyy',  [1,14,   11,4,  14,1]); // minimum y

  IsMatching('nested {} greedy ',
             'Aa(x|y(x|y){3,4}){3,4}',
             'Aayyyyyyyyyyyyy',  [1,15,   12,4,  15,1]); // one extra y

  IsMatching('nested {} greedy ',
             'Aa(x|y(x|y){3,4}){3,4}',
             'Aayyyyyyyyyyyyyy',  [1,16,   13,4,  16,1]); // two extra y

  IsNotMatching('nested {} greedy ',
             'Aa(x|y(x|y){3,4}){3,4}',
             'Aayyyyyyyyyyy'  );




  IsMatching('nested {} greedy ',
             'Aa(x|y(x|y(x|y){3,4}){3,4}){3,4}',
             'Aayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy',  [1,41,   29,13,  38,4,  41,1]); // 39 y

  IsMatching('nested {} greedy ',
             'Aa(x|y(x|y(x|y){3,4}){3,4}){3,4}',
             'Aayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy',  [1,42,   30,13,  39,4,  42,1]); // 40 y

  IsNotMatching('nested {} greedy ',
             'Aa(x|y(x|y(x|y){3,4}){3,4}){3,4}',
             'Aayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy');  // 38 y


  IsMatching('nested {} no greedy ',
             'Aa(x|y(x|y(x|y){3,4}?){3,4}?){3,4}?',
             'Aayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy',  [1,41,   29,13,  38,4,  41,1]); // 39 y

  IsMatching('nested {} no greedy ',
             'Aa(x|y(x|y(x|y){3,4}?){3,4}?){3,4}?',
             'Aayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy',  [1,41,   29,13,  38,4,  41,1]); // 40 y

  IsNotMatching('nested {} no greedy ',
             'Aa(x|y(x|y(x|y){3,4}?){3,4}?){3,4}?',
             'Aayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy');  // 38 y



  IsMatching('nested branch *? ',
             'A(?>(?:b|c(?:d|e)*?)*?_)a',
             'Acece_x_Acece_a_',  [9,7]);

  IsMatching('nested branch *? ',
             'A(?>(?:b|c(?:d|e(?:f|g)*?)*?)*?_)a',
             'Acece_x_Acecegg_a_',  [9,9]);

  IsMatching('nested branch *? ',  // uses OP_BACK
             'A((ce+?)c?|ce)*a',
             'Aceecea_',  [1,7,   5,2,  5,2]);

  IsMatching('nested branch *? ',
             'A(?>(?:b|c(?:d|e(?:f|g){0,2}?){1,1}?){2,2}?_)a',
             'Acece_x_Acecegg_a_',  [9,9]);

  IsMatching('nested branch *? ',
             'A(?>(?:b|c(?:d|e(?:f|g){0,2}?){1,1}?){2,2}?_)a(x|y(x|y(x|y){3,4}){3,4}){3,4}',
             'Acece_x_Acecegg_ayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy',
             [9,93,   81,21,  97,5,  101, 1]);

  IsNotMatching('nested branch *? ',
             'A(?>(?:b|c(?:d|e(?:f|g){0,2}?){1,1}?){2,2}?_)a(x|y(x|y(x|y){3,4}){3,4}){3,4}',
             'Acece_x_Acecegg_ayyyyyyyyyyyyyyyyyyyyyyyyy'
             );


  IsMatching('atomic nested {} no greedy / no branches',
             '(?:(?>Aa(y){3,4}?)|.*)B',
             'AayyyyBB',   [1,8,   -1,-1] ); // 40 y
  IsMatching('NOT atomic nested {} no greedy / no branches', // ensure non-atomic has different result
             '(?:(?:Aa(y){3,4}?)|.*)B',
             'AayyyyBB',   [1,7,   6,1] ); // 40 y

  IsMatching('atomic nested {} no greedy ',
             '(?:(?>Aa(x|y(x|y(x|y){3,4}?){3,4}?){3,4}?)|.*)B',
             'AayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyBB',   [1,44,   -1,-1, -1,-1, -1,-1] ); // 40 y


end;

procedure TTestRegexpr.TestIsFixedLength;

  procedure HasLength(AErrorMessage: String; ARegEx: RegExprString; ExpLen: Integer);
  var
    r: Boolean;
    op: TREOp;
    ALen: integer;
  begin
    CompileRE(ARegEx);
    r := RE.IsFixedLength(op, ALen);
    if ExpLen < 0 then begin
      IsFalse(AErrorMessage, r);
    end
    else begin
      IsTrue(AErrorMessage, r);
      AreEqual(AErrorMessage, ExpLen, ALen);
    end;
  end;

begin
  HasLength('bound', '^',      0);
  HasLength('bound', '$',      0);
  HasLength('bound', '\G',     0);
  HasLength('bound', '\b',     0);
  HasLength('{0}', 'a{0}',     0);
  HasLength('{0,0}', 'a{0,0}', 0);

  HasLength('A', 'A', 1);
  HasLength('branch', 'A|B',     1);
  HasLength('branch', 'A|B|C',   1);
  HasLength('branch', 'AA|B',   -1);
  HasLength('branch', 'A|BB',   -1);
  HasLength('branch', 'A|^',    -1);
  HasLength('branch', '$|B',    -1);
  HasLength('branch', 'AA|B|C', -1);
  HasLength('branch', 'A|BB|C', -1);
  HasLength('branch', 'A|B|CC', -1);
  HasLength('branch', '\b|B|C', -1);
  HasLength('branch', 'A|\b|C', -1);
  HasLength('branch', 'A|B|\b', -1);

  HasLength('branch ()', '(A)',       1);
  HasLength('branch ()', '(A|B)',     1);
  HasLength('branch ()', '(A|B|C)',   1);
  HasLength('branch ()', '(A|BB)',   -1);
  HasLength('branch ()', '(AA|B)',   -1);
  HasLength('branch ()', 'A|^',      -1);
  HasLength('branch ()', '$|B',      -1);
  HasLength('branch ()', '(AA|B|C)', -1);
  HasLength('branch ()', '(A|BB|C)', -1);
  HasLength('branch ()', '(A|B|CC)', -1);
  HasLength('branch ()', '(\b|B|C)', -1);
  HasLength('branch ()', '(A|\b|C)', -1);
  HasLength('branch ()', '(A|B|\b)', -1);

  HasLength('branch () mixed', 'x(A|B)',     2);
  HasLength('branch () mixed', 'x(A|B|C)',   2);
  HasLength('branch () mixed', 'x(A|BB)',   -1);
  HasLength('branch () mixed', 'x(AA|B|C)', -1);
  HasLength('branch () mixed', 'x(A|\b|C)', -1);

  HasLength('branch () mixed', 'xx(A|B)',     3);
  HasLength('branch () mixed', 'xx(A|B|C)',   3);
  HasLength('branch () mixed', 'xx(A|BB)',   -1);
  HasLength('branch () mixed', 'xx(AA|B|C)', -1);
  HasLength('branch () mixed', 'xx(A|\b|C)', -1);

  HasLength('branch () mixed', '\b(A|B)',     1);
  HasLength('branch () mixed', '\b(A|B|C)',   1);
  HasLength('branch () mixed', '\b(A|BB)',   -1);
  HasLength('branch () mixed', '\b(AA|B|C)', -1);
  HasLength('branch () mixed', '\b(A|\b|C)', -1);

  HasLength('branch () mixed', '(A|B)xxx',     4);
  HasLength('branch () mixed', '(A|B|C)xxx',   4);
  HasLength('branch () mixed', '(A|BB)xxx',   -1);
  HasLength('branch () mixed', '(AA|B|C)xxx', -1);
  HasLength('branch () mixed', '(A|\b|C)xxx', -1);

  HasLength('branch () twice', '(A|B)(D|E)',     2);
  HasLength('branch () twice', '(A|B|C)(D|E)',   2);
  HasLength('branch () twice', '(A|BB)(D|E)',   -1);
  HasLength('branch () twice', '(A|B)(DD|E)',   -1);
  HasLength('branch () twice', '(AA|B|C)(D|E)', -1);
  HasLength('branch () twice', '(A|B|C)(D|EE)', -1);
  HasLength('branch () twice', '(A|\b|C)(D|E)', -1);
  HasLength('branch () twice', '(A|B|C)(D|^)',  -1);

  HasLength('branch () twice |', '(A|B)|(D|E)',     1);
  HasLength('branch () twice |', '(A|B|C)|(D|E)',   1);
  HasLength('branch () twice |', '(A|BB)|(D|E)',   -1);
  HasLength('branch () twice |', '(A|B)|(DD|E)',   -1);
  HasLength('branch () twice |', '(AA|B|C)|(D|E)', -1);
  HasLength('branch () twice |', '(A|B|C)|(D|EE)', -1);
  HasLength('branch () twice |', '(A|\b|C)|(D|E)', -1);
  HasLength('branch () twice |', '(A|B|C)|(D|^)',  -1);

  HasLength('branch () nested', '(A(x)|B(D|E))',     2);
  HasLength('branch () nested', '(A(x)|Bx|C(D|E))',   2);
  HasLength('branch () nested', '(A(x)|BB(D|E))',   -1);
  HasLength('branch () nested', '(A(x)|B(DD|E))',   -1);

  HasLength('branch () some zero len', 'x(A|B\b|Cx{0})',   2);

end;

procedure TTestRegexpr.TestAnchor;

  procedure HasAnchor(AErrorMessage: String; ARegEx: RegExprString; ExpAnchor: TRegExAnchor);
  var
    d: RegExprString;
  begin
    CompileRE(ARegEx);
    d := RE.Dump;
    case ExpAnchor of
      raNone:     IsTrue(AErrorMessage, pos('Anchored', d) < 1);
      raBOL:      IsTrue(AErrorMessage, pos('Anchored(BOL)', d) > 0);
      raEOL:      IsTrue(AErrorMessage, pos('Anchored(EOL)', d) > 0);
      raContinue: IsTrue(AErrorMessage, pos('Anchored(\G)', d) > 0);
      raOnlyOnce: IsTrue(AErrorMessage, pos('Anchored(start)', d) > 0);
    end;
  end;

const
  TestOnlyOnceData: array [1..6] of RegExprString = (
    '.{0,}+', '.{0,}?', '.{0,}', '.*+', '.*', '.*?'
  );
  TestNotOnlyOnceData: array [1..9] of RegExprString = (
    '.{1,}+', '.{1,}?', '.{1,}', '.{0,2}+', '.{0,2}?', '.{0,2}',
    '.+', '.++', '.?'
  );
var
  i: Integer;
  s: RegExprString;
begin
  HasAnchor('', 'abc', raNone);
  HasAnchor('', '.a', raNone);
  HasAnchor('', '(a)', raNone);
  HasAnchor('', '(.a)', raNone);
  HasAnchor('', 'a*', raNone);
  HasAnchor('', '.a*', raNone);
  HasAnchor('', '(a)*', raNone);
  HasAnchor('', '(.a)*', raNone);

  HasAnchor('', '^', raBOL);
  HasAnchor('', '^a', raBOL);
  HasAnchor('', '^a|a', raNone);
  HasAnchor('', '^|a', raNone);
  HasAnchor('', '^?a', raNone);
  HasAnchor('', '^?|a', raNone);
  HasAnchor('', '(^?|a)', raNone);

  HasAnchor('', '$', raEOL);
  HasAnchor('', '$a', raEOL);
  HasAnchor('', '$?', raNone);
  HasAnchor('', '$|a', raNone);
  HasAnchor('', '($)?', raNone);
  HasAnchor('', '($|a)?', raNone);

  HasAnchor('', '\G', raContinue);
  HasAnchor('', '\Ga', raContinue);
  HasAnchor('', '\Ga|a', raNone);
  HasAnchor('', '\G|a', raNone);
  HasAnchor('', '\G?a', raNone);
  HasAnchor('', '\G?|a', raNone);
  HasAnchor('', '(\G?|a)', raNone);

  for i := low(TestOnlyOnceData) to high(TestOnlyOnceData) do begin
    s := TestOnlyOnceData[i];
    HasAnchor('', s, raOnlyOnce);
    HasAnchor('', s+'a', raOnlyOnce);
    HasAnchor('', s+'|a', raNone);
    HasAnchor('', '('+s+'|a)', raNone);
  end;

  for i := low(TestNotOnlyOnceData) to high(TestNotOnlyOnceData) do begin
    s := TestNotOnlyOnceData[i];
    HasAnchor('', s, raNone);
    HasAnchor('', s+'a', raNone);
    HasAnchor('', s+'|a', raNone);
    HasAnchor('', '('+s+'|a)', raNone);
  end;


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

procedure TTestRegexpr.RunTest45;
begin
  RunRETest(45);
end;

procedure TTestRegexpr.RunTest46;
begin
  RunRETest(46);
end;

procedure TTestRegexpr.RunTest47;
begin
  RunRETest(47);
end;

procedure TTestRegexpr.RunTest48;
begin
  RunRETest(48);
end;

procedure TTestRegexpr.RunTest49;
begin
  RunRETest(49);
end;

procedure TTestRegexpr.RunTest50;
begin
  RunRETest(50);
end;

{$IFDEF UnicodeRE}
procedure TTestRegexpr.RunTest51unicode;
begin
  RunRETest(51);
end;

procedure TTestRegexpr.RunTest52unicode;
begin
  RunRETest(52);
end;

procedure TTestRegexpr.RunTest70russian;
//Alexey: if I add Russian test directly to array of tests,
//I have problems with UTF8 coding then, which I cannot solve in this test
var
  T: TRegExTest;
begin
  T.Expression:= UTF8Decode('[а-я]+');
  T.InputText:= UTF8Decode('12морошка');
  T.ExpectedResult:= UTF8Decode('морошка');
  T.MatchStart:= 3;
  T.SubstitutionText:= '';
  CompileRE(T.Expression);
  RE.Exec(T.inputText);
  AreEqual('Search position', T.MatchStart, RE.MatchPos[0]);
  AreEqual('Matched text', PrintableString(T.ExpectedResult), PrintableString(RE.Match[0]));
end;
{$ENDIF}

procedure TTestRegexpr.RunTest53;
begin
  RunRETest(53);
end;

procedure TTestRegexpr.RunTest54;
begin
  RunRETest(54);
end;

procedure TTestRegexpr.RunTest55;
begin
  RunRETest(55);
end;

procedure TTestRegexpr.RunTest56;
begin
  RunRETest(56);
end;

procedure TTestRegexpr.RunTest57;
begin
  RunRETest(57);
end;

procedure TTestRegexpr.RunTest58;
begin
  RunRETest(58);
end;

procedure TTestRegexpr.RunTest59;
begin
  RunRETest(59);
end;

procedure TTestRegexpr.RunTest60;
begin
  RunRETest(60);
end;

procedure TTestRegexpr.RunTest61;
begin
  RunRETest(61);
end;

procedure TTestRegexpr.RunTest62;
begin
  RunRETest(62);
end;

procedure TTestRegexpr.RunTest63;
begin
  RunRETest(63);
end;

procedure TTestRegexpr.RunTest64;
begin
  RunRETest(64);
end;

procedure TTestRegexpr.RunTest65;
begin
  RunRETest(65);
end;

procedure TTestRegexpr.RunTest66;
begin
  RunRETest(66);
end;

procedure TTestRegexpr.RunTest67;
begin
  RunRETest(67);
end;

procedure TTestRegexpr.RunTest68;
begin
  RunRETest(68);
end;

procedure TTestRegexpr.RunTest69;
begin
  RunRETest(69);
end;

procedure TTestRegexpr.RunTest70;
begin
  RunRETest(70);
end;

procedure TTestRegexpr.RunTest71;
begin
  RunRETest(71);
end;

procedure TTestRegexpr.RunTest72;
begin
  RunRETest(72);
end;

procedure TTestRegexpr.TestGroups;
var
  R: TTestableRegExpr;
begin
  R:= TTestableRegExpr.Create;
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

procedure TTestRegexpr.CompileRE(const AExpression: RegExprString);
begin
  if (RE = Nil) then
  begin
    RE := TTestableRegExpr.Create;
    RE.ReplaceLineEnd := #10;
  end;
  RE.Expression := AExpression;
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
  S: RegExprString;

  procedure DoMatchAssertions;
  begin
    AreEqual('Search position', T.MatchStart, RE.MatchPos[0]);
    AreEqual('Matched text', PrintableString(T.ExpectedResult), PrintableString(RE.Match[0]));
  end;

begin
  T:= testCases[aIndex];
{$IFDEF DUMPTESTS}
  Writeln('Test: ',TestName);
{$ENDIF}
  CompileRE(T.Expression);
  if T.SubstitutionText<>'' then
  begin
    S:= RE.Replace(T.InputText, T.SubstitutionText, True);
    AreEqual('Replace failed', PrintableString(T.ExpectedResult), PrintableString(S))
  end
  else
  begin
    RE.Exec(T.inputText);
    DoMatchAssertions;

    // Test via InputString
    RE.InputString := T.InputText;
    RE.Exec;
    DoMatchAssertions;

    // Test via SetInputSubString
    RE.SetInputSubString('abc' + T.InputText + '12345', 4, Length(t.InputText));
    RE.Exec;
    DoMatchAssertions;
  end;
end;

procedure TTestableRegExpr.Error(AErrorID: integer);
begin
  if FTestErrorCatching then
    FTestLastError := AErrorID
  else
    inherited Error(AErrorID);
end;

procedure TTestableRegExpr.TestStartErrorCatching;
begin
  FTestErrorCatching := True;
  TestClearError;
end;

procedure TTestableRegExpr.TestEndErrorCatching;
begin
  FTestErrorCatching := False;
  TestClearError;
end;

procedure TTestableRegExpr.TestClearError;
begin
  FTestLastError := 0;
end;

initialization

{$IFDEF FPC}
  RegisterTest(TTestRegexpr);
{$ENDIF}
end.

