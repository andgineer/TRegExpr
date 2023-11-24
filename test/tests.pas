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

{$DEFINE UnicodeRE}

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
    FErrorInfo: String;
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
    procedure IsMatching(AErrorMessage: String; ARegEx, AInput: RegExprString;
      AExpectStartLenPairs: array of Integer; AOffset: integer = 1; AMustMatchBefore: integer = 0);
    procedure IsNotMatching(AErrorMessage: String; ARegEx, AInput: RegExprString; AOffset: integer = 1; AMustMatchBefore: integer = 0);
    procedure IsErrorOnMatch(AErrorMessage: String; ARegEx, AInput: RegExprString; AOffset: integer = 1; AMustMatchBefore: integer = 0; ExpErrorId: Integer = 0);
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestNotFound;
    procedure TestBads;
    procedure TestModifiers;
    procedure TestContinueAnchor;
    procedure TestRegMustExist;
    procedure TestAtomic;
    procedure TestQuesitonMark;
    procedure TestBraces;
    procedure TestLoop;
    procedure TestEmptyLoop;
    procedure TestBranches; // Also checks "FillFirstChar"
    procedure TestReferences;
    procedure TestSubCall;
    procedure TestNamedGroups;
    procedure TestCaptures;
    procedure TestRecurseAndCaptures;
    procedure TestIsFixedLength;
    procedure TestMatchBefore;
    procedure TestAnchor;
    procedure TestRegLookAhead;
    procedure TestRegLookBehind;
    procedure TestRegLookAroundMixed;
    procedure TestResetMatchPos;
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
    procedure RunTest73;
    procedure RunTest74;
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
  testCases: array [1 .. 74] of TRegExTest = (
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
    ),
    // 73
    (
    expression: '\R';
    inputText: '<'#10'-'#13'-'#13#10'-'#10#13';'#$0B'-'#$0C'-'#$85'>'
      {$IFDEF UnicodeRE} + '<'#$2028#$2029'>' {$ENDIF};
    substitutionText: 'R';
    expectedResult: '<R-R-R-RR;R-R-R>'
      {$IFDEF UnicodeRE} + '<RR>' {$ENDIF};
    MatchStart: 0
    ),
    // 74
    (
    expression: '[\R]+';
    inputText: '<'#10#13#13#10#10#$0B#$0C#$85'>';
    substitutionText: 'many';
    expectedResult: '<many>';
    MatchStart: 0
    )
  );

procedure TTestRegexpr.IsFalse(AErrorMessage: string; AConditionToCheck: boolean);
begin
  IsTrue(AErrorMessage+FErrorInfo, not AConditionToCheck)
end;

procedure TTestRegexpr.IsTrue(AErrorMessage: string; AConditionToCheck: boolean);
begin
  {$IFDEF FPC}
  AssertTrue(AErrorMessage+FErrorInfo, AConditionToCheck);
  {$ELSE}
  CheckTrue(AConditionToCheck, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.IsNotNull(AErrorMessage: string; AObjectToCheck: TObject
  );
begin
  {$IFDEF FPC}
  AssertNotNull(AErrorMessage+FErrorInfo, AObjectToCheck);
  {$ELSE}
  CheckNotNull(AObjectToCheck, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.AreEqual(AErrorMessage: string; s1, s2: string);
begin
  {$IFDEF FPC}
  AssertEquals(AErrorMessage+FErrorInfo, s1,s2);
  {$ELSE}
  CheckEquals(s1,s2, AErrorMessage)
  {$ENDIF}
end;

procedure TTestRegexpr.AreEqual(AErrorMessage: string; i1, i2: integer);
begin
  {$IFDEF FPC}
  AssertEquals(AErrorMessage+FErrorInfo, i1,i2);
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
      AreEqual(AErrorMessage+FErrorInfo, ExpErrorId, RE.TestLastError)
    else
      IsTrue(AErrorMessage, RE.TestLastError <> 0);
  finally
    RE.TestEndErrorCatching;
  end;
end;

procedure TTestRegexpr.IsMatching(AErrorMessage: String; ARegEx,
  AInput: RegExprString; AExpectStartLenPairs: array of Integer;
  AOffset: integer; AMustMatchBefore: integer);
var
  i: Integer;
  L: SizeInt;
begin
  CompileRE(ARegEx);
  RE.InputString:= AInput;

  IsTrue(AErrorMessage + ' Exec must give True', RE.ExecPos(AOffset, AMustMatchBefore));

  L := Length(AExpectStartLenPairs) div 2;
  AreEqual(AErrorMessage + ': MatchCount', L - 1, RE.SubExprMatchCount);
  for i := 0 to L - 1 do begin
    AreEqual(AErrorMessage + ': MatchPos['+inttostr(i)+']', AExpectStartLenPairs[i*2], RE.MatchPos[i]);
    AreEqual(AErrorMessage + ': MatchLen['+inttostr(i)+']', AExpectStartLenPairs[i*2+1], RE.MatchLen[i]);
  end;
end;

procedure TTestRegexpr.IsNotMatching(AErrorMessage: String; ARegEx,
  AInput: RegExprString; AOffset: integer; AMustMatchBefore: integer);
var
  r: Boolean;
begin
  CompileRE(ARegEx);
  RE.InputString:= AInput;
  r := RE.ExecPos(AOffset, AMustMatchBefore);

  if r then
    IsFalse(AErrorMessage + ': Exec must give False, but found at ' + IntToStr(RE.MatchPos[0]), r);
end;

procedure TTestRegexpr.IsErrorOnMatch(AErrorMessage: String; ARegEx,
  AInput: RegExprString; AOffset: integer; AMustMatchBefore: integer;
  ExpErrorId: Integer);
var
  r: Boolean;
begin
  CompileRE(ARegEx);
  RE.InputString:= AInput;
  r := True;
  try
    r := RE.ExecPos(AOffset, AMustMatchBefore);
  except
    r := False;
  end;

  if r then
    IsFalse(AErrorMessage + ': Exec must give False, but found at ' + IntToStr(RE.MatchPos[0]), r);

  if ExpErrorId <> 0 then
    AreEqual(AErrorMessage, ExpErrorId, RE.TestLastError);
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
  TestBadRegex('Error for invalid loop', '*');
  TestBadRegex('Error for invalid loop', '^*');
  TestBadRegex('Error for invalid loop', '\b*');
  TestBadRegex('Error for invalid loop', '+');
  TestBadRegex('Error for invalid loop', '^+');
  TestBadRegex('Error for invalid loop', '\b+');
  TestBadRegex('Error for invalid loop', '.?*');
  TestBadRegex('Error for invalid loop', '.**');
  TestBadRegex('Error for invalid loop', '.+*');
  TestBadRegex('Error for invalid loop', '.++?');

  RE.AllowUnsafeLookBehind := False;
  TestBadRegex('No Error for var-len look behind with capture', '.(?<=(.+))', 153);

  TestBadRegex('value for reference to big', '()\9999999999999999999999999999999999999999999999999999()');
  TestBadRegex('value for reference to big', '()\g9999999999999999999999999999999999999999999999999999()');

  // Only if RegExpWithStackOverflowCheck is enabled and supported
  //IsErrorOnMatch('Too Complex',   '^((b|a){1,5000}){1,5000}',   StringOfChar('a', 991*503)); // both factors are prime
end;

procedure TTestRegexpr.TestModifiers;
begin
  RE.ModifierI := True;
  IsMatching   ('NOT-CaseSens',                  'A',            '1A2',     [2,1]);
  IsMatching   ('NOT-CaseSens (?i)',             '(?i)A',        '1A2',     [2,1]);
  IsMatching   ('NOT-CaseSens (?-i)(?i)',        '(?-i)(?i)A',   '1A2',     [2,1]);
  IsMatching   ('NOT-CaseSens (?i)(?-i)',        '(?i)(?-i)A',   '1A2',     [2,1]);
  IsMatching   ('NOT-CaseSens diff',             'A',            '1a2',     [2,1]);
  IsNotMatching('NOT-CaseSens diff (?-i)',       '(?-i)A',       '1a2');
  IsMatching   ('NOT-CaseSens diff (?-i)(?i)',   '(?-i)(?i)A',   '1a2',     [2,1]);
  IsNotMatching('NOT-CaseSens diff (?i)(?-i)',   '(?i)(?-i)A',   '1a2');

  RE.ModifierI := False;
  IsMatching   ('CaseSens',                  'A',            '1A2',     [2,1]);
  IsMatching   ('CaseSens (?i)',             '(?i)A',        '1A2',     [2,1]);
  IsMatching   ('CaseSens (?-i)(?i)',        '(?-i)(?i)A',   '1A2',     [2,1]);
  IsMatching   ('CaseSens (?i)(?-i)',        '(?i)(?-i)A',   '1A2',     [2,1]);
  IsNotMatching('CaseSens diff',             'A',            '1a2');
  IsNotMatching('CaseSens diff (?-i)',       '(?-i)A',       '1a2');
  IsMatching   ('CaseSens diff (?-i)(?i)',   '(?-i)(?i)A',   '1a2',     [2,1]);
  IsNotMatching('CaseSens diff (?i)(?-i)',   '(?i)(?-i)A',   '1a2');



  IsMatching   ('CaseSens On/Off',          '(?i)A(?-i)BC',     '1aBC',     [2,3]);
  IsNotMatching('CaseSens On/Off',          '(?i)A(?-i)BC',     '1abC');
  IsNotMatching('CaseSens On/Off',          '(?i)A(?-i)BC',     '1aBc');
  IsNotMatching('CaseSens On/Off',          '(?i)A(?-i)BC',     '1abc');

  // (?i) to the end of the enclosing bracket
  IsMatching   ('CaseSens On/Off in ()',    '(?i)A(?:(?-i))BC',     '1aBC',     [2,3]);
  IsMatching   ('CaseSens On/Off in ()',    '(?i)A(?:(?-i))BC',     '1abC',     [2,3]);
  IsMatching   ('CaseSens On/Off in ()',    '(?i)A(?:(?-i))BC',     '1aBc',     [2,3]);
  IsMatching   ('CaseSens On/Off in ()',    '(?i)A(?:(?-i))BC',     '1abc',     [2,3]);

  // (?i) to the end of the enclosing bracket
  IsMatching   ('CaseSens On/Off in () for B',  '(?i)A(?:(?-i)B)C',     '1aBC',     [2,3]);
  IsNotMatching('CaseSens On/Off in () for B',  '(?i)A(?:(?-i)B)C',     '1abC');
  IsMatching   ('CaseSens On/Off in () for B',  '(?i)A(?:(?-i)B)C',     '1aBc',     [2,3]);
  IsNotMatching('CaseSens On/Off in () for B',  '(?i)A(?:(?-i)B)C',     '1abc');

  IsMatching   ('CaseSens On/Off (?i:B)',   '(?i)A(?-i:B)C',     '1aBC',     [2,3]);
  IsNotMatching('CaseSens On/Off (?i:B)',   '(?i)A(?-i:B)C',     '1abC');
  IsMatching   ('CaseSens On/Off (?i:B)',   '(?i)A(?-i:B)C',     '1aBc',     [2,3]);
  IsNotMatching('CaseSens On/Off (?i:B)',   '(?i)A(?-i:B)C',     '1abc');

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

  CompileRE('(?<=^.\GA...)(X)');
  CompileRE('(?<=^.\GA...)(X)');
  RE.InputString:= '_A123X3';
  IsTrue('Exec must give True', RE.Exec(2));
  AssertMatch('(?<=^.\GA...)(X)  _A123X3 offset 2 ', 6, 1);
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

procedure TTestRegexpr.TestQuesitonMark;
var
  i: Integer;
  c: RegExprString;
begin
  for i := 0 to 5 do begin
    case i of
      0: c := 'c';
      1: c := '(?:c)';
      2: c := '(?:c|x)';
      3: c := '(?>c)';
      4: c := '(?>c?)';
      5: c := '(?>c|x)';
    end;

    IsMatching(   '?',  'abcc'+c+'?d',  'abccd',  [1,5]);
    IsMatching(   '??', 'abcc'+c+'??d', 'abccd',  [1,5]);
    IsMatching(   '?+', 'abcc'+c+'?+d',  'abccd',  [1,5]);
    IsMatching(   '?+', 'abc'+c+'?+d',  'abccd',  [1,5]);

    IsMatching(   '?',  'ab'+c+'?ccd',  'abccd',  [1,5]);
    IsMatching(   '??', 'ab'+c+'??ccd', 'abccd',  [1,5]);
    IsMatching(   '?+', 'ab'+c+'?+cd',  'abccd',  [1,5]);
    IsMatching(   '?+', 'abc'+c+'?+d',  'abccd',  [1,5]);

    IsNotMatching('?+', 'ab'+c+'?+ccd', 'abccd');
  end;
end;

procedure TTestRegexpr.TestBraces;
var
  i: Integer;
  s: RegExprString;
begin
  for i := 0 to 9 do begin
    case i of
      0: s := 'd';
      1: s := 'd?';
      2: s := 'd+';
      3: s := 'd*';
      4: s := 'd*?';
      5: s := 'd*+';
      6: s := '(?=.)';
      7: s := '(?=.)?';
      8: s := '(?=.)*';
      9: s := '(?=.)+';
    end;
    RE.AllowLiteralBraceWithoutRange:= False;
    TestBadRegex('No Error for bad braces', s+'{');
    TestBadRegex('No Error for bad braces', s+'{22');
    TestBadRegex('No Error for bad braces', s+'{}');
    TestBadRegex('No Error for bad braces', s+'{,}');
    TestBadRegex('No Error for bad braces', s+'{x}');
    TestBadRegex('No Error for bad braces', s+'{1x}');
    TestBadRegex('No Error for bad braces', s+'{1,x}');
    TestBadRegex('No Error for bad braces', s+'{1,2{');
    TestBadRegex('No Error for bad braces', s+'{1,2,3}');
    RE.AllowBraceWithoutMin := False;
    TestBadRegex('No Error for bad braces', s+'{,2}');
  end;


  RE.AllowBraceWithoutMin := True;
  IsMatching('{,5} ',  'a{,5}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [1,0]);


  RE.AllowLiteralBraceWithoutRange := True;
  RE.AllowBraceWithoutMin := True;
  IsMatching('{2,5} ', 'a{2,5}', 'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,5]);
  IsMatching('{,5} ',  'a{,5}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [1,0]);
  IsMatching('{,5} ',  '.{,5}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [1,5]);
  IsMatching('{2,} ',  'a{2,}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,8]);

  for i := 0 to 6 do begin
    case i of
      0: s := 'a';
      1: s := 'a?';
      2: s := '.';
      3: s := 'a(?=.)';
      4: s := 'a(?=.)?';
      5: s := 'a(?=.)+';
      6: s := 'a(?=.)*';
    end;
    IsMatching('{}',     s+'{}',    'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [25,3]);
    IsMatching('{,} ',   s+'{,}',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [29,4]);
    IsMatching('{x} ',   s+'{x}',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [40,4]);
  end;

  IsMatching('{2,5}?', 'a{2,5}?', 'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,2]);
  IsMatching('{,5}?',  'a{,5}?',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [1,0]);
  IsMatching('{,5}?',  '.{,5}?',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [1,0]);
  IsMatching('{2,}?',  'a{2,}?',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,2]);

  for i := 0 to 6 do begin
    case i of
      0: s := 'a';
      1: s := 'a?';
      2: s := '.';
      3: s := 'a(?=.)';
      4: s := 'a(?=.)?';
      5: s := 'a(?=.)+';
      6: s := 'a(?=.)*';
    end;
    IsMatching('{}?',    'a{}?',    'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [12,2]);
    IsMatching('{,}?',   'a{,}?',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [29,4]);
    IsMatching('{x}?',   'a{x}?',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [40,4]);
  end;

  RE.AllowBraceWithoutMin := False;
  IsMatching('{2,5} ', 'a{2,5}', 'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,5]);
  IsMatching('{,5} ',  'a{,5}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [34,5]);
  IsMatching('{,5} ',  '.{,5}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [34,5]);
  IsMatching('{2,} ',  'a{2,}',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,8]);
  IsMatching('{}',     'a{}',    'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [25,3]);
  IsMatching('{,} ',   'a{,}',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [29,4]);
  IsMatching('{x} ',   'a{x}',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [40,4]);

  IsMatching('{2,5}?', 'a{2,5}?', 'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,2]);
  IsMatching('{,5}?',  'a{,5}?',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [34,5]);
  IsMatching('{,5}?',  '.{,5}?',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [34,5]);
  IsMatching('{2,}?',  'a{2,}?',  'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [3,2]);
  IsMatching('{}?',    'a{}?',    'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [12,2]);
  IsMatching('{,}?',   'a{,}?',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [29,4]);
  IsMatching('{x} ',   'a{x}?',   'bcaaaaaaaaXa{2,5}Xa{2,}Xa{}Xa{,}Xa{,5}Xa{x}_',  [40,4]);


  TestBadRegex('No Error for bad braces', 'd{2,1}');
  TestBadRegex('No Error for bad braces', 'd{2,1}');
end;

procedure TTestRegexpr.TestLoop;
var
  i, j: Integer;
  y, q: RegExprString;
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


  IsMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}){3,4}+',
             'Aayyyyyyyyyyyy',  [1,14,   11,4,  14,1]); // minimum y
  IsNotMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}+){3,4}+',
             'Aayyyyyyyyyyyy'); // minimum y
  IsNotMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}+){3,4}',
             'Aayyyyyyyyyyyy'); // minimum y
  IsMatching('nested {} poss',
             'Aa(x|y(x|y){2,3}+){3,4}+',
             'Aayyyyyyyyyyyy',  [1,14,   11,4,  14,1]); // minimum y

  IsMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}){3,4}+',
             'Aayyyyyyyyyyyyy',  [1,15,   12,4,  15,1]); // minimum y
  IsNotMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}+){3,4}+',
             'Aayyyyyyyyyyyyy'); // minimum y
  IsNotMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}+){3,4}',
             'Aayyyyyyyyyyyyy'); // minimum y

  IsMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}+){3,4}',
             'Aayyyyyyyyyyyyyy',  [1,16,   13,4,  16,1]); // two extra y
  IsMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}+){3,4}+',
             'Aayyyyyyyyyyyyyy',  [1,16,   13,4,  16,1]); // two extra y
  IsMatching('nested {} poss',
             'Aa(x|y(x|y){3,4}){3,4}+',
             'Aayyyyyyyyyyyyyy',  [1,16,   13,4,  16,1]); // two extra y


  for i := 0 to 3 do
  for j := 0 to 3 do begin
    case i of
      0: y := 'y';
      1: y := '(?:y)';
      2: y := 'z|y';
      3: y := 'z|(?:y)';
    end;
    case j of
      0: q := '*';
      1: q := '*?';
      2: q := '+';
      3: q := '+?';
    end;
    IsMatching('nested not poss',
               'Aa((('+y+q+'b)*)y*b)',
               'Aayyybyyybyyybyyybyyybyyy',  [1,22,   3,20, 3,16,  15,4]);
    IsNotMatching('nested  poss',
               'Aa((('+y+q+'b)*+)y*b)',
               'Aayyybyyybyyybyyybyyybyyy');
    IsMatching('nested not poss',
               'Aa((('+y+q+'b)*){0,1}y*b)',
               'Aayyybyyybyyybyyybyyybyyy',  [1,22,   3,20, 3,16,  15,4]);
    IsMatching('nested not poss',
               'Aa((((?>'+y+q+'b))*){0,1}y*b)',
               'Aayyybyyybyyybyyybyyybyyy',  [1,22,   3,20, 3,16,  15,4]);
    IsNotMatching('nested not poss',
               'Aa((?>(((?>'+y+q+'b))*))+y*b)',
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested not poss',
               'Aa((?>(('+y+q+'b)*))+y*b)',  // The possesive ?> will match to much, and wont allow the inner * to back-track
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested not poss',
               'Aa((('+y+q+'b)*){0,1}+y*b)',  // The possesive {0,1}+ will match to much, and wont allow the inner * to back-track
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested not poss',
               'Aa((((?>'+y+q+'b))*){0,1}+y*b)',  // The possesive {0,1}+ will match to much, and wont allow the inner * to back-track
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested not poss',
               'Aa((?>(((?>'+y+q+'b))*)){0,1}+y*b)',  // The possesive {0,1}+ will match to much, and wont allow the inner * to back-track
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested not poss',
               'Aa((?>(('+y+q+'b)*)){0,1}+y*b)',  // The possesive {0,1}+ will match to much, and wont allow the inner * to back-track
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested not poss',
               'Aa((('+y+q+'b)*){1,1}+y*b)',  //
               'Aayyybyyybyyybyyybyyybyyy');
    IsNotMatching('nested  poss',
               'Aa((('+y+q+'b)*+){1,2}y*b)',
               'Aayyybyyybyyybyyybyyybyyy');
    IsMatching('nested not poss',
               'Aa((('+y+q+'b)*)y*b){1,2}',
               'Aayyybyyybyyybyyybyyybyyy',  [1,22,   3,20, 3,16,  15,4]);
    IsNotMatching('nested  poss',
               'Aa((('+y+q+'b)*+)y*b){1,2}',
               'Aayyybyyybyyybyyybyyybyyy');
  end;


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

procedure TTestRegexpr.TestEmptyLoop;
var
  WithAtomic, LoopIdx, PtnIdx, ExpGrpPos1, ExpGrpLen1: Integer;
  LoopTxt, PtnText: RegExprString;
  s: String;
  MatchZeroTimes, ForceMatchZeroTimes: Boolean;
begin
  for LoopIdx := 0 to 18 do begin
    case LoopIdx of
       0: LoopTxt := '';
       1: LoopTxt := '?';
       2: LoopTxt := '??';
       3: LoopTxt := '+';
       4: LoopTxt := '+?';
       5: LoopTxt := '++';
       6: LoopTxt := '*';
       7: LoopTxt := '*?';
       8: LoopTxt := '*+';
       9: LoopTxt := '{0}';
      10: LoopTxt := '{1}';
      11: LoopTxt := '{5}';
      12: LoopTxt := '{0,5}';
      13: LoopTxt := '{1,5}';
      14: LoopTxt := '{0}?';
      15: LoopTxt := '{1}?';
      16: LoopTxt := '{5}?';
      17: LoopTxt := '{0,5}?';
      18: LoopTxt := '{1,5}?';
      //19: LoopTxt := '{0}+';
      //20: LoopTxt := '{1}+';
      //21: LoopTxt := '{5}+';
      //22: LoopTxt := '{0,5}+';
      //23: LoopTxt := '{1,5}+';
    end;

    for PtnIdx := 0 to 18 do
    for WithAtomic := 0 to 2 do begin
      case PtnIdx of
         0: PtnText := '()';
         1: PtnText := '(xx|)';
         2: PtnText := '(|xx)';
         3: PtnText := '(xx|(?:xx)?)';
         4: PtnText := '(xx|(?:))';
         5: PtnText := '(xx|(?:)*)'; // nested
         6: PtnText := '(xx|(?:)*?)'; // nested
         7: PtnText := '(xx|(?:)*+)'; // nested
         8: PtnText := '(xx|(?:)+)'; // nested
         9: PtnText := '(xx|(?:)+?)'; // nested
        10: PtnText := '(xx|(?:)++)'; // nested
        11: PtnText := '((?:(?:)+)*|(?:(?:)*)+)'; // nested
        12: PtnText := '((?=))';
        13: PtnText := '((?!X))';
        14: PtnText := '((?=(?:)*))'; // nested in look-around
        15: PtnText := '((?:(?:(?:)*){10,11}){20,21})';
        16: PtnText := '((?>(?:)+)*|(?:(?:)*)+)'; // nested inner atom
        17: PtnText := '((?:(?>)+)*|(?:(?>)*)+)'; // nested inner atom
        18: PtnText := '(\b)';
      end;
       // (?> atomic
      case WithAtomic of
        0: ;
        1: begin
          Insert('(?>', PtnText, 2);
          PtnText := PtnText + ')';
        end;
        2: PtnText := '(?>' + PtnText + ')';
      end;

      MatchZeroTimes := LoopIdx in [2,7,9,14,17,19];
      ForceMatchZeroTimes := LoopIdx in [9,14,19];

      ExpGrpPos1 := 1;
      ExpGrpLen1 := 0;
      if MatchZeroTimes then begin
        ExpGrpPos1 := -1;
        ExpGrpLen1 := -1;
      end;

      s := LoopTxt + ' / ' + PtnText;
      IsMatching('Empty group '+s,                PtnText+LoopTxt,      'a',   [1,0,  ExpGrpPos1,ExpGrpLen1]);
      if PtnIdx < 18 then
        IsMatching('Empty group in empty text '+s,  PtnText+LoopTxt,      '',    [1,0,  ExpGrpPos1,ExpGrpLen1]);
      IsMatching('Empty group before text '+s,    PtnText+LoopTxt+'a',  'a',   [1,1,  ExpGrpPos1,ExpGrpLen1]);

      if ForceMatchZeroTimes then
        IsNotMatching('Empty group backref '+s,   PtnText+LoopTxt+'\1', 'a')
      else
        IsMatching('Empty group backref '+s,          PtnText+LoopTxt+'\1', 'a',   [1,0,  1,0]);

      if ForceMatchZeroTimes or (PtnIdx = 18) then
        IsNotMatching('Empty group backref in empty'+s,  PtnText+LoopTxt+'\1', '')
      else
        IsMatching('Empty group backref in empty'+s,  PtnText+LoopTxt+'\1', '',    [1,0,  1,0]);



      if ExpGrpPos1 > 0 then ExpGrpPos1 := 2;
      IsMatching('Empty group after text '+s,     'a'+PtnText+LoopTxt,     'a',   [1,1,  ExpGrpPos1,ExpGrpLen1]);

      if PtnIdx < 18 then begin
        IsMatching('Empty group mid   text '+s,     'a'+PtnText+LoopTxt,     'aa',  [1,1,  ExpGrpPos1,ExpGrpLen1]);
        IsMatching('Empty group mid   text '+s,     'a'+PtnText+LoopTxt+'a', 'aa',  [1,2,  ExpGrpPos1,ExpGrpLen1]);

        if ForceMatchZeroTimes then begin
          IsNotMatching('Empty group mid + backref at end '+s,  'a'+PtnText+LoopTxt+'a\1', 'aa');
        end
        else begin
          IsMatching('Empty group mid + backref at end '+s,  'a'+PtnText+LoopTxt+'a\1', 'aa',  [1,2,  2,0]);
          IsMatching('Empty group mid + backref at end '+s,  'a'+PtnText+LoopTxt+'a(\1)', 'aa',  [1,2,  2,0,  3,0]);
        end;
      end;

    end;
  end;

  // The backref in the loop changes each iteration. So even if the match is empty, it must be executed the correct amount of times
  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$){1,4}', '1234567890abcdefghi',   [1,0,  1,0,  15,1]);
  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$){1,5}', '1234567890abcdefghi',   [1,0,  1,0,  16,1]);

  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$){1,4}?', '1234567890abcdefghi',   [1,0,  1,0,  12,1]);
  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$){1,5}?', '1234567890abcdefghi',   [1,0,  1,0,  12,1]);

  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$)+', '1234567890abcdefghi',   [1,0,  1,0,  12,1]);
  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$)+?', '1234567890abcdefghi',   [1,0,  1,0,  12,1]);
  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$)*', '1234567890abcdefghi',   [1,0,  1,0,  12,1]);

  IsMatching('Empty group match-count',  '((?=.*(?:\2|a)(.))|$)*?', '1234567890abcdefghi',   [1,0,  -1,-1,  -1,-1]);


end;

procedure TTestRegexpr.TestBranches;

  const
    MAX_A =  5*32 -1;
    MAX_B = 14 + 4;
  function BArg(Base: string; a, b: Integer; out L: integer): string;
  var
    OBase: String;
  begin
    L := 1;
    OBase := Base;
    if (a and  1) <> 0 then Base := '[' +Base+']';
    if (a and  2) <> 0 then Base := Base+'+';
    if (a and  4) <> 0 then
      if (a and  2) <> 0 then Base := '(?>X|' +Base+ ')'  // already has a quantifier +
      else Base := Base+ '{1,9}';

    if (a and   8) <> 0 then Base := Base+ '(?>.){0,0}+';
    if (a and  16) <> 0 then Base := '\b' + Base;
    case a and (not 31) of
      0 *32: Result := Base;
      1 *32: Result := '(?='+Base+').';
      2 *32: Result := '(?='+Base+')'+Base;
      3 *32: begin
          Result := '(?='+Base+')';     // zero len
          L := 0;
        end;
      4 *32: begin
          Result := '(?='+Base+')\b';     // zero len
          L := 0;
        end;
    end;

    case b mod 14 of
       0: ;
       1: Result := '\K' + Result;
       2: Result := '(?=\b)' + Result;
       3: Result := '(?=\w)' + Result;
       4: Result := '(?=X)?' + Result;
       5: Result := '(?!X)' + Result;
       6: Result := '(?!X)?' + Result;
       7: Result := '(?!'+OBase+')?' + Result;
       8: Result := '(?<=\b)' + Result;
       9: Result := '(?<=\b)?' + Result;
      10: Result := '(?<!\B)?' + Result;
      11: Result := '\b?' + Result;
      12: Result := '(?>)' + Result;
      13: Result := '(?>(?<=\b)|X){0,9}' + Result;
    end;
    if b > 13 then Result := '(?:X|' +Result+ ')+';
  end;

var
  Lead: Integer;
  a1, a2, b1, b2: integer;
  L1, L2: Integer;
  sLead, sB1, sB2: RegExprString;
begin
  (* Hide the canditate for (each) branch, in all and any surrounding expressions
     Make sure it still matches
     - must NOT be missing in FirstChar
     - must NOT be missing GBranch (if GBranch is used)

     This test does not assert that
     - FirstChar does not have unwanted extra (that does not invalidate the result)
     - GBranch is used whenever possible, only that if it happens to be used, it works
  *)
  for Lead := 0 to 1 do
  for a1 := 0 to MAX_A do
  for b1 := 0 to MAX_B do
  for a2 := 0 to 1 do
  for b2 := 1 to 2 do
  begin
    sLead := '';
    if Lead = 1 then sLead := ' ';

    sB1 := BArg('a', a1, b1, L1);
    sB2 := BArg('b', a2*32, b2, L2);

    IsMatching('branch ', sB1+'|'+sB2,  sLead+'a',  [1+Lead, L1  ]);
    IsMatching('branch ', sB2+'|'+sB1,  sLead+'a',  [1+Lead, L1  ]);
    IsMatching('branch ', sB1+'|'+sB2,  sLead+'b',  [1+Lead, L2  ]);
    IsMatching('branch ', sB2+'|'+sB1,  sLead+'ba',  [1+Lead, L2  ]);
    IsMatching('branch ', '('+sB1+'|'+sB2+')',  sLead+'aba',  [1+Lead, L1,   1+Lead, L1]);

    IsMatching('branch ', '[d-m]|'+sB1+'|'+sB2,  sLead+'a',  [1+Lead, L1  ]);
    IsMatching('branch ', sB1+'|[d-m]|'+sB2,     sLead+'a',  [1+Lead, L1  ]);
    IsMatching('branch ', sB1+'|'+sB2+'|[d-m]',  sLead+'a',  [1+Lead, L1  ]);
  end;

  IsNotMatching('branch ', '(?:(?=[abc])|d)+c|e',  '.a');
  IsMatching('branch ', '(?:(?=[abc])|d)+c|e',  '.c',  [2, 1  ]);
  IsMatching('branch ', '(?:(?=[abc])|d)+c|e',  '.dc',  [2, 2  ]);
  IsMatching('branch ', '(?:(?=[abc])|d)+c|e',  '.ec',  [2, 1  ]);
end;

procedure TTestRegexpr.TestReferences;
begin
  IsMatching('match backref greater 9 (two digit) ',
             '(?i)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)\g11',
             'x123456789ABCbD',   [2,13,   2,1, 3,1, 4,1, 5,1, 6,1, 7,1, 8,1, 9,1, 10,1,  11,1,  12,1,  13,1] );


  IsMatching('Valid capture idx', '(.)(.)\2',  'aABBC',  [2,3,  2,1, 3,1]);
  TestBadRegex('Invalid capture idx', '(.)(.)\3');

  IsMatching('Valid capture idx \g', '(.)(.)\g2',  'aABBC',  [2,3,  2,1, 3,1]);
  TestBadRegex('Invalid capture idx \c', '(.)(.)\g3');

  IsMatching('Valid call idx', '(.)(.)(?2)',  'aABBC',  [1,3,  1,1, 2,1]);
  TestBadRegex('Invalid call idx', '(.)(.)(?3)');

  IsMatching('ref in branch', '((A)|B|C\2)*',  'AACACB',  [1,4,  3,2, 2,1]);
  IsMatching('ref in call', '[^A]*((A)|B|C\2)*',  'DAACACBE',  [1,5,  4,2, 3,1]);
end;

procedure TTestRegexpr.TestSubCall;
begin
  IsMatching('simple call', '(1)_(?1)',  '1_1',  [1,3,  1,1]);
  IsNotMatching('simple call', '(1)_(?1)',  '1_2'  );

  IsMatching('recurse call', '(1(?1)?_)',  'x11__',  [2,4,  2,4]);
  IsMatching('recurse call', '(1(?1)?_)',  'x11_1',  [3,2,  3,2]);
  IsMatching('recurse call', '(1(?1)?_)',  'x1__',   [2,2,  2,2]);
  IsMatching('recurse call', '(1(?1)?_)',  'x111__', [3,4,  3,4]);

  IsMatching('deep recurse call', '(1(?1)?_)',  'x1111____',   [2,8,  2,8]);

  IsMatching('side by side recurse call', '(1((?1)?)_((?1)?)2)',  'x1111_2_1_22_1_222_',   [3,15,  3,15, 4,9, 14,3]);

  IsMatching('nested call to outer', '(1(2(3(?1)?))A)_((?3))',  '123A_3123',  [1,6,  1,4, 2,2, 3,1, 6,1]);
  IsMatching('nested call to outer', '(1(2(3(?1)?))A)_((?3))',  '123A_3123A',  [1,10,  1,4, 2,2, 3,1, 6,5]);

  IsMatching('ref in branch', '((A)|B|C(?2))*',  'AACACB',  [1,4,  3,2, 2,1]);
  IsMatching('ref in branch', '[^A]*((A)|B|C(?2))*',  'DAACACBE',  [1,5,  4,2, 3,1]);
  IsMatching('ref in branch', '((A)|B|C(?2))+',  'AACB',  [1,2,  2,1, 2,1]);
end;

procedure TTestRegexpr.TestNamedGroups;

  function ExprNamedGrp(ASyntax: integer; AName, AMatch: RegExprString): RegExprString;
  begin
    case ASyntax of
      0: Result := '(?P<' + AName + '>'  + AMatch + ')';
      1: Result := '(?<'  + AName + '>'  + AMatch + ')';
      2: Result := '(?''' + AName + '''' + AMatch + ')';
    end;
  end;

  function ExprNamedRef(ASyntax: integer; AName: RegExprString; AGrpNum: Integer): RegExprString;
  begin
    case ASyntax of
      0: Result := '(?P=' + AName + ')';
      1: Result := '\g{'  + AName + '}';
      2: Result := '\k{'  + AName + '}';
      3: Result := '\k<'  + AName + '>';
      4: Result := '\k''' + AName + '''';
      // Test ref to named group by number
      5: Result := '\' + IntToStr(AGrpNum);
      6: Result := '\g' + IntToStr(AGrpNum);
      7: Result := '\g0' + IntToStr(AGrpNum); // with leading zero
    end;
  end;

  function ExprNamedCall(ASyntax: integer; AName: RegExprString; AGrpNum: Integer): RegExprString;
  begin
    case ASyntax of
      0: Result := '(?P>' + AName + ')';
      1: Result := '(?&'  + AName + ')';
      2: Result := '\g<'  + AName + '>';
      3: Result := '\g''' + AName + '''';
      // Test ref to named group by number
      4: Result := '(?' + IntToStr(AGrpNum) + ')';
    end;
  end;

var
  NameSyntax, RefSyntax, CallSyntax: Integer;
  n, r, c, n2, r2, c2: RegExprString;
begin
  for NameSyntax := 0 to 2 do begin
    n := ExprNamedGrp(NameSyntax, 'Foo_1', '[aA].');
    n2 := ExprNamedGrp(NameSyntax, 'Foo_2', '[bB].');
    for RefSyntax := 0 to 7 do begin
      r := ExprNamedRef(RefSyntax, 'Foo_1', 1);
      IsMatching('Named ref',
                 '^' + n + r + '_',      'abab_ab',  [1,5,  1,2]);
      IsNotMatching('Named ref (is REF, not CALL)',
                 '^' + n + r,      'abAB_ab');

      if RefSyntax <= 4 then begin
        TestBadRegex('Named ref (wrong name)',
                   '^' + n + ExprNamedRef(RefSyntax, 'Foo_2', 0),      142);
        TestBadRegex('Named ref (wrong name)',
                   '^' + n + ExprNamedRef(RefSyntax, 'Foo_', 0),       142);
        TestBadRegex('Named ref  (wrong name)',
                   '^' + n + ExprNamedRef(RefSyntax, 'Foo_11', 0),     142);
      end;

      // 2 named patterns
      r2 := ExprNamedRef(RefSyntax, 'Foo_2', 2);
      IsMatching('2 Named ref',
                 '^' + n + n2 + r + r2 + '_',      'axbxaxbx__',  [1,9,  1,2,  3,2]);
      IsMatching('2 Named ref backwards',
                 '^' + n + n2 + r2 + r + '_',      'axbxbxax__',  [1,9,  1,2,  3,2]);
      IsNotMatching('2 Named ref',
                 '^' + n + n2 + r + r2,      'axbxbxax__' );
      IsNotMatching('2 Named ref backwards',
                 '^' + n + n2 + r2 + r,      'axbxaxbx__' );

      // forward ref
      IsMatching('Named forward ref',
                 '(?:(?:' + r + '|x)' + n + ')+_',      'abxababab_ab',  [3,8,  8,2]);
    end;


    for CallSyntax := 0 to 4 do begin
      c := ExprNamedCall(CallSyntax, 'Foo_1', 1);
      IsMatching('Named call',
                 '^' + n + c + '_',      'abab_ab',  [1,5,  1,2]);
      IsMatching('Named call (match changed text)',
                 '^' + n + c + '_',      'abAB_ab',  [1,5,  1,2]);

      if CallSyntax <= 3 then begin
        TestBadRegex('Named ref (wrong name)',
                   '^' + n + ExprNamedRef(CallSyntax, 'Foo_2', 0),      142);
        TestBadRegex('Named ref (wrong name)',
                   '^' + n + ExprNamedRef(CallSyntax, 'Foo_', 0),       142);
        TestBadRegex('Named ref  (wrong name)',
                   '^' + n + ExprNamedRef(CallSyntax, 'Foo_11', 0),     142);
      end;

      // 2 named patterns
      c2 := ExprNamedCall(CallSyntax, 'Foo_2', 2);
      IsMatching('2 Named ref',
                 '^' + n + n2 + c + c2 + '_',      'axbxAxBx__',  [1,9,  1,2,  3,2]);
      IsMatching('2 Named ref backwards',
                 '^' + n + n2 + c2 + c + '_',      'axbxBxAx__',  [1,9,  1,2,  3,2]);
      IsNotMatching('2 Named ref',
                 '^' + n + n2 + c + c2,      'axbxbxax__' );
      IsNotMatching('2 Named ref backwards',
                 '^' + n + n2 + c2 + c,      'axbxaxbx__' );

      // forward call
      IsMatching('Named forward call',
                 '(?:(?:' + c + '|x)' + n + ')+_',      'abxabABab_ab',  [3,8,  8,2]);
    end;
  end;




end;

procedure TTestRegexpr.TestCaptures;
begin
  IsMatching('simple capture', '(a)',    'abc',  [1,1,  1,1]);
  IsMatching('simple capture', '(b)',    'abc',  [2,1,  2,1]);
  IsMatching('simple capture', '(c)',    'abc',  [3,1,  3,1]);
  IsMatching('simple capture .', '(.)',  'abc',  [1,1,  1,1]);

  IsMatching('capture optional content',    '(d?)',  'abc',  [1,0,  1,0]);
  IsMatching('optional capture not found',  '(d)?',  'abc',  [1,0,  -1,-1]);
  IsMatching('optional capture found',      '(a)?',  'abc',  [1,1,  1,1]);

  IsMatching('zero width capture', '(^)',    'abc',  [1,0,  1,0]);
  IsMatching('zero width capture', '($)',    'abc',  [4,0,  4,0]);
  IsMatching('zero width capture', '(\b)',   'abc',  [1,0,  1,0]);
  IsMatching('zero width capture', '(\b).',  'abc',  [1,1,  1,0]);

  IsMatching('backtracking', '(ad)',  'abcade',  [4,2,  4,2]);
  IsMatching('backtracking', '^.*(a)',   'abcade',  [1,4,  4,1]);
  IsMatching('backtracking', '^.*(a)b',  'abcade',  [1,2,  1,1]);
  IsMatching('backtracking', '^.*(ab)',   'abcade',  [1,2,  1,2]);

  IsMatching('backtracking', '(ad)*',  'adadae',  [1,4,  3,2]);

  IsMatching('backtracking many', '^.*(a).(b)c',  'a.bda.bc',     [1,8,  5,1, 7,1]);
  IsMatching('backtracking many', '^.*?(a).(b)c',  'a.bda.bc',    [1,8,  5,1, 7,1]);
  IsMatching('backtracking many', '^.*?(a).*?(b)c',  'a.bda.bc',  [1,8,  1,1, 7,1]);
end;

procedure TTestRegexpr.TestRecurseAndCaptures;
begin
  // recurse capture "B", but outer does not capture
  IsMatching('Capture in recurse does not bleed into result',
             '[aA](?R)?(?:X|([bB]))',
             'aABXc',  [1,4,  -1,-1]);

  IsMatching('backref does NOT see outer capture',
             '(?:x|([abc]))(?R)?-\1*',  'aabxa-a-b-b-a-a',  [4, 5,  -1,-1]);
  IsMatching('backref does NOT see outer capture',
             '(?:x|([abc]))(?R)?-\1*',  'aabxa-a--b-a-a',  [1, 14,  1,1]);
  IsMatching('backref does NOT see outer capture',
             '(?:x|([abc]))(?R)?-\1',  'aabxa-a-b-b-a-a',  [5, 3,  5,1]);


  IsMatching('2nd recurse does NOT see capture from earlier recurse',
             '[aA](?R)?(?:X|([bcBC]))(?R)?\1',
             'aABBcAXBc',  [2,3,  3,1]);

  IsMatching('2nd recurse does NOT see capture from earlier recurse',
             '[aA]((?R))?(?:X|([bcBC]))((?R))?\2',
             'aABBcAXBc',  [2,3,  -1,-1,  3,1,  -1,-1]);


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

  procedure HasFixedLookBehind(AErrorMessage: String; ARegEx: RegExprString);
  var
    s: RegExprString;
  begin
    CompileRE(ARegEx);
    s := RE.Dump();
    //IsTrue(AErrorMessage, pos('Len:', s) > 0);
    IsTrue(AErrorMessage, pos('greedy', s) <= 0);
  end;

  procedure HasVarLenLookBehind(AErrorMessage: String; ARegEx: RegExprString);
  var
    s: RegExprString;
  begin
    CompileRE(ARegEx);
    s := RE.Dump();
    //IsTrue(AErrorMessage, pos('Len:', s) <= 0);
    IsTrue(AErrorMessage, pos('greedy', s) > 0);
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


  HasLength('look behind is not (yet) fixed', '(?<=.A...)(X)',   -1);

  HasVarLenLookBehind('', '()A(?<=.(?<=\1))');
  HasVarLenLookBehind('', '()()()()A(?<=.(?<=\4))');
  HasVarLenLookBehind('', '()A(?<=.(?<=(?1)))');
  HasVarLenLookBehind('', '()()()()A(?<=.(?<=(?4)))');
  HasVarLenLookBehind('', '()A(?<=.(?<=(?R)))');
  HasFixedLookBehind ('', '()A(?<=.(?<=\p{Lu}))');
  HasFixedLookBehind ('', '()A(?<=.(?<=[a-x]))');

end;

procedure TTestRegexpr.TestMatchBefore;
begin
  IsMatching('',   '2',    '123456789',     [2,1], 1, 0);
  IsMatching('',   '2',    '123456789',     [2,1], 1, 4);
  IsMatching('',   '2',    '123456789',     [2,1], 1, 3);
  IsNotMatching('',  '2',    '123456789',     1, 2);
  IsNotMatching('',  '2',    '123456789',     1, 1);
end;

procedure TTestRegexpr.TestAnchor;

  procedure HasAnchor(AErrorMessage: String; ARegEx: RegExprString; ExpAnchor: TRegExprAnchor);
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
    RE.ModifierS := True;
    HasAnchor('', s, raOnlyOnce);
    HasAnchor('', s+'a', raOnlyOnce);
    HasAnchor('', s+'|a', raNone);
    HasAnchor('', '('+s+'|a)', raNone);
    RE.ModifierS := False;
    if i <> 1 then begin    // {0,}+ possesive not allowed
      HasAnchor('', s, raNone);
      HasAnchor('', s+'a', raNone);
      HasAnchor('', s+'|a', raNone);
      HasAnchor('', '('+s+'|a)', raNone);
    end;
  end;

  for i := low(TestNotOnlyOnceData) to high(TestNotOnlyOnceData) do begin
    s := TestNotOnlyOnceData[i];
    RE.ModifierS := True;
    HasAnchor('', s, raNone);
    HasAnchor('', s+'a', raNone);
    HasAnchor('', s+'|a', raNone);
    HasAnchor('', '('+s+'|a)', raNone);
    RE.ModifierS := False;
    if (i <> 1) and (i <> 4) then begin
      HasAnchor('', s, raNone);
      HasAnchor('', s+'a', raNone);
      HasAnchor('', s+'|a', raNone);
      HasAnchor('', '('+s+'|a)', raNone);
    end;
  end;


end;

procedure TTestRegexpr.TestRegLookAhead;
begin
  // Match look-ahead: One look-ahead
  IsMatching('Ahead found after "A"',
                'A(?=B)',    'A2AB34AB_',                [3,1]);
  IsMatching('Ahead found after "."',
                '.(?=B)',    'A2AB34AB_',                [3,1]);
  IsMatching('Ahead found after capture "(A)"',
                '(A)(?=B)',    'A2AB34AB_',              [3,1,  3,1]);

  IsMatching('Ahead found before "B"',
                '(?=B)B',    '12AB34',                   [4,1]);
  IsMatching('Ahead found before ".B"',
                '(?=C).B',   '12AB34CB5',                [7,2]);
  IsMatching('Ahead found before "(.)"',
                '(?=B)(.)',    '12AB34',                 [4,1,  4,1]);
  IsMatching('Ahead found, stand alone',
                '(?=B)',    '12AB34',                    [4,0]);
  IsMatching('Ahead found, stand alone - full len',
                '(?=....)',    '1234',                   [1,0]);

  // Anchors
  IsMatching('Ahead found first at BOL',
                '(?=B)(.)',    'B34',                    [1,1,  1,1]);
  IsMatching('Ahead found "^" first at BOL',
                '(?=^)(.)',    'B34',                    [1,1,  1,1]);
  IsMatching('Ahead found "$" first at EOL',
                '(.)(?=$)',    'B34',                    [3,1,  3,1]);

  IsMatching('Ahead found "\b" before "."',
                '(?=\b).',    '   abc ',                 [4,1]);
  IsMatching('Ahead found "\b" after "."',
                '.(?=\b)',    '   abc ',                 [3,1]);

  IsMatching('Ahead found "^" stand alone',
                '(?=^)',    'B34',                    [1,0]);
  IsMatching('Ahead found "$" stand alone at offset',
                '(?=$)',    'B34',                    [4,0],  2);
  IsMatching('Ahead found "$" stand alone',
                '(?=$)',    'B34',                    [4,0]);
  IsMatching('Ahead found "\G" stand alone',
                '(?=\G)',    'B34',                    [1,0]);
  IsMatching('Ahead found "\G" stand alone at offset',
                '(?=\G)',    'B34',                    [3,0],  3);
  IsNotMatching('Ahead not found "^" stand alone at offset',
                '(?=^)',    'B34',           2);

  IsNotMatching('Ahead not found after "A"',            'A(?=X)',    'A2AB34AB_');
  IsNotMatching('Ahead not found after "."',            '.(?=X)',    'A2AB34AB_');
  IsNotMatching('Ahead not found before "."',           '(?=X).',    'A2AB34AB_');
  IsNotMatching('Ahead not found stand alone',          '(?=X)',     'A2AB34AB_');
  IsNotMatching('Ahead not found impossible',           '(?=.^)',    'A2AB34AB_');
  IsNotMatching('Ahead not found past EOL',             'c(?=..)',    'abcd');
  IsNotMatching('Ahead not found past EOL stand alone', '(?=.....)',    'abcd');
  IsNotMatching('Ahead not found after "A=X"',          '(?=X)A',    'A2AB34AB_');
  IsNotMatching('Ahead not found after "A=2" stand alone',          '(?=A)(?=2)',    'A2AB34AB_');

  // Effect of ahead on MatchLen[0]
  IsMatching('Ahead found mid pattern (shorter)',
                'A(?=BB).',    '12ABB34',                 [3,2]);
  IsMatching('Ahead found mid pattern (same len)',
                'A(?=BB)..',    '12ABB34',                [3,3]);
  IsMatching('Ahead found mid pattern (longer)',
                'A(?=BB)...',    '12ABB34',               [3,4]);
  IsMatching('Ahead found, then capture in pattern (shorter)',
                '(A)(?=BB)(.)', '12ABB34',                [3,2,  3,1, 4,1]);
  IsMatching('Ahead found, then capture in pattern (same len)',
                '(A)(?=BB)(..)', '12ABB34',               [3,3,  3,1, 4,2]);
  IsMatching('Ahead found, then capture in pattern (longer)',
                '(A)(?=BB)(...)', '12ABB34',              [3,4,  3,1, 4,3]);

  // Match look-ahead: One look-ahead - variable len
  IsMatching('Ahead (var-len) found "B" after "A"',
                'A(?=[^\d]*B)',    '_A_1_B_AxyzB123_A_',    [8,1]);
  IsMatching('Ahead (var-len) found no"A" after "A"',
                'A(?=[^A]*$)',    '_A_1_B_AxyzB123_A_',     [17,1]);
  IsMatching('Ahead (var-len) stand alone"',
                '(?=.*$)',        '_A_1_B_AxyzB123_A_',     [1,0]);
  IsMatching('Ahead (var-len) stand alone, max len"',
                '(?=.{2}$)',        '_A_1_B_AxyzB123_A_',   [17,0]);
  IsNotMatching('Ahead (var-len) not found no"_" after "A"',
                'A(?=[^_]*$)',    '_A_1_B_AxyzB123_A_');

  // Optional
  IsMatching('Ahead *',
                '(?=B)*.A',    'BA',                [1,2]);
  IsMatching('Optional Ahead found after "A"',
                'A(?=B)?',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional Ahead found after "A"',
                'A(?=X)?',    '_A2AB34AB_',                [2,1]);
  IsNotMatching('Ahead not found stand alone',
                '(?=X)',     '_A2AB34AB_');
  IsMatching('Optional Ahead found stand alone',
                '(?=X)?',     '_A2AB34AB_',                [1,0]);
  IsMatching('Optional Ahead found (too long)',
                'A(?=......)?',     '_A2_',                [2,1]);

  IsMatching('Optional * Ahead found after "A"',
                'A(?=B)*',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional * Ahead found after "A"',
                'A(?=X)*',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional * Ahead found stand alone',
                '(?=X)*',     '_A2AB34AB_',                [1,0]);
  IsMatching('Optional * Ahead found (too long)',
                'A(?=......)*',     '_A2_',                [2,1]);

  IsMatching('Optional {0} Ahead found after "A"',
                'A(?=B){0}',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional {0} Ahead found after "A"',
                'A(?=X){0}',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional {0} Ahead found stand alone',
                '(?=X){0}',     '_A2AB34AB_',                [1,0]);
  IsMatching('Optional {0} Ahead found (too long)',
                'A(?=......){0}',     '_A2_',                [2,1]);


  // Match look-ahead: In capture and branch
  IsMatching('Ahead found for capture/branch"',
                '(A(?=BB)|Cd)',    '_Ax_Cd_ABB_',    [5,2,  5,2]);
  IsMatching('Ahead found for capture/branch"',
                '(Cd|A(?=BB))',    '_Ax_Cd_ABB_',    [5,2,  5,2]);
  IsMatching('Ahead found for capture/branch"',
                '(A(?=BB)|Cd)',    '_ABB_Cd_ABB_',    [2,1,  2,1]);
  IsMatching('Ahead found for capture/branch"',
                '(Cd|A(?=BB))',    '_ABB_Cd_ABB_',    [2,1,  2,1]);

  IsNotMatching('Ahead found after branch capture',
                '(A|B)(?=C).x',  '_AC_BDx_');
  IsMatching('Ahead found after branch capture',
                '(A|B)(?=[CD]).x',  '_AC_BDx_',       [5,3,  5,1]);
  IsMatching('Ahead found after branch capture',
                '(A|B)(?=C|D).x',  '_AC_BDx_',       [5,3,  5,1]);


  // Match look-ahead: One look-ahead - with capture(s)
  IsMatching('Ahead found, with capture in look-ahead',
                '(A)(?=(B))', '12AB34',                   [3,1,  3,1, 4,1]);
  IsMatching('Ahead found, with capture in look-ahead',
                '(2)(?=.(B))', '12AB34',                  [2,1,  2,1, 4,1]);

  IsMatching('Ahead found, capture in L-A, and capture in pattern',
                '(A)(?=...(5))(.)', '12AB345',            [3,2,  3,1, 7,1, 4,1]);
  IsMatching('Ahead found, with capture in L-A, and dot in pattern (same len)',
                '(A)(?=(B)).', '12AB34',                  [3,2,  3,1, 4,1]);
  IsMatching('Ahead found, capture in L-A, and longer capture in pattern',
                '(A)(?=(B))(..)', '12AB34',               [3,3,  3,1, 4,1, 4,2]);
  IsMatching('Ahead found, capture in L-A, and shorter capture in pattern',
                '(A)(?=(B.))(.)', '12AB34',               [3,2,  3,1, 4,2, 4,1]);
  IsMatching('Ahead found, with branch-capture in look-ahead',
                '(A)(?=(B|C))', '12AB34',                 [3,1,  3,1, 4,1]);
  IsMatching('Ahead found, with branch-capture in look-ahead',
                '(A)(?=(B|C))', '12AC34',                 [3,1,  3,1, 4,1]);


  // Match look-ahead: Multiple look-ahead
  IsMatching('Two Ahead found from same pos',
                'A(?=B)(?=.3)',    'AB2AB34AB_A_3_',                [4,1]);
  IsMatching('Two Ahead found before/after"',
                '(?=A)[aA](?=.3)',    '_aB3_AB3_AB39_A_3_',         [6,1]);
  IsMatching('Three Ahead found before/after/nested',
                '(?=A)[aA](?=(?=..9).3)',    '_aB3_AB3_AB39_A_3_',  [10,1]);
  IsMatching('Three Ahead found before/after/nested"',
                '(?=A)[aA](?=.(?=.9)3)',    '_aB3_AB3_AB39_A_3_',   [10,1]);
  IsMatching('Three Ahead found before/after/nested-var-len',
                '(?=A)[aA](?=(?=.*9).3)',    '_aB3_AB3_AB39_A_3_',  [6,1]);
  IsMatching('Three Ahead found before/after/nested-var-len-witch-capture',
                '(?=(A))([aA])(?=(?=.*(9)).(3))',    '_aB3_AB3_AB39_A_3_',  [6,1,  6,1, 6,1, 13,1, 8,1]);
  IsMatching('Three Ahead found before/after/nested-var-len-witch-nested-capture',
                '(?=(A))([aA](?=(?=.*(9)).(3)).)',    '_aB3_AB3_AB39_A_3_',  [6,2,  6,1, 6,2, 13,1, 8,1]);
  IsNotMatching('Third Ahead not found before/after/nested-var-len"',
                '(?=A)[aA](?=(?=.*Z).3)',    '_aB3_AB3_AB39_A_3_');


  // "(C)" may be matched, but then traced back // try all order-variations
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(A|B)(?=(?:(C)|D)x)',  '_AC_BDx_',       [5,1,  5,1, -1,-1]);
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(A|B)(?=(?:D|(C))x)',  '_AC_BDx_',       [5,1,  5,1, -1,-1]);
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(B|a)(?=(?:(C)|D)x)',  '_AC_BDx_',       [5,1,  5,1, -1,-1]);
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(B|a)(?=(?:D|(C))x)',  '_AC_BDx_',       [5,1,  5,1, -1,-1]);
  IsMatching(' Ahead / capture cleared after switching branch',
         '(A|B).x',  '_AC_BDx_',       [5,3,  5,1]);

  IsMatching('Two Ahead / capture cleared after switching branch',
                '(A|B)(?=(C)|D).x',  '_AC_BDx_',       [5,3,  5,1, -1,-1]);
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(A|B)(?=D|(C)).x',  '_AC_BDx_',       [5,3,  5,1, -1,-1]);
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(B|a)(?=(C)|D).x',  '_AC_BDx_',       [5,3,  5,1, -1,-1]);
  IsMatching('Two Ahead / capture cleared after switching branch',
                '(B|a)(?=D|(C)).x',  '_AC_BDx_',       [5,3,  5,1, -1,-1]);

  IsMatching('Ahead / acts atomic',
                'A(?=(bc)|(b))..\2',  '1Abcb__Ab_b_',  [8,4,  -1,-1, 9,1]);

  (* ***************************************************************************
   *** Negative look ahead
   ************************************************************************** *)

  // Match neg look-ahead: One look-ahead
  IsMatching('Neg-Ahead found after "A"',
                'A(?!2)',    'A2AB34AB_',                [3,1]);
  IsMatching('Neg-Ahead found after "."',
                '.(?![A2])',  'A2AB34AB_',               [3,1]);
  IsMatching('Neg-Ahead found after capture "(A)"',
                '(A)(?!2)',    'A2AB34AB_',              [3,1,  3,1]);

  IsMatching('Neg-Ahead found before "B"',
                '(?!.A)B',    '1BAB34',                  [4,1]);
  IsMatching('Neg-Ahead found before "(.)"',
                '(?!1)(.)',    '12AB34',                 [2,1,  2,1]);
  IsMatching('Neg-Ahead found, stand alone',
                '(?!1)',    '12AB34',                    [2,0]);

  IsMatching('Neg-Ahead found first at BOL',
                '(?!B)',    '12AB34',                    [1,0]);
  IsMatching('Neg-Ahead found "." first at EOL',
                '(.)(?!.)',    'B34',                    [3,1,  3,1]);

  IsMatching('Neg-Ahead found "\b" before "."',
                '(?!\b).',    'a   abc ',                [3,1]);
  IsMatching('Neg-Ahead found "\b" after "."',
                '.(?!\b)',    'a abc ',                  [3,1]);

  // Double negative // same as positive look ahead
  IsMatching('Neg-Ahead nested found after "A"',
                'A(?!(?!B))',    'A2AB34AB_',                [3,1]);

  IsNotMatching('Neg-Ahead not found after "A"',            'A(?![^X])',    'A2AB34AB_');
  IsNotMatching('Neg-Ahead not found after "A"',            'A(?![2B])',    'A2AB34AB_');
  IsNotMatching('Neg-Ahead not found after "."',            '.(?![^X]|$)',  'A2AB34AB_');
  IsNotMatching('Neg-Ahead not found before "."',           '(?![^X]|$).',  'A2AB34AB_');
  IsNotMatching('Neg-Ahead not found stand alone',          '(?![^X]|$)',   'A2AB34AB_');
  IsNotMatching('Neg-Ahead not found impossible',           '(?!.|$)',      'A2AB34AB_');
//  IsNotMatching('Neg-Ahead not found past EOL',             'c(?!..)',    'abcd');
//  IsNotMatching('Neg-Ahead not found past EOL stand alone', '(?=.....)',    'abcd');

  // Effect of ahead on MatchLen[0]
  IsMatching('Neg-Ahead found mid pattern (shorter)',
                'A(?!XX).',    '12ABB34',                 [3,2]);
  IsMatching('Neg-Ahead found mid pattern (same len)',
                'A(?!XX)..',    '12ABB34',                [3,3]);
  IsMatching('Neg-Ahead found mid pattern (longer)',
                'A(?!XX)...',    '12ABB34',               [3,4]);
  IsMatching('Neg-Ahead found, then capture in pattern (shorter)',
                '(A)(?!XX)(.)', '12ABB34',                [3,2,  3,1, 4,1]);
  IsMatching('Neg-Ahead found, then capture in pattern (same len)',
                '(A)(?!XX)(..)', '12ABB34',               [3,3,  3,1, 4,2]);
  IsMatching('Neg-Ahead found, then capture in pattern (longer)',
                '(A)(?!XX)(...)', '12ABB34',              [3,4,  3,1, 4,3]);

  // Match look-ahead: One look-ahead - variable len
  IsMatching('Neg-Ahead (var-len) found "B" after "A"',
                'A(?!_.*_)',    '_A_1_B_AxyzB123_A_',    [8,1]);
  IsMatching('Neg-Ahead (var-len) found no .. after "A"',
                'A(?!..)',    '_A_1_B_AxyzB123_A_',     [17,1]);
  IsMatching('Neg-Ahead (var-len) stand alone"',
                '(?!.*X$)',        '_A_1_B_AxyzB123_A_',     [1,0]);
  IsMatching('Neg-Ahead (var-len) stand alone, max len"',
                '(?!.{18}$)',        '_A_1_B_AxyzB123_A_',   [2,0]);
  IsNotMatching('Neg-Ahead (var-len) not found no"_" after "A"',
                'A(?!.*$)',    '_A_1_B_AxyzB123_A_');

  // Optional
  IsMatching('Neg-Ahead *',
                '(?!B)*A',    'BA',                [2,1]);
  IsMatching('Optional Neg-Ahead found after "A"',
                'A(?!.)?',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional Neg-Ahead found after "A"',
                'A(?!X)?',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional Neg-Ahead found stand alone',
                '(?!)?',     '_A2AB34AB_',                [1,0]);

  IsMatching('Optional * Neg-Ahead found after "A"',
                'A(?!.)*',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional * Neg-Ahead found after "A"',
                'A(?!X)*',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional * Neg-Ahead found stand alone',
                '(?!)*',     '_A2AB34AB_',                [1,0]);

  IsMatching('Optional {0} Neg-Ahead found after "A"',
                'A(?!.){0}',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional {0} Neg-Ahead found after "A"',
                'A(?!X){0}',    '_A2AB34AB_',                [2,1]);
  IsMatching('Optional {0} Neg-Ahead found stand alone',
                '(?!){0}',     '_A2AB34AB_',                [1,0]);



  // Match look-ahead: In capture and branch
  IsMatching('Neg-Ahead found for capture/branch"',
                '(A(?!x_)|Cd)',    '_Ax_Cd_ABB_',    [5,2,  5,2]);
  IsMatching('Neg-Ahead found for capture/branch"',
                '(Cd|A(?!x_))',    '_Ax_Cd_ABB_',    [5,2,  5,2]);

  IsNotMatching('Neg-Ahead found after branch capture',
                '(A|B)(?!D).x',  '_AC_BDx_');
  IsMatching('Neg-Ahead found after branch capture',
                '(A|B)(?![cd]).x',  '_AC_BDx_',       [5,3,  5,1]);
  IsMatching('Neg-Ahead found after branch capture',
                '(A|B)(?!c|d).x',  '_AC_BDx_',       [5,3,  5,1]);

  // Match look-ahead: One look-ahead - with capture(s)
  // Nothing captured
  IsMatching('Ahead found, with capture in look-ahead',
                '(A)(?!(\d))', 'A2AB34',                   [3,1,  3,1, -1,-1]);

  // Match look-ahead: Multiple look-ahead
  IsMatching('Two Neg-Ahead found from same pos',
                'A(?!X)(?!.2)',    'AB2AB34AB_A_3_',                [4,1]);
  IsMatching('Two Neg-Ahead found before/after"',
                '(?!a)[aA](?!.2)',    '_aB3_AB2_AB39_A_3_',         [10,1]);
  IsMatching('Three Neg-Ahead found before/after/nested',
                '(?!a)[aA](?!.2)(?!(?!..9).3)',    '_aB3_AB2_AB38_AB39_A_3_',  [15,1]);
  IsMatching('Three Neg-Ahead found before/after/nested"',
                '(?!a)[aA](?!.2)(?!.(?!.9)3)',    '_aB3_AB2_AB38_AB39_A_3_',  [15,1]);
  IsMatching('Three Neg-Ahead found before/after/nested-var-len',
                '(?!a)[aA](?!.2)(?!(?!.{0,4}9).3)',    '_aB3_AB2_AB38_AB39_A_3_',  [15,1]);

  // Test success without OP_LOOKAHEAD_END reached
  IsMatching('look-ahead (opt) in branch',     '^.(?:a|.(?=2)?|b)',    '.xb9',  [1,2]);
  IsMatching('look-ahead-neg (opt) in branch', '^.(?:a|.(?!.)?|b)',    '.xb9',  [1,2]);
  IsMatching('look-ahead-neg in branch',       '^.(?:a|.(?!Y)|b)',     '.xb9',  [1,2]);
end;

procedure TTestRegexpr.TestRegLookBehind;
begin
  re.AllowUnsafeLookBehind := True;
  (* ***************************************************************************
   *** look behind
   ************************************************************************** *)

  // Match look-behind: One look-behind
  IsMatching('behind found before "A"',
                '(?<=B)A',    'A2AB3BA_',                 [7,1]);
  IsMatching('behind found before "."',
                '(?<=B).',    'A2AB34AB_',                [5,1]);
  IsMatching('behind found before capture "(A)"',
                '(?<=B)(A)',    'A2AB34BA_',              [8,1,  8,1]);

  IsMatching('behind found after "B"',
                'B(?<=B)',    '12AB34',                   [4,1]);
  IsMatching('behind found after ".B"',
                '.B(?<=C.)',   '12AB34CB5',               [7,2]);
  IsMatching('behind found after "(.)"',
                '(.)(?<=B)',    '12AB34',                 [4,1,  4,1]);
  IsMatching('behind found, stand alone',
                '(?<=B)',    '12AB34',                    [5,0]);
  IsMatching('behind found, stand alone - full len',
                '(?<=....)',    '1234',                   [5,0]);

  // Anchors
  IsMatching('behind found first at BOL',
                '(.)(?<=B)',    'B34',                     [1,1,  1,1]);
  IsMatching('behind found "^" first at BOL',
                '(.)(?<=^.)',    'B34',                    [1,1,  1,1]);
  IsMatching('behind found "^" first at BOL',
                '(?<=^)(.)',    'B34',                     [1,1,  1,1]);
  IsMatching('behind found "$" first at EOL',
                '(.)(?<=$)',    'B34',                     [3,1,  3,1]);
  IsMatching('behind found "$" first at EOL',
                '(.)(?<=.*$)',    'B34',                   [3,1,  3,1]);
  IsNotMatching('behind found "$" first at EOL',
                '(?<=.$)(.)',    'B34');

  IsMatching('behind found "^" stand alone',
                '(?<=^)',    'B34',                    [1,0]);
  IsMatching('behind found "$" stand alone at offset',
                '(?<=$)',    'B34',                    [4,0], 2);
  IsMatching('behind found "$" stand alone',
                '(?<=$)',    'B34',                    [4,0]);
  IsMatching('behind found "\G" stand alone',
                '(?<=\G)',    'B34',                    [1,0]);
  IsMatching('behind found "\G" stand alone at offset',
                '(?<=\G)',    'B34',                    [3,0],  3);
  IsNotMatching('behind not found "^" stand alone at offset',
                '(?<=^)',    'B34',            2);



  IsMatching('behind found "\b" after "."',
                '.(?<=\b)',    '   abc ',                 [3,1]);
  IsMatching('behind found "\b" before "."',
                '(?<=\b).',    '   abc ',                 [4,1]);

  IsNotMatching('behind not found before "A"',            '(?<=X)A',    'A2AB34AB_');
  IsNotMatching('behind not found before "."',            '(?<=X).',    'A2AB34AB_');
  IsNotMatching('behind not found after "."',           '.(?<=X)',    'A2AB34AB_');
  IsNotMatching('behind not found stand alone',          '(?<=X)',     'A2AB34AB_');
  IsNotMatching('behind not found impossible',           '(?<=.^)',    'A2AB34AB_');
  IsNotMatching('behind not found past BOL',             '(?<=..)b',    'abcd');
  IsNotMatching('behind not found past BOL stand alone', '(?<=.....)',    'abcd');
  IsNotMatching('behind not found before "A=X"',          'A(?<=X)',    'A2AB34AB_');
  IsNotMatching('behind not found before "A=2" stand alone', '(?<=A)(?<=2)',    'A2AB34AB_');

  // Effect of behind on MatchLen[0]
  IsMatching('behind found mid pattern (shorter)',
                '.(?<=BB)A',    '12BBA34',                 [4,2]);
  IsMatching('behind found mid pattern (same len)',
                '..(?<=BB)A',    '12BBA34',                [3,3]);
  IsMatching('behind found mid pattern (longer)',
                '...(?<=BB)A',    '12BBA34',               [2,4]);
  IsMatching('behind found, then capture in pattern (shorter)',
                '(.)(?<=BB)(A)', '12BBA34',                [4,2,  4,1, 5,1]);
  IsMatching('behind found, then capture in pattern (same len)',
                '(..)(?<=BB)(A)', '12BBA34',               [3,3,  3,2, 5,1]);
  IsMatching('behind found, then capture in pattern (longer)',
                '(...)(?<=BB)(A)', '12BBA34',              [2,4,  2,3, 5,1]);

  // Match look-behind: One look-behind - variable len
  IsMatching('behind (var-len) found "B" before "A"',
                '(?<=B[^\d]*)A',    '_A_1_B1_AxyzB____A_',   [18,1]);
  IsMatching('behind (var-len) found no"A" before "A"',
                '.*(?<=^[^A]*)A',    '_A_1_B_AxyzB123_A_',   [1,2]);
  IsMatching('behind (var-len) stand alone"',
                '(?<=.*$)',        '_A_1_B_AxyzB123_A_',     [19,0]);
  IsMatching('behind (var-len) stand alone, max len"',
                '(?<=.{2}$)',        '_A_1_B_AxyzB123_A_',   [19,0]);
  IsNotMatching('behind (var-len) not found no"_" before "A"',
                '(?<=[^_]*$)A',    '_A_1_B_AxyzB123_A_');

  // Optional
  IsMatching('behind ',
                '(?<=X)?A',    'BA',                [2,1]);


  // Match look-behind: In capture and branch
  IsMatching('behind found for capture/branch"',
                '((?<=BB)A|Cd)',    '_Ax_Cd_BBA_',    [5,2,  5,2]);
  IsMatching('behind found for capture/branch"',
                '(Cd|(?<=BB)A)',    '_Ax_Cd_BBA_',    [5,2,  5,2]);
  IsMatching('behind found for capture/branch"',
                '((?<=BB)A|Cd)',    '_BBA_Cd_BBA_',    [4,1,  4,1]);
  IsMatching('behind found for capture/branch"',
                '(Cd|(?<=BB)A)',    '_BBA_Cd_BBA_',    [4,1,  4,1]);

  IsNotMatching('behind found before branch capture',
                '.(?<=C)(A|B)x',  '_CA_BDx_');
  IsMatching('behind found before branch capture',
                '.(?<=[CD])(A|B)x',  '_CA_DBx_',       [5,3,  6,1]);
  IsMatching('behind found before branch capture',
                '.(?<=C|D)(A|B)x',  '_CA_DBx_',       [5,3,  6,1]);


  // Match look-behind: One look-behind - with capture(s)
  IsMatching('behind found, with capture in look-behind',
                '(?<=(B))(A)', '12BA34',                   [4,1,  3,1, 4,1]);
  IsMatching('behind found, with capture in look-behind',
                '(?<=(B).)(2)', '1BA234',                  [4,1,  2,1, 4,1]);


  // Match look-behind: Multiple look-behind
  IsMatching('Two behind found from same pos',
                '(?<=B)(?<=3.)A',    'AB23BA4AB_A_3_',                [6,1]);
  IsMatching('Two behind found after/before"',
                '(?<=3)[aA](?<=A)',    '_3aB_2AB_3AB39_A_3_',         [11,1]);
  IsMatching('Three behind found after/before/nested',
                '(?<=(?<=9)3)[aA](?<=A)',    '_93aB_92AB_83AB_93AB_3_',  [19,1]);
  IsMatching('Three behind found after/before/nested"',
                '(?<=(?<=9)3.)[aA](?<=A)',    '_93_aB_92_AB_83_AB_93_AB_3_',  [23,1]);
  IsMatching('Three behind found after/before/nested"',
                '(?<=(?<=9).3)[aA](?<=A)',    '_9_3aB_9_2AB_8_3AB_9_3AB_3_',  [23,1]);
  IsMatching('Three behind found after/before/nested"',
                '(?<=(?<=9.)3)[aA](?<=A)',    '_9_3aB_9_2AB_8_3AB_9_3AB_3_',  [23,1]);
  IsMatching('Three behind found after/before/nested" with captures',
                '(?<=(?<=(9).)(3))([aA])(?<=(A))',    '_9_3aB_9_2AB_8_3AB_9_3AB_3_',  [23,1,   20,1, 22,1, 23,1, 23,1]);
  IsMatching('Three behind found after/before/nested" with captures',
                '(?<=(?<=.(9).)(3))([aA])(?<=(A))',    '_9_3aB_9_2AB_8_3AB_9_3AB_3_',  [23,1,   20,1, 22,1, 23,1, 23,1]);
  IsNotMatching('Three behind not found after/before/nested"',
                '(?<=(?<=X)3.)[aA](?<=A)',    '_93_aB_92_AB_83_AB_93_AB_3_');


  // "(C)" may be matched, but then traced back // try all order-variations
  IsMatching('Two behind / capture cleared before switching branch',
                '(?<=(?:(C)|D)x)(A|B)',  '_CA_DxB_',       [7,1,  -1,-1, 7,1]);
  IsMatching('Two behind / capture cleared before switching branch',
                '(?<=(?:D|(C))x)(A|B)',  '_CA_DxB_',       [7,1,  -1,-1, 7,1]);
  IsMatching('Two behind / capture cleared before switching branch',
                '(?<=(?:(C)|D)x)(B|a)',  '_CA_DxB_',       [7,1,  -1,-1, 7,1]);
  IsMatching('Two behind / capture cleared before switching branch',
                '(?<=(?:D|(C))x)(B|a)',  '_CA_DxB_',       [7,1,  -1,-1, 7,1]);

  (* ***************************************************************************
   *** Negative look behind
   ************************************************************************** *)

  // Match neg look-behind: One look-behind
  IsMatching('Neg-behind found before "A"',
                '(?<!2)A',    'A2AB34AB_',                [1,1]);
  IsMatching('Neg-behind found before "A"',
                '(?<!2)A',    'x2AB34AB_',                [7,1]);
  IsMatching('Neg-behind found before "."',
                '(?<![A2]).',  'A2AB34AB_',               [1,1]);
  IsMatching('Neg-behind found before "."',
                '(?<![A2]|^).',  'A2AB34AB_',               [5,1]);
  IsMatching('Neg-behind found before capture "(A)"',
                '(?<!2)(A)',    'A2AB34AB_',              [1,1,  1,1]);
  IsMatching('Neg-behind found before capture "(A)"',
                '(?<!2)(A)',    '_2AB34AB_',              [7,1,  7,1]);

  IsMatching('Neg-behind found after "B"',
                'B(?<!A.)',    'AB1B34',                  [4,1]);
  IsMatching('Neg-behind found after "(.)"',
                '(.)(?<!1)',    '12AB34',                 [2,1,  2,1]);
  IsMatching('Neg-behind found, stand alone at BOL',
                '(?<!1)',    '12AB34',                    [1,0]);
  IsMatching('Neg-behind found, stand alone',
                '(?<!^|1)',    '12AB34',                  [3,0]);

  IsMatching('Neg-behind found first at BOL',
                '(?<!B)',    '12AB34',                    [1,0]);
  IsMatching('Neg-behind found "." first at BOL',
                '(?<!.)(.)',    'B34',                    [1,1,  1,1]);

  IsMatching('Neg-behind found "\b" after "."',
                '.(?<!\b)',    'a   abc ',                [2,1]);
  IsMatching('Neg-behind found "\b" before "."',
                '(?<!\b).',    'a abc ',                  [4,1]);

  // Double negative // same as positive look behind
  IsMatching('Neg-behind nested found before "A"',
                '(?<!(?<!B))A',    'A2AB BA34AB_',                [7,1]);

  IsNotMatching('Neg-behind not found before "A"',            '(?<![^X]|^)A',    'A2AB34AB_');
  IsNotMatching('Neg-behind not found before "A"',            '(?<![24B])A',    'BA2AB34AB_');
  IsNotMatching('Neg-behind not found before "."',            '(?<![^X]|^).',  'A2AB34AB_');
  IsNotMatching('Neg-behind not found after "."',           '.(?<![^X]|^)',  'A2AB34AB_');
  IsNotMatching('Neg-behind not found stand alone',          '(?<![^X]|^)',   'A2AB34AB_');
  IsNotMatching('Neg-behind not found impossible',           '(?<!.|^)',      'A2AB34AB_');
//  IsNotMatching('Neg-behind not found past EOL',             'c(?<!..)',    'abcd');
//  IsNotMatching('Neg-behind not found past EOL stand alone', '(?<!.....)',    'abcd');

  // Effect of behind on MatchLen[0]
  IsMatching('Neg-behind found mid pattern (shorter)',
                '.(?<!XX)A',    '12BBA34',                 [4,2]);
  IsMatching('Neg-behind found mid pattern (same len)',
                '..(?<!XX)A',    '12BBA34',                [3,3]);
  IsMatching('Neg-behind found mid pattern (longer)',
                '...(?<!XX)A',    '12BBA34',               [2,4]);
  IsMatching('Neg-behind found, then capture in pattern (shorter)',
                '(.)(?<!XX)(A)', '12BBA34',                [4,2,  4,1, 5,1]);
  IsMatching('Neg-behind found, then capture in pattern (same len)',
                '(..)(?<!XX)(A)', '12BBA34',               [3,3,  3,2, 5,1]);
  IsMatching('Neg-behind found, then capture in pattern (longer)',
                '(...)(?<!XX)(A)', '12BBA34',              [2,4,  2,3, 5,1]);

  // Optional
  IsMatching('behind ',
                '(?<!)?A',    'BA',                [2,1]);
  IsNotMatching('behind ',
                '(?<!)A',    'BA' );

  // Match look-behind: One look-behind - variable len
  IsMatching('Neg-behind (var-len) found "B" after "A"',
                '.*(?<!_.*_)A',    '_A_1_B_AxyzB123_A_',    [1,2]);
  IsMatching('Neg-behind (var-len) stand alone"',
                '(?<!.*X\G)',        '_A_1_B_AxyzB123_A_',     [1,0]);
  IsNotMatching('Neg-behind (var-len) not found no"_" after "A"',
                '(?<!^.*)A',    '_A_1_B_AxyzB123_A_');


  // GREEDY and NOT...
  IsMatching('behind greedy',
                '(?<=(.*))X',       'abcX ',              [4,1,   1,3]);
  IsMatching('behind greedy',
                '(?<=(.+))X',       'abcX ',              [4,1,   1,3]);
  IsMatching('behind greedy',
                '(?<=(.{0,2}))X',   'abcX ',              [4,1,   2,2]);

  IsMatching('behind not greedy',
                '(?<=(.*?))X',      'abcX ',              [4,1,   4,0]);
  IsMatching('behind not greedy',
                '(?<=(.+?))X',      'abcX ',              [4,1,   3,1]);
  IsMatching('behind not greedy',
                '(?<=(.{0,2}?))X',  'abcX ',              [4,1,   4,0]);


  IsMatching('',  '(?<=.A...)(X)',  '_A123X3',              [6,1,   6,1]);
  IsMatching('',  '(?<=.A...)(X)',  '_A123X3',              [6,1,   6,1], 1);
  IsMatching('',  '(?<=.A...)(X)',  '_A123X3',              [6,1,   6,1], 2);
  IsMatching('',  '(?<=.A...)(X)',  '_A123X3',              [6,1,   6,1], 3);

  // Test success without OP_LOOKBEHIND_END reached
  IsMatching('look-behind (opt) in branch',     '^.(?:a|.(?<=Y)?|b)',    '.xb9',  [1,2]);
  IsMatching('look-behind-neg (opt) in branch', '^.(?:a|.(?<!.)?|b)',    '.xb9',  [1,2]);
  IsMatching('look-behind-neg in branch',       '^.(?:a|.(?<!Y)|b)',     '.xb9',  [1,2]);
end;

procedure TTestRegexpr.TestRegLookAroundMixed;
begin
  IsMatching('behind (var-len) stand alone"',
                '(?<=(?=.*$))',        '_A_1_B_AxyzB123_A_',     [1,0]);
  IsMatching('behind (var-len) stand alone"',
                '(?<=^.*(?=.*$))',        '_A_1_B_AxyzB123_A_',     [1,0]);
  IsMatching('behind (var-len) stand alone"',
                '(?<=^(?=.*$).*)',        '_A_1_B_AxyzB123_A_',     [1,0]);

  IsMatching('behind (var-len) ',
                '(?<=(?=.*$))B',        '_A_1_B_AxyzB123_A_',        [6,1]);
  IsMatching('behind (var-len) ',
                '(?<=^.*(?=.*$))B',        '_A_1_B_AxyzB123_A_',     [6,1]);
  IsMatching('behind (var-len) ',
                '(?<=^(?=.*$).*)B',        '_A_1_B_AxyzB123_A_',     [6,1]);

  IsNotMatching('behind not found before "A=2" for dot', '(?=A).(?<=2)',    'A2AB34AB_');

end;

procedure TTestRegexpr.TestResetMatchPos;
begin
  IsMatching('Set Matchstart middle',          'a\Kb',  'ab',    [2,1]);
  IsMatching('Set Matchstart end of match',    'a\K',   'ab',    [2,0]);
  IsMatching('Set Matchstart begin of match',  '\Ka',   'ab',    [1,1]);
  IsMatching('Set Matchstart stand-alone',     '\K',    'ab',    [1,0]);

  IsMatching('Set Matchstart middle',          'a\Kb',  'xab',    [3,1]);
  IsMatching('Set Matchstart end of match',    'a\K',   'xab',    [3,0]);
  IsMatching('Set Matchstart begin of match',  '\Ka',   'xab',    [2,1]);
  IsMatching('Set Matchstart stand-alone',     '\K',    'xab',    [1,0]);
  IsMatching('Set Matchstart stand-alone with offset',  '\K',    'xabde',    [3,0], 3);

  IsMatching('Set Matchstart EOL',     '$\K',    'xab',    [4,0]);
  IsMatching('Set Matchstart EOL',     '\K$',    'xab',    [4,0]);


  IsMatching('backtracking',     '(?:.\K)*?b',    'xabc',    [3,1]);
  IsMatching('backtracking',     '(?:.\K)*b',     'xabc',    [3,1]);


  IsMatching('backtracking',     '^(?=.*\Kx)?.*c',    'abcd',    [1,3]);
  IsMatching('backtracking',     '^(?=\K.*x)?.*c',    'abcd',    [1,3]);
  IsMatching('backtracking',     '^(?:.*\Kx)?.*c',    'abcd',    [1,3]);
  IsMatching('backtracking',     '^(?:\K.*x)?.*c',    'abcd',    [1,3]);

  IsMatching('multiple \K',     '^a\Kb\Kc',    'abcd',    [3,1]);

  IsNotMatching('lookahead not possible',   '^(?=.*\K).*c',    'abcd');
  IsMatching('lookahead',   '^(?=.\K).*c',    'abcd',    [2,2]);
  IsMatching('lookbehind',  '(?<=\K.)c',    'abcd',    [2,2]);

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

procedure TTestRegexpr.RunTest73;
begin
  RunRETest(73);
end;

procedure TTestRegexpr.RunTest74;
begin
  RunRETest(74);
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
  FErrorInfo := LineEnding + AExpression;
  if (RE = Nil) then
  begin
    RE := TTestableRegExpr.Create;
    RE.ReplaceLineEnd := #10;
  end;
  RE.Expression := AExpression;
  RE.Compile;
  FErrorInfo := LineEnding + AExpression + LineEnding + RE.Dump(2);
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

