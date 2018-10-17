{%RunFlags MESSAGES+}
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
  LCLIntf, RegExpr in '../src/RegExpr.pas';

type
    TTestCase = record
        expression: string;
        inputText: string;
        substitutionText: string;
        expectedResult: string;
        matchStart: integer;
      end;

const
    testCases: array [0..24] of TTestCase = (
              (
              expression: '\nd';
              inputText: 'abc'#13#10'def';
              substitutionText: '\n\x{10}\r\\';
              expectedResult: 'abc'#13#10#16#13'\ef'
              ),
              (
              expression: '(\w*)';
              inputText: 'name.ext';
              substitutionText: '$1.new';
              expectedResult: 'name.new.new.ext.new.new'
              ),
              (
              expression: #$d'('#$a')';
              inputText: 'word'#$d#$a;
              substitutionText: '$1';
              expectedResult: 'word'#$a
              ),
              (
              expression: '(word)';
              inputText: 'word';
              substitutionText: '\U$1\\r';
              expectedResult: 'WORD\r'
              ),
              (
              expression: '(word)';
              inputText: 'word';
              substitutionText: '$1\n';
              expectedResult: 'word'#$a
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
              )
    );

procedure TRegExprTestCase.TestHookUp;

    function PrintableString(AString: string): string;
      var
          ch: Char;
      begin
        Result := '';
        for ch in AString do begin
          if ch < #31 then Result := Result + '#' + IntToStr(Ord(ch))
            else Result := Result + ch;
        end;
      end;

    function CheckRE (const testCase: TTestCase; const r: TRegExpr): string;
      begin
        // AssertEquals(r.SubExprMatchCount, ASubExprMatchCount);
        // AssertEquals(r.MatchPos [0], APos);
        // AssertEquals(r.MatchLen [0], ALen);
        r.Exec(testCase.inputText);
        if (r.MatchPos [0] <> testCase.matchStart) or (r.Match [0] <> testCase.expectedResult) then begin
            Result :=
                 'Expected matching string "' + PrintableString(testCase.expectedResult) + '" at position ' +
                 IntToStr(testCase.matchStart) + ', ' +
                 'actual match: "' + PrintableString(r.Match [0]) +
                 '" at position ' + IntToStr(r.MatchPos [0]);
        end;
      end;

    function CheckREReplace(const testCase: TTestCase; const r: TRegExpr): string;
      var
          actualResult: string;
      begin
        if (testCase.substitutionText <> '') then begin
          actualResult := r.Replace(testCase.inputText, testCase.substitutionText, True);
          if (actualResult <> testCase.expectedResult) then begin
              Result :=
                      'Test *FAILED*: expected "' +
                      PrintableString(testCase.expectedResult) + '", actual: "' +
                      PrintableString(actualResult);
          end;
        end;
      end;

    var
        r : TRegExpr;
        testCase: TTestCase;
        checkResult: string;

    begin
      r := TRegExpr.Create;

      for testCase in testCases do begin
        r.Expression := testCase.expression;
        r.Compile;
        if (testCase.substitutionText <> '')
            then checkResult := CheckREReplace(testCase, r)
            else checkResult := CheckRE(testCase, r);
        if (checkResult <> '') then begin
            writeln('Modifiers "', r.ModifierStr, '"');
            writeln('Regular expression: ', testCase.expression);
            writeln('... compiled into p-code:');
            writeln(r.Dump);
            writeln('Input text: "', PrintableString(testCase.inputText), '"');
            if (testCase.substitutionText <> '')
                then writeln('Substitution text: "', PrintableString(testCase.substitutionText), '"');
            writeln('***** Error: ', checkResult, ' *****');
            HALT(1);
        end;
      end;

      writeln;
      writeln('===== ', length(testCases), ' tests passed =====');
      writeln;
    end;

initialization
  RegisterTest(TRegExprTestCase);
end.
