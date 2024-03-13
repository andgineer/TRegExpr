|     |                                                      |                                                                   |         |                                                                     |                                                                    |                                                                    |
|-----|------------------------------------------------------|-------------------------------------------------------------------|---------|---------------------------------------------------------------------|--------------------------------------------------------------------|--------------------------------------------------------------------|
|     | [Englisch](https://regex.sorokin.engineer/tregexpr/) | [Русский](https://regex.sorokin.engineer/ru/tregexpr/) | Deutsch | [Български](https://regex.sorokin.engineer/bg/tregexpr/) | [Français](https://regex.sorokin.engineer/fr/tregexpr/) | [Spanisch](https://regex.sorokin.engineer/es/tregexpr/) |

# TRegExpr

To use it, copy files "regexpr.pas", "regexpr_unicodedata.pas",
"regexpr_compilers.inc", to your project folder.

The library is already included into [Lazarus (Free
Pascal)](http://wiki.freepascal.org/Regexpr) project so you do not need
to copy anything if you use [Lazarus](https://www.lazarus-ide.org/).

## Klasse TRegExpr

### Haupt- und Nebenversion

Return major and minor version of the component.

    VersionMajor = 1
    VersionMinor = 101

### Ausdruck

Ein regulärer Ausdruck.

For optimization, regular expression is automatically compiled into
P-code. Human-readable form of the P-code is returned by [Dump](#dump).

In case of any errors in compilation, `Error` method is called (by
default `Error` raises exception [ERegExpr](#eregexpr)).

### ModifierStr

Set or get values of [regular expression
modifiers](regular_expressions.md#modifiers).

Format of the string is similar to
[(?ismx-ismx)](regular_expressions.md#inlinemodifiers). For example
`ModifierStr := ‘i-x’` will switch on the modifier
[/i](regular_expressions.md#i), switch off
[/x](regular_expressions.md#x) and leave unchanged others.

Wenn Sie versuchen, einen nicht unterstützten Modifikator einzustellen,
wird `Error` aufgerufen.

### ModifierI

[Modifier /i, "case-insensitive"](regular_expressions.md#i),
initialized with [RegExprModifierI](#regexprmodifieri) value.

### ModifierR

[Modifier /r, "Russian range extension"](regular_expressions.md#r),
initialized with [RegExprModifierR](#regexprmodifierr) value.

### ModifierS

[Modifier /s, "single line strings"](regular_expressions.md#s),
initialized with [RegExprModifierS](#regexprmodifiers) value.

### ModifierG

[Modifier /g, "greediness"](regular_expressions.md#g), initialized
with [RegExprModifierG](#regexprmodifierg) value.

### ModifierM

[Modifier /m, "multi-line strings"](regular_expressions.md#m),
initialized with [RegExprModifierM](#regexprmodifierm) value.

### ModifierX

[Modifier /x, "eXtended syntax"](regular_expressions.md#x),
initialized with [RegExprModifierX](#regexprmodifierx) value.

### Exec

Finds regular expression against `AInputString`, starting from the
beginning.

The overloaded `Exec` version without `AInputString` exists, it uses
`AInputString` from previous call.

See also global function [ExecRegExpr](#execregexpr) that you can use
without explicit `TRegExpr` object creation.

### ExecNext

Finds next match. If parameter `ABackward` is True, it goes downto
position 1, ie runs backward search.

Without parameter it works the same as:

    if SpielLen [0] = 0
      then ExecPos (SpielPos [0] + 1)
      else ExecPos (SpielPos [0] + SpielLen [0]);

Raises exception if used without preceeding successful call to
[Exec](#exec), [ExecPos](#execpos) or [ExecNext](#execnext).

So you always must use something like:

    if Exec(InputString)
      then
        repeat
          { proceed results}
        until not ExecNext;

### ExecPos

Finds match for `AInputString` starting from `AOffset` position
(1-based).

Parameter `ABackward` means going from `AOffset` downto 1, ie backward
search.

Parameter `ATryOnce` means that testing for regex will be only at the
initial position, without going to next/previous positions.

### InputString

Returns current input string (from last [Exec](#exec) call or last
assign to this property).

Jede Zuweisung zu dieser Eigenschaft setzt [Spiel](#spiel),
[SpielPos](#spielpos) und [SpielLen](#spiellen) zurück.

### Substitute

    function Substitute (const ATemplate: RegExprString): RegExprString;

Returns `ATemplate`, where `$&` or `$0` are replaced with the found
match, and `$1` to `$9` are replaced with found groups 1 to 9.

To use in template the characters `$` or `\`, escape them with a
backslash `\`, like `\\` or `\$`.

| Symbol       | Description                                   |
|--------------|-----------------------------------------------|
| `$&`         | ganze Übereinstimmung des regulären Ausdrucks |
| `$0`         | ganze Übereinstimmung des regulären Ausdrucks |
| `$1` .. `$9` | contents of numbered group 1 .. 9             |
| `\n`         | in Windows durch `\r\n` ersetzt               |
| `\l`         | lowercase one next char                       |
| `\L`         | Kleinbuchstaben alle Zeichen danach           |
| `\u`         | uppercase one next char                       |
| `\U`         | Großbuchstaben alle Zeichen danach            |

    '1\$ is $2\\rub\\' -> '1$ is <Spiel[2]>\rub\'
    '\U$1\\r' transforms into '<Spiel[1] in uppercase>\r'

If you want to place raw digit after ‘\$n’ you must delimit `n` with
curly braces `{}`.

    'a$12bc' -> 'a<Spiel[12]>bc'
    'a${1}2bc' -> 'a<Spiel[1]>2bc'.

To use found named groups, use syntax `${name}`, where "name" is valid
identifier of previously found named group (starting with non-digit).

### Split

Splits `AInputStr` into `APieces` by regex occurrences.

Ruft intern [Exec](#exec) / [ExecNext](#execnext) auf

See also global function [SplitRegExpr](#splitregexpr) that you can use
without explicit `TRegExpr` object creation.

<a name="Replace"></a>

### Replace, ReplaceEx

    function Replace (Const AInputStr : RegExprString;
      const AReplaceStr : RegExprString;
      AUseSubstitution : boolean= False)
     : RegExprString; overload;

    function Replace (Const AInputStr : RegExprString;
      AReplaceFunc : TRegExprReplaceFunction)
     : RegExprString; overload;

    function ReplaceEx (Const AInputStr : RegExprString;
      AReplaceFunc : TRegExprReplaceFunction):
      RegExprString;

Returns the string with regex occurencies replaced by the replace
string.

If last argument (`AUseSubstitution`) is true, then `AReplaceStr` will
be used as template for Substitution methods.

    Ausdruck := '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*';
    Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);

Liefert `def "BLOCK" Wert &quot;test1&quot;`

    Ersetzen (&#39;BLOCK (test1)&#39;, &#39;def &quot;$ 1&quot; Wert &quot;$ 2&quot;&#39;, False)

Liefert `def &quot;$ 1&quot; Wert &quot;$ 2&quot;`

Ruft intern [Exec](#exec) / [ExecNext](#execnext) auf

Overloaded version and `ReplaceEx` operate with callback function, so
you can implement really complex functionality.

See also global function [ReplaceRegExpr](#replaceregexpr) that you can
use without explicit `TRegExpr` object creation.

### SubExprSpielCount

Count of groups (subexpressions) found in last [Exec](#exec) /
[ExecNext](#execnext) call.

If there are no groups found, but some string was found (Exec\* returned
True), it returns 0. If no groups nor some string were found
([Exec](#exec) / [ExecNext](#execnext) returned false), it returns -1.

Note, that some group may be not found, and for such group
`MathPos=SpielLen=-1` and `Spiel=’’`.

    Ausdruck := '(1)?2(3)?';
    Exec ('123'): SubExprSpielCount=2, Spiel[0]='123', [1]='1', [2]='3'

    Exec ('12'): SubExprSpielCount=1, Spiel[0]='12', [1]='1'

    Exec ('23'): SubExprSpielCount=2, Spiel[0]='23', [1]='', [2]='3'

    Exec ('2'): SubExprSpielCount=0, Spiel[0]='2'

    Exec ('7') - return False: SubExprSpielCount=-1

### SpielPos

Position (1-based) of group with specified index. Result is valid only
after some match was found. First group has index 1, the entire match
has index 0.

Returns -1 if no group with specified index was found.

### SpielLen

Length of group with specified index. Result is valid only after some
match was found. First group has index 1, the entire match has index 0.

Returns -1 if no group with specified index was found.

### Spiel

String of group with specified index. First group has index 1, the
entire match has index 0. Returns empty string, if no such group was
found.

### SpielIndexFromName

Returns group index (1-based) from group name, which is needed for
"named groups". Returns -1 if no such named group was found.

### LastError

Returns Id of last error, or 0 if no errors occured (unusable if `Error`
method raises exception). It also clears internal status to 0 (no
errors).

### ErrorMsg

Gibt die `Error`-Nachricht für einen Fehler mit `ID = AErrorID` zurück.

### KompilierenrErrorPos

Returns position in regex, where P-code compilation was stopped.

Useful for error diagnostics.

### SpaceChars

Contains chars, treated as `\s` (initially filled with
[RegExprSpaceChars](#regexprspacechars) global constant).

### WordChars

Contains chars, treated as `\w` (initially filled with
[RegExprWordChars](#regexprwordchars) global constant).

### LineSeparators

Line separators (like `\n` in Unix), initially filled with
[RegExprLineSeparators]() global constant).

See also [Line Boundaries](regular_expressions.md#lineseparators)

### UseLinePairedBreak

Boolean property, enables to detect paired line separator CR LF.

See also [Line Boundaries](regular_expressions.md#lineseparators)

For example, if you need only Unix-style separator LF, assign
`LineSeparators := #$a` and `UseLinePairedBreak := False`.

If you want to accept as line separators only CR LF but not CR or LF
alone, then assign `LineSeparators := ''` (empty string) and
`UseLinePairedBreak := True`.

By default, "mixed" mode is used (defined in RegExprLineSeparators
global constant):

    LineSeparators := #$d#$a; 
    UseLinePairedBreak := True;

Behaviour of this mode is described in the [Line
Boundaries](regular_expressions.md#lineseparators).

### Kompilieren

Kompilierens regular expression to internal P-code.

Useful for example for GUI regular expressions editors - to check
regular expression without using it.

### Dump

Shows P-code (compiled regular expression) as human-readable string.

## Globale Konstanten

### EscChar

Escape character, by default backslash `'\'`.

### SubstituteGroupChar

Char used to prefix groups (numbered and named) in Substitute method, by
default `'$'`.

### RegExprModifierI

[Modifier i](regular_expressions.md#i) default value.

### RegExprModifierR

[Modifier r](regular_expressions.md#r) default value.

### RegExprModifierS

[Modifier s](regular_expressions.md#s) default value.

### RegExprModifierG

[Modifier g](regular_expressions.md#g) default value.

### RegExprModifierM

[Modifier m](regular_expressions.md#m) default value.

### RegExprModifierX

[Modifier x](regular_expressions.md#x) default value.

### RegExprSpaceChars

Default for [SpaceChars](#spacechars) property.

### RegExprWordChars

Default value for [WordChars](#wordchars) property.  
RegExprLineSeparators \~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~~

Default value for [LineSeparators](#lineseparators) property.

## Globale Funktionen

All this functionality is available as methods of `TRegExpr`, but with
global functions you do not need to create `TReExpr` instance so your
code would be more simple if you just need one function.

### ExecRegExpr

Returns True if the string matches the regular expression. Just like
[Exec](#exec) in `TRegExpr`.

### SplitRegExpr

Splits the string by regular expression occurences. See also
[Split](#split) if you prefer to create `TRegExpr` instance explicitly.

### ReplaceRegExpr

    function ReplaceRegExpr (
        const ARegExpr, AInputStr, AReplaceStr : RegExprString;
        AUseSubstitution : boolean= False
    ) : RegExprString; overload;

    Type
      TRegexReplaceOption = (rroModifierI,
                             rroModifierR,
                             rroModifierS,
                             rroModifierG,
                             rroModifierM,
                             rroModifierX,
                             rroUseSubstitution,
                             rroUseOsLineEnd);
      TRegexReplaceOptions = Set of TRegexReplaceOption;

    function ReplaceRegExpr (
        const ARegExpr, AInputStr, AReplaceStr : RegExprString;
        Options :TRegexReplaceOptions
    ) : RegExprString; overload;

Returns the string with regular expressions replaced by the
`AReplaceStr`. See also [Replace]() if you prefer to create TRegExpr
instance explicitly.

If last argument (`AUseSubstitution`) is True, then `AReplaceStr` will
be used as template for `Substitution methods`:

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"',
      True
    )

Returns `def 'BLOCK' value 'test1'`

Aber dieses hier (Anmerkung: Es gibt kein letztes Argument):

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"'
    )

Liefert `def &quot;$ 1&quot; Wert &quot;$ 2&quot;`

#### Version mit Optionen

With `Options` you control `\n` behaviour (if `rroUseOsLineEnd` then
`\n` is replaced with `\n\r` in Windows and `\n` in Linux). And so on.

``` pascal
Type
  TRegexReplaceOption = (rroModifierI,
                         rroModifierR,
                         rroModifierS,
                         rroModifierG,
                         rroModifierM,
                         rroModifierX,
                         rroUseSubstitution,
                         rroUseOsLineEnd);
```

### QuoteRegExprMetaChars

Replace all metachars with its safe representation, for example
`abc'cd.(` is converted to `abc\'cd\.\(`

This function is useful for regex auto-generation from user input.

### RegExprSubAusdrucks

Makes list of subexpressions found in `ARegExpr`.

In `ASubExps` every item represents subexpression, from first to last,
in format:

 String - Unterausdruck (ohne &#39;()&#39;)

 Low word of Object - starting position in ARegExpr, including ‘(’ if
exists! (first position is 1)

 High word of Object - length, including starting ‘(’ and ending ‘)’ if
exist!

`AExtendedSyntax` - must be True if modifier `/x` os on, while using the
regex.

Usefull for GUI editors of regex (you can find example of usage in
[REStudioMain.pas](https://github.com/andgineer/TRegExpr/blob/74ab342b639fc51941a4eea9c7aa53dcdf783592/restudio/REStudioMain.pas#L474))

=========== ======= Ergebniscode Bedeutung =========== ======= 0        
  Success. No unbalanced brackets were found. -1          Not enough
closing brackets `)`. - (n + 1)      At position n it was found opening
`[` without corresponding closing `]`. n           At position n it was
found closing bracket `)` without corresponding opening `(`. ===========
======= 

If `Result <> 0`, then `ASubExprs` can contain empty items or illegal
ones.

## ERegExpr

    ERegExpr = class (Exception)
      public
       ErrorCode : integer; // error code. Compilation error codes are before 1000
       KompilierenrErrorPos : integer; // Position in r.e. where compilation error occured
     end;

## Unicode

In Unicode mode, all strings (InputString, Ausdruck, internal strings)
are of type UnicodeString/WideString, instead of simple "string".
Unicode slows down performance, so use it only if you really need
Unicode support.

To use Unicode, uncomment `{$DEFINE UniCode}` in
[regexpr.pas](https://github.com/andgineer/TRegExpr/blob/29ec3367f8309ba2ecde7d68d5f14a514de94511/src/RegExpr.pas#L86)
(remove `off`).
