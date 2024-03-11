|     |                                                     |                                                                   |                                                                   |                                                                     |                                                                    |                                                                   |
|-----|-----------------------------------------------------|-------------------------------------------------------------------|-------------------------------------------------------------------|---------------------------------------------------------------------|--------------------------------------------------------------------|-------------------------------------------------------------------|
|     | [Anglais](https://regex.sorokin.engineer/tregexpr/) | [Русский](https://regex.sorokin.engineer/ru/tregexpr/) | [Deutsch](https://regex.sorokin.engineer/de/tregexpr/) | [Български](https://regex.sorokin.engineer/bg/tregexpr/) | [Français](https://regex.sorokin.engineer/fr/tregexpr/) | [Español](https://regex.sorokin.engineer/es/tregexpr/) |

# TRegExpr

Implements [regular expressions](../regular_expressions/) in pure
Pascal. Compatible with Free Pascal, Delphi 2-7, C++Builder 3-6.

To use it, copy files "regexpr.pas", "regexpr_unicodedata.pas",
"regexpr_compilers.inc", to your project folder.

The library is already included into [Lazarus (Free
Pascal)](http://wiki.freepascal.org/Regexpr) project so you do not need
to copy anything if you use [Lazarus](https://www.lazarus-ide.org/).

## Classe TRegExpr

### VersionMajor, VersionMinor

Return major and minor version of the component.

    VersionMajor = 1
    VersionMinor = 101

### Expression

Expression régulière.

For optimization, regular expression is automatically compiled into
P-code. Human-readable form of the P-code is returned by
[Déverser](#déverser).

In case of any errors in compilation, `Error` method is called (by
default `Error` raises exception [ERegExpr](#eregexpr)).

### Modificateurstr

Set or get values of [regular expression
modifiers](../regular_expressions/#modifiers).

Format of the string is similar to
[(?ismx-ismx)](../regular_expressions/#inlinemodifiers). For example
`Modificateurstr := ‘i-x’` will switch on the modifier
[/i](../regular_expressions/#i), switch off
[/x](../regular_expressions/#x) and leave unchanged others.

Si vous essayez de définir un modificateur non supporté, `Error` sera
appelé.

### Modificateur

[Modifier /i, "case-insensitive"](../regular_expressions/#i),
initialized with [RegExprModificateur](#regexprmodificateur-3) value.

### Modificateur

[Modifier /r, "Russian range extension"](../regular_expressions/#r),
initialized with [RegExprModificateur](#regexprmodificateur-3) value.

### Modificateurs

[Modifier /s, "single line strings"](../regular_expressions/#s),
initialized with [RegExprModificateurs](#regexprmodificateurs) value.

### Modificateur

[Modifier /g, "greediness"](../regular_expressions/#g), initialized
with [RegExprModificateur](#regexprmodificateur-3) value.

### Modificateur

[Modifier /m, "multi-line strings"](../regular_expressions/#m),
initialized with [RegExprModificateur](#regexprmodificateur-3) value.

### ModificateurX

[Modifier /x, "eXtended syntax"](../regular_expressions/#x),
initialized with [RegExprModificateurX](#regexprmodificateurx) value.

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

    if RencontreLen [0] = 0
      then ExecPos (RencontrePos [0] + 1)
      else ExecPos (RencontrePos [0] + RencontreLen [0]);

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

Toute affectation à cette propriété efface [Rencontre](#rencontre),
[RencontrePos](#rencontrepos) et [RencontreLen](#rencontrelen).

### Remplacer

    function Remplacer (const ATemplate : RegExprString) : RegExprString;

Returns `ATemplate`, where `$&` or `$0` are replaced with the found
match, and `$1` to `$9` are replaced with found groups 1 to 9.

To use in template the characters `$` or `\`, escape them with a
backslash `\`, like `\\` or `\$`.

| Symbol       | Description                              |
|--------------|------------------------------------------|
| `$&`         | match entier expression régulière        |
| `$0`         | match entier expression régulière        |
| `$1` .. `$9` | contents of numbered group 1 .. 9        |
| `\n`         | dans Windows remplacé par `\r\n`         |
| `\l`         | lowercase one next char                  |
| `\L`         | minuscule tous les caractères après cela |
| `\u`         | uppercase one next char                  |
| `\U`         | majuscule tous les caractères après cela |

    '1\$ is $2\\rub\\' -> '1$ is <Rencontre[2]>\rub\'
    '\U$1\\r' transforms into '<Rencontre[1] in uppercase>\r'

If you want to place raw digit after ‘\$n’ you must delimit `n` with
curly braces `{}`.

    'a$12bc' -> 'a<Rencontre[12]>bc'
    'a${1}2bc' -> 'a<Rencontre[1]>2bc'.

To use found named groups, use syntax `${name}`, where "name" is valid
identifier of previously found named group (starting with non-digit).

### Divisé

Divisés `AInputStr` into `APieces` by regex occurrences.

Appelle en interne [Exec](#exec) / [ExecNext](#execnext)

See also global function [DiviséRegExpr](#diviséregexpr) that you can
use without explicit `TRegExpr` object creation.

<a name="Replace"></a>

### Remplacer, RemplacerEx

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

    Expression := '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*';
    Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);

Retourne `def "BLOCK" valeur "test1"`

    Remplacez (&#39;BLOCK (test1)&#39;, &#39;def &quot;$ 1&quot;, valeur &quot;$ 2&quot;&#39;, False)

Retourne `def &quot;$ 1&quot; valeur &quot;$ 2&quot;`

Appelle en interne [Exec](#exec) / [ExecNext](#execnext)

Overloaded version and `ReplaceEx` operate with callback function, so
you can implement really complex functionality.

See also global function [ReplaceRegExpr](#replaceregexpr) that you can
use without explicit `TRegExpr` object creation.

### SubExprRencontreCount

Count of groups (subexpressions) found in last [Exec](#exec) /
[ExecNext](#execnext) call.

If there are no groups found, but some string was found (Exec\* returned
True), it returns 0. If no groups nor some string were found
([Exec](#exec) / [ExecNext](#execnext) returned false), it returns -1.

Note, that some group may be not found, and for such group
`MathPos=RencontreLen=-1` and `Rencontre=’’`.

    Expression := '(1)?2(3)?';
    Exec ('123'): SubExprRencontreCount=2, Rencontre[0]='123', [1]='1', [2]='3'

    Exec ('12'): SubExprRencontreCount=1, Rencontre[0]='12', [1]='1'

    Exec ('23'): SubExprRencontreCount=2, Rencontre[0]='23', [1]='', [2]='3'

    Exec ('2'): SubExprRencontreCount=0, Rencontre[0]='2'

    Exec ('7') - return False: SubExprRencontreCount=-1

### RencontrePos

Position (1-based) of group with specified index. Result is valid only
after some match was found. First group has index 1, the entire match
has index 0.

Returns -1 if no group with specified index was found.

### RencontreLen

Length of group with specified index. Result is valid only after some
match was found. First group has index 1, the entire match has index 0.

Returns -1 if no group with specified index was found.

### Rencontre

String of group with specified index. First group has index 1, the
entire match has index 0. Returns empty string, if no such group was
found.

### RencontreIndexFromName

Returns group index (1-based) from group name, which is needed for
"named groups". Returns -1 if no such named group was found.

### LastError

Returns Id of last error, or 0 if no errors occured (unusable if `Error`
method raises exception). It also clears internal status to 0 (no
errors).

### ErrorMsg

Renvoie le message `Error` en cas d&#39;erreur avec `ID = AErrorID`.

### CompilerrErrorPos

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

See also [Line Boundaries](../regular_expressions/#lineseparators)

### UseLinePairedBreak

Boolean property, enables to detect paired line separator CR LF.

See also [Line Boundaries](../regular_expressions/#lineseparators)

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
Boundaries](../regular_expressions/#lineseparators).

### Compiler

Compilers regular expression to internal P-code.

Useful for example for GUI regular expressions editors - to check
regular expression without using it.

### Déverser

Shows P-code (compiled regular expression) as human-readable string.

## Constantes globales

### EscChar

Escape character, by default backslash `'\'`.

### RemplacerGroupChar

Char used to prefix groups (numbered and named) in Remplacer method, by
default `'$'`.

### RegExprModificateur

[Modifier i](../regular_expressions/#i) default value.

### RegExprModificateur

[Modifier r](../regular_expressions/#r) default value.

### RegExprModificateurs

[Modifier s](../regular_expressions/#s) default value.

### RegExprModificateur

[Modifier g](../regular_expressions/#g) default value.

### RegExprModificateur

[Modifier m](../regular_expressions/#m) default value.

### RegExprModificateurX

[Modifier x](../regular_expressions/#x) default value.

### RegExprSpaceChars

Default for [SpaceChars](#spacechars) property.

### RegExprWordChars

Default value for [WordChars](#wordchars) property.  
RegExprLineSeparators \~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~~

Default value for [LineSeparators](#lineseparators) property.

## Fonctions globales

All this functionality is available as methods of `TRegExpr`, but with
global functions you do not need to create `TReExpr` instance so your
code would be more simple if you just need one function.

### ExecRegExpr

Returns True if the string matches the regular expression. Just like
[Exec](#exec) in `TRegExpr`.

### DiviséRegExpr

Divisés the string by regular expression occurences. See also
[Divisé](#divisé) if you prefer to create `TRegExpr` instance
explicitly.

### ReplaceRegExpr

    function ReplaceRegExpr (
        const ARegExpr, AInputStr, AReplaceStr : RegExprString;
        AUseSubstitution : boolean= False
    ) : RegExprString; overload;

    Type
      TRegexReplaceOption = (rroModificateur,
                             rroModificateur,
                             rroModificateurs,
                             rroModificateur,
                             rroModificateur,
                             rroModificateurX,
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

Mais celui-ci (notez qu&#39;il n&#39;y a pas de dernier argument):

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"'
    )

Retourne `def &quot;$ 1&quot; valeur &quot;$ 2&quot;`

#### Version avec options

With `Options` you control `\n` behaviour (if `rroUseOsLineEnd` then
`\n` is replaced with `\n\r` in Windows and `\n` in Linux). And so on.

``` pascal
Type
  TRegexReplaceOption = (rroModificateur,
                         rroModificateur,
                         rroModificateurs,
                         rroModificateur,
                         rroModificateur,
                         rroModificateurX,
                         rroUseSubstitution,
                         rroUseOsLineEnd);
```

### QuoteRegExprMetaChars

Replace all metachars with its safe representation, for example
`abc'cd.(` is converted to `abc\'cd\.\(`

This function is useful for regex auto-generation from user input.

### RegExprSubExpressions

Makes list of subexpressions found in `ARegExpr`.

In `ASubExps` every item represents subexpression, from first to last,
in format:

 String - texte de sous-expression (sans &#39;()&#39;)

 Low word of Object - starting position in ARegExpr, including ‘(’ if
exists! (first position is 1)

 High word of Object - length, including starting ‘(’ and ending ‘)’ if
exist!

`AExtendedSyntax` - must be True if modifier `/x` os on, while using the
regex.

Usefull for GUI editors of regex (you can find example of usage in
[REStudioMain.pas](https://github.com/andgineer/TRegExpr/blob/74ab342b639fc51941a4eea9c7aa53dcdf783592/restudio/REStudioMain.pas#L474))

=========== ======= Code de résultat Sens =========== ======= 0        
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
       CompilerrErrorPos : integer; // Position in r.e. where compilation error occured
     end;

## Unicode

In Unicode mode, all strings (InputString, Expression, internal strings)
are of type UnicodeString/WideString, instead of simple "string".
Unicode slows down performance, so use it only if you really need
Unicode support.

To use Unicode, uncomment `{$DEFINE UniCode}` in
[regexpr.pas](https://github.com/andgineer/TRegExpr/blob/29ec3367f8309ba2ecde7d68d5f14a514de94511/src/RegExpr.pas#L86)
(remove `off`).
