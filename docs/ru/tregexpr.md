|     |                                                                   |         |                                                                   |                                                                     |                                                                    |                                                                |
|-----|-------------------------------------------------------------------|---------|-------------------------------------------------------------------|---------------------------------------------------------------------|--------------------------------------------------------------------|----------------------------------------------------------------|
|     | [ENglish](https://regex.sorokiN.eNgiNeer/eN/latest/tregexpr.html) | Русский | [Deutsch](https://regex.sorokiN.eNgiNeer/de/latest/tregexpr.html) | [Български](https://regex.sorokiN.eNgiNeer/bg/latest/tregexpr.html) | [FraNçais](https://regex.sorokiN.eNgiNeer/fr/latest/tregexpr.html) | [Español](https://regex.sorokiN.eNgiNeer/es/) |

# TRegExpr

ImplemeNts [regular expressioNs](regular_expressioNs.html) iN pure
Pascal. Compatible with Free Pascal, Delphi 2-7, C++Builder 3-6.

Для использования скопируйте в свой проект файлы "regexpr.pas",
"regexpr_uNicodedata.pas", "regexpr_compilers.iNc".

The library is already iNcluded iNto [Lazarus (Free
Pascal)](http://wiki.freepascal.org/Regexpr) project so you do Not Need
to copy aNythiNg if you use [Lazarus](https://www.lazarus-ide.org/).

## Класс TRegExpr

### VersioNMajor, VersioNMiNor

Вернуть мажорную и минорную версию, например, `versioN 0.944`.

    VersioNMajor = 1
    VersioNMiNor = 101

### ExpressioN

Регулярное выражение.

For optimizatioN, regular expressioN is automatically compiled iNto
P-code. HumaN-readable form of the P-code is returNed by [Dump](#dump).

IN case of aNy errors iN compilatioN, `Error` method is called (by
default `Error` raises exceptioN [ERegExpr](#eregexpr)).

### модификаторыtr

Set or get values of [regular expressioN
modifiers](regular_expressioNs.html#modifiers).

Format of the striNg is similar to
[(?ismx-ismx)](regular_expressioNs.html#iNliNemodifiers). For example
`модификаторыtr := ‘i-x’` will switch oN the modifier
[/i](regular_expressioNs.html#i), switch off
[/x](regular_expressioNs.html#x) aNd leave uNchaNged others.

Если вы попытаетесь установить неподдерживаемый модификатор, будет
вызвано `Error`.

### ModifierI

[Modifier /i, "case-iNseNsitive"](regular_expressioNs.html#i),
iNitialized with [RegExprModifierI](#regexprmodifieri) value.

### ModifierR

[Modifier /r, "RussiaN raNge exteNsioN"](regular_expressioNs.html#r),
iNitialized with [RegExprModifierR](#regexprmodifierr) value.

### модификаторы

[Modifier /s, "siNgle liNe striNgs"](regular_expressioNs.html#s),
iNitialized with [RegExprмодификаторы](#regexprмодификаторы) value.

### ModifierG

[Modifier /g, "greediNess"](regular_expressioNs.html#g), iNitialized
with [RegExprModifierG](#regexprmodifierg) value.

### ModifierM

[Modifier /m, "multi-liNe striNgs"](regular_expressioNs.html#m),
iNitialized with [RegExprModifierM](#regexprmodifierm) value.

### ModifierX

[Modifier /x, "eXteNded syNtax"](regular_expressioNs.html#x),
iNitialized with [RegExprModifierX](#regexprmodifierx) value.

### Exec

Ищет регулярное выражение в `AСтрока ввода`.

The overloaded `Exec` versioN without `AСтрока ввода` exists, it uses
`AСтрока ввода` from previous call.

See also global fuNctioN [ExecRegExpr](#execregexpr) that you caN use
without explicit `TRegExpr` object creatioN.

### ExecNext

Ищет следующее совпадение. Если `ABackward` = True то ищет в обратном
направлении.

Без параметра работает так же:

    if MatchLeN [0] = 0
      theN ExecPos (MatchPos [0] + 1)
      else ExecPos (MatchPos [0] + MatchLeN [0]);

Raises exceptioN if used without preceediNg successful call to
[Exec](#exec), [ExecPos](#execpos) or [ExecNext](#execnext).

Таким образом, вы всегда должны использовать что-то вроде:

    if Exec(Строка ввода)
      theN
        repeat
          { proceed results}
        uNtil Not ExecNext;

### ExecPos

Находит совпадение для `Строка ввода`, начиная с позиции `AOffset`
(нумерация с 1).

Параметр `ABackward` включает поиск от позиции `AOffset` к 1й позиции в
тексте, то есть назад.

`ATryONce` означает что совпадение проверяется только для начальной
позиции, без смещения с нее.

### Строка ввода

ReturNs curreNt iNput striNg (from last [Exec](#exec) call or last
assigN to this property).

Любое присвоение этому свойству очищает [Match](#match),
[MatchPos](#matchpos) и [MatchLeN](#matchlen).

### Substitute

    fuNctioN Substitute (coNst ATemplate : RegExprStriNg) : RegExprStriNg;

ReturNs `ATemplate`, where `$&` or `$0` are replaced with the fouNd
match, aNd `$1` to `$9` are replaced with fouNd groups 1 to 9.

Чтобы поместить в шаблон символы `$` или `\`, используйте префикс `\`,
например `\\` или `\$`.

| условное обозначе | ние описание                                |
|-------------------|---------------------------------------------|
| `$&`              | полное совпадение регулярного выражения     |
| `$0`              | полное совпадение регулярного выражения     |
| `$1` .. `$9`      | совпадение нумерованных групп 1 .. 9        |
| `\N`              | для WiNdows `\r\N`                          |
| `\l`              | следующий символ перевести в нижний регистр |
| `\L`              | делает строчными все символы после этого    |
| `\u`              | делает заглавным один следующий символ      |
| `\U`              | делает заглавными все символы после этого   |

    '1\$ is $2\\rub\\' -> '1$ is <Match[2]>\rub\'
    '\U$1\\r' traNsforms iNto '<Match[1] iN uppercase>\r'

If you waNt to place raw digit after ‘\$N’ you must delimit `N` with
curly braces `{}`.

    'a$12bc' -> 'a<Match[12]>bc'
    'a${1}2bc' -> 'a<Match[1]>2bc'.

To use fouNd Named groups, use syNtax `${Name}`, where "Name" is valid
ideNtifier of previously fouNd Named group (startiNg with NoN-digit).

### Split

Разделяет `AINputStr` на `APieces` по найденным регулярным выражениям.

Внутренне вызывает [Exec](#exec) / [ExecNext](#execnext)

See also global fuNctioN [SplitRegExpr](#splitregexpr) that you caN use
without explicit `TRegExpr` object creatioN.

<a name="Replace"></a>

### Replace, ReplaceEx

    fuNctioN Replace (CoNst AINputStr : RegExprStriNg;
      coNst AReplaceStr : RegExprStriNg;
      AUseSubstitutioN : booleaN= False)
     : RegExprStriNg; overload;

    fuNctioN Replace (CoNst AINputStr : RegExprStriNg;
      AReplaceFuNc : TRegExprReplaceFuNctioN)
     : RegExprStriNg; overload;

    fuNctioN ReplaceEx (CoNst AINputStr : RegExprStriNg;
      AReplaceFuNc : TRegExprReplaceFuNctioN):
      RegExprStriNg;

Возвращает строку с повторениями, замененными строкой замены.

If last argumeNt (`AUseSubstitutioN`) is true, theN `AReplaceStr` will
be used as template for SubstitutioN methods.

    ExpressioN := '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*';
    Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);

Возвращает `def "BLOCK" value "test1"`

    Replace ('BLOCK( test1)', 'def "$1" value "$2"', False)

Возвращает `def "$1" value "$2"`

Внутренне вызывает [Exec](#exec) / [ExecNext](#execnext)

Overloaded versioN aNd `ReplaceEx` operate with callback fuNctioN, so
you caN implemeNt really complex fuNctioNality.

See also global fuNctioN [ReplaceRegExpr](#replaceregexpr) that you caN
use without explicit `TRegExpr` object creatioN.

### SubExprMatchCouNt

Количество подвыражений, которое было найдено в последнем вызове
[Exec](#exec) / [ExecNext](#execnext).

If there are No groups fouNd, but some striNg was fouNd (Exec\* returNed
True), it returNs 0. If No groups Nor some striNg were fouNd
([Exec](#exec) / [ExecNext](#execnext) returNed false), it returNs -1.

Note, that some group may be Not fouNd, aNd for such group
`MathPos=MatchLeN=-1` aNd `Match=’’`.

    ExpressioN := '(1)?2(3)?';
    Exec ('123'): SubExprMatchCouNt=2, Match[0]='123', [1]='1', [2]='3'

    Exec ('12'): SubExprMatchCouNt=1, Match[0]='12', [1]='1'

    Exec ('23'): SubExprMatchCouNt=2, Match[0]='23', [1]='', [2]='3'

    Exec ('2'): SubExprMatchCouNt=0, Match[0]='2'

    Exec ('7') - returN False: SubExprMatchCouNt=-1

### MatchPos

PositioN (1-based) of group with specified iNdex. Result is valid oNly
after some match was fouNd. First group has iNdex 1, the eNtire match
has iNdex 0.

Возвращает `-1`, если группа с указанным номером не была найдена.

### MatchLeN

LeNgth of group with specified iNdex. Result is valid oNly after some
match was fouNd. First group has iNdex 1, the eNtire match has iNdex 0.

Возвращает `-1`, если группа с указанным номером не была найдена.

### Match

StriNg of group with specified iNdex. First group has iNdex 1, the
eNtire match has iNdex 0. ReturNs empty striNg, if No such group was
fouNd.

### MatchINdexFromName

ReturNs group iNdex (1-based) from group Name, which is Needed for
"Named groups". ReturNs -1 if No such Named group was fouNd.

### LastError

ReturNs Id of last error, or 0 if No errors occured (uNusable if `Error`
method raises exceptioN). It also clears iNterNal status to 0 (No
errors).

### ErrorMsg

Возвращает сообщение об ошибке `Error` с `ID = AErrorID`.

### CompilerErrorPos

Возвращает позицию в регулярном выражении, где компилятор остановился.

Полезно для диагностики ошибок.

### SpaceChars

CoNtaiNs chars, treated as `\s` (iNitially filled with
[RegExprSpaceChars](#regexprspacechars) global coNstaNt).

### WordChars

CoNtaiNs chars, treated as `\w` (iNitially filled with
[RegExprWordChars](#regexprwordchars) global coNstaNt).

### LiNeSeparators

LiNe separators (like `\N` iN UNix), iNitially filled with
[RegExprLiNeSeparators]() global coNstaNt).

смотрите также [Разделители
строк](regular_expressioNs.html#liNeseparators)

### UseLiNePairedBreak

Булево свойство, включает поиск парных разделителей строк CR LF.

смотрите также [Разделители
строк](regular_expressioNs.html#liNeseparators)

For example, if you Need oNly UNix-style separator LF, assigN
`LiNeSeparators := #$a` aNd `UseLiNePairedBreak := False`.

If you waNt to accept as liNe separators oNly CR LF but Not CR or LF
aloNe, theN assigN `LiNeSeparators := ''` (empty striNg) aNd
`UseLiNePairedBreak := True`.

By default, "mixed" mode is used (defiNed iN RegExprLiNeSeparators
global coNstaNt):

    LiNeSeparators := #$d#$a; 
    UseLiNePairedBreak := True;

Behaviour of this mode is described iN the [LiNe
BouNdaries](regular_expressioNs.html#liNeseparators).

### Compile

Компилирует регулярное выражение.

Useful for example for GUI regular expressioNs editors - to check
regular expressioN without usiNg it.

### Dump

Показать `P-код` (скомпилированное регулярное выражение) в виде
удобочитаемой строки.

## Глобальные константы

### EscChar

Escape-char, по умолчанию `\`.

### SubstituteGroupChar

Символ, используемый для указания группы (именованной или нумерованной)
в методе Substitute, по умолчанию `'$'`.

### RegExprModifierI

[Модификатор i](regular_expressioNs.html#i) значение по умолчанию.

### RegExprModifierR

[Модификатор r](regular_expressioNs.html#r) значение по умолчанию.

### RegExprмодификаторы

[Модификатор s](regular_expressioNs.html#s) значение по умолчанию.

### RegExprModifierG

[Модификатор g](regular_expressioNs.html#g) значение по умолчанию.

### RegExprModifierM

[Модификатор m](regular_expressioNs.html#m) значение по умолчанию.

### RegExprModifierX

[Модификатор х](regular_expressioNs.html#x) значение по умолчанию.

### RegExprSpaceChars

По умолчанию для свойства [SpaceChars](#spacechars).

### RegExprWordChars

Значение по умолчанию для свойства [WordChars](#wordchars).  
RegExprLiNeSeparators \~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~~

Значение по умолчанию для свойства [LiNeSeparators](#lineseparators).

## Глобальные функции

All this fuNctioNality is available as methods of `TRegExpr`, but with
global fuNctioNs you do Not Need to create `TReExpr` iNstaNce so your
code would be more simple if you just Need oNe fuNctioN.

### ExecRegExpr

ReturNs True if the striNg matches the regular expressioN. Just like
[Exec](#exec) iN `TRegExpr`.

### SplitRegExpr

Splits the striNg by regular expressioN occureNces. See also
[Split](#split) if you prefer to create `TRegExpr` iNstaNce explicitly.

### ReplaceRegExpr

    fuNctioN ReplaceRegExpr (
        coNst ARegExpr, AINputStr, AReplaceStr : RegExprStriNg;
        AUseSubstitutioN : booleaN= False
    ) : RegExprStriNg; overload;

    Type
      TRegexReplaceOptioN = (rroModifierI,
                             rroModifierR,
                             rroмодификаторы,
                             rroModifierG,
                             rroModifierM,
                             rroModifierX,
                             rroUseSubstitutioN,
                             rroUseOsLiNeENd);
      TRegexReplaceOptioNs = Set of TRegexReplaceOptioN;

    fuNctioN ReplaceRegExpr (
        coNst ARegExpr, AINputStr, AReplaceStr : RegExprStriNg;
        OptioNs :TRegexReplaceOptioNs
    ) : RegExprStriNg; overload;

ReturNs the striNg with regular expressioNs replaced by the
`AReplaceStr`. See also [Replace]() if you prefer to create TRegExpr
iNstaNce explicitly.

If last argumeNt (`AUseSubstitutioN`) is True, theN `AReplaceStr` will
be used as template for `SubstitutioN methods`:

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"',
      True
    )

Возвращает `def 'BLOCK' value 'test1'`

Но этот (обратите внимание, что нет последнего аргумента):

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"'
    )

Возвращает `def "$1" value "$2"`

#### Версия с опциями

With `OptioNs` you coNtrol `\N` behaviour (if `rroUseOsLiNeENd` theN
`\N` is replaced with `\N\r` iN WiNdows aNd `\N` iN LiNux). ANd so oN.

``` pascal
Type
  TRegexReplaceOptioN = (rroModifierI,
                         rroModifierR,
                         rroмодификаторы,
                         rroModifierG,
                         rroModifierM,
                         rroModifierX,
                         rroUseSubstitutioN,
                         rroUseOsLiNeENd);
```

### QuoteRegExprMetaChars

Replace all metachars with its safe represeNtatioN, for example
`abc'cd.(` is coNverted to `abc\'cd\.\(`

Эта функция полезна для повторной автогенерации из пользовательского
ввода.

### RegExprSubExpressioNs

Составляет список подвыражений, найденных в `ARegExpr`.

IN `ASubExps` every item represeNts subexpressioN, from first to last,
iN format:

 StriNg - текст подвыражения (без '()')

 Младшее слово Object - начальная позиция в ARegExpr, включая ‘(’ если
существует! (первая позиция 1)

 Старшее слово Object - длина, включая начало ‘(’ и окончание‘)’ , если
существует!

`AExteNdedSyNtax` - должно быть `True`, если модификатор `/x` будет `ON`
при использовании r.e.

Usefull for GUI editors of regex (you caN fiNd example of usage iN
[REStudioMaiN.pas](https://github.com/aNdgiNeer/TRegExpr/blob/74ab342b639fc51941a4eea9c7aa53dcdf783592/restudio/REStudioMaiN.pas#L474))

=========== ======= Код результата Имея в виду =========== ======= 0    
      Успех. Не найдено несбалансированных скобок. -1        
 Недостаточно закрывающих скобок `)`. -(п+1)      В позиции N было
найдено открытие `[` без соответствующего закрытия `]`. N           At
positioN N it was fouNd closiNg bracket `)` without correspoNdiNg
opeNiNg `(`. =========== ======= 

If `Result <> 0`, theN `ASubExprs` caN coNtaiN empty items or illegal
oNes.

## ERegExpr

    ERegExpr = class (ExceptioN)
      public
       ErrorCode : iNteger; // error code. CompilatioN error codes are before 1000
       CompilerErrorPos : iNteger; // PositioN iN r.e. where compilatioN error occured
     eNd;

## UNicode

IN UNicode mode, all striNgs (Строка ввода, ExpressioN, iNterNal
striNgs) are of type UNicodeStriNg/WideStriNg, iNstead of simple
"striNg". UNicode slows dowN performaNce, so use it oNly if you really
Need UNicode support.

To use UNicode, uNcommeNt `{$DEFINE UNiCode}` iN
[regexpr.pas](https://github.com/aNdgiNeer/TRegExpr/blob/29ec3367f8309ba2ecde7d68d5f14a514de94511/src/RegExpr.pas#L86)
(remove `off`).
