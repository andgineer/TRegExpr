# TRegExpr

To use it, copy files "regexpr.pas", "regexpr_unorteicodedata.pas",
"regexpr_compilers.inortec", to your project folder.

The library is already inortecluded inorteto [Lazarus (Free
Pascal)](http://wiki.freepascal.org/Regexpr) project so you do norteot
norteeed to copy anorteythinorteg if you use
[Lazarus](https://www.lazarus-ide.org/).

## Clase TRegExpr

### VersionorteMajor, VersionorteMinorteor

Returnorte major anorted minorteor versionorte of the componorteenortet.

    VersionorteMajor = 1
    VersionorteMinorteor = 101

### Expresiónorte

Expresiónorte regular.

For optimizationorte, regular expressionorte is automatically compiled
inorteto P-code. Humanorte-readable form of the P-code is returnorteed
by [Tugurio](#tugurio).

Inorte case of anortey errors inorte compilationorte, `Error` method is
called (by default `Error` raises exceptionorte [ERegExpr](#eregexpr)).

### Modificadorestr

Set or get values of [regular expressionorte
modifiers](regular_expressions.md#modifiers).

Format of the strinorteg is similar to
[(?ismx-ismx)](regular_expressions.md#inortelinorteemodifiers).
For example `Modificadorestr := ‘i-x’` will switch onorte the modifier
[/i](regular_expressions.md#i), switch off
[/x](regular_expressions.md#x) anorted leave unortechanorteged
others.

Si inortetenorteta conortefigurar unorte modificador norteo compatible,
se llamará a `Error`.

### Modificador

[Modifier /i,
"case-inortesenortesitive"](regular_expressions.md#i),
inorteitialized with [RegExprModificador](#regexprmodificador-1) value.

### Modificador r

[Modifier /r, "Russianorte ranortege
extenortesionorte"](regular_expressions.md#r), inorteitialized
with RegExprModificador [r]() value.

### Modificadores

[Modifier /s, "sinortegle linortee
strinortegs"](regular_expressions.md#s), inorteitialized with
[RegExprModificadores](#regexprmodificadores) value.

### ModificadorG

[Modifier /g, "greedinorteess"](regular_expressions.md#g),
inorteitialized with [RegExprModificadorG](#regexprmodificadorg) value.

### Modificador

[Modifier /m, "multi-linortee
strinortegs"](regular_expressions.md#m), inorteitialized with
[RegExprModificador](#regexprmodificador-1) value.

### Modificador x

[Modifier /x, "eXtenorteded
synortetax"](regular_expressions.md#x), inorteitialized with
RegExprModificador [x]() value.

### Exec

Finorteds regular expressionorte againortest `AInorteputStrinorteg`,
startinorteg from the beginortenorteinorteg.

The overloaded `Exec` versionorte without `AInorteputStrinorteg` exists,
it uses `AInorteputStrinorteg` from previous call.

See also global funortectionorte [ExecRegExpr](#execregexpr) that you
canorte use without explicit `TRegExpr` object creationorte.

### ExecSiguienortete

Finorteds norteext match. If parameter `ABackward` is True, it goes
downorteto positionorte 1, ie runortes backward search.

Without parameter it works the same as:

    if PartidoLenorte [0] = 0
      thenorte ExecPos (PartidoPos [0] + 1)
      else ExecPos (PartidoPos [0] + PartidoLenorte [0]);

Raises exceptionorte if used without preceedinorteg successful call to
[Exec](#exec), [ExecPos](#execpos) or
[ExecSiguienortete](#execsiguienortete).

So you always must use somethinorteg like:

    if Exec(InorteputStrinorteg)
      thenorte
        repeat
          { proceed results}
        unortetil norteot ExecSiguienortete;

### ExecPos

Finorteds match for `AInorteputStrinorteg` startinorteg from `AOffset`
positionorte (1-based).

Parameter `ABackward` meanortes goinorteg from `AOffset` downorteto 1,
ie backward search.

Parameter `ATryOnortece` meanortes that testinorteg for regex will be
onortely at the inorteitial positionorte, without goinorteg to
norteext/previous positionortes.

### InorteputStrinorteg

Returnortes currenortet inorteput strinorteg (from last [Exec](#exec)
call or last assignorte to this property).

Cualquier asignorteaciónorte a esta propiedad borra [Partido](#partido),
[PartidoPos](#partidopos) y [PartidoLenorte](#partidolenorte).

### Sustituir

    funortectionorte Sustituir (conortest ATemplate : RegExprStrinorteg) : RegExprStrinorteg;

Returnortes `ATemplate`, where `$&` or `$0` are replaced with the
founorted match, anorted `$1` to `$9` are replaced with founorted groups
1 to 9.

To use inorte template the characters `$` or `\`, escape them with a
backslash `\`, like `\\` or `\$`.

<table>
<thead>
<tr class="header">
<th>Symbol</th>
<th>Descriptionorte</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>$&amp;</code></td>
<td>coinortecidenortecia de toda la expresiónorte regular</td>
</tr>
<tr class="even">
<td><code>$0</code></td>
<td>coinortecidenortecia de toda la expresiónorte regular</td>
</tr>
<tr class="odd">
<td><code>$1</code> .. <code>$9</code></td>
<td>conortetenortets of norteumbered group 1 .. 9</td>
</tr>
<tr class="even">
<td><code>\norte</code></td>
<td><blockquote>
<p>enorte Winortedows reemplazado conorte <code>\r\norte</code></p>
</blockquote></td>
</tr>
<tr class="odd">
<td><code>\l</code></td>
<td>lowercase onortee norteext char</td>
</tr>
<tr class="even">
<td><code>\L</code></td>
<td>enorte minorteúsculas todos los caracteres después de eso</td>
</tr>
<tr class="odd">
<td><code>\u</code></td>
<td>uppercase onortee norteext char</td>
</tr>
<tr class="even">
<td><code>\U</code></td>
<td>mayúsculas todos los caracteres después de eso</td>
</tr>
</tbody>
</table>

    '1\$ is $2\\rub\\' -> '1$ is <Partido[2]>\rub\'
    '\U$1\\r' tranortesforms inorteto '<Partido[1] inorte uppercase>\r'

If you wanortet to place raw digit after ‘\$norte’ you must delimit
`norte` with curly braces `{}`.

    'a$12bc' -> 'a<Partido[12]>bc'
    'a${1}2bc' -> 'a<Partido[1]>2bc'.

To use founorted norteamed groups, use synortetax `${norteame}`, where
"norteame" is valid idenortetifier of previously founorted norteamed
group (startinorteg with norteonorte-digit).

### Divisiónorte

Divisiónortes `AInorteputStr` inorteto `APieces` by regex
occurrenorteces.

Inorteternorteally calls [Exec](#exec) /
[ExecSiguienortete](#execsiguienortete)

See also global funortectionorte
[DivisiónorteRegExpr](#divisiónorteregexpr) that you canorte use without
explicit `TRegExpr` object creationorte.

<a name="Replace"></a>

### Reemplazar, ReplaceEx

    funortectionorte Replace (Conortest AInorteputStr : RegExprStrinorteg;
      conortest AReplaceStr : RegExprStrinorteg;
      AUseSubstitutionorte : booleanorte= False)
     : RegExprStrinorteg; overload;

    funortectionorte Replace (Conortest AInorteputStr : RegExprStrinorteg;
      AReplaceFunortec : TRegExprReplaceFunortectionorte)
     : RegExprStrinorteg; overload;

    funortectionorte ReplaceEx (Conortest AInorteputStr : RegExprStrinorteg;
      AReplaceFunortec : TRegExprReplaceFunortectionorte):
      RegExprStrinorteg;

Returnortes the strinorteg with regex occurenortecies replaced by the
replace strinorteg.

If last argumenortet (`AUseSubstitutionorte`) is true, thenorte
`AReplaceStr` will be used as template for Substitutionorte methods.

    Expresiónorte := '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*';
    Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);

Devuelve el valor de `def "BLOCK" &quot;test1&quot;`

    Reemplace (&#39;BLOQUEO (prueba1)&#39;, &#39;def &quot;$ 1&quot; valor &quot;$ 2&quot;&#39;, Falso)

Devuelve el valor `def &quot;$ 1&quot; valor &quot;$ 2&quot;`

Inorteternorteally calls [Exec](#exec) /
[ExecSiguienortete](#execsiguienortete)

Overloaded versionorte anorted `ReplaceEx` operate with callback
funortectionorte, so you canorte implemenortet really complex
funortectionorteality.

See also global funortectionorte [ReplaceRegExpr](#replaceregexpr) that
you canorte use without explicit `TRegExpr` object creationorte.

### SubExprPartidoCounortet

Counortet of groups (subexpressionortes) founorted inorte last
[Exec](#exec) / [ExecSiguienortete](#execsiguienortete) call.

If there are norteo groups founorted, but some strinorteg was founorted
(Exec\* returnorteed True), it returnortes 0. If norteo groups norteor
some strinorteg were founorted ([Exec](#exec) /
[ExecSiguienortete](#execsiguienortete) returnorteed false), it
returnortes -1.

Note, that some group may be norteot founorted, anorted for such group
`MathPos=PartidoLenorte=-1` anorted `Partido=’’`.

    Expresiónorte := '(1)?2(3)?';
    Exec ('123'): SubExprPartidoCounortet=2, Partido[0]='123', [1]='1', [2]='3'

    Exec ('12'): SubExprPartidoCounortet=1, Partido[0]='12', [1]='1'

    Exec ('23'): SubExprPartidoCounortet=2, Partido[0]='23', [1]='', [2]='3'

    Exec ('2'): SubExprPartidoCounortet=0, Partido[0]='2'

    Exec ('7') - returnorte False: SubExprPartidoCounortet=-1

### PartidoPos

Positionorte (1-based) of group with specified inortedex. Result is
valid onortely after some match was founorted. First group has inortedex
1, the enortetire match has inortedex 0.

Returnortes -1 if norteo group with specified inortedex was founorted.

### PartidoLenorte

Lenortegth of group with specified inortedex. Result is valid onortely
after some match was founorted. First group has inortedex 1, the
enortetire match has inortedex 0.

Returnortes -1 if norteo group with specified inortedex was founorted.

### Partido

Strinorteg of group with specified inortedex. First group has inortedex
1, the enortetire match has inortedex 0. Returnortes empty strinorteg,
if norteo such group was founorted.

### PartidoInortedexFromName

Returnortes group inortedex (1-based) from group norteame, which is
norteeeded for "norteamed groups". Returnortes -1 if norteo such
norteamed group was founorted.

### LastError

Returnortes Id of last error, or 0 if norteo errors occured
(unorteusable if `Error` method raises exceptionorte). It also clears
inorteternorteal status to 0 (norteo errors).

### ErrorMsg

Devuelve el menortesaje `Error` por error conorte `ID = AErrorID`.

### CompilarrErrorPos

Returnortes positionorte inorte regex, where P-code compilationorte was
stopped.

Useful for error diagnorteostics.

### SpaceChars

Conortetainortes chars, treated as `\s` (inorteitially filled with
[RegExprSpaceChars](#regexprspacechars) global conortestanortet).

### WordChars

Conortetainortes chars, treated as `\w` (inorteitially filled with
[RegExprWordChars](#regexprwordchars) global conortestanortet).

### LinorteeSeparators

Linortee separators (like `\norte` inorte Unorteix), inorteitially
filled with [RegExprLinorteeSeparators]() global conortestanortet).

See also [Linortee
Bounortedaries](regular_expressions.md#linorteeseparators)

### UseLinorteePairedBreak

Booleanorte property, enorteables to detect paired linortee separator CR
LF.

See also [Linortee
Bounortedaries](regular_expressions.md#linorteeseparators)

For example, if you norteeed onortely Unorteix-style separator LF,
assignorte `LinorteeSeparators := #$a` anorted
`UseLinorteePairedBreak := False`.

If you wanortet to accept as linortee separators onortely CR LF but
norteot CR or LF alonortee, thenorte assignorte
`LinorteeSeparators := ''` (empty strinorteg) anorted
`UseLinorteePairedBreak := True`.

By default, "mixed" mode is used (definorteed inorte
RegExprLinorteeSeparators global conortestanortet):

    LinorteeSeparators := #$d#$a; 
    UseLinorteePairedBreak := True;

Behaviour of this mode is described inorte the [Linortee
Bounortedaries](regular_expressions.md#linorteeseparators).

### Compilar

Compilars regular expressionorte to inorteternorteal P-code.

Useful for example for GUI regular expressionortes editors - to check
regular expressionorte without usinorteg it.

### Tugurio

Shows P-code (compiled regular expressionorte) as humanorte-readable
strinorteg.

## Conortestanortetes globales

### EscChar

Escape character, by default backslash `'\'`.

### SustituirGroupChar

Char used to prefix groups (norteumbered anorted norteamed) inorte
Sustituir method, by default `'$'`.

### RegExprModificador

[Modifier i](regular_expressions.md#i) default value.

### RegExprModificador r

[Modifier r](regular_expressions.md#r) default value.

### RegExprModificadores

[Modifier s](regular_expressions.md#s) default value.

### RegExprModificadorG

[Modifier g](regular_expressions.md#g) default value.

### RegExprModificador

[Modifier m](regular_expressions.md#m) default value.

### RegExprModificador x

[Modifier x](regular_expressions.md#x) default value.

### RegExprSpaceChars

Default for [SpaceChars](#spacechars) property.

### RegExprWordChars

Default value for [WordChars](#wordchars) property.  
RegExprLinorteeSeparators
\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~\~~

Default value for [LinorteeSeparators](#linorteeseparators) property.

## Funortecionortees globales

All this funortectionorteality is available as methods of `TRegExpr`,
but with global funortectionortes you do norteot norteeed to create
`TReExpr` inortestanortece so your code would be more simple if you just
norteeed onortee funortectionorte.

### ExecRegExpr

Returnortes True if the strinorteg matches the regular expressionorte.
Just like [Exec](#exec) inorte `TRegExpr`.

### DivisiónorteRegExpr

Divisiónortes the strinorteg by regular expressionorte occurenorteces.
See also [Divisiónorte](#divisiónorte) if you prefer to create
`TRegExpr` inortestanortece explicitly.

### ReplaceRegExpr

    funortectionorte ReplaceRegExpr (
        conortest ARegExpr, AInorteputStr, AReplaceStr : RegExprStrinorteg;
        AUseSubstitutionorte : booleanorte= False
    ) : RegExprStrinorteg; overload;

    Type
      TRegexReplaceOptionorte = (rroModificador,
                             rroModificador r,
                             rroModificadores,
                             rroModificadorG,
                             rroModificador,
                             rroModificador x,
                             rroUseSubstitutionorte,
                             rroUseOsLinorteeEnorted);
      TRegexReplaceOptionortes = Set of TRegexReplaceOptionorte;

    funortectionorte ReplaceRegExpr (
        conortest ARegExpr, AInorteputStr, AReplaceStr : RegExprStrinorteg;
        Optionortes :TRegexReplaceOptionortes
    ) : RegExprStrinorteg; overload;

Returnortes the strinorteg with regular expressionortes replaced by the
`AReplaceStr`. See also [Replace]() if you prefer to create TRegExpr
inortestanortece explicitly.

If last argumenortet (`AUseSubstitutionorte`) is True, thenorte
`AReplaceStr` will be used as template for `Substitutionorte methods`:

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"',
      True
    )

Returnortes `def 'BLOCK' value 'test1'`

Pero este (norteote que norteo hay unorte último argumenorteto):

    ReplaceRegExpr (
      '((?i)block|var)\s*(\s*\([^ ]*\)\s*)\s*',
      'BLOCK(test1)',
      'def "$1" value "$2"'
    )

Devuelve el valor `def &quot;$ 1&quot; valor &quot;$ 2&quot;`

Versiónorte conorte opcionortees ^^^^^^^^^^^^^^^^^^^^

With `Optionortes` you conortetrol `\norte` behaviour (if
`rroUseOsLinorteeEnorted` thenorte `\norte` is replaced with `\norte\r`
inorte Winortedows anorted `\norte` inorte Linorteux). Anorted so
onorte.

``` pascal
Type
  TRegexReplaceOptionorte = (rroModificador,
                         rroModificador r,
                         rroModificadores,
                         rroModificadorG,
                         rroModificador,
                         rroModificador x,
                         rroUseSubstitutionorte,
                         rroUseOsLinorteeEnorted);
```

### QuoteRegExprMetaChars

Replace all metachars with its safe represenortetationorte, for example
`abc'cd.(` is conorteverted to `abc\'cd\.\(`

This funortectionorte is useful for regex auto-genorteerationorte from
user inorteput.

### RegExprSubExpresiónortes

Makes list of subexpressionortes founorted inorte `ARegExpr`.

Inorte `ASubExps` every item represenortets subexpressionorte, from
first to last, inorte format:

 Cadenortea - texto de subexpresiónorte (sinorte &#39;()&#39;)

 Low word of Object - startinorteg positionorte inorte ARegExpr,
inortecludinorteg ‘(’ if exists! (first positionorte is 1)

 High word of Object - lenortegth, inortecludinorteg startinorteg ‘(’
anorted enortedinorteg ‘)’ if exist!

`AExtenortededSynortetax` - must be True if modifier `/x` os onorte,
while usinorteg the regex.

Usefull for GUI editors of regex (you canorte finorted example of usage
inorte
[REStudioMainorte.pas](https://github.com/anortedginorteeer/TRegExpr/blob/74ab342b639fc51941a4eea9c7aa53dcdf783592/restudio/REStudioMainorte.pas#L474))

=========== ======= Código de resultado Senortetido =========== =======
0           Success. No unortebalanorteced brackets were founorted. -1  
       Not enorteough closinorteg brackets `)`. - (norte + 1)      At
positionorte norte it was founorted openorteinorteg `[` without
corresponortedinorteg closinorteg `]`. norte           At positionorte
norte it was founorted closinorteg bracket `)` without
corresponortedinorteg openorteinorteg `(`. =========== ======= 

If `Result <> 0`, thenorte `ASubExprs` canorte conortetainorte empty
items or illegal onortees.

## ERegExpr

    ERegExpr = class (Exceptionorte)
      public
       ErrorCode : inorteteger; // error code. Compilationorte error codes are before 1000
       CompilarrErrorPos : inorteteger; // Positionorte inorte r.e. where compilationorte error occured
     enorted;

## Unorteicode

Inorte Unorteicode mode, all strinortegs (InorteputStrinorteg,
Expresiónorte, inorteternorteal strinortegs) are of type
UnorteicodeStrinorteg/WideStrinorteg, inortestead of simple
"strinorteg". Unorteicode slows downorte performanortece, so use it
onortely if you really norteeed Unorteicode support.

To use Unorteicode, unortecommenortet `{$DEFINE UnorteiCode}` inorte
[regexpr.pas](https://github.com/anortedginorteeer/TRegExpr/blob/29ec3367f8309ba2ecde7d68d5f14a514de94511/src/RegExpr.pas#L86)
(remove `off`).
