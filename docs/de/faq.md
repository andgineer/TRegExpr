|     |                                                     |                                                              |                                                              |                                                                |                                                               |                                                              |
|-----|-----------------------------------------------------|--------------------------------------------------------------|--------------------------------------------------------------|----------------------------------------------------------------|---------------------------------------------------------------|--------------------------------------------------------------|
|     | [Englisch](https://regex.sorokin.engineer/faq/) | [Русский](https://regex.sorokin.engineer/ru/faq/) | [Deutsch](https://regex.sorokin.engineer/de/faq/) | [Български](https://regex.sorokin.engineer/bg/faq/) | [Français](https://regex.sorokin.engineer/fr/faq/) | [Español](https://regex.sorokin.engineer/es/faq/) |

# FAQ

## Ich habe einen schrecklichen Fehler gefunden: TRegExpr löst Zugriffsverletzung aus!

**Antworten**

You must create the object before usage. So, after you declared
something like:

``` pascal
r: TRegExpr
```

Vergessen Sie nicht, die Objektinstanz zu erstellen:

``` pascal
r: = TRegExpr.Create. 
```

## Reguläre Ausdrücke mit (? = ...) funktionieren nicht

Look ahead is not implemented in the TRegExpr. But in many cases you can
easily [replace it with simple
subexpressions](../regular_expressions/#lookahead).

## Unterstützt es Unicode?

**Antworten**

\`Wie verwende ich Unicode? \<tregexpr/#unicode\> \_\_

## Warum gibt TRegExpr mehr als eine Zeile zurück?

For example, r.e. `<font .\*>` returns the first `<font`, then the rest
of the file including last `</html>`.

**Antworten**

For backward compatibility, [modifier
/s](../regular_expressions/#modifier_s) is `On` by default.

Switch it Off and `.` will match any but [Line
separators](../regular_expressions/#syntax_line_separators) - exactly
as you wish.

Übrigens empfehle ich `<font ([^\n>] *)&gt;`, in `Match [1]` wird die
URL.\</font\>.

## Warum gibt TRegExpr mehr als ich erwarte?

For example r.e. `<p>(.+)</p>` applyed to string `<p>a</p><p>b</p>`
returns `a</p><p>b` but not `a` as I expected.

**Antworten**

By default all operators works in `greedy` mode, so they match as more
as it possible.

If you want `non-greedy` mode you can use `non-greedy` operators like
`+?` and so on or switch all operators into `non-greedy` mode with help
of modifier `g` (use appropriate TRegExpr properties or operator `?(-g)`
in r.e.).

## Wie kann man mit TRegExpr Quellen wie HTML analysieren?

**Antworten**

Sorry Leute, aber es ist fast unmöglich!

Of course, you can easily use TRegExpr for extracting some information
from HTML, as shown in my examples, but if you want accurate parsing you
have to use real parser, not r.e.

You can read full explanation in Tom Christiansen and Nathan Torkington
`Perl Cookbook`, for example.

In short - there are many structures that can be easy parsed by real
parser but cannot at all by r.e., and real parser is much faster to do
the parsing, because r.e. doesn't simply scan input stream, it performs
optimization search that can take a lot of time.

## Gibt es eine Möglichkeit, mehrere Übereinstimmungen eines Musters auf TRegExpr abzurufen?

**Antworten**

Sie können Übereinstimmungen mit der ExecNext-Methode wiederholen.

If you want some example, please take a look at `TRegExpr.Replace`
method implementation or at the examples for
[HyperLinksDecorator](../demos/)

## Ich überprüfe die Benutzereingaben. Warum gibt TRegExpr für falsche Eingabezeichenfolgen &quot;True&quot; zurück?

**Antworten**

In many cases TRegExpr users forget that regular expression is for
**search** in input string.

So, for example if you use `\d{4,4}` expression, you will get success
for wrong user inputs like `12345` or `any letters 1234`.

You have to check from line start to line end to ensure there are no
anything else around: `^\d{4,4}$`.

<a name="nongreedyoptimization"></a>

## Warum funktionieren nichtgierige Iteratoren manchmal wie im gierigen Modus?

For example, the r.e. `a+?,b+?` applied to string `aaa,bbb` matches
`aaa,b`, but should it not match `a,b` because of non-greediness of
first iterator?

**Antworten**

This is because of TRegExpr way to work. In fact many others r.e.
engines work exactly the same: they performe only `simple` search
optimization, and do not try to do the best optimization.

In some cases it's bad, but in common it's rather advantage then
limitation, because of performance and predictability reasons.

The main rule - r.e. first of all try to match from current place and
only if that's completely impossible move forward by one char and try
again from next position in the text.

So, if you use `a,b+?` it'll match `a,b`. In case of `a+?,b+?` it's now
not recommended (we add non-greedy modifyer) but still possible to match
more then one `a`, so TRegExpr will do it.

TRegExpr like Perl's or Unix's r.e. doesn't attempt to move forward and
check - would it will be "better" match. Fisrt of all, just because
there is no way to say it's more or less good match.

## Wie kann ich TRegExpr mit Borland C ++ Builder verwenden?

Ich habe ein Problem, da keine Header-Datei (`.h` oder `.hpp`) verfügbar
ist.

**Antworten**

- Fügen Sie `RegExpr.pas` zu `bcb` hinzu.
- Projekt kompilieren Dadurch wird die Header-Datei `RegExpr.hpp`
  generiert.
- Jetzt können Sie Code schreiben, der die `RegExpr`-Einheit verwendet.
- Vergessen Sie nicht, `#include “RegExpr.hpp”` bei Bedarf hinzuzufügen.
- Don't forget to replace all `\` in regular expressions with `\\` or
  redefined [EscChar](../tregexpr/#escchar) const.

## Warum arbeiten viele (einschließlich TRegExpr-Hilfe und -Demo) in Borland C ++ Builder falsch?

**Antworten**

The hint is in the previous question ;) Symbol `\` has special meaning
in `C++`, so you have to `escape` it (as described in previous answer).
But if you don't like r.e. like `\\w+\\\\w+\\.\\w+` you can redefine the
constant `EscChar` (in `RegExpr.pas`). For example `EscChar = "/"`. Then
you can write `/w+/w+/./w+`, looks unusual but more readable.
