# Reguläre Ausdrücke (RegEx)

## Einführung

Reguläre Ausdrücke sind eine praktische Methode, um Muster in Texten zu definieren.

Mit regulären Ausdrücken kannst du Benutzereingaben validieren, nach Mustern wie E-Mails oder Telefonnummern auf Webseiten oder in Dokumenten suchen und vieles mehr.

Unten findest du das komplette Cheat Sheet für reguläre Ausdrücke.

## Zeichen

### Einfache Übereinstimmungen

Jedes einzelne Zeichen (außer speziellen RegEx-Zeichen) entspricht sich selbst. Eine
Reihe von (nicht speziellen) Zeichen entspricht dieser Reihe von Zeichen im Eingabestring.

| RegEx    | Entsprechungen |
|----------|----------------|
| `foobar` | `foobar`       |

### Nicht-druckbare Zeichen (Escape-Codes)

Um ein Zeichen durch seinen Unicode-Code zu spezifizieren, verwende das Präfix `\x` gefolgt
von dem Hex-Code. Für 3-4 stellige Codes (nach U+00FF), schließe den Code
in Klammern ein.

| RegEx        | Entsprechungen                              |
|--------------|---------------------------------------------|
| `\xAB`       | Zeichen mit 2-stelligem Hex-Code `AB`       |
| `\x{AB20}`   | Zeichen mit 1..4-stelligem Hex-Code `AB20`  |
| `foo\x20bar` | `foo bar` (beachte Leerzeichen in der Mitte)|

Es gibt eine Anzahl vordefinierter Escape-Codes für nicht-druckbare
Zeichen, ähnlich wie in der C-Sprache:

| RegEx       | Entsprechungen                              |
|-------------|---------------------------------------------|
| `\t`        | Tabulator (HT/TAB), wie `\x09`              |
| `\n`        | Zeilenvorschub (LF), wie `\x0a`             |
| `\r`        | Wagenrücklauf (CR), wie `\x0d`              |
| `\f`        | Seitenvorschub (FF), wie `\x0c`             |
| `\a`        | Alarm (BEL), wie `\x07`                     |
| `\e`        | Escape (ESC), wie `\x1b`                    |
| `\cA` ... `\cZ` | chr(0) bis chr(25). Zum Beispiel entspricht `\cI` dem Tabulator-Zeichen. Kleinbuchstaben "a"..."z" werden ebenfalls unterstützt.|

### Escaping

Um ein spezielles RegEx-Zeichen (eines von `.+*?|\()[]{}^$`) darzustellen, setze
einen Backslash `\` davor. Der Backslash muss ebenfalls escaped werden.

| RegEx         | Entsprechungen                                                                 |
|---------------|---------------------------------------------------------------------------------|
| `\^FooBarPtr` | `^FooBarPtr`, dies ist `^` und nicht [Anfang der Zeile](#lineseparators)       |
| `\[a\]`       | `[a]`, dies ist keine [Zeichenklasse](#userclass)                               |

## Zeichenklassen

### Benutzerdefinierte Zeichenklassen

Eine Zeichenklasse ist eine Liste von Zeichen innerhalb eckiger Klammern `[]`. Die
Klasse entspricht jedem **einzelnen** Zeichen, das in dieser Klasse aufgelistet ist.

| RegEx            | Entsprechungen                                        |
|------------------|--------------------------------------------------------|
| `foob[aeiou]r`   | `foobar`, `foober` usw., aber nicht `foobbr`, `foobcr` |

Du kannst die Klasse "invertieren" - wenn das erste Zeichen nach der `[` ein
`^` ist, dann entspricht die Klasse jedem Zeichen **außer** den in der Klasse
aufgelisteten Zeichen.

| RegEx           | Entsprechungen                                        |
|-----------------|--------------------------------------------------------|
| `foob[^aeiou]r` | `foobbr`, `foobcr` usw., aber nicht `foobar`, `foober` |

Innerhalb einer Liste wird das Minuszeichen `-` verwendet, um einen Bereich anzugeben, sodass
`a-z` alle Zeichen zwischen `a` und `z` einschließlich darstellt.

Wenn du das Minuszeichen `-` selbst als Mitglied einer Klasse haben möchtest, setze es an den
Anfang oder das Ende der Liste oder [escape](#escape) es mit einem Backslash.

Wenn du `]` als Teil der Klasse haben möchtest, kannst du es am Anfang der
Liste platzieren oder [escape](#escape) es mit einem Backslash.

| RegEx       | Entsprechungen                         |
|-------------|----------------------------------------|
| `[-az]`     | `a`, `z` und `-`                       |
| `[az-]`     | `a`, `z` und `-`                       |
| `[a\-z]`    | `a`, `z` und `-`                       |
| `[a-z]`     | Zeichen von `a` bis `z`                |
| `[\n-\x0D]` | Zeichen von chr(10) bis chr(13)        |

### Punkt-Meta-Zeichen

Das Meta-Zeichen `.` (Punkt) entspricht standardmäßig jedem Zeichen. Aber wenn du den [Modifikator /s](#s) **ausschaltest**, entspricht es nicht Zeilenumbruch-Zeichen.

Der `.` fungiert nicht als Meta-Klasse innerhalb [benutzerdefinierter Zeichenklassen](#user-character-classes). `[.]` bedeutet einen wörtlichen ".".

### Meta-Klassen

Es gibt eine Reihe vordefinierter Zeichenklassen, die reguläre Ausdrücke kompakter machen, "Meta-Klassen":

| RegEx | Entsprechungen                                   |
|-------|--------------------------------------------------|
| `\w`  | ein alphanumerisches Zeichen, einschließlich `_` |
| `\W`  | ein nicht-alphanumerisches Zeichen               |
| `\d`  | eine Ziffer (wie `[0-9]`)                        |
| `\D`  | eine nicht-Ziffer                                |
| `\s`  | ein Leerzeichen (wie `[ \t\n\r\f]`)              |
| `\S`  | ein Nicht-Leerzeichen                            |
| `\h`  | horizontaler Leerraum: der Tabulator und alle Zeichen in der "space separator" Unicode-Kategorie |
| `\H`  | kein horizontaler Leerraum                       |
| `\v`  | vertikaler Leerraum: alle Zeichen, die im Unicode-Standard als Zeilenumbrüche behandelt werden |
| `\V`  | kein vertikaler Leerraum                         |
| `\R`  | Unicode-Zeilenumbruch: LF, Paar CR LF, CR, FF (Seitenvorschub), VT (vertikaler Tabulator), U+0085, U+2028, U+2029 |

Du kannst alle oben genannten Meta-Klassen innerhalb [benutzerdefinierter Zeichenklassen](#user-character-classes) verwenden.

| RegEx         | Entsprechungen                                                             |
|---------------|----------------------------------------------------------------------------|
| `foob\dr`     | `foob1r`, `foob6r` usw., aber nicht `foobar`, `foobbr` usw.                |
| `foob[\w\s]r` | `foobar`, `foob r`, `foobbr` usw., aber nicht `foob1r`, `foob=r` usw.       |

> [TRegExpr](tregexpr.md)
>
> Die Eigenschaften [SpaceChars](tregexpr.md#spacechars) und
> [WordChars](tregexpr.md#wordchars) definieren die Zeichenklassen `\w`,
> `\W`, `\s`, `\S`.
>
> So können Sie diese Klassen neu definieren.

## Grenzen

### Zeilengrenzen

| Meta-Zeichen | Entsprechungen                                         |
|--------------|--------------------------------------------------------|
| `^`          | Übereinstimmung der Länge Null am Anfang der Zeile     |
| `$`          | Übereinstimmung der Länge Null am Ende der Zeile       |
| `\A`         | Übereinstimmung der Länge Null ganz am Anfang          |
| `\z`         | Übereinstimmung der Länge Null ganz am Ende            |
| `\Z`         | wie `\z`, entspricht aber auch vor dem letzten Zeilenumbruch |
| `\G`         | Übereinstimmung der Länge Null am Ende der vorherigen Übereinstimmung |

Beispiele:

| RegEx      | Entsprechungen                                 |
|------------|------------------------------------------------|
| `^foobar`  | `foobar` nur, wenn es am Anfang einer Zeile steht |
| `foobar$`  | `foobar` nur, wenn es am Ende einer Zeile steht   |
| `^foobar$` | `foobar` nur, wenn es die einzige Zeichenkette in einer Zeile ist |
| `foob.r`   | `foobar`, `foobbr`, `foob1r` usw.              |

Das Meta-Zeichen `^` entspricht einer Position der Länge Null am Anfang des Eingabestrings. `$` - am Ende. Wenn der [Modifikator /m](#m) **eingeschaltet** ist, entsprechen sie auch am Anfang/Ende einzelner Zeilen im mehrzeiligen Text.

Beachte, dass es keine leere Zeile innerhalb der Sequenz `\x0D\x0A` gibt.

> [TRegExpr](tregexpr.md)
>
> Wenn Sie die [Unicode-Version](tregexpr.md#unicode) verwenden, dann
> passen `^`/`$` auch zu `\x2028`, `\x2029`, `\x0B`, `\x0C` oder `\x85`.

Das Meta-Zeichen `\A` passt auf die nulllange Position ganz am Anfang des
Eingabestrings, `\z` - ganz am Ende. Sie ignorieren den [Modifikator /m](#m).
`\Z` ist wie `\z`, passt aber auch vor dem finalen Zeilenumbruch (LF und
CR LF). Das Verhalten von `\A`, `\z`, `\Z` ist wie in den meisten großen
Regex-Engines (Perl, PCRE usw.) gestaltet.

Beachten Sie, dass `^.*$` nicht auf einen String zwischen `\x0D\x0A` passt, weil
dies ein unteilbarer Zeilenseparator ist. Aber es passt auf den leeren String
innerhalb der Sequenz `\x0A\x0D`, weil dies 2 Zeilenumbrüche in der
falschen Reihenfolge sind.

> [TRegExpr](tregexpr.md)
>
> Die Verarbeitung von mehrzeiligen Texten kann durch die Eigenschaften
> [LineSeparators](tregexpr.md#lineseparators) und
> [UseLinePairedBreak](tregexpr.md#linepairedseparator) angepasst werden.
>
> So können Sie Unix-Stil Trennzeichen `\n` oder DOS/Windows-Stil `\r\n`
> verwenden oder sie zusammen mischen (wie im oben beschriebenen Standardverhalten).

Wenn Sie eine mathematisch korrekte Beschreibung bevorzugen, finden Sie diese auf
[www.unicode.org](http://www.unicode.org/unicode/reports/tr18/).

### Wortgrenzen

| RegEx | Entsprechungen         |
|-------|------------------------|
| `\b`  | eine Wortgrenze        |
| `\B`  | keine Wortgrenze       |

Eine Wortgrenze `\b` ist ein Punkt zwischen zwei Zeichen, von denen eines ein `\w` und das andere ein `\W` ist (in beliebiger Reihenfolge).

## Quantifizierung

### Quantoren

Jedes Element eines regulären Ausdrucks kann von einem Quantor gefolgt werden.
Ein Quantor gibt die Anzahl der Wiederholungen des Elements an.

| RegEx    | Entsprechungen                                              |
|----------|--------------------------------------------------------------|
| `{n}`    | genau `n` Mal                                                |
| `{n,}`   | mindestens `n` Mal                                           |
| `{,m}`   | nicht mehr als `m` Mal (nur mit AllowBraceWithoutMin)       |
| `{n,m}`  | mindestens `n`, aber nicht mehr als `m` Mal                  |
| `*`      | null oder mehr, ähnlich wie `{0,}`                           |
| `+`      | einmal oder mehr, ähnlich wie `{1,}`                         |
| `?`      | null oder einmal, ähnlich wie `{0,1}`                        |

Ziffern in geschweiften Klammern `{n,m}`, geben die minimale Anzahl von
Malen `n` und die maximale `m` an.

`{n}` ist äquivalent zu `{n,n}` und entspricht genau `n` Mal. `{n,}`
entspricht `n` oder mehr Mal.

Die Variante `{,m}` wird nur unterstützt, wenn die Eigenschaft
AllowBraceWithoutMin gesetzt ist.

Es gibt keine praktische Begrenzung für die Werte n und m (Grenze ist maximaler
vorzeichenbehafteter 32-Bit-Wert).

Die Verwendung von `{` ohne einen korrekten Bereich führt zu einem Fehler. Dieses Verhalten kann
geändert werden, indem die Eigenschaft AllowLiteralBraceWithoutRange gesetzt wird, welche
`{` als Literalzeichen akzeptiert, wenn es nicht von einem Bereich gefolgt wird. Ein Bereich
mit einem niedrigeren Wert größer als der hohe Wert führt immer zu einem Fehler.

| RegEx            | Entsprechungen                                                          |
|------------------|--------------------------------------------------------------------------|
| `foob.*r`        | `foobar`,  `foobalkjdflkj9r` und `foobr`                                 |
| `foob.+r`        | `foobar`, `foobalkjdflkj9r` aber nicht `foobr`                          |
| `foob.?r`        | `foobar`, `foobbr` und `foobr` aber nicht `foobalkj9r`                  |
| `fooba{2}r`      | `foobaar`                                                                |
| `fooba{2,}r`     | `foobaar'`, `foobaaar`, `foobaaaar` usw.                                 |
| `fooba{2,3}r`    | `foobaar`, oder `foobaaar`  aber nicht `foobaaaar`                       |
| `(foobar){8,10}` | 8...10 Instanzen von `foobar` (`()` ist [Gruppe](#subexpression))       |

<a name="greedy"></a>

### Gierigkeit

[Quantoren](#iterator) im "gierigen" Modus nehmen so viel wie möglich, im
"faulen" Modus - so wenig wie möglich.

Standardmäßig sind alle Quantoren "gierig". Füge das Zeichen `?` hinzu, um
jeden Quantor "faul" zu machen.

Für die Zeichenkette `abbbbc`:

| RegEx     | Übereinstimmungen |
|-----------|-------------------|
| `b+`      | `bbbb`            |
| `b+?`     | `b`               |
| `b*?`     | leere Zeichenkette|
| `b{2,3}?` | `bb`              |
| `b{2,3}`  | `bbb`             |

Du kannst alle Quantoren in den "faulen" Modus umschalten ([Modifikator /g](#g),
unten verwenden wir [Änderung des Modifikators im Text](#inlinemodifiers)).

| RegEx     | Übereinstimmungen |
|-----------|-------------------|
| `(?-g)b+` | `b`               |

### Besitzergreifender Quantor

Die Syntax lautet: `a++`, `a*+`, `a?+`, `a{2,4}+`. Derzeit wird sie nur
für einfache Klammern unterstützt, aber nicht für Klammern nach einer Gruppe wie
`(foo|bar){3,5}+`.

Diese Regex-Funktion wird [hier beschrieben.](https://regular-expressions.mobi/possessive.html?wlr=1) Kurz gesagt,
beschleunigt der besitzergreifende Quantor das Finden von Übereinstimmungen in komplexen Fällen.

## Auswahl

Ausdrücke in der Auswahl werden durch das vertikale Balken `|` getrennt.

So wird `fee|fie|foe` jede von `fee`, `fie` oder `foe` in der Ziel-
zeichenkette übereinstimmen (wie auch `f(e|i|o)e`).

Der erste Ausdruck umfasst alles vom letzten Musterbegrenzer
(`(`, `[`, oder der Anfang des Musters) bis zum ersten `|`, und der
letzte Ausdruck enthält alles vom letzten `|` zum nächsten
Musterbegrenzer.

Das klingt ein wenig kompliziert, daher ist es üblich, die
Auswahl in Klammern zu setzen, um Verwirrung darüber zu minimieren, wo sie beginnt und endet.

Ausdrücke in der Auswahl werden von links nach rechts ausprobiert, sodass der erste
Ausdruck, der passt, der gewählte ist.

Zum Beispiel wird der reguläre Ausdruck `foo|foot` in der Zeichenkette `barefoot`
`foo` übereinstimmen. Nur der erste Ausdruck, der passt.

Denk auch daran, dass `|` als wörtliches Zeichen innerhalb von eckigen
Klammern interpretiert wird, also wenn du `[fee|fie|foe]` schreibst, übereinstimmst du wirklich nur
mit `[feio|]`.

| RegEx          | Übereinstimmungen       |
|----------------|-------------------------|
| `foo(bar|foo)` | `foobar` oder `foofoo`  |

<a name="subexpression"></a>

## Gruppen

Die Klammern `()` werden verwendet, um Gruppen (d.h. Unterexpressionen) zu definieren.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Gruppenpositionen, Längen und tatsächliche Werte werden in
> [MatchPos](tregexpr.md#matchpos), [MatchLen](tregexpr.md#matchlen)
> und [Match](tregexpr.md#match) sein.
>
> Du kannst sie mit [Substitute](tregexpr.md#substitute) ersetzen.

Gruppen werden von links nach rechts durch ihre öffnende Klammer nummeriert
(einschließlich verschachtelter Gruppen). Die erste Gruppe hat den Index 1. Der gesamte Regex hat
den Index 0.

> | Gruppe | Wert     |
> |--------|----------|
> | 0      | `foobar` |
> | 1      | `foobar` |
> | 2      | `bar`    |

## Rückverweise

Metazeichen `\1` bis `\9` werden als Rückverweise auf
Erfassungsgruppen interpretiert. Sie stimmen mit der zuvor gefundenen Gruppe mit dem angegebenen
Index überein.

Das Metazeichen `\g` gefolgt von einer Zahl wird ebenfalls als
Rückverweise auf Erfassungsgruppen interpretiert. Es kann von einer mehrstelligen
Zahl gefolgt werden.

| RegEx      | Übereinstimmungen             |
|------------|-------------------------------|
| `(.)\1+`   | `aaaa` und `cc`               |
| `(.+)\1+`  | auch `abab` und `123123`      |
| `(.)\g1+`  | `aaaa` und `cc`               |

RegEx `(['"]?)(\d+)\1` passt zu `"13"` (in Anführungszeichen), oder `'4'` (in
einfachen Anführungszeichen) oder `77` (ohne Anführungszeichen) usw.

## Benannte Gruppen und Rückverweise

Benannte Gruppen in regulären Ausdrücken ermöglichen es Ihnen, einen Teil Ihres Musters zu beschriften. 
Das macht Ihre Muster leichter verständlich und aktualisierbar.

Um eine benannte Gruppe zu erstellen, verwenden Sie `(?<name>pattern)` oder `(?'name'pattern)`, 
wobei `name` der Name der Gruppe ist und `pattern` das Regex-Muster, das Sie erfassen möchten.

Rückverweise ermöglichen es Ihnen, denselben Text wie eine zuvor getroffene Gruppe zu matchen. 
Benannte Rückverweise verwenden `\k<name>`, wobei `name` der Name der Gruppe ist, die Sie erneut matchen möchten.

TRegExpr unterstützt auch die Perl-Version: `(?P<name>pattern)` um eine benannte Gruppe zu definieren und `(?P=name)` 
für Rückverweise.

Beispiel

| RegEx                    | Übereinstimmungen      |
|--------------------------|------------------------|
| `(?P<qq>['"])\w+(?P=qq)` | `"wort"` und `'wort'`  |

## Übereinstimmendes Ergebnis

Der Beginn der gemeldeten Übereinstimmung kann mit `\K` gesetzt werden.

Standardmäßig wird der gesamte von einem Muster abgedeckte Text als übereinstimmend angesehen.
Es ist jedoch möglich, explizit festzulegen, was gemeldet wird.

Das Muster `a\Kb` erfordert, dass der Text "ab" enthält. Aber nur der
"b" wird als übereinstimmend gemeldet. Es kann mehrere `\K`
in einem Muster geben, Das letzte setzt die Startposition der Übereinstimmung. Nur `\K`
in aktiven Teilen des Musters werden berücksichtigt. Z.B. `a(\Kb)?` wird nicht
`\K` berücksichtigen, wenn es kein "b" gibt. Erfassungen können außerhalb der durch `\K`
gesetzten Übereinstimmung existieren.

Wenn es in anderen Konstrukten verwendet wird, die außerhalb der gemeldeten Übereinstimmung
angewendet werden können (wie Vorausschau), dann muss die durch `\K` markierte Position vor oder bei
dem gemeldeten Ende der Übereinstimmung liegen. Wenn die Position später markiert wird, wird die
Übereinstimmung als gescheitert angesehen.

`\K` ist in gewisser Weise ähnlich einer Rückwärtssuche. Im Gegensatz zu einer Rückwärtssuche muss der Teil
des Musters vor dem `\K` nach dem Startpunkt der
Übereinstimmung liegen, wenn das Muster von einer Versatzposition innerhalb des
Textes angewendet wird.

## Modifikatoren

Modifikatoren dienen zur Änderung des Verhaltens von regulären Ausdrücken.

Du kannst Modifikatoren global in deinem System setzen oder innerhalb des
regulären Ausdrucks ändern, indem du [(?imsxr-imsxr)](#inlinemodifiers) verwendest.

> [TRegExpr](tregexpr.md)
>
> Um Modifikatoren zu ändern, verwende [ModifierStr](tregexpr.md#modifierstr) oder
> entsprechende `TRegExpr`-Eigenschaften
> [Modifier\*](tregexpr.md#modifieri).
>
> Die Standardwerte sind in [globalen
> Variablen](tregexpr.md#global-constants) definiert. Zum Beispiel definiert die globale
> Variable `RegExprModifierX` den Standardwert für die Eigenschaft `ModifierX`.

<a name="i"></a>

### i, Groß-/Kleinschreibung ignorieren

Groß-/Kleinschreibung ignorieren. Verwendet die in deinem System installierten Gebietsschema-Einstellungen, siehe auch
[InvertCase](tregexpr.md#invertcase).

<a name="m"></a>

### m, Mehrzeilige Zeichenketten

Behandelt die Zeichenkette als mehrere Zeilen. So passen `^` und `$` auf den Anfang oder das Ende
einer beliebigen Zeile innerhalb der Zeichenkette.

Siehe auch [Zeilenbegrenzungen](#lineseparators).

<a name="s"></a>

### s, Einzeilige Zeichenketten

Behandelt die Zeichenkette als eine einzige Zeile. So passt `.` auf jedes beliebige Zeichen,
sogar auf Zeilentrenner.

Siehe auch [Zeilenbegrenzungen](#lineseparators), mit denen es normalerweise nicht
übereinstimmen würde.

<a name="g"></a>

### g, Gierigkeit

> [TRegExpr](tregexpr.md) nur Modifikator.

Durch Ausschalten wechselst du [Quantoren](#iterator) in
[nicht-gierigen](#greedy) Modus.

Wenn also der Modifikator `/g` `Aus` ist, dann funktioniert `+` als `+?`, `*` als `*?` und so
weiter.

Standardmäßig ist dieser Modifikator `Ein`.

<a name="x"></a>

### x, Erweiterte Syntax

Ermöglicht es, reguläre Ausdrücke zu kommentieren und in mehrere
Zeilen aufzuteilen.

Wenn der Modifikator `Ein` ist, ignorieren wir alle Leerzeichen, die weder
mit einem Rückstrich versehen noch innerhalb einer Zeichenklasse sind.

Und das `#` Zeichen trennt Kommentare.

Beachte, dass du leere Zeilen verwenden kannst, um den regulären Ausdruck für
bessere Lesbarkeit zu formatieren:

``` text
(
(abc) # Kommentar 1
#
(efg) # Kommentar 2
)
```

Das bedeutet auch, dass wenn du echte Leerzeichen oder `#` Zeichen im
Muster möchtest (außerhalb einer Zeichenklasse, wo sie von
`/x` unbeeinflusst sind), musst du sie entweder escapen oder sie mit oktalen oder
hexadezimalen Escapes kodieren.

<a name="r"></a>

### r, Russische Bereiche

> [TRegExpr](tregexpr.md) nur Modifikator.

In der russischen ASCII-Tabelle sind die Zeichen `ё`/`Ё` separat von
anderen platziert.

Große und kleine russische Buchstaben sind in getrennten Bereichen, das ist
dasselbe wie bei englischen Buchstaben, aber dennoch wollte ich eine kurze
Form.

Mit diesem Modifikator kannst du statt `[а-яА-ЯёЁ]` `[а-Я]` schreiben, wenn du alle russischen Buchstaben
brauchst.

Wenn der Modifikator `Ein` ist:

| RegEx | Übereinstimmungen                |
|-------|----------------------------------|
| `а-я` | Zeichen von `а` bis `я` und `ё`  |
| `А-Я` | Zeichen von `А` bis `Я` und `Ё`  |
| `а-Я` | alle russischen Symbole          |

Der Modifikator ist standardmäßig `Ein` gesetzt.

## Behauptungen (Vorausschau, Rückblick)

<a name="assertions"></a>

Positive Vorausschau-Behauptung: `foo(?=bar)` passt zu "foo" nur vor
"bar", und "bar" ist von der Übereinstimmung ausgeschlossen.

Negative Vorausschau-Behauptung: `foo(?!bar)` passt zu "foo" nur, wenn es nicht von "bar" gefolgt wird.

Positive Rückblick-Behauptung: `(?<=foo)bar` passt zu "bar" nur nach
"foo", und "foo" ist von der Übereinstimmung ausgeschlossen.

Negative Rückblick-Behauptung: `(?<!foo)bar` passt zu "bar" nur, wenn es nicht mit "foo" beginnt.

Einschränkungen:

- Rückblicke variabler Länge dürfen keine Erfassungsgruppen enthalten.
  Dies kann durch Einstellen der Eigenschaft `AllowUnsafeLookBehind` erlaubt werden.
  Wenn dies aktiviert ist und es im Text mehr als eine Übereinstimmung gibt, die
  die Gruppe erfassen könnte, dann könnte die falsche Übereinstimmung erfasst werden. Dies
  beeinträchtigt nicht die Richtigkeit der gesamten Behauptung. (D.h., der
  Rückblick wird korrekt zurückgeben, ob der Text davor dem
  Muster entsprach).
- Rückblicke variabler Länge können langsam ausgeführt werden, wenn sie nicht
  übereinstimmen.

## Nicht erfassende Gruppen

Die Syntax ist wie folgt: `(?:expr)`.

Solche Gruppen haben keinen "Index" und sind für
Rückverweise unsichtbar. Nicht erfassende Gruppen werden verwendet, wenn du eine
Unterexpression gruppieren möchtest, aber sie nicht als übereinstimmenden/erfassten Teil der Zeichenkette speichern möchtest. Also ist dies nur eine Möglichkeit, deinen Regex in
Unterexpressionen zu organisieren, ohne die Last der Erfassung des Ergebnisses:

| RegEx                          | Übereinstimmungen                                                       |
|--------------------------------|-------------------------------------------------------------------------|
| `(https?|ftp)://([^/\r\n]+)`   | in `https://sorokin.engineer` übereinstimmt `https` und `sorokin.engineer` |
| `(?:https?|ftp)://([^/\r\n]+)` | in `https://sorokin.engineer` übereinstimmt nur `sorokin.engineer`      |

## Atomare Gruppen

Die Syntax ist wie folgt: `(?>expr|expr|...)`.

Atomare Gruppen sind ein Spezialfall von nicht erfassenden Gruppen. [Beschreibung von
ihnen.](https://regular-expressions.mobi/atomic.html?wlr=1)

## Inline-Modifikatoren

<a name="inlinemodifiers"></a>

Syntax für einen Modifikator: `(?i)` zum Einschalten, und `(?-i)` zum Ausschalten.
Viele Modifikatoren sind erlaubt wie: `(?msgxr-imsgxr)`.

Du kannst sie innerhalb des regulären Ausdrucks verwenden, um Modifikatoren
on-the-fly zu ändern. Dies kann besonders praktisch sein, weil es lokalen Geltungsbereich in einem
regulären Ausdruck hat. Es betrifft nur den Teil des regulären Ausdrucks, der auf `(?imsgxr-imsgxr)` folgt.

Und wenn es innerhalb einer Gruppe ist, betrifft es nur diese Gruppe - speziell
den Teil der Gruppe, der auf die Modifikatoren folgt. Also in
`((?i)Saint)-Petersburg` betrifft es nur die Gruppe `((?i)Saint)`, sodass es `saint-Petersburg` aber nicht `saint-petersburg` übereinstimmt.

Inline-Modifikatoren können auch als Teil einer nicht erfassenden Gruppe gegeben werden:
`(?i:Muster)`.

| RegEx                        | Übereinstimmungen                                   |
|------------------------------|-----------------------------------------------------|
| `(?i)Saint-Petersburg`       | `Saint-petersburg` und `Saint-Petersburg`           |
| `(?i)Saint-(?-i)Petersburg`  | `Saint-Petersburg` aber nicht `Saint-petersburg`    |
| `(?i)(Saint-)?Petersburg`    | `Saint-petersburg` und `saint-petersburg`           |
| `((?i)Saint-)?Petersburg`    | `saint-Petersburg`, aber nicht `saint-petersburg`   |

## Kommentare

Die Syntax ist wie folgt: `(?#text)`. Text innerhalb der Klammern wird ignoriert.

Beachte, dass der Kommentar durch das nächstgelegene `)` geschlossen wird, also gibt es keine Möglichkeit,
ein wörtliches `)` im Kommentar zu setzen.

## Rekursion

Die Syntax ist `(?R)`, das Alias ist `(?0)`.

Der Regex `a(?R)?z` passt auf ein oder mehrere "a" gefolgt von genau
derselben Anzahl von "z".

Der Hauptzweck der Rekursion ist das Abgleichen von ausgeglichenen oder verschachtelten
Konstrukten. Der generische Regex lautet `b(?:m|(?R))*e`, wobei "b" den Anfang des Konstrukts,
"m" das, was in der Mitte des Konstrukts vorkommen kann, und "e" das, was am Ende des Konstrukts steht, darstellt.

Wenn das, was in der Mitte des ausgeglichenen Konstrukts erscheinen kann, auch
allein ohne die Anfangs- und Endteile erscheinen kann, dann lautet der generische Regex `b(?R)*e|m`.

## Unterprogrammaufrufe

Syntax für den Aufruf von nummerierten Gruppen: `(?1)` ... `(?90)` (der maximale Index ist
durch den Code begrenzt).

Syntax für den Aufruf von benannten Gruppen: `(?P>name)`. Auch die Perl-Syntax wird
unterstützt: `(?&name)`, `\g<name>` und `\g'name'`.

Dies ähnelt der Rekursion, ruft aber nur den Code der Erfassungsgruppe mit
dem angegebenen Index auf.

## Unicode-Kategorien

Der Unicode-Standard hat Namen für Zeichenkategorien. Dies sind 2-Buchstaben-Strings. Zum Beispiel ist "Lu" für Großbuchstaben, "Ll" für Kleinbuchstaben. Und die 1-Buchstaben-Kategorie "L" steht für alle Buchstaben.

- Cc - Steuerzeichen
- Cf - Format
- Co - Private Nutzung
- Cs - Surrrogat
- Ll - Kleinbuchstabe
- Lm - Modifikationsbuchstabe
- Lo - Anderer Buchstabe
- Lt - Titelbuchstabe
- Lu - Großbuchstabe
- Mc - Zwischenraumzeichen
- Me - Umschließendes Zeichen
- Mn - Nichtabstandzeichen
- Nd - Dezimalzahl
- Nl - Buchstabennummer
- No - Andere Zahl
- Pc - Verbindungszeichen
- Pd - Bindestrich
- Pe - Schließendes Satzzeichen
- Pf - Endendes Satzzeichen
- Pi - Beginnendes Satzzeichen
- Po - Anderes Satzzeichen
- Ps - Öffnendes Satzzeichen
- Sc - Währungszeichen
- Sk - Modifikatorzeichen
- Sm - Mathematisches Zeichen
- So - Anderes Symbol
- Zl - Zeilenseparator
- Zp - Absatzseparator
- Zs - Leerzeichen

Das Metazeichen `\p` steht für ein Unicode-Zeichen der angegebenen Kategorie.
Syntax: `\pL` und `\p{L}` für einen 1-Buchstaben-Namen, `\p{Lu}` für 2-Buchstaben-Namen.

Das Metazeichen `\P` ist invertiert, es steht für ein Unicode-Zeichen **nicht** in
der angegebenen Kategorie.

Diese Metazeichen werden auch innerhalb von Zeichenklassen unterstützt.

## Nachwort

In diesem [alten Blogbeitrag aus dem letzten
Jahrhundert](https://sorokin.engineer/posts/en/text_processing_from_birds_eye_view.html)
illustriere ich einige Anwendungen von regulären Ausdrücken.
