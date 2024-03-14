# FAQ

## Ich habe einen schrecklichen Fehler gefunden: TRegExpr verursacht eine Zugriffsverletzungs-Ausnahme!

**Antwort**

Sie müssen das Objekt vor der Verwendung erstellen. Also, nachdem Sie etwas wie folgt deklariert haben:

``` pascal
r : TRegExpr
```

vergessen Sie nicht, die Objektinstanz zu erstellen:

``` pascal
r := TRegExpr.Create.
```

## Unterstützt es Unicode?

**Antwort**

[Wie man Unicode verwendet](tregexpr.md#unicode)

## Warum gibt TRegExpr mehr als eine Zeile zurück?

Zum Beispiel gibt das r.e. `<font .\*>` den ersten `<font` zurück, dann den Rest der Datei einschließlich des letzten `</html>`.

**Antwort**

Aus Gründen der Abwärtskompatibilität ist der [Modifikator /s](regular_expressions.md#s) standardmäßig eingeschaltet.

Schalten Sie ihn aus und `.` wird alles außer [Zeilentrennern](regular_expressions.md#lineseparators) entsprechen - genau wie Sie es wünschen.

Übrigens schlage ich `<font ([^\n>]*)>` vor, in `Match[1]` wird die URL sein.

## Warum gibt TRegExpr mehr zurück, als ich erwarte?

Zum Beispiel gibt das r.e. `<p>(.+)</p>` angewandt auf den String `<p>a</p><p>b</p>` `a</p><p>b` zurück, aber nicht `a`, wie ich erwartet hätte.

**Antwort**

Standardmäßig arbeiten alle Operatoren im „gierigen“ Modus, sodass sie so viel wie möglich entsprechen.

Wenn Sie den „nicht gierigen“ Modus möchten, können Sie „nicht gierige“ Operatoren wie `+?` usw. verwenden oder alle Operatoren mit Hilfe des Modifikators `g` in den „nicht gierigen“ Modus umschalten (verwenden Sie entsprechende TRegExpr-Eigenschaften oder den Operator `?(-g)` in r.e.).

## Wie kann man Quellen wie HTML mit Hilfe von TRegExpr parsen?

**Antwort**

Entschuldigung, Leute, aber das ist nahezu unmöglich!

Natürlich können Sie TRegExpr leicht verwenden, um einige Informationen aus HTML zu extrahieren, wie in meinen Beispielen gezeigt, aber wenn Sie ein genaues Parsing möchten, müssen Sie einen echten Parser verwenden, nicht r.e.

Eine vollständige Erklärung können Sie zum Beispiel im `Perl Cookbook` von Tom Christiansen und Nathan Torkington nachlesen.

Kurz gesagt - es gibt viele Strukturen, die leicht von einem echten Parser analysiert werden können, aber überhaupt nicht von r.e., und ein echter Parser ist viel schneller beim Parsen, weil r.e. nicht einfach den Eingabestrom scannt, sondern eine Optimierungssuche durchführt, die viel Zeit in Anspruch nehmen kann.

## Gibt es eine Möglichkeit, mehrere Übereinstimmungen eines Musters bei TRegExpr zu erhalten?

**Antwort**

Sie können Übereinstimmungen mit der Methode ExecNext iterieren.

Wenn Sie ein Beispiel möchten, schauen Sie sich bitte die Implementierung der Methode `TRegExpr.Replace` an oder die Beispiele für [HyperLinksDecorator](demos.md)

## Ich überprüfe Benutzereingaben. Warum gibt TRegExpr `True` für falsche Eingabestrings zurück?

**Antwort**

In vielen Fällen vergessen TRegExpr-Benutzer, dass der reguläre Ausdruck für die **Suche** im Eingabestring vorgesehen ist.

Also, wenn Sie zum Beispiel den Ausdruck `\d{4,4}` verwenden, werden Sie Erfolg haben bei falschen Benutzereingaben wie `12345` oder `beliebige Buchstaben 1234`.

Sie müssen vom Zeilenanfang bis zum Zeilenende prüfen, um sicherzustellen, dass nichts anderes drum herum ist: `^\d{4,4}$`.

## Warum funktionieren nicht-gierige Iteratoren manchmal wie im gierigen Modus?

Zum Beispiel entspricht das r.e. `a+?,b+?` angewandt auf den String `aaa,bbb` `aaa,b`, aber sollte es nicht `a,b` entsprechen wegen der Nicht-Gierigkeit des ersten Iterators?

**Antwort**

Dies liegt an der Arbeitsweise von TRegExpr. Tatsächlich arbeiten viele andere r.e. Engines genau gleich: Sie führen nur eine „einfache“ Suchoptimierung durch und versuchen nicht, die beste Optimierung zu finden.

In einigen Fällen ist das schlecht, aber im Allgemeinen ist es eher ein Vorteil als eine Einschränkung, aus Gründen der Leistung und Vorhersagbarkeit.

Die Hauptregel - r.e. versucht zuerst, vom aktuellen Ort aus zu entsprechen und nur, wenn das völlig unmöglich ist, bewegt es sich um ein Zeichen vorwärts und versucht es erneut von der nächsten Position im Text.

Also, wenn Sie `a,b+?` verwenden, wird es `a,b` entsprechen. Im Falle von `a+?,b+?` ist es jetzt nicht empfohlen (wir haben den nicht-gierigen Modifikator hinzugefügt), aber es ist immer noch möglich, mehr als ein `a` zu entsprechen, also wird TRegExpr es tun.

TRegExpr wie Perl oder Unix r.e. versucht nicht, vorwärts zu bewegen und zu prüfen - wäre es eine "bessere" Übereinstimmung. Vor allem, weil es keinen Weg gibt zu sagen, dass eine Übereinstimmung besser oder schlechter ist.

## Wie kann ich TRegExpr mit Borland C++ Builder verwenden?

Ich habe ein Problem, da keine Header-Datei (`.h` oder `.hpp`) verfügbar ist.

**Antwort**

- Fügen Sie `RegExpr.pas` zum `bcb`-Projekt hinzu.
- Kompilieren Sie das Projekt. Dies generiert die Header-Datei `RegExpr.hpp`.
- Jetzt können Sie Code schreiben, der die Einheit `RegExpr` verwendet.
- Vergessen Sie nicht, `#include “RegExpr.hpp”` hinzuzufügen, wo es benötigt wird.
- Vergessen Sie nicht, alle `\` in regulären Ausdrücken mit `\\` zu ersetzen oder die Konstante [EscChar](tregexpr.md#escchar) neu zu definieren.

## Warum funktionieren viele r.e. (einschließlich r.e. aus der Hilfe und Demo von TRegExpr) falsch in Borland C++ Builder?

**Antwort**

Der Hinweis ist in der vorherigen Frage ;) Das Symbol `\` hat eine besondere Bedeutung in `C++`, daher müssen Sie es „escapen“ (wie in der vorherigen Antwort beschrieben). Aber wenn Sie r.e. wie `\\w+\\w+\\.\\w+` nicht mögen, können Sie die Konstante `EscChar` (in `RegExpr.pas`) neu definieren. Zum Beispiel `EscChar = "/"`. Dann können Sie `/w+/w+/./w+` schreiben, sieht ungewöhnlich aus, aber lesbarer.
