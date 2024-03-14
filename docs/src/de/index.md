<table>
  <tr>
    <td><a href="https://regex.sorokin.engineer/">English</a></td>
    <td><a href="https://regex.sorokin.engineer/ru/">Русский</a></td>
    <td>Deutsch</td>
    <td><a href="https://regex.sorokin.engineer/bg/">Български</a></td>
    <td><a href="https://regex.sorokin.engineer/fr/">Français</a></td>
    <td><a href="https://regex.sorokin.engineer/es/">Español</a></td>
  </tr>
</table>

# Einführung

Die TRegExpr-Bibliothek implementiert [reguläre Ausdrücke](regular_expressions.md).

Reguläre Ausdrücke sind ein einfach zu benutzendes und leistungsfähiges Werkzeug für komplexe
Such- und Ersetzungsoperationen sowie für die Überprüfung von Texten anhand von Vorlagen.

Sie sind besonders nützlich für die Validierung von Benutzereingaben in Formularen - zum
Validieren von E-Mail-Adressen und so weiter.

Außerdem können Sie Telefonnummern, Postleitzahlen usw. von Webseiten oder
Dokumenten extrahieren, nach komplexen Mustern in Logdateien suchen und alles, was Sie sich
vorstellen können. Regeln (Vorlagen) können geändert werden, ohne Ihr Programm
neu kompilieren zu müssen.

TRegExpr ist in reinem Pascal implementiert. Es ist in [Lazarus
(Free Pascal)](http://wiki.freepascal.org/Regexpr) enthalten: 
[Paket](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). 
Aber es
existiert auch als separate Bibliothek und kann mit Delphi 2-7, Borland
C++ Builder 3-6 kompiliert werden.

[Wie gut die Bibliothek aufgenommen wurde](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Schnellstart

Um die Bibliothek zu verwenden, fügen Sie einfach [die Quellen](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
zu Ihrem Projekt hinzu und verwenden Sie die Klasse [TRegExpr](tregexpr.md).

Im [FAQ](faq.md) können Sie von den Problemen anderer Benutzer lernen.

Die sofort einsatzbereite Windows-Anwendung
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
hilft Ihnen, reguläre Ausdrücke zu lernen und zu debuggen.

Wenn Sie Probleme sehen, bitte [erstellen Sie ein Ticket](https://github.com/andgineer/TRegExpr/issues).

# Übersetzungen

Die Dokumentation wurde ins
[Englische](https://regex.sorokin.engineer/) und
[Russische](https://regexpr.sorokin.engineer/ru/) übersetzt.

Es gibt unvollständige Übersetzungen in mehrere andere Sprachen.
Wenn Sie helfen möchten, sie zu vervollständigen,
[kontaktieren Sie mich](https://github.com/andgineer).

# Dankbarkeit

Viele Funktionen wurden vorgeschlagen und viele Fehler gefunden (und sogar behoben) von
den Beitragenden von TRegExpr.

Ich kann hier nicht alle auflisten, aber ich schätze alle Fehlerberichte,
Funktionsvorschläge und Fragen, die ich von Ihnen erhalte.

- [Alexey Torgashin](https://github.com/Alexey-T) - Hauptbeitragender seit 2019, z.B.
- benannte Gruppen, nicht erfassende Gruppen, Behauptungen, rückwärtige Suche und vieles mehr
- Guido Muehlwitz - fand und korrigierte einen schweren Fehler bei der Verarbeitung großer Zeichenketten
- Stephan Klimek - Testen in C++Builder und Vorschlagen/Implementieren
  vieler Funktionen
- Steve Mudford - implementierte den Offset-Parameter
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) - deutsche
  Übersetzung, nützliche Vorschläge
- Yury Finkel - implementierte Unicode-Unterstützung, fand und korrigierte einige Fehler
- Ralf Junker - implementierte einige Funktionen, viele Optimierungsvorschläge
- Simeon Lilov - Bulgarische Übersetzung
- Filip Jirsбk und Matthew Winter - Hilfe bei der Implementierung des nicht gierigen
  Modus
- Kit Eason - viele Beispiele für den Einführungshilfeabschnitt
- Juergen Schroth - Fehlersuche und nützliche Vorschläge
- Martin Ledoux - Französische Übersetzung
- Diego Calp, Argentinien - Spanische Übersetzung
