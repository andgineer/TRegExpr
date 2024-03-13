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

Reguläre Ausdrücke sind einfach zu verwenden und ein leistungsstarkes Werkzeug für 
anspruchsvolle Such- und Ersetzungsvorgänge sowie für die Überprüfung von Texten auf
Basis von Vorlagen.

Sie sind besonders nützlich für die Validierung von Benutzereingaben in 
Eingabeformularen - zum Validieren von E-Mail-Adressen und so weiter.

Außerdem können Sie Telefonnummern, Postleitzahlen usw. von Webseiten oder Dokumenten 
extrahieren, nach komplexen Mustern in Protokolldateien suchen und alles, was Sie 
sich vorstellen können. Regeln (Vorlagen) können geändert werden, ohne Ihr Programm 
neu kompilieren zu müssen.

TRegExpr ist in reinem Pascal implementiert. 
Es ist in das [Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr): 
[package](https://github.com/fpc/FPCSource/tree/main/packages/regexpr)
Es existiert aber auch als separate Bibliothek und kann mit Delphi 2-7, Borland C++ 
Builder 3-6 kompiliert werden.

Machen Sie sich ein Bild von der
[Resonanz](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html)
bei den Anwendern.

# Schnellstart

To use the library just add [the sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.md).

In den [FAQ](faq.md) können Sie aus den Problemen anderer Nutzer
lernen.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

If you see any problems, please [create the bug](https://github.com/andgineer/TRegExpr/issues).

# Übersetzungen

Die Dokumentation wurde ins
[Englische](https://regex.sorokin.engineer/) und
[Russische](https://regexpr.sorokin.engineer/ru/) übersetzt.

Es gibt unvollständige Übersetzungen in einige andere Sprachen. 
Wenn Sie helfen möchten, diese zu vervollständigen,
[kontaktieren Sie mich](https://github.com/andgineer).

# Dankbarkeit

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- [Alexey Torgashin](https://github.com/Alexey-T) - main contributor since 2019, e.g. 
- named groups, non-capturing groups, assertions, backward search and much more
- Guido Muehlwitz - hässlicher Fehler in der Verarbeitung großer Seiten
  gefunden und behoben
- Stephan Klimek - testing in C++Builder and suggesting/implementing
  many features
- Steve Mudford - Offset-Parameter implementiert
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) -German
  translation, usefull suggestions
- Yury Finkel - implemented Unicode support, found and fixed some bugs
- Ralf Junker - implemented some features, many optimization suggestions
- Simeon Lilov - Bulgarische Übersetzung
- Filip Jirsбk and Matthew Winter - help in implementation non-greedy
  mode
- Kit Eason - many examples for introduction help section
- Jürgen Schroth - Käferjagd und nützliche Vorschläge
- Martin Ledoux - französische Übersetzung
- Diego Calp, Argentinien - spanische Übersetzung
