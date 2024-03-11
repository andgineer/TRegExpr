|     |                                                                |                                                                |         |                                                                  |                                                                 |                                                                |
|-----|----------------------------------------------------------------|----------------------------------------------------------------|---------|------------------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------|
|     | [English](https://regex.sorokin.engineer/en/) | [Русский](https://regex.sorokin.engineer/ru/) | Deutsch | [Български](https://regex.sorokin.engineer/bg/) | [Français](https://regex.sorokin.engineer/fr/) | [Español](https://regex.sorokin.engineer/es/) |

# Einführung

TRegExpr library implements [regular
expressions](regular_expressions.html).

Regular expressions are easy to use and powerful tool for sophisticated
search and substitution and for template based text check.

It is especially useful for user input validation in input forms - to
validate e-mail addresses and so on.

Also you can extract phone numbers, ZIP-codes etc from web-pages or
documents, search for complex patterns in log files and all you can
imagine. Rules (templates) can be changed without your program
recompilation.

TRegExpr is implemented in pure Pascal. It's included into [Lazarus
(Free Pascal)](http://wiki.freepascal.org/Regexpr) project. But also it
exists as separate library and can be compiled by Delphi 2-7, Borland
C++ Builder 3-6.

# Rezensionen

Machen Sie sich ein Bild von der
[Resonanz](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html)
bei den Anwendern.

# Schnellstart

To use the library just add [the
sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.html).

In den [FAQ](faq.html) können Sie aus den Problemen anderer Nutzer
lernen.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

# Feedback

If you see any problems, please [create the
bug](https://github.com/andgineer/TRegExpr/issues).

# Quellcode

Ausschließlich Object Pascal.

- [Originalversion](https://github.com/andgineer/TRegExpr)
- [FreePascal-Fork (GitHub-Spiegel der
  Subversion)](https://github.com/graemeg/freepascal/blob/master/packages/regexpr/src/regexpr.pas)

# Dokumentation

<div class="toctree" glob="" maxdepth="2">

regular_expressions tregexpr faq demos

</div>

# Übersetzungen

The documentation is available in
[English](https://regex.sorokin.engineer/en/) and
[Russian](https://regexpr.sorokin.engineer/ru/latest/).

There are also old translations to German, Bulgarian, French and
Spanish. If you want to help to update this old translations please
[contact me](https://github.com/andgineer).

New translations are based on
[GetText](https://en.wikipedia.org/wiki/Gettext) and can be edited with
[Weblate](https://hosted.weblate.org/projects/tregexpr/).

They are already machine-translated and need only proof-reading and may
be some copy-pasting from old translations.

# Dankbarkeit

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- Alexey Torgashin - added many features in 2019-2020, e.g. named
  groups, non-capturing groups, assertions, backward search
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
