<table>
  <tr>
    <td>English</td>
    <td><a href="https://regex.sorokin.engineer/ru/">Русский</a></td>
    <td><a href="https://regex.sorokin.engineer/de/">Deutsch</a></td>
    <td><a href="https://regex.sorokin.engineer/bg/">Български</a></td>
    <td><a href="https://regex.sorokin.engineer/fr/">Français</a></td>
    <td><a href="https://regex.sorokin.engineer/es/">Español</a></td>
  </tr>
</table>

# Introduction

TRegExpr library implements [regular
expressions](regular_expressions.md).

Regular expressions are easy to use and powerful tool for sophisticated
search and substitution and for template based text check.

It is especially useful for user input validation in input forms - to
validate e-mail addresses and so on.

Also you can extract phone numbers, ZIP-codes etc from web-pages or
documents, search for complex patterns in log files and all you can
imagine. Rules (templates) can be changed without your program
recompilation.

TRegExpr is implemented in pure Pascal. It's included into [Lazarus
(Free Pascal)](http://wiki.freepascal.org/Regexpr): 
[package](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). 
But also it
exists as separate library and can be compiled by Delphi 2-7, Borland
C++ Builder 3-6.

[How good the library was met](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Quick start

To use the library just add [the sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.md).

In the [FAQ](faq.md) you can learn from others users problems.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

If you see any problems, please [create the bug](https://github.com/andgineer/TRegExpr/issues).

# Translations

The documentation has been translated into
[English](https://regex.sorokin.engineer/) and
[Russian](https://regexpr.sorokin.engineer/ru/).

There are incomplete translations into several other languages. 
If you want to help complete them,
[contact me](https://github.com/andgineer).

# Gratitude

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- [Alexey Torgashin](https://github.com/Alexey-T) - main contributor since 2019, e.g. 
- named groups, non-capturing groups, assertions, backward search and much more
- Guido Muehlwitz - found and fixed ugly bug in big string processing
- Stephan Klimek - testing in C++Builder and suggesting/implementing
  many features
- Steve Mudford - implemented Offset parameter
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) -German
  translation, usefull suggestions
- Yury Finkel - implemented Unicode support, found and fixed some bugs
- Ralf Junker - implemented some features, many optimization suggestions
- Simeon Lilov - Bulgarian translation
- Filip Jirsбk and Matthew Winter - help in implementation non-greedy
  mode
- Kit Eason - many examples for introduction help section
- Juergen Schroth - bug hunting and useful suggestions
- Martin Ledoux - French translation
- Diego Calp, Argentina - Spanish translation
