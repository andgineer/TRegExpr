|     |                                                                |         |                                                                |                                                                  |                                                                 |                                                                |
|-----|----------------------------------------------------------------|---------|----------------------------------------------------------------|------------------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------|
|     | [English](https://regex.sorokin.engineer/en/latest/index.html) | Русский | [Deutsch](https://regex.sorokin.engineer/de/latest/index.html) | [Български](https://regex.sorokin.engineer/bg/latest/index.html) | [Français](https://regex.sorokin.engineer/fr/latest/index.html) | [Español](https://regex.sorokin.engineer/es/latest/index.html) |

# Вступление

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

# Отзывы

[Как библиотека была
встречена](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Быстрый старт

To use the library just add [the
sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.html).

Благодаря [FAQ](faq.html) вы можете учиться на чужих ошибках.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

# Обратная связь

If you see any problems, please [create the
bug](https://github.com/andgineer/TRegExpr/issues).

# Исходный код

Чистый Object Pascal.

- [Оригинальная версия](https://github.com/andgineer/TRegExpr)
- [FreePascal fork (GitHub зеркало
  SubVersion)](https://github.com/graemeg/freepascal/blob/master/packages/regexpr/src/regexpr.pas)

# Документация

<div class="toctree" glob="" maxdepth="2">

regular_expressions tregexpr faq demos

</div>

# Переводы

The documentation is available in
[English](https://regex.sorokin.engineer/en/latest/index.html) and
[Russian](https://regexpr.sorokin.engineer/ru/latest/).

There are also old translations to German, Bulgarian, French and
Spanish. If you want to help to update this old translations please
[contact me](https://github.com/andgineer).

New translations are based on
[GetText](https://en.wikipedia.org/wiki/Gettext) and can be edited with
[Weblate](https://hosted.weblate.org/projects/tregexpr/).

They are already machine-translated and need only proof-reading and may
be some copy-pasting from old translations.

# Благодарности

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- Alexey Torgashin - основной контрибутор 2019-2020. реализовал
  именованные группы, не захватывающие группы, заглядывания вперед и
  назад, обратный поиск
- Guido Muehlwitz - обнаружена и исправлена ошибка в обработке больших
  строк
- Stephan Klimek - тестирование в CPPB и предложение / реализация многих
  функций
- Steve Mudford - реализован параметр Offset
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) -German
  translation, usefull suggestions
- Yury Finkel - реализовал поддержку UniCode, нашел и исправил некоторые
  ошибки
- Ralf Junker - Реализованы некоторые функции, много предложений по
  оптимизации
- Симеон Лилов - болгарский перевод
- Филип Джирсбк и Мэтью Винтер - помогли в реализации не жадного режима
- Kit Eason много примеров для документации
- Juergen Schroth - поиск ошибок и полезные советы
- Martin Ledoux - французский перевод
- Diego Calp, Аргентина - испанский перевод
