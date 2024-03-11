<table>
  <tr>
    <td><a href="https://regex.sorokin.engineer/">English</a></td>
    <td><a href="https://regex.sorokin.engineer/ru/">Русский</a></td>
    <td><a href="https://regex.sorokin.engineer/de/">Deutsch</a></td>
    <td>Български</td>
    <td><a href="https://regex.sorokin.engineer/fr/">Français</a></td>
    <td><a href="https://regex.sorokin.engineer/es/">Español</a></td>
  </tr>
</table>

# Въведение

Библиотеката TRegExpr имплементира [регулярни изрази](regular_expressions.md).

Регулярните изрази са лесни за използване и мощен инструмент за сложно 
търсене и замяна, както и за проверка на текст на базата на шаблони.

Те са особено полезни за валидация на потребителски вход във форми - за валидиране 
на електронни адреси и така нататък.

Също така можете да извличате телефонни номера, пощенски кодове и др. от уеб страници 
или документи, да търсите сложни модели в лог файлове и всичко, което можете да си 
представите. Правилата (шаблоните) могат да бъдат променяни без да е необходима 
рекомпилация на вашата програма.

TRegExpr е имплементиран на чист Pascal. Включен е в проекта 
[Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr): 
[package](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). 
Но също така съществува като отделна библиотека и може да бъде компилиран с Delphi 2-7, 
Borland C++ Builder 3-6.

[Колко хубава беше библиотеката](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html)

# Бърз старт

To use the library just add [the sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.md).

В [Често задавани въпроси](faq.md) можете да научите от проблемите на
другите потребители.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

If you see any problems, please [create the bug](https://github.com/andgineer/TRegExpr/issues).

# Преводи

Документацията е преведена на
[английски](https://regex.sorokin.engineer/) и
[руски](https://regexpr.sorokin.engineer/ru/).

Има незавършени преводи на няколко други езика. Ако искате да помогнете да ги завършите,
[свържете се с мен](https://github.com/andgineer).

# благодарност

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- [Alexey Torgashin](https://github.com/Alexey-T) - main contributor since 2019, e.g. 
- named groups, non-capturing groups, assertions, backward search and much more
- Guido Muehlwitz - открил и фиксирал грозна грешка при обработката на
  големи низове
- Stephan Klimek - testing in C++Builder and suggesting/implementing
  many features
- Стив Мъдфорд - внедрен параметър Offset
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) -German
  translation, usefull suggestions
- Yury Finkel - implemented Unicode support, found and fixed some bugs
- Ralf Junker - implemented some features, many optimization suggestions
- Симеон Лилов - превод на български език
- Filip Jirsбk and Matthew Winter - help in implementation non-greedy
  mode
- Kit Eason - many examples for introduction help section
- Юрген Шрот - лов на бъгове и полезни предложения
- Мартин Леду - превод на френски
- Diego Calp, Аржентина - превод на испански
