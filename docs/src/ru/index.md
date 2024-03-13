<table>
  <tr>
    <td><a href="https://regex.sorokin.engineer/">English</a></td>
    <td>Русский</td>
    <td><a href="https://regex.sorokin.engineer/de/">Deutsch</a></td>
    <td><a href="https://regex.sorokin.engineer/bg/">Български</a></td>
    <td><a href="https://regex.sorokin.engineer/fr/">Français</a></td>
    <td><a href="https://regex.sorokin.engineer/es/">Español</a></td>
  </tr>
</table>

# Вступление
Библиотека TRegExpr реализует [регулярные выражения](regular_expressions.md).

Регулярные выражения являются простым и мощным инструментом для сложного 
поиска и замены, а также для проверки текста на основе шаблонов.

Они особенно полезны для проверки пользовательского ввода в web-формах - для проверки 
электронных адресов и так далее.

Также вы можете извлекать номера телефонов, почтовые индексы и т.д. из веб-страниц или 
документов, искать сложные паттерны в лог-файлах и всё, что можете себе представить. 
Правила (шаблоны) могут быть изменены без перекомпиляции вашей программы.

TRegExpr реализован на чистом Pascal. 
Он включен в проект [Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr): 
[package](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). 
Но также существует как отдельная библиотека и может быть скомпилирован с помощью 
Delphi 2-7, Borland C++ Builder 3-6.

[Как библиотека была встречена](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Быстрый старт
Просто добавьте [исходники](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
в Ваш проект и используйте класс [TRegExpr](tregexpr.md).

Благодаря [FAQ](faq.md) вы можете учиться на чужих ошибках.

Вы можете скачать Windows приложение
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
созданное на базе этой библиотеки и предназначенное для отладки регулярных приложений.

Вы можете [создать тикет](https://github.com/andgineer/TRegExpr/issues).

# Переводы
Документация переведена на
[English](https://regex.sorokin.engineer/) и
[Russian](https://regexpr.sorokin.engineer/ru/).

Есть незавершенные переводы на несколько других языков. Если вы хотите помочь завершить их
[contact me](https://github.com/andgineer).

# Благодарности
Множество функций предложено и множество ошибок найдено (и даже исправлено) благодаря 
контрибуторам TRegExpr.

Я не могу перечислить здесь всех их, но я ценю все сообщения об ошибках, 
предложения по улучшению функционала и вопросы, которые я получаю от вас.

- [Alexey Torgashin](https://github.com/Alexey-T) - основной контрибутор начиная с 2019. реализовал
  именованные группы, не захватывающие группы, заглядывания вперед и
  назад, обратный поиск и многое другое
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
