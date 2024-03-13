# Регулярные выражения (RegEx)

## Вступление

Регулярные выражения являются простым и мощным инструментом для сложного 
поиска и замены, а также для проверки текста на основе шаблонов.

Ниже приведена исчерпывающая шпаргалка по регулярных выражениям всего на
одной странице.

## Символы

### Простые совпадения

Любой отдельный символ (кроме специальных символов регулярных выражений) 
совпадает сам с собой. 
Последовательность символов (не специальных) совпадает с такой же последовательностью 
символов во входной строке.

| RegEx    | Находит  |
|----------|----------|
| `foobar` | `foobar` |

### Непечатаемые символы (escape-коды)
Чтобы указать символ по его Unicode коду, используйте префикс `\x`, за которым следует 
шестнадцатеричный код. 
Для кода из 3-4 цифр (после U+00FF) заключите код в фигурные скобки.

| RegEx        | Находит                                             |
|--------------|-----------------------------------------------------|
| `\xAB`       | символ с 2-значным шестнадцатеричным кодом `AB`     |
| `\x{AB20}`   | символ с 1-4 значным шестнадцатеричным кодом `AB20` |
| `foo\x20bar` | `foo bar` (обратите внимание на пробел в середине)  |

Существует ряд предопределённых escape-кодов для непечатаемых символов, как в языке C:

<table>
<thead>
<tr class="header">
<th>RegEx</th>
<th>Находит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\t</code>  </td>
<td>tab (HT/TAB), тоже что <code>\x09</code></td>
</tr>
<tr class="even">
<td><code>\n</code>  </td>
<td>символ новой строки (LF), то же что <code>\x0a</code></td>
</tr>
<tr class="odd">
<td><code>\r</code>  </td>
<td>возврат каретки (CR), тоже что <code>\x0d</code></td>
</tr>
<tr class="even">
<td><code>\f</code>  </td>
<td>form feed (FF), то же что <code>\x0c</code></td>
</tr>
<tr class="odd">
<td><code>\a</code>  </td>
<td>звонок (BEL), тоже что <code>\x07</code></td>
</tr>
<tr class="even">
<td><code>\e</code>  </td>
<td>escape (ESC), то же что <code>\x1b</code></td>
</tr>
<tr class="odd">
<td><p><code>\cA</code> ... <code>\cZ</code></p></td>
<td><div class="line-block">chr(0) по chr(25).<br />
Например <code>\cI</code> соответствует табуляции.<br />
Также поддерживаются буквы в нижнем регистре "a"..."z".</div></td>
</tr>
</tbody>
</table>

<a name="escape"></a>

### Экранирование(escaping

Чтобы представить специальный символ регулярного выражения (один из `.+*?|\()[]{}^$`), 
поставьте перед ним обратный слэш `\`. 
Сам обратный слэш также должен быть экранирован.

| RegEx         | Находит                                                             |
|---------------|---------------------------------------------------------------------|
| `\^FooBarPtr` | `^FooBarPtr` здесь `^` не означает [начало строки](#lineseparators) |
| `\[a\]`       | `[a]` это не [класс символов](#userclass)                           |

## Классы символов

<a name="userclass"></a>

### User Классы символов

Класс символов - это список символов внутри квадратных скобок `[]`. 
Класс совпадает с любым **одиночным** символом, указанным в этом классе.

| RegEx            | Находит                                                      |
|------------------|--------------------------------------------------------------|
| `foob[aeiou]r`   | `foobar`, `foober` и т. д., но не `foobbr`, `foobcr` и т. д. |

Вы можете "инвертировать" класс - если первым символом после `[` является `^`, 
тогда класс совпадает с любым символом, **кроме** символов, перечисленных в классе.

| RegEx           | Находит                                                      |
|-----------------|--------------------------------------------------------------|
| `foob[^aeiou]r` | `foobbr`, `foobcr` и т. д., но не `foobar`, `foober` и т. д. |

Внутри списка символ тире `-` используется для указания диапазона, так что `a-z` 
представляет все символы между `a` и `z`, включая их.

Если вы хотите, чтобы само тире `-` было элементом класса, поместите его в начало или в 
конец списка, или [экранируйте](#escape) его обратным слэшем.

Если вам нужен символ `]` в качестве части класса, вы можете разместить его в начале 
списка или [экранируйте](#escape) его обратным слэшем.


| RegEx       | Находит                   |
|-------------|---------------------------|
| `[-az]`     | `a`, `z` и `-`            |
| `[az-]`     | `a`, `z` и `-`            |
| `[А\-z]`    | `a`, `z` и `-`            |
| `[a-z]`     | символы от `a` до `z`     |
| `[\n-\x0D]` | символы от `#10` до `#13` |

### Метасимвол `.` (точка)

Метасимвол `.` (точка) по умолчанию совпадает с любым символом. 
Но если вы **выключите** [модификатор /s](#s), тогда он не будет совпадать с символами 
переноса строки.

Символ `.` не действует как мета-класс внутри 
[пользовательских классов символов](#user-классы-символов). 
`[.]` означает буквальную точку.

### Метаклассы

Существует ряд предопределённых классов символов, которые делают регулярные 
выражения более компактными, "мета-классы":

<table>
<thead>
<tr class="header">
<th>RegEx</th>
<th>Находит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\w</code>    </td>
<td>буквенно-цифровой символ (включая <code>_</code>)</td>
</tr>
<tr class="even">
<td><code>\W</code>    </td>
<td>не буквенно-цифровой</td>
</tr>
<tr class="odd">
<td><code>\d</code>    </td>
<td>числовой символ (тоже, что <code>[0-9]</code>)</td>
</tr>
<tr class="even">
<td><code>\D</code>    </td>
<td>нечисловой</td>
</tr>
<tr class="odd">
<td><code>\s</code>    </td>
<td>любой пробел (такой же как <code>[\t\n\r\f]</code>)</td>
</tr>
<tr class="even">
<td><code>\S</code>    </td>
<td>не пробел</td>
</tr>
<tr class="odd">
<td><p><code>\h</code></p></td>
<td><div class="line-block">горизонтальный разделитель. Табуляция,
пробел и все символы<br />
в Unicode категории "разделители" (space separator Unicode
category)</div></td>
</tr>
<tr class="even">
<td><code>\H</code></td>
<td>не горизонтальный разделитель</td>
</tr>
<tr class="odd">
<td><p><code>\v</code></p></td>
<td><div class="line-block">вертикальные разделители. новая строка и все
символы<br />
"разделители строк" в Unicode</div></td>
</tr>
<tr class="even">
<td><code>\V</code></td>
<td>не вертикальный разделитель</td>
</tr>
<tr class="odd">
<td><p><code>\R</code></p></td>
<td><div class="line-block">unicode line break: LF, pair CR LF,
CR,<br />
FF (form feed), VT (vertical tab), U+0085, U+2028, U+2029</div></td>
</tr>
</tbody>
</table>

Вы можете использовать все мета-классы, упомянутые в таблице выше, внутри 
[пользовательских классов символов](#user-классы-символов).


| RegEx         | Находит                                                                |
|---------------|------------------------------------------------------------------------|
| `foob\dr`     | `foob1r`, `foob6r` и т. д., но не `foobar`, `foobbr` и т. д.           |
| `foob[\w\s]r` | `foobar`, `foob r`, `foobbr` и т. д., но не `foob1r`, `foob=r` и т. д. |

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Properties [SpaceChars](tregexpr.md#spacechars) and
> [WordChars](tregexpr.md#wordchars) define character classes `\w`,
> `\W`, `\s`, `\S`.
>
> Таким образом, вы можете переопределить эти классы.

## Разделители

<a name="lineseparators"></a>

### Разделители строк

| Метасимвол | Находит                                                                               |
|------------|---------------------------------------------------------------------------------------|
| `^`        | совпадение нулевой длины в начале строки                                              |
| `$`        | совпадение нулевой длины в конце строки                                               |
| `\A`       | совпадение нулевой длины в начале строки                                              |
| `\z`       | совпадение нулевой длины в конце строки                                               |
| `\Z`       | похож на `\z` но совпадает перед разделителем строки, а не сразу после него, как `\z` |
| `\G`       | совпадение нулевой длины в конечной позиции предыдущего совпадения                    |

Примеры:

| RegEx      | Находит                                               |
|------------|-------------------------------------------------------|
| `^foobar`  | `foobar` только если он находится в начале строки     |
| `foobar$`  | `foobar`, только если он в конце строки               |
| `^foobar$` | `foobar` только если это единственная строка в строке |
| `foob.r`   | `foobar`, `foobbr`, `foob1r` и так далее              |

Метасимвол `^` совпадает с позицией нулевой длины в начале входной строки. `$` - в конце. 
Если [модификатор /m](#m) **включен**, они также совпадают с началом/концом отдельных 
строк в многострочном тексте.

Обратите внимание, что в последовательности `\x0D\x0A` нет пустой
строки.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Если вы используете [версию Unicode](tregexpr.md#unicode), то
> `^`/`$` также совпадают с `\x2028`, `\x2029`, `\x0B`, `\x0C` или `\x85`.

Метасимвол `\A` совпадает с позицией нулевой длины в самом начале входной строки, 
`\z` - в самом конце. 
Они игнорируют [модификатор /m](#m). `\Z` похож на `\z`, но совпадает перед 
разделителем строки, а не сразу после него, как `\z` (LF и CR LF). 
Поведение `\A`, `\z`, `\Z` реализовано подобно большинству движков regex
(Perl, PCRE и т.д.).

Отметим, что `^.*$` не совпадает со строкой между `\x0D\x0A`, потому что это 
неразрывный разделитель строк. 
Но оно совпадает с пустой строкой в последовательности `\x0A\x0D`, потому что это 
2 перевода строки в неправильном порядке.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Обработка многострочного текста может быть настроена с помощью свойств
> [LineSeparators](tregexpr.md#lineseparators) и
> [UseLinePairedBreak](tregexpr.md#linepairedseparator).
>
> Таким образом, вы можете использовать разделители в стиле Unix `\n` или DOS/Windows `\r\n`
> или смешивать их вместе (как в описанном выше поведении по умолчанию).

Если вы предпочитаете математически точное описание, вы можете найти его на
[www.unicode.org](http://www.unicode.org/unicode/reports/tr18/).

### Разделители слов

| RegEx | Находит                     |
|-------|-----------------------------|
| `\b`  | разделитель слов            |
| `\B`  | разделитель с **не**-словом |

Граница слова `\b` - это место между двумя символами, где с одной стороны находится 
`\w`, а с другой - `\W` (в любом порядке).

<a name="iterator"></a>

## Повторы

### Повтор

Любой элемент регулярного выражения может быть снабжён квантификатором. 
Квантификатор указывает количество повторений элемента.

| RegEx    | Находит                                                  |
|----------|----------------------------------------------------------|
| `{n}`    | ровно `n` раз                                            |
| `{n,}`   | по крайней мере `n` раз                                  |
| `{,m}`   | not more than `m` times (only with AllowBraceWithoutMin) |
| `{n,m}`  | по крайней мере `n`, но не более чем `m` раз             |
| `*`      | ноль или более, аналогично `{0,}`                        |
| `+`      | один или несколько, похожие на `{1,}`                    |
| `?`      | ноль или единица, похожая на `{0,1}`                     |

Так, цифры в фигурных скобках `{n,m}` указывают минимальное количество совпадений 
`n` и максимальное `m`.

`{n}` эквивалентен `{n,n}` и совпадает ровно `n` раз. `{n,}` совпадает `n` или более раз.

Вариант `{,m}` поддерживается только если установлено свойство AllowBraceWithoutMin.

Теоретически значение n и m не ограничены (можно использовать
максимальное значение для 32-х битного числа).

Использование `{` без указания корректного диапазона приведет к ошибке. 
Это поведение может быть изменено установкой свойства AllowLiteralBraceWithoutRange, 
которое позволит принять `{` как буквальный символ, если за ним не следует диапазон. 
Диапазон, в котором нижнее значение больше верхнего, всегда приведет к ошибке.

| RegEx            | Находит                                                                                                                                                                                           |
|------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `foob.*r`        | `foobar`, `foobalkjdflkj9r` и `foobr`                                                                                                                                                             |
| `foob.+r`        | `foobar`, `foobalkjdflkj9r`, но не `foobr`                                                                                                                                                        |
| `foob.?r`        | `foobar`, `foobbr` и `foobr`, но не `foobalkj9r`                                                                                                                                                  |
| `fooba{2}r`      | `foobaar`                                                                                                                                                                                         |
| `fooba{2}r` \`   | <span class="title-ref">foobaar</span><span class="title-ref">, </span><span class="title-ref">foobaaar</span><span class="title-ref">, </span><span class="title-ref">foobaaaar</span>\` и т. д. |
| `fooba{2,3}r`    | `foobaar`, или `foobaaar`, но не `foobaaaar`                                                                                                                                                      |
| `(foobar){8,10}` | `8`, `9` или `10` экземпляров `foobar` (`()` это [Группа](#subexpression))                                                                                                                        |

<a name="greedy"></a>

### Жадность

[Повтор](#iterator) в "жадном" режиме берет столько, сколько возможно, в
"ленивом" режиме - как можно меньше.

По умолчанию все квантификаторы "жадные". 
Добавьте символ `?`, чтобы сделать любой квантификатор "ленивым".

Для строки `abbbbc`:

| RegEx     | Находит       |
|-----------|---------------|
| `b+`      | `bbbb`        |
| `Ь+?`     | `b`           |
| `b*?`     | пустую строку |
| `b{2,3}?` | `bb`          |
| `b{2,3}`  | `bbb`         |

Вы можете переключить все квантификаторы в "ленивый" режим 
([модификатор /g](#g), ниже мы используем 
[изменение модификатора в строке](#inlinemodifiers)).

| RegEx     | Находит |
|-----------|---------|
| `(?-g)Ь+` | `b`     |

### Сверхжадные повторы (Possessive Quantifier)

Синтаксис следующий: `a++`, `a*+`, `a?+`, `a{2,4}+`. 
В настоящее время это поддерживается только для простых скобок, но не для скобок 
после группы, как в `(foo|bar){3,5}+`.

Эта функция регулярных выражений 
[описана здесь](https://regular-expressions.mobi/possessive.html?wlr=1). 
Коротко говоря, владеющий квантификатор ускоряет совпадение в сложных случаях.

## Альтернативы

Выражения в списке альтернатив разделяются `|`.

Таким образом, `fee|fie|foe` совпадет с любым из `fee`, `fie` или `foe` в 
целевой строке (как и `f(e|i|o)e`).

Первое выражение включает все, начиная от последнего разделителя шаблона (`(`, `[` 
или начала шаблона) до первого `|`, а последнее выражение содержит все от последнего `|` 
до следующего разделителя шаблона.

Звучит немного сложно, поэтому обычно выбор заключают в скобки, чтобы минимизировать 
путаницу относительно его начала и конца.

Выражения в выборе пробуются слева направо, поэтому выбирается первое совпадающее 
выражение.

Например, регулярное выражение `foo|foot` в строке `barefoot` совпадет с `foo`. 
Просто с первым совпадающим выражением.

Также помните, что `|` интерпретируется как буквальный символ в квадратных скобках, 
так что если вы напишете `[fee|fie|foe]`, на самом деле вы совпадете только с `[feio|]`.

| RegEx          | Находит               |
|----------------|-----------------------|
| `foo(bar|foo)` | `foobar` или `foofoo` |

<a name="subexpression"></a>

## Группы (подвыражения)

Скобки `(...)` также могут использоваться для определения групп
(подвыражений) регулярного выражения.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Группы (подвыражения) positions, lengths and actual values will be in
> [MatchPos](tregexpr.md#matchpos), [MatchLen](tregexpr.md#matchlen)
> and [Match](tregexpr.md#match).
>
> You can substitute them with [Substitute](tregexpr.md#substitute).

Группы (подвыражения) are numbered from left to right by their opening
parenthesis (including nested groups). First group has index 1. The
entire regex has index 0.

> | Группы | (подвыражения) значение |
> |--------|-------------------------|
> | 0      | `foobar`                |
> | 1      | `foobar`                |
> | 2      | `bar`                   |

## Ссылки на группы (Backreferences)

Метасимволs `\1` through `\9` are interpreted as backreferences to
capture groups. They match the previously found group with the specified
index.

The meta char `\g` followed by a number is also interpreted as
backreferences to capture groups. It can be followed by a multi-digit
number.

| RegEx      | Находит                 |
|------------|-------------------------|
| `(.)\1+`   | `aaaa` и `cc`           |
| `(.+)\1+`  | также `abab` и `123123` |
| `(.)\g1+`  | `aaaa` и `cc`           |

RegEx `(['"]?)(\d+)\1` matches `"13"` (in double quotes), or `'4'` (in
single quotes) or `77` (without quotes) etc.

## Named Группы (подвыражения) and Ссылки на группы (Backreferences)

Чтобы присвоить имя группе используйте `(?P<name>expr)` или
`(?'name'expr)`. And further: `(?<name>expr)`

Имя группы должно начинаться с буквы или `_`, далее следуют буквы, цифры
или `_`. Именованные и не именованные группы имеют общую нумерацию от
`1` до `9`.

Ссылки на группы (Backreferences) to named groups are `(?P=name)`, the
numbers `\1` to `\9` can also be used. As well as the example `\g` and
`\k` in the table below.

# Supported syntax are

`(?P=name)` `\g{name}` `\k{name}` `\k<name>` `\k'name'` ============

Example

| RegEx                    | Находит             |
|--------------------------|---------------------|
| `(?P<qq>['"])\w+(?P=qq)` | `"word"` и `'word'` |

## Matched Result

The begin of the reported match can be set using `\K`.

By default the entire text covered by a pattern is considered matched.
However it is possible to set explicitly what will be reported.

The pattern `a\Kb` will require the text to contain "ab". But only the
"b" will be reported as having been matched. Their can be several `\K`
in a pattern, The last one will set the match-start position. Only `\K`
in active parts of the pattern are considered. E.g. `a(\Kb)?` will not
consider `\K` if there is no "b". Captures can exist outside the match
set by `\K`.

If used in other constructs that can apply outside the reported match
(like look-ahead), then the position marked by `\K` must be before or at
the reported end of the match. If the position is marked later, the
match is considered failed.

`\K` is somewhat similar to a look-behind. Unlike a look-behind the part
of the pattern before the `\K` must be after the start position of the
matching, if the pattern is applied from an offset position within the
text.

## Модификаторы

Модификаторы are for changing behaviour of regular expressions.

You can set modifiers globally in your system or change inside the
regular expression using the [(?imsxr-imsxr)](#inlinemodifiers).

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> To change modifiers use [ModifierStr](tregexpr.md#modifierstr) or
> appropriate `TRegExpr` properties
> [Modifier\*](tregexpr.md#modifieri).
>
> The default values are defined in [global
> variables](tregexpr.md#global-constants). For example global
> variable `RegExprModifierX` defines default value for `ModifierX`
> property.

<a name="i"></a>

### i, без учета регистра

Case-insensitive. Use installed in you system locale settings, see also
[InvertCase](tregexpr.md#invertcase).

<a name="m"></a>

### m, многострочные строки

Treat string as multiple lines. So `^` and `$` matches the start or end
of any line anywhere within the string.

See also [Line Разделители](#lineseparators).

<a name="s"></a>

### s, одиночные строки

Treat string as single line. So `.` matches any character whatsoever,
even a line separators.

See also [Line Разделители](#lineseparators), which it normally would
not match.

<a name="g"></a>

### г, жадность

> [!NOTE]
> Специфичный для [TRegExpr](tregexpr.md) модификатор.

Switching it `Off` you’ll switch [quantifiers](#iterator) into
[non-greedy](#greedy) mode.

So, if modifier `/g` is `Off` then `+` works as `+?`, `*` as `*?` and so
on.

По умолчанию этот модификатор имеет значение `Выкл`.

<a name="x"></a>

### x, расширенный синтаксис

Allows to comment regular expression and break them up into multiple
lines.

If the modifier is `On` we ignore all whitespaces that is neither
backslashed nor within a character class.

Также символ `#` отделяет комментарии.

Notice that you can use empty lines to format regular expression for
better readability:

``` text
(
(abc) # comment 1
#
(efg) # comment 2
)
```

This also means that if you want real whitespace or `#` characters in
the pattern (outside a character class, where they are unaffected by
`/x`), you’ll either have to escape them or encode them using octal or
hex escapes.

<a name="r"></a>

### г, русские диапазоны

> [!NOTE]
> Специфичный для [TRegExpr](tregexpr.md) модификатор.

In Russian ASCII table characters `ё`/`Ё` are placed separately from
others.

Big and small Russian characters are in separated ranges, this is the
same as with
[English](https://regex.sorokin.engineer/regular_expressions/)
characters but nevertheless I wanted some short form.

With this modifier instead of `[а-яА-ЯёЁ]` you can write `[а-Я]` if you
need all Russian characters.

Когда модификатор включен:

| RegEx | Находит                     |
|-------|-----------------------------|
| `а-я` | символы от `а` до `я` и `ё` |
| `А-Я` | символы от `А` до `Я` и `Ё` |
| `а-Я` | все русские символы         |

Модификатор по умолчанию установлен на `Вкл`.

## Проверки или заглядывания вперед и назад (Assertions)

<a name="assertions"></a>

Заглядывание вперед (lookahead assertion) `foo(?=bar)` совпадает "foo"
только перед "bar", при этом сама строка "bar" не войдет в найденный
текст.

Отрицательное заглядывание вперед (negative lookahead assertion):
`foo(?!bar)` совпадает "foo" только если после этой строки не следует
"bar".

Ретроспективная проверка (lookbehind assertion): `(?<=foo)bar` совпадает
"bar" только после "foo", при этом сама строка "foo" не войдет в
найденный текст.

Отрицательное заглядывание назад (negative lookbehind assertion):
`foo(?!bar)` совпадает "bar" только если перед этой строкой нет "foo".

Ограничения:

- Variable length lookbehind are not allowed to contain capture groups.
  This can be allowed by setting the property `AllowUnsafeLookBehind`.
  If this is enabled and there is more than one match in the text that
  the group might capture, then the wrong match may be captured. This
  does not affect the correctness of the overall assertion. (I.e., the
  lookbehind will correctly return if the text before matched the
  pattern).
- Variable length lookbehind may be slow to execute, if they do not
  match.

## Non-capturing Группы (подвыражения)

Синтаксис: `(?:subexpression)`.

Such groups do not have the "index" and are invisible for
backreferences. Non-capturing groups are used when you want to group a
subexpression, but you do not want to save it as a matched/captured
portion of the string. So this is just a way to organize your regex into
subexpressions without overhead of capturing result:

| RegEx                          | Находит                                                              |
|--------------------------------|----------------------------------------------------------------------|
| `(https?|ftp)://([^/\r\n]+)`   | in `https://sorokin.engineer` matches `https` and `sorokin.engineer` |
| `(?:https?|ftp)://([^/\r\n]+)` | in `https://sorokin.engineer` matches only `sorokin.engineer`        |

## Atomic Группы (подвыражения)

Синтаксис: `(?>expr|expr|...)`.

Atomic groups are special case of non-capturing groups. [Description of
them.](https://regular-expressions.mobi/atomic.html?wlr=1)

## Inline Модификаторы

<a name="inlinemodifiers"></a>

Синтаксис для одного модификатора: `(?i)` чтобы включить, и `(?-i)`
чтобы выключить. Для большого числа модификаторов используется
синтаксис: `(?msgxr-imsgxr)`.

You may use it inside regular expression for modifying modifiers
on-the-fly. This can be especially handy because it has local scope in a
regular expression. It affects only that part of regular expression that
follows `(?imsgxr-imsgxr)` operator.

And if it's inside group, it will affect only this group - specifically
the part of the group that follows the modifiers. So in
`((?i)Saint)-Petersburg` it affects only group `((?i)Saint)` so it will
match `saint-Petersburg` but not `saint-petersburg`.

Inline modifiers can also be given as part of a non-capturing group:
`(?i:pattern)`.

| RegEx                        | Находит                                      |
|------------------------------|----------------------------------------------|
| `(?i)Saint-Petersburg`       | `Saint-petersburg` и `Saint-Petersburg`      |
| `(?i)Saint-(?-i)Petersburg`  | `Saint-Petersburg`, но не `Saint-petersburg` |
| `(?i)(Saint-)?Petersburg`    | `Saint-petersburg` и `saint-petersburg`      |
| `((?i)Saint-)?Petersburg`    | `saint-Petersburg`, но не `saint-petersburg` |

## Комментарии

Синтаксис: `(?#text)`. Все, что внутри скобок, игнорируется.

Note that the comment is closed by the nearest `)`, so there is no way
to put a literal `)` in the comment.

## Рекурсия

Синтаксис `(?R)`, синоним `(?0)`.

Выражение `a(?R)?z` совпадает с одним или более символом "a" за которым
следует точно такое же число символов "z".

Основное назначение рекурсии - сбалансировать обрамление вложенного
текста. Общий вид `b(?:m|(?R))*e` где "b" это то что начинает
обрамляемый текст, "m" это собственно текст, и "e" это то, что завершает
обрамление.

Если же обрамляемый текст также может встречаться без обрамления то
выражение будет `b(?R)*e|m`.

## Вызовы подвыражений

Нумерованные группы (подвыражения) обозначают `(?1)` ... `(?90)`
(максимальное число групп определяется константой в TRegExpr).

Синтаксис для именованных групп : `(?P>name)`. Поддерживается также Perl
вариант синтаксиса: `(?&name)`.

# Supported syntax are

`(?number)` `(?P>name)` `(?&name)` `\g<name>` `\g'name'` ============

Это похоже на рекурсию, но повторяет только указанную группу
(подвыражение).

## Unicode категории (category)

В стандарте Unicode есть именованные категории символов (Unicode
category). Категория обозначается одной буквой, и еще одна добавляется,
чтобы указать подкатегорию. Например "L" это буква в любом регистре,
"Lu" - буквы в верхнем регистре, "Ll" - в нижнем.

- Cc - Control
- Cf - Формат
- Co - Частное использование
- Cs - Заменитель (Surrrogate)
- Ll - Буква нижнего регистра
- Lm - Буква-модификатор
- Lo - Прочие буквы
- Lt - Titlecase Letter
- Lu - Буква в верхнем регистре
- Mc - Разделитель
- Me - Закрывающий знак (Enclosing Mark)
- Mn - Несамостоятельный символ, как умляут над буквой (Nonspacing Mark)
- Nd - Десятичная цифра
- Nl - Буквенная цифра - например, китайская, римская, руническая и т.д.
  (Letter Number)
- No - Другие цифры
- Pc - Connector Punctuation
- Pd - Dash Punctuation
- Pe - Close Punctuation
- Pf - Final Punctuation
- Pi - Initial Punctuation
- Po - Other Punctuation
- Ps - Open Punctuation
- Sc - Currency Symbol
- Sk - Modifier Symbol
- Sm - Математический символ
- So - Прочие символы
- Zl - Разделитель строк
- Zp - Разделитель параграфов
- Zs - Space Separator

Метасимволacter `\p` denotes one Unicode char of specified category.
Syntax: `\pL` and `\p{L}` for 1-letter name, `\p{Lu}` for 2-letter
names.

Метасимволacter `\P` is inverted, it denotes one Unicode char **not** in
the specified category.

These meta-characters are supported within character classes too.

## Послесловие

In this [ancient blog post from previous
century](https://sorokin.engineer/posts/en/text_processing_from_birds_eye_view.html)
I illustrate some usages of regular expressions.
