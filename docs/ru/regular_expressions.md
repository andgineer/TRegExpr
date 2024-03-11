# Регулярные выражения (RegEx)

## Вступление

Regular expressions are a handy way to specify patterns of text.

With regular expressions you can validate user input, search for some
patterns like emails of phone numbers on web pages or in some documents
and so on.

Ниже приведена исчерпывающая шпаргалка по регулярных выражениям всего на
одной странице.

## Символы

### Простые совпадения

Any single character (except special regex characters) matches itself. A
series of (not special) characters matches that series of characters in
the input string.

| RegEx    | Находит  |
|----------|----------|
| `foobar` | `foobar` |

### Non-Printable Символы (escape-codes)

To specify character by its Unicode code, use the prefix `\x` followed
by the hex code. For 3-4 digits code (after U+00FF), enclose the code
into braces.

| RegEx        | Находит                                             |
|--------------|-----------------------------------------------------|
| `\xAB`       | символ с 2-значным шестнадцатеричным кодом `AB`     |
| `\x{AB20}`   | символ с 1-4 значным шестнадцатеричным кодом `AB20` |
| `foo\x20bar` | `foo bar` (обратите внимание на пробел в середине)  |

There are a number of predefined escape-codes for non-printable
characters, like in C language:

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

### Эскейпинг

To represent special regex character (one of `.+*?|\()[]{}^$`), prefix
it with a backslash `\`. The literal backslash must be escaped too.

| RegEx         | Находит                                                             |
|---------------|---------------------------------------------------------------------|
| `\^FooBarPtr` | `^FooBarPtr` здесь `^` не означает [начало строки](#lineseparators) |
| `\[a\]`       | `[a]` это не [класс символов](#userclass)                           |

## Классы символов

<a name="userclass"></a>

### User Классы символов

Character class is a list of characters inside square brackets `[]`. The
class matches any **single** character listed in this class.

| RegEx            | Находит                                                      |
|------------------|--------------------------------------------------------------|
| `foob[aeiou]r`   | `foobar`, `foober` и т. д., но не `foobbr`, `foobcr` и т. д. |

You can "invert" the class - if the first character after the `[` is
`^`, then the class matches any character **except** the characters
listed in the class.

| RegEx           | Находит                                                      |
|-----------------|--------------------------------------------------------------|
| `foob[^aeiou]r` | `foobbr`, `foobcr` и т. д., но не `foobar`, `foober` и т. д. |

Within a list, the dash `-` character is used to specify a range, so
that `a-z` represents all characters between `a` and `z`, inclusive.

If you want the dash `-` itself to be a member of a class, put it at the
start or end of the list, or [escape](#escape) it with a backslash.

If you want `]` as part of the class you may place it at the start of
list or [escape](#escape) it with a backslash.

| RegEx       | Находит                   |
|-------------|---------------------------|
| `[-az]`     | `a`, `z` и `-`            |
| `[az-]`     | `a`, `z` и `-`            |
| `[А\-z]`    | `a`, `z` и `-`            |
| `[a-z]`     | символы от `a` до `z`     |
| `[\n-\x0D]` | символы от `#10` до `#13` |

### Dot Meta-Char

Метасимвол `.` (dot) by default matches any character. But if you turn
**off** the [modifier /s](#s), then it won't match line-break
characters.

The `.` does not act as meta-class inside [user character
classes](#user-классы-символов). `[.]` means a literal ".".

### Метаклассы

There are a number of predefined character classes that keeps regular
expressions more compact, "meta-classes":

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

You may use all meta-classes, mentioned in the table above, within [user
character classes](#user-классы-символов).

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

### Line Разделители

| Метасимвол | Находит                                                                               |
|------------|---------------------------------------------------------------------------------------|
| `^`        | совпадение нулевой длины в начале строки                                              |
| `$`        | совпадение нулевой длины в конце строки                                               |
| `\A`       | совпадение нулевой длины в начале строки                                              |
| `\z`       | совпадение нулевой длины в конце строки                                               |
| `\Z`       | похож на `\z` но совпадает перед разделителем строки, а не сразу после него, как `\z` |
| `\G`       | zero-length match at the end pos of the previous match                                |

Примеры:

| RegEx      | Находит                                               |
|------------|-------------------------------------------------------|
| `^foobar`  | `foobar` только если он находится в начале строки     |
| `foobar$`  | `foobar`, только если он в конце строки               |
| `^foobar$` | `foobar` только если это единственная строка в строке |
| `foob.r`   | `foobar`, `foobbr`, `foob1r` и так далее              |

Метасимвол `^` matches zero-length position at the beginning of the
input string. `$` - at the ending. If [modifier /m](#m) is **on**, they
also match at the beginning/ending of individual lines in the multi-line
text.

Обратите внимание, что в последовательности `\x0D\x0A` нет пустой
строки.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> If you are using [Unicode version](tregexpr.md#unicode), then
> `^`/`$` also matches `\x2028`, `\x2029`, `\x0B`, `\x0C` or `\x85`.

Метасимвол `\A` matches zero-length position at the very beginning of
the input string, `\z` - at the very ending. They ignore [modifier
/m](#m). `\Z` is похож на `\z` но совпадает перед разделителем строки, а
не сразу после него, как `\z` (LF and CR LF). Behaviour of `\A`, `\z`,
`\Z` is made like in most of major regex engines (Perl, PCRE, etc).

Note that `^.*$` does not match a string between `\x0D\x0A`, because
this is unbreakable line separator. But it matches the пустую строку
within the sequence `\x0A\x0D` because this is 2 line-breaks in the
wrong order.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Multi-line processing can be tuned by properties
> [LineSeparators](tregexpr.md#lineseparators) and
> [UseLinePairedBreak](tregexpr.md#linepairedseparator).
>
> So you can use Unix style separators `\n` or DOS/Windows style `\r\n`
> or mix them together (as in described above default behaviour).

If you prefer mathematically correct description you can find it on
[www.unicode.org](http://www.unicode.org/unicode/reports/tr18/).

### Word Разделители

| RegEx | Находит                     |
|-------|-----------------------------|
| `\b`  | разделитель слов            |
| `\B`  | разделитель с **не**-словом |

A word boundary `\b` is a spot between two characters that has a `\w` on
one side of it and a `\W` on the other side of it (in either order).

<a name="iterator"></a>

## Повторы

### Повтор

Any item of a regular expression may be followed by quantifier.
Quantifier specifies number of repetitions of the item.

| RegEx    | Находит                                                  |
|----------|----------------------------------------------------------|
| `{n}`    | ровно `n` раз                                            |
| `{n,}`   | по крайней мере `n` раз                                  |
| `{,m}`   | not more than `m` times (only with AllowBraceWithoutMin) |
| `{n,m}`  | по крайней мере `n`, но не более чем `m` раз             |
| `*`      | ноль или более, аналогично `{0,}`                        |
| `+`      | один или несколько, похожие на `{1,}`                    |
| `?`      | ноль или единица, похожая на `{0,1}`                     |

So, digits in curly brackets `{n,m}`, specify the minimum number of
times to match `n` and the maximum `m`.

The `{n}` is equivalent to `{n,n}` and matches ровно `n` раз. The `{n,}`
matches `n` or more times.

The variant `{,m}` is only supported if the property
AllowBraceWithoutMin is set.

Теоретически значение n и m не ограничены (можно использовать
максимальное значение для 32-х битного числа).

Using `{` without a correct range will give an error. This behaviour can
be changed by setting the property AllowLiteralBraceWithoutRange, which
will accept `{` as a literal char, if not followed by a range. A range
with a low value bigger than the high value will always give an error.

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

[Повтор](#iterator) in "greedy" mode takes as many as possible, in
"lazy" mode - as few as possible.

By default all quantifiers are "greedy". Append the character `?` to
make any quantifier "lazy".

Для строки `abbbbc`:

| RegEx     | Находит       |
|-----------|---------------|
| `b+`      | `bbbb`        |
| `Ь+?`     | `b`           |
| `b*?`     | пустую строку |
| `b{2,3}?` | `bb`          |
| `b{2,3}`  | `bbb`         |

You can switch all quantifiers into "lazy" mode ([modifier /g](#g),
below we use [in-line modifier change](#inlinemodifiers)).

| RegEx     | Находит |
|-----------|---------|
| `(?-g)Ь+` | `b`     |

### Сверхжадные повторы (Possessive Quantifier)

The syntax is: `a++`, `a*+`, `a?+`, `a{2,4}+`. Currently it's supported
only for simple braces, but not for braces after group like
`(foo|bar){3,5}+`.

This regex feature is [described
here.](https://regular-expressions.mobi/possessive.html?wlr=1) In short,
possessive quantifier speeds up matching in complex cases.

## Альтернативы

Выражения в списке альтернатив разделяются `|`.

So `fee|fie|foe` will match any of `fee`, `fie`, or `foe` in the target
string (as would `f(e|i|o)e`).

The first expression includes everything from the last pattern delimiter
(`(`, `[`, or the beginning of the pattern) up to the first `|`, and the
last expression contains everything from the last `|` to the next
pattern delimiter.

Sounds a little complicated, so it’s common practice to include the
choice in parentheses, to minimize confusion about where it starts and
ends.

Expressions in the choice are tried from left to right, so the first
expression that matches, is the one that is chosen.

For example, regular expression `foo|foot` in string `barefoot` will
match `foo`. Just a first expression that matches.

Also remember that `|` is interpreted as a literal within square
brackets, so if you write `[fee|fie|foe]` you’re really only matching
`[feio|]`.

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
