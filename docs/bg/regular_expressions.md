|     |         |                                                                              |                                                                              |                                                                                |                                                                               |                                                                              |
|-----|---------|------------------------------------------------------------------------------|------------------------------------------------------------------------------|--------------------------------------------------------------------------------|-------------------------------------------------------------------------------|------------------------------------------------------------------------------|
|     | [English](https://regex.sorokin.engineer/regular_expressions/) | [Русский](https://regex.sorokin.engineer/ru/regular_expressions/) | [Deutsch](https://regex.sorokin.engineer/de/regular_expressions/) | [Български](https://regex.sorokin.engineer/bg/regular_expressions/) | [Français](https://regex.sorokin.engineer/fr/regular_expressions/) | [Español](https://regex.sorokin.engineer/es/regular_expressions/) |

# Редовни изрази (регулярен израз)

## Въведение

Regular expressions are a handy way to specify patterns of text.

With regular expressions you can validate user input, search for some
patterns like emails of phone numbers on web pages or in some documents
and so on.

Below is the complete regular expressions cheat sheet.

## Герои

### Обикновено съвпадение

Any single character (except special regex characters) matches itself. A
series of (not special) characters matches that series of characters in
the input string.

| регулярен и | зраз кибрит |
|-------------|-------------|
| `foobar`    | `foobar`    |

### Non-Printable Герои (escape-codes)

To specify character by its Unicode code, use the prefix `\x` followed
by the hex code. For 3-4 digits code (after U+00FF), enclose the code
into braces.

| регулярен израз                             | кибрит                                                                                                |
|---------------------------------------------|-------------------------------------------------------------------------------------------------------|
| `\xAB`                                      | character with 2-digit hex code `AB`                                                                  |
| `\x{AB20}`                                  | character with 1..4-digit hex code `AB20`                                                             |
| \`<span class="title-ref"> Фу x20bar</span> | <span class="title-ref"> </span><span class="title-ref">foo bar</span>\` (забележете място в средата) |

There are a number of predefined escape-codes for non-printable
characters, like in C language:

<table>
<thead>
<tr class="header">
<th>регулярен израз</th>
<th>кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\t</code>  </td>
<td>tab (HT/TAB), същото като <code>\x09</code></td>
</tr>
<tr class="even">
<td><code>\n</code>  </td>
<td>line feed (LF), same as <code>\x0a</code></td>
</tr>
<tr class="odd">
<td><code>\r</code>  </td>
<td>carriage return (CR), same as <code>\x0d</code></td>
</tr>
<tr class="even">
<td><code>\f</code>  </td>
<td>form feed (FF), същата като <code>\x0c</code></td>
</tr>
<tr class="odd">
<td><code>\a</code>  </td>
<td>alarm (BEL), същата като <code>\x07</code></td>
</tr>
<tr class="even">
<td><code>\e</code>  </td>
<td>escape (ESC), същото като <code>\x1b</code></td>
</tr>
<tr class="odd">
<td><p><code>\cA</code> ... <code>\cZ</code></p></td>
<td><div class="line-block">chr(0) to chr(25).<br />
For example, <code>\cI</code> matches the tab-char.<br />
Lower-case letters "a"..."z" are also supported.</div></td>
</tr>
</tbody>
</table>

<a name="escape"></a>

### Escaping

To represent special regex character (one of `.+*?|\()[]{}^$`), prefix
it with a backslash `\`. The literal backslash must be escaped too.

| регулярен израз | кибрит                                                              |
|-----------------|---------------------------------------------------------------------|
| `\^FooBarPtr`   | `^FooBarPtr`, this is `` ^ and not [start of line](#lineseparators) |
| `\[a\]`         | `[a]`, this is not [character class](#userclass)                    |

## Класове на знаците

<a name="userclass"></a>

### User Класове на знаците

Character class is a list of characters inside square brackets `[]`. The
class matches any **single** character listed in this class.

| регулярен израз                                    | кибрит                                                                                                   |
|----------------------------------------------------|----------------------------------------------------------------------------------------------------------|
| \`<span class="title-ref"> foob \[аеиоу\] r</span> | \`  &quot;foobar&quot;, &quot;foober&quot; и т.н., но не и &quot;foobbr&quot;, &quot;foobcr&quot; и т.н. |

You can "invert" the class - if the first character after the `[` is ``
^, then the class matches any character **except** the characters listed
in the class.

| регулярен израз       | кибрит                                                                                                   |
|-----------------------|----------------------------------------------------------------------------------------------------------|
| \`\` Foob \[^ аеиоу\] | r\`\` &quot;foobbr&quot;, &quot;foobcr&quot; и т.н., но не &quot;foobar&quot;, &quot;foober&quot; и т.н. |

Within a list, the dash `-` character is used to specify a range, so
that `a-z` represents all characters between `a` and `z`, inclusive.

If you want the dash `-` itself to be a member of a class, put it at the
start or end of the list, or [escape](#escape) it with a backslash.

If you want `]` as part of the class you may place it at the start of
list or [escape](#escape) it with a backslash.

<table>
<thead>
<tr class="header">
<th>регулярен изра</th>
<th>з кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>[-az]</code>    </td>
<td>&amp;quot;а&amp;quot;, &amp;quot;z&amp;quot; и
&amp;quot;-`&amp;quot;</td>
</tr>
<tr class="even">
<td><code>[az-]</code>    </td>
<td>&amp;quot;а&amp;quot;, &amp;quot;z&amp;quot; и
&amp;quot;-`&amp;quot;</td>
</tr>
<tr class="odd">
<td><code>[А \ -Z]</code></td>
<td><blockquote>
<p>  &amp;quot;а&amp;quot;, &amp;quot;z&amp;quot; и
&amp;quot;-`&amp;quot;</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>[a-z]</code>    </td>
<td>символи от <code>а</code> до <code></code></td>
</tr>
<tr class="odd">
<td><code>[\n-\x0D]</code></td>
<td>characters from chr(10) to chr(13)</td>
</tr>
</tbody>
</table>

### Dot Meta-Char

Meta-char `.` (dot) by default matches any character. But if you turn
**off** the [modifier /s](#s), then it won't match line-break
characters.

The `.` does not act as meta-class inside [user character
classes](#user-класове-на-знаците). `[.]` means a literal ".".

### Meta-Classes

There are a number of predefined character classes that keeps regular
expressions more compact, "meta-classes":

<table>
<thead>
<tr class="header">
<th>регулярен и</th>
<th>зраз кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\W</code>    </td>
<td>an alphanumeric character, including <code>_</code></td>
</tr>
<tr class="even">
<td><code>\W</code>    </td>
<td>a non-alphanumeric</td>
</tr>
<tr class="odd">
<td><code>\ D</code>  </td>
<td>  a numeric character (same as <code>[0-9]</code>)</td>
</tr>
<tr class="even">
<td><code>\ D</code>  </td>
<td>  не-цифров</td>
</tr>
<tr class="odd">
<td><code>\ S</code>  </td>
<td>  всяко пространство (същото като `` [t</td>
</tr>
<tr class="even">
<td><code>\ S</code>  </td>
<td>  a non-space</td>
</tr>
<tr class="odd">
<td><p><code>\h</code></p></td>
<td><div class="line-block">horizontal whitespace: the tab and all
characters<br />
in the "space separator" Unicode category</div></td>
</tr>
<tr class="even">
<td><code>\H</code></td>
<td>not a horizontal whitespace</td>
</tr>
<tr class="odd">
<td><p><code>\v</code></p></td>
<td><div class="line-block">vertical whitespace: all characters treated
as<br />
line-breaks in the Unicode standard</div></td>
</tr>
<tr class="even">
<td><code>\V</code></td>
<td>not a vertical whitespace</td>
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
character classes](#user-класове-на-знаците).

| регулярен израз | кибрит                                                                          |
|-----------------|---------------------------------------------------------------------------------|
| `foob\dr`       | `foob1r`, `foob6r` and so on, but not `foobar`, `foobbr` and so on              |
| \`\` Foob \[т S | \] r``foobar`,`foob r`,`foobbr`and so on, but not`foob1r`,`foob=r\`\` and so on |

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/)
>
> Properties [SpaceChars](../tregexpr/#spacechars) and
> [WordChars](../tregexpr/#wordchars) define character classes `\W`,
> `\W`, `\ S`, `\ S`.
>
> So you can redefine these classes.

## граници

<a name="lineseparators"></a>

### Line граници

<table>
<thead>
<tr class="header">
<th>Meta-char</th>
<th>кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code></code> ^ </td>
<td><blockquote>
<p>zero-length match at start of line</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code></code> $ </td>
<td><blockquote>
<p>zero-length match at end of line</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><code>\ A</code></td>
<td><blockquote>
<p>zero-length match at the very beginning</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>\z</code></td>
<td>zero-length match at the very end</td>
</tr>
<tr class="odd">
<td><code>\ Z</code></td>
<td><blockquote>
<p>like <code>\z</code> but also matches before the final line-break</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>\G</code></td>
<td>zero-length match at the end pos of the previous match</td>
</tr>
</tbody>
</table>

Examples:

<table>
<thead>
<tr class="header">
<th>регулярен изра</th>
<th>з кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>^ Foobar</code></td>
<td><blockquote>
<p>&amp;quot;foobar&amp;quot; само ако е в началото на линията</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code></code> Foobar $</td>
<td><blockquote>
<p>&amp;quot;foobar&amp;quot;, само ако е в края на реда</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><code>^foobar$</code></td>
<td><code>foobar</code> само ако е единственият низ в ред</td>
</tr>
<tr class="even">
<td><code>Foob.r</code>  </td>
<td><blockquote>
<p>&amp;quot;foobar&amp;quot;, &amp;quot;foobbr&amp;quot;,
&amp;quot;foob1r&amp;quot; и т.н.</p>
</blockquote></td>
</tr>
</tbody>
</table>

Meta-char `` ^ matches zero-length position at the beginning of the
input string. `` \$ - at the ending. If [modifier /m](#m) is **on**,
they also match at the beginning/ending of individual lines in the
multi-line text.

Забележете, че няма празна линия в последователността &quot;x0D0A&quot;.

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/)
>
> If you are using [Unicode version](../tregexpr/#unicode), then ``
> ^/`` \$ also matches `\x2028`, `\x2029`, `\x0B`, `\x0C` or `\x85`.

Meta-char `\ A` matches zero-length position at the very beginning of
the input string, `\z` - at the very ending. They ignore [modifier
/m](#m). `\ Z` is like `\z` but also matches before the final line-break
(LF and CR LF). Behaviour of `\ A`, `\z`, `\ Z` is made like in most of
major regex engines (Perl, PCRE, etc).

Note that `^.*$` does not match a string between `\x0D\x0A`, because
this is unbreakable line separator. But it matches the празен низ within
the sequence `\x0A\x0D` because this is 2 line-breaks in the wrong
order.

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/)
>
> Multi-line processing can be tuned by properties
> [LineSeparators](../tregexpr/#lineseparators) and
> [UseLinePairedBreak](../tregexpr/#linepairedseparator).
>
> So you can use Unix style separators `\n` or DOS/Windows style `\r\n`
> or mix them together (as in described above default behaviour).

If you prefer mathematically correct description you can find it on
[www.unicode.org](http://www.unicode.org/unicode/reports/tr18/).

### Word граници

<table>
<thead>
<tr class="header">
<th>регуляр</th>
<th>ен израз кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\b</code>`</td>
<td><blockquote>
<p>граница на думата</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>\B</code> </td>
<td>граница без думи</td>
</tr>
</tbody>
</table>

A word boundary `\b`<span class="title-ref"> is a spot between two
characters that has a
</span><span class="title-ref">W</span><span class="title-ref"> on one
side of it and a </span><span class="title-ref">W</span>\` on the other
side of it (in either order).

<a name="iterator"></a>

## количествено определяне

### Quantifiers

Any item of a regular expression may be followed by quantifier.
Quantifier specifies number of repetitions of the item.

<table>
<thead>
<tr class="header">
<th>регулярен и</th>
<th>зраз кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>{N}</code>  </td>
<td><blockquote>
<p>точно <code>n</code> пъти</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>{П}</code>  </td>
<td><blockquote>
<p>поне <code>n</code> пъти</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><code>{,m}</code>  </td>
<td>not more than <code>m</code> times (only with
AllowBraceWithoutMin)</td>
</tr>
<tr class="even">
<td>`<span class="title-ref"> {П, т} </span></td>
<td><span class="title-ref"> най-малко </span><span
class="title-ref">n</span><span class="title-ref">, но не повече от
</span><span class="title-ref">m</span>` пъти</td>
</tr>
<tr class="odd">
<td><code>*</code>    </td>
<td>нула или повече, подобно на <code>{0,}</code></td>
</tr>
<tr class="even">
<td><code>+</code>  </td>
<td>една или повече, подобни на <code>{1,}</code></td>
</tr>
<tr class="odd">
<td><code>?</code>  </td>
<td>нула или едно, подобно на <code>{0,1}</code></td>
</tr>
</tbody>
</table>

So, digits in curly brackets `{П, т}`, specify the minimum number of
times to match `n` and the maximum `m`.

The `{N}` is equivalent to `{n,n}` and matches точно `n` пъти. The `{П}`
matches `n` or more times.

The variant `{,m}` is only supported if the property
AllowBraceWithoutMin is set.

There is no practical limit to the values n and m (limit is maximal
signed 32-bit value).

Using `{` without a correct range will give an error. This behaviour can
be changed by setting the property AllowLiteralBraceWithoutRange, which
will accept `{` as a literal char, if not followed by a range. A range
with a low value bigger than the high value will always give an error.

<table>
<thead>
<tr class="header">
<th>регулярен израз</th>
<th>кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>foob.*r</code>    </td>
<td>&amp;quot;foobar&amp;quot;, &amp;quot;foobalkjdflkj9r&amp;quot; и
&amp;quot;foobr&amp;quot;</td>
</tr>
<tr class="even">
<td><code>foob.+r</code>    </td>
<td><code>foobar</code>, <code>foobalkjdflkj9r</code>, но не
<code>foobr</code></td>
</tr>
<tr class="odd">
<td><code>foob.?r</code>    </td>
<td><code>foobar</code>, <code>foobbr</code> и <code>foobr</code>, но не
<code>foobalkj9r</code></td>
</tr>
<tr class="even">
<td><code>fooba{2}r</code>  </td>
<td><code>foobaar</code></td>
</tr>
<tr class="odd">
<td><code>fooba{2}r</code> `</td>
<td><span class="title-ref">foobaar</span><span class="title-ref">,
</span><span class="title-ref">foobaaar</span><span class="title-ref">,
</span><span class="title-ref">foobaaaar</span>` и т.н.</td>
</tr>
<tr class="even">
<td><code>Fooba {2,3} r</code></td>
<td><blockquote>
<p>&amp;quot;foobaar&amp;quot;, или &amp;quot;foobaaar&amp;quot;, но не
и &amp;quot;foobaaaar&amp;quot;</p>
</blockquote></td>
</tr>
<tr class="odd">
<td>`` (Foobar) {} 8,10</td>
<td><blockquote>
<p><code>8...10 instances of</code>foobar<code>(</code>()`<span
class="title-ref"> is `group &lt;#subexpression&gt;</span>__)</p>
</blockquote></td>
</tr>
</tbody>
</table>

<a name="greedy"></a>

### лакомия

[Quantifiers](#iterator) in "greedy" mode takes as many as possible, in
"lazy" mode - as few as possible.

By default all quantifiers are "greedy". Append the character `?` to
make any quantifier "lazy".

За низ `abbbbc`:

<table>
<thead>
<tr class="header">
<th>регулярен из</th>
<th>раз кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>Б +</code></td>
<td><blockquote>
<p><code>bbbb</code></p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>Б +?</code></td>
<td><blockquote>
<p><code>b</code></p>
</blockquote></td>
</tr>
<tr class="odd">
<td><code>В *?</code></td>
<td><blockquote>
<p>празен низ</p>
</blockquote></td>
</tr>
<tr class="even">
<td>`` б {2,3}?</td>
<td><code></code>bb``</td>
</tr>
<tr class="odd">
<td>`<span class="title-ref"> Б {2,3} </span></td>
<td><span class="title-ref"> </span><span
class="title-ref">bbb</span>`</td>
</tr>
</tbody>
</table>

You can switch all quantifiers into "lazy" mode ([modifier /g](#g),
below we use [in-line modifier change](#inlinemodifiers)).

<table>
<thead>
<tr class="header">
<th>регулярен изр</th>
<th>аз кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>`` (? -G) б +</td>
<td><blockquote>
<p><code></code>b``</p>
</blockquote></td>
</tr>
</tbody>
</table>

### Possessive Quantifier

The syntax is: `a++`, `a*+`, `a?+`, `a{2,4}+`. Currently it's supported
only for simple braces, but not for braces after group like
`(foo|bar){3,5}+`.

This regex feature is [described
here.](https://regular-expressions.mobi/possessive.html?wlr=1) In short,
possessive quantifier speeds up matching in complex cases.

## Choice

Expressions in the choice are separated by vertical bar `|`.

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

| регулярен израз      | кибрит                                           |
|----------------------|--------------------------------------------------|
| \`\` Foo (бар \| Foo | ) \`\` &quot;foobar&quot; или &quot;foofoo&quot; |

<a name="subexpression"></a>

## Groups

The brackets `()` are used to define groups (ie subexpressions).

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/)
>
> Group positions, lengths and actual values will be in
> [MatchPos](../tregexpr/#matchpos), [MatchLen](../tregexpr/#matchlen)
> and [Match](../tregexpr/#match).
>
> You can substitute them with [Substitute](../tregexpr/#substitute).

Groups are numbered from left to right by their opening parenthesis
(including nested groups). First group has index 1. The entire regex has
index 0.

> | Group | Value    |
> |-------|----------|
> | 0     | `foobar` |
> | 1     | `foobar` |
> | 2     | `bar`    |

## Backreferences

Meta-chars `\1` through `\9` are interpreted as backreferences to
capture groups. They match the previously found group with the specified
index.

The meta char `\g` followed by a number is also interpreted as
backreferences to capture groups. It can be followed by a multi-digit
number.

| регулярен из | раз кибрит                                         |
|--------------|----------------------------------------------------|
| `(.)\1+`     | &quot;aaaa&quot; и &quot;cc&quot;                  |
| \`\`. (+) 1  | \+ \`\` също &quot;abab&quot; и &quot;123123&quot; |
| `(.)\g1+`    | &quot;aaaa&quot; и &quot;cc&quot;                  |

регулярен израз `(['"]?)(\d+)\1` matches `"13"` (in double quotes), or
`'4'` (in single quotes) or `77` (without quotes) etc.

## Named Groups and Backreferences

To make some group named, use this syntax: `(?P<name>expr)`. Also Perl
syntax is supported: `(?'name'expr)`. And further: `(?<name>expr)`

Name of group must be valid identifier: first char is letter or "\_",
other chars are alphanumeric or "\_". All named groups are also usual
groups and share the same numbers 1 to 9.

Backreferences to named groups are `(?P=name)`, the numbers `\1` to `\9`
can also be used. As well as the example `\g` and `\k` in the table
below.

# Supported syntax are

`(?P=name)` `\g{name}` `\k{name}` `\k<name>` `\k'name'` ============

Example

| регулярен израз          | кибрит                |
|--------------------------|-----------------------|
| `(?P<qq>['"])\w+(?P=qq)` | `"word"` and `'word'` |

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

## Модификатори

Модификатори are for changing behaviour of regular expressions.

You can set modifiers globally in your system or change inside the
regular expression using the [(?imsxr-imsxr)](#inlinemodifiers).

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/)
>
> To change modifiers use [ModifierStr](../tregexpr/#modifierstr) or
> appropriate `Tрегулярен изразpr` properties
> [Modifier\*](../tregexpr/#modifieri).
>
> The default values are defined in [global
> variables](../tregexpr/#global-constants). For example global
> variable `регулярен изразprModifierX` defines default value for
> `ModifierX` property.

<a name="i"></a>

### i, без чувствителност

Case-insensitive. Use installed in you system locale settings, see also
[InvertCase](../tregexpr/#invertcase).

<a name="m"></a>

### м, многолинейни низове

Treat string as multiple lines. So `` ^ and `` \$ matches the start or
end of any line anywhere within the string.

See also [Line граници](#lineseparators).

<a name="s"></a>

### s, единични низове

Treat string as single line. So `.` matches any character whatsoever,
even a line separators.

See also [Line граници](#lineseparators), which it normally would not
match.

<a name="g"></a>

### g, алчност

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/) only modifier.

Switching it `Off` you’ll switch [quantifiers](#iterator) into
[non-greedy](#greedy) mode.

So, if modifier `/g` is `Off` then `+` works as `+?`, `*` as `*?` and so
on.

По подразбиране този модификатор е `On`.

<a name="x"></a>

### x, разширен синтаксис

Allows to comment regular expression and break them up into multiple
lines.

If the modifier is `On` we ignore all whitespaces that is neither
backslashed nor within a character class.

А символът `#` отделя коментарите.

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

### r, руски диапазони

> [!NOTE]
> [Tрегулярен изразpr](../tregexpr/) only modifier.

In Russian ASCII table characters `ё`/`Ё` are placed separately from
others.

Big and small Russian characters are in separated ranges, this is the
same as with English characters but nevertheless I wanted some short
form.

With this modifier instead of `[а-яА-ЯёЁ]` you can write `[а-Я]` if you
need all Russian characters.

Когато модификаторът е `On`:

<table>
<thead>
<tr class="header">
<th>регуляре</th>
<th>н израз кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>А-я</code></td>
<td><blockquote>
<p>символи от <code>а</code> до <code>я</code> и <code></code>`</p>
</blockquote></td>
</tr>
<tr class="even">
<td><code>А-Я</code></td>
<td><blockquote>
<p>символи от <code>А</code> до <code>Я</code> и <code>Ё</code></p>
</blockquote></td>
</tr>
<tr class="odd">
<td><code>А-Я</code></td>
<td><blockquote>
<p>всички руски символи</p>
</blockquote></td>
</tr>
</tbody>
</table>

Модификаторът е зададен по подразбиране &quot;On&quot;.

## Assertions

<a name="assertions"></a>

Positive lookahead assertion: `foo(?=bar)` matches "foo" only before
"bar", and "bar" is excluded from the match.

Negative lookahead assertion: `foo(?!bar)` matches "foo" only if it's
not followed by "bar".

Positive lookbehind assertion: `(?<=foo)bar` matches "bar" only after
"foo", and "foo" is excluded from the match.

Negative lookbehind assertion: `(?<!foo)bar` matches "bar" only if it's
not prefixed with "foo".

Limitations:

- Variable length lookbehind are not allowed to contain capture groups.
  This can be allowed by setting the property `AllowUnsafeLookBehind`.
  If this is enabled and there is more than one match in the text that
  the group might capture, then the wrong match may be captured. This
  does not affect the correctness of the overall assertion. (I.e., the
  lookbehind will correctly return if the text before matched the
  pattern).
- Variable length lookbehind may be slow to execute, if they do not
  match.

## Non-capturing Groups

Syntax is like this: `(?:expr)`.

Such groups do not have the "index" and are invisible for
backreferences. Non-capturing groups are used when you want to group a
subexpression, but you do not want to save it as a matched/captured
portion of the string. So this is just a way to organize your regex into
subexpressions without overhead of capturing result:

| регулярен израз                | кибрит                                                               |
|--------------------------------|----------------------------------------------------------------------|
| `(https?|ftp)://([^/\r\n]+)`   | in `https://sorokin.engineer` matches `https` and `sorokin.engineer` |
| `(?:https?|ftp)://([^/\r\n]+)` | in `https://sorokin.engineer` matches only `sorokin.engineer`        |

## Atomic Groups

Syntax is like this: `(?>expr|expr|...)`.

Atomic groups are special case of non-capturing groups. [Description of
them.](https://regular-expressions.mobi/atomic.html?wlr=1)

## Inline Модификатори

<a name="inlinemodifiers"></a>

Syntax for one modifier: `(?i)` to turn on, and `(?-i)` to turn off.
Many modifiers are allowed like this: `(?msgxr-imsgxr)`.

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

<table>
<thead>
<tr class="header">
<th>регулярен израз</th>
<th>кибрит</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>(? I) Saint-Petersburg</code>  </td>
<td>   <code>Saint-petersburg</code> и <code>Санкт Петербург</code></td>
</tr>
<tr class="even">
<td>`` Свети (К) - (- и) Petersbur</td>
<td>g`` &amp;quot;Санкт Петербург&amp;quot;, но не и
&amp;quot;Санкт-Петербург&amp;quot;</td>
</tr>
<tr class="odd">
<td><code>(Saint -) (К)? Petersburg</code></td>
<td><blockquote>
<p>  <code>Saint-petersburg</code> и <code>saint-petersburg</code></p>
</blockquote></td>
</tr>
<tr class="even">
<td>`<span class="title-ref"> (? (И) Saint -) Petersburg</span></td>
<td><span class="title-ref">?   </span><span class="title-ref"> Санкт
Петербург</span><span class="title-ref">, но не </span><span
class="title-ref">saint-petersburg</span>`</td>
</tr>
</tbody>
</table>

## Comments

Syntax is like this: `(?#text)`. Text inside brackets is ignored.

Note that the comment is closed by the nearest `)`, so there is no way
to put a literal `)` in the comment.

## Recursion

Syntax is `(?R)`, the alias is `(?0)`.

The regex `a(?R)?z` matches one or more letters "a" followed by exactly
the same number of letters "z".

The main purpose of recursion is to match balanced constructs or nested
constructs. The generic regex is `b(?:m|(?R))*e` where "b" is what
begins the construct, "m" is what can occur in the middle of the
construct, and "e" is what occurs at the end of the construct.

If what may appear in the middle of the balanced construct may also
appear on its own without the beginning and ending parts then the
generic regex is `b(?R)*e|m`.

## Subroutine calls

Syntax for call to numbered groups: `(?1)` ... `(?90)` (maximal index is
limited by code).

Syntax for call to named groups: `(?P>name)`. Also Perl syntax is
supported: `(?&name)`.

# Supported syntax are

`(?number)` `(?P>name)` `(?&name)` `\g<name>` `\g'name'` ============

This is like recursion but calls only code of capturing group with
specified index.

## Unicode Categories

Unicode standard has names for character categories. These are 2-letter
strings. For example "Lu" is uppercase letters, "Ll" is lowercase
letters. And 1-letter bigger category "L" is all letters.

- Cc - Control
- Cf - Format
- Co - Private Use
- Cs - Surrrogate
- Ll - Lowercase Letter
- Lm - Modifier Letter
- Lo - Other Letter
- Lt - Titlecase Letter
- Lu - Uppercase Letter
- Mc - Spacing Mark
- Me - Enclosing Mark
- Mn - Nonspacing Mark
- Nd - Decimal Number
- Nl - Letter Number
- No - Other Number
- Pc - Connector Punctuation
- Pd - Dash Punctuation
- Pe - Close Punctuation
- Pf - Final Punctuation
- Pi - Initial Punctuation
- Po - Other Punctuation
- Ps - Open Punctuation
- Sc - Currency Symbol
- Sk - Modifier Symbol
- Sm - Math Symbol
- So - Other Symbol
- Zl - Line Separator
- Zp - Paragraph Separator
- Zs - Space Separator

Meta-character `\p` denotes one Unicode char of specified category.
Syntax: `\pL` and `\p{L}` for 1-letter name, `\p{Lu}` for 2-letter
names.

Meta-character `\P` is inverted, it denotes one Unicode char **not** in
the specified category.

These meta-characters are supported within character classes too.

## послеслов

In this [ancient blog post from previous
century](https://sorokin.engineer/posts/en/text_processing_from_birds_eye_view.html)
I illustrate some usages of regular expressions.
