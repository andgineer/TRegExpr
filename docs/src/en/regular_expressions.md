# Regular expressions (RegEx)

## Introduction

Regular expressions are a handy way to specify patterns of text.

With regular expressions you can validate user input, search for some
patterns like emails of phone numbers on web pages or in some documents
and so on.

Below is the complete regular expressions cheat sheet.

## Characters

### Simple matches

Any single character (except special regex characters) matches itself. A
series of (not special) characters matches that series of characters in
the input string.

| RegEx    | Matches  |
|----------|----------|
| `foobar` | `foobar` |

### Non-Printable Characters (escape-codes)

To specify character by its Unicode code, use the prefix `\x` followed
by the hex code. For 3-4 digits code (after U+00FF), enclose the code
into braces.

| RegEx        | Matches                                   |
|--------------|-------------------------------------------|
| `\xAB`       | character with 2-digit hex code `AB`      |
| `\x{AB20}`   | character with 1..4-digit hex code `AB20` |
| `foo\x20bar` | `foo bar` (note space in the middle)      |

There are a number of predefined escape-codes for non-printable
characters, like in C language:

<table>
<thead>
<tr class="header">
<th>RegEx</th>
<th>Matches</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\t</code>  </td>
<td>tab (HT/TAB), same as <code>\x09</code></td>
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
<td>form feed (FF), same as <code>\x0c</code></td>
</tr>
<tr class="odd">
<td><code>\a</code>  </td>
<td>alarm (BEL), same as <code>\x07</code></td>
</tr>
<tr class="even">
<td><code>\e</code>  </td>
<td>escape (ESC), same as <code>\x1b</code></td>
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

| RegEx         | Matches                                                            |
|---------------|--------------------------------------------------------------------|
| `\^FooBarPtr` | `^FooBarPtr`, this is `^` and not [start of line](#lineseparators) |
| `\[a\]`       | `[a]`, this is not [character class](#userclass)                   |

## Character Classes

<a name="userclass"></a>

### User Character Classes

Character class is a list of characters inside square brackets `[]`. The
class matches any **single** character listed in this class.

| RegEx            | Matches                                               |
|------------------|-------------------------------------------------------|
| `foob[aeiou]r`   | `foobar`, `foober` etc but not `foobbr`, `foobcr` etc |

You can "invert" the class - if the first character after the `[` is
`^`, then the class matches any character **except** the characters
listed in the class.

| RegEx           | Matches                                               |
|-----------------|-------------------------------------------------------|
| `foob[^aeiou]r` | `foobbr`, `foobcr` etc but not `foobar`, `foober` etc |

Within a list, the dash `-` character is used to specify a range, so
that `a-z` represents all characters between `a` and `z`, inclusive.

If you want the dash `-` itself to be a member of a class, put it at the
start or end of the list, or [escape](#escape) it with a backslash.

If you want `]` as part of the class you may place it at the start of
list or [escape](#escape) it with a backslash.

| RegEx       | Matches                            |
|-------------|------------------------------------|
| `[-az]`     | `a`, `z` and `-`                   |
| `[az-]`     | `a`, `z` and `-`                   |
| `[a\-z]`    | `a`, `z` and `-`                   |
| `[a-z]`     | characters from `a` to `z`         |
| `[\n-\x0D]` | characters from chr(10) to chr(13) |

### Dot Meta-Char

Meta-char `.` (dot) by default matches any character. But if you turn
**off** the [modifier /s](#s), then it won't match line-break
characters.

The `.` does not act as meta-class inside [user character
classes](#user-character-classes). `[.]` means a literal ".".

### Meta-Classes

There are a number of predefined character classes that keeps regular
expressions more compact, "meta-classes":

<table>
<thead>
<tr class="header">
<th>RegEx</th>
<th>Matches</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\w</code>    </td>
<td>an alphanumeric character, including <code>_</code></td>
</tr>
<tr class="even">
<td><code>\W</code>    </td>
<td>a non-alphanumeric</td>
</tr>
<tr class="odd">
<td><code>\d</code>    </td>
<td>a numeric character (same as <code>[0-9]</code>)</td>
</tr>
<tr class="even">
<td><code>\D</code>    </td>
<td>a non-numeric</td>
</tr>
<tr class="odd">
<td><code>\s</code>    </td>
<td>any space (same as <code>[ \t\n\r\f]</code>)</td>
</tr>
<tr class="even">
<td><code>\S</code>    </td>
<td>a non-space</td>
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
character classes](#user-character-classes).

| RegEx         | Matches                                                                      |
|---------------|------------------------------------------------------------------------------|
| `foob\dr`     | `foob1r`, `foob6r` and so on, but not `foobar`, `foobbr` and so on           |
| `foob[\w\s]r` | `foobar`, `foob r`, `foobbr` and so on, but not `foob1r`, `foob=r` and so on |

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Properties [SpaceChars](tregexpr.md#spacechars) and
> [WordChars](tregexpr.md#wordchars) define character classes `\w`,
> `\W`, `\s`, `\S`.
>
> So you can redefine these classes.

## Boundaries

<a name="lineseparators"></a>

### Line Boundaries

| Meta-char | Matches                                                |
|-----------|--------------------------------------------------------|
| `^`       | zero-length match at start of line                     |
| `$`       | zero-length match at end of line                       |
| `\A`      | zero-length match at the very beginning                |
| `\z`      | zero-length match at the very end                      |
| `\Z`      | like `\z` but also matches before the final line-break |
| `\G`      | zero-length match at the end pos of the previous match |

Examples:

| RegEx      | Matches                                        |
|------------|------------------------------------------------|
| `^foobar`  | `foobar` only if it's at the beginning of line |
| `foobar$`  | `foobar` only if it's at the end of line       |
| `^foobar$` | `foobar` only if it's the only string in line  |
| `foob.r`   | `foobar`, `foobbr`, `foob1r` and so on         |

Meta-char `^` matches zero-length position at the beginning of the input
string. `$` - at the ending. If [modifier /m](#m) is **on**, they also
match at the beginning/ending of individual lines in the multi-line
text.

Note that there is no empty line within the sequence `\x0D\x0A`.

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> If you are using [Unicode version](tregexpr.md#unicode), then
> `^`/`$` also matches `\x2028`, `\x2029`, `\x0B`, `\x0C` or `\x85`.

Meta-char `\A` matches zero-length position at the very beginning of the
input string, `\z` - at the very ending. They ignore [modifier /m](#m).
`\Z` is like `\z` but also matches before the final line-break (LF and
CR LF). Behaviour of `\A`, `\z`, `\Z` is made like in most of major
regex engines (Perl, PCRE, etc).

Note that `^.*$` does not match a string between `\x0D\x0A`, because
this is unbreakable line separator. But it matches the empty string
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

### Word Boundaries

| RegEx | Matches             |
|-------|---------------------|
| `\b`  | a word boundary     |
| `\B`  | a non-word boundary |

A word boundary `\b` is a spot between two characters that has a `\w` on
one side of it and a `\W` on the other side of it (in either order).

<a name="iterator"></a>

## Quantification

### Quantifiers

Any item of a regular expression may be followed by quantifier.
Quantifier specifies number of repetitions of the item.

| RegEx    | Matches                                                  |
|----------|----------------------------------------------------------|
| `{n}`    | exactly `n` times                                        |
| `{n,}`   | at least `n` times                                       |
| `{,m}`   | not more than `m` times (only with AllowBraceWithoutMin) |
| `{n,m}`  | at least `n` but not more than `m` times                 |
| `*`      | zero or more, similar to `{0,}`                          |
| `+`      | one or more, similar to `{1,}`                           |
| `?`      | zero or one, similar to `{0,1}`                          |

So, digits in curly brackets `{n,m}`, specify the minimum number of
times to match `n` and the maximum `m`.

The `{n}` is equivalent to `{n,n}` and matches exactly `n` times. The
`{n,}` matches `n` or more times.

The variant `{,m}` is only supported if the property
AllowBraceWithoutMin is set.

There is no practical limit to the values n and m (limit is maximal
signed 32-bit value).

Using `{` without a correct range will give an error. This behaviour can
be changed by setting the property AllowLiteralBraceWithoutRange, which
will accept `{` as a literal char, if not followed by a range. A range
with a low value bigger than the high value will always give an error.

| RegEx            | Matches                                                        |
|------------------|----------------------------------------------------------------|
| `foob.*r`        | `foobar`,  `foobalkjdflkj9r` and `foobr`                       |
| `foob.+r`        | `foobar`, `foobalkjdflkj9r` but not `foobr`                    |
| `foob.?r`        | `foobar`, `foobbr` and `foobr` but not `foobalkj9r`            |
| `fooba{2}r`      | `foobaar`                                                      |
| `fooba{2,}r`     | `foobaar'`, `foobaaar`, `foobaaaar` etc.                       |
| `fooba{2,3}r`    | `foobaar`, or `foobaaar`  but not `foobaaaar`                  |
| `(foobar){8,10}` | 8...10 instances of `foobar` (`()` is [group](#subexpression)) |

<a name="greedy"></a>

### Greediness

[Quantifiers](#iterator) in "greedy" mode takes as many as possible, in
"lazy" mode - as few as possible.

By default all quantifiers are "greedy". Append the character `?` to
make any quantifier "lazy".

For string `abbbbc`:

| RegEx     | Matches      |
|-----------|--------------|
| `b+`      | `bbbb`       |
| `b+?`     | `b`          |
| `b*?`     | empty string |
| `b{2,3}?` | `bb`         |
| `b{2,3}`  | `bbb`        |

You can switch all quantifiers into "lazy" mode ([modifier /g](#g),
below we use [in-line modifier change](#inlinemodifiers)).

| RegEx     | Matches |
|-----------|---------|
| `(?-g)b+` | `b`     |

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

| RegEx          | Matches              |
|----------------|----------------------|
| `foo(bar|foo)` | `foobar` or `foofoo` |

<a name="subexpression"></a>

## Groups

The brackets `()` are used to define groups (ie subexpressions).

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Group positions, lengths and actual values will be in
> [MatchPos](tregexpr.md#matchpos), [MatchLen](tregexpr.md#matchlen)
> and [Match](tregexpr.md#match).
>
> You can substitute them with [Substitute](tregexpr.md#substitute).

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

| RegEx      | Matches                  |
|------------|--------------------------|
| `(.)\1+`   | `aaaa` and `cc`          |
| `(.+)\1+`  | also `abab` and `123123` |
| `(.)\g1+`  | `aaaa` and `cc`          |

RegEx `(['"]?)(\d+)\1` matches `"13"` (in double quotes), or `'4'` (in
single quotes) or `77` (without quotes) etc.

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

| RegEx                    | Matches               |
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

## Modifiers

Modifiers are for changing behaviour of regular expressions.

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

### i, case-insensitive

Case-insensitive. Use installed in you system locale settings, see also
[InvertCase](tregexpr.md#invertcase).

<a name="m"></a>

### m, multi-line strings

Treat string as multiple lines. So `^` and `$` matches the start or end
of any line anywhere within the string.

See also [Line Boundaries](#lineseparators).

<a name="s"></a>

### s, single line strings

Treat string as single line. So `.` matches any character whatsoever,
even a line separators.

See also [Line Boundaries](#lineseparators), which it normally would not
match.

<a name="g"></a>

### g, greediness

> [!NOTE]
> [TRegExpr](tregexpr.md) only modifier.

Switching it `Off` you’ll switch [quantifiers](#iterator) into
[non-greedy](#greedy) mode.

So, if modifier `/g` is `Off` then `+` works as `+?`, `*` as `*?` and so
on.

By default this modifier is `On`.

<a name="x"></a>

### x, eXtended syntax

Allows to comment regular expression and break them up into multiple
lines.

If the modifier is `On` we ignore all whitespaces that is neither
backslashed nor within a character class.

And the `#` character separates comments.

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

### r, Russian ranges

> [!NOTE]
> [TRegExpr](tregexpr.md) only modifier.

In Russian ASCII table characters `ё`/`Ё` are placed separately from
others.

Big and small Russian characters are in separated ranges, this is the
same as with English characters but nevertheless I wanted some short
form.

With this modifier instead of `[а-яА-ЯёЁ]` you can write `[а-Я]` if you
need all Russian characters.

When the modifier is `On`:

| RegEx | Matches                       |
|-------|-------------------------------|
| `а-я` | chars from `а` to `я` and `ё` |
| `А-Я` | chars from `А` to `Я` and `Ё` |
| `а-Я` | all russian symbols           |

The modifier is set <span class="title-ref">On</span> by default.

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

| RegEx                          | Matches                                                              |
|--------------------------------|----------------------------------------------------------------------|
| `(https?|ftp)://([^/\r\n]+)`   | in `https://sorokin.engineer` matches `https` and `sorokin.engineer` |
| `(?:https?|ftp)://([^/\r\n]+)` | in `https://sorokin.engineer` matches only `sorokin.engineer`        |

## Atomic Groups

Syntax is like this: `(?>expr|expr|...)`.

Atomic groups are special case of non-capturing groups. [Description of
them.](https://regular-expressions.mobi/atomic.html?wlr=1)

## Inline Modifiers

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

| RegEx                        | Matches                                        |
|------------------------------|------------------------------------------------|
| `(?i)Saint-Petersburg`       | `Saint-petersburg` and `Saint-Petersburg`      |
| `(?i)Saint-(?-i)Petersburg`  | `Saint-Petersburg` but not `Saint-petersburg`  |
| `(?i)(Saint-)?Petersburg`    | `Saint-petersburg` and `saint-petersburg`      |
| `((?i)Saint-)?Petersburg`    | `saint-Petersburg`, but not `saint-petersburg` |

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

## Afterword

In this [ancient blog post from previous
century](https://sorokin.engineer/posts/en/text_processing_from_birds_eye_view.html)
I illustrate some usages of regular expressions.
