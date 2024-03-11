# Часто задаваемые вопросы

## Я обнаружил ужасную ошибку: TRegExpr вызывает исключение Access Violation!

**Ответ**

You must create the object before usage. So, after you declared
something like:

``` pascal
r: TRegExpr
```

не забудьте создать экземпляр объекта:

``` pascal
r: = TRegExpr.Create. 
```

## Регулярные выражения с (? = ...) не работают

Look ahead is not implemented in the TRegExpr. But in many cases you can
easily [replace it with simple
subexpressions](regular_expressions.md#lookahead).

## Поддерживает ли он Юникод?

**Ответ**

[Как использовать Юникод](tregexpr.md#unicode)

## Почему TRegExpr возвращает более одной строки?

For example, r.e. `<font .\*>` returns the first `<font`, then the rest
of the file including last `</html>`.

**Ответ**

For backward compatibility, [modifier
/s](regular_expressions.md#modifier_s) is `On` by default.

Switch it Off and `.` will match any but [Line
separators](regular_expressions.md#syntax_line_separators) - exactly
as you wish.

Я лично предлагаю `<font ([^\n>] *)>`, тогда в `Match [1]` будет URL.

## Почему TRegExpr возвращает больше, чем я ожидаю?

For example r.e. `<p>(.+)</p>` applyed to string `<p>a</p><p>b</p>`
returns `a</p><p>b` but not `a` as I expected.

**Ответ**

By default all operators works in `greedy` mode, so they match as more
as it possible.

If you want `non-greedy` mode you can use `non-greedy` operators like
`+?` and so on or switch all operators into `non-greedy` mode with help
of modifier `g` (use appropriate TRegExpr properties or operator `?(-g)`
in r.e.).

## Как анализировать HTML, с помощью TRegExpr?

**Ответ**

Извините, ребята, но это почти невозможно!

Of course, you can easily use TRegExpr for extracting some information
from HTML, as shown in my examples, but if you want accurate parsing you
have to use real parser, not r.e.

You can read full explanation in Tom Christiansen and Nathan Torkington
`Perl Cookbook`, for example.

In short - there are many structures that can be easy parsed by real
parser but cannot at all by r.e., and real parser is much faster to do
the parsing, because r.e. doesn't simply scan input stream, it performs
optimization search that can take a lot of time.

## Есть ли способ получить несколько совпадений шаблона на TRegExpr?

**Ответ**

Вы искать последующие совпадения с помощью метода ExecNext.

If you want some example, please take a look at `TRegExpr.Replace`
method implementation or at the examples for
[HyperLinksDecorator](demos.md)

## Я проверяю пользовательский ввод. Почему TRegExpr возвращает `True` для неправильных входных строк?

**Ответ**

In many cases TRegExpr users forget that regular expression is for
**search** in input string.

So, for example if you use `\d{4,4}` expression, you will get success
for wrong user inputs like `12345` or `any letters 1234`.

You have to check from line start to line end to ensure there are no
anything else around: `^\d{4,4}$`.

<a name="nongreedyoptimization"></a>

## Почему не жадные итераторы иногда работают в жадном режиме?

For example, the r.e. `a+?,b+?` applied to string `aaa,bbb` matches
`aaa,b`, but should it not match `a,b` because of non-greediness of
first iterator?

**Ответ**

This is because of TRegExpr way to work. In fact many others r.e.
engines work exactly the same: they performe only `simple` search
optimization, and do not try to do the best optimization.

In some cases it's bad, but in common it's rather advantage then
limitation, because of performance and predictability reasons.

The main rule - r.e. first of all try to match from current place and
only if that's completely impossible move forward by one char and try
again from next position in the text.

So, if you use `a,b+?` it'll match `a,b`. In case of `a+?,b+?` it's now
not recommended (we add non-greedy modifyer) but still possible to match
more then one `a`, so TRegExpr will do it.

TRegExpr like Perl's or Unix's r.e. doesn't attempt to move forward and
check - would it will be "better" match. Fisrt of all, just because
there is no way to say it's more or less good match.

## Как использовать TRegExpr с Borland C ++ Builder?

У меня проблема, нет файла заголовка (`.h` или `.hpp`).

**Ответ**

- Добавьте `RegExpr.pas` к проекту `bcb`.
- Скомпилировать проект. Это создает заголовочный файл `RegExpr.hpp`.
- Теперь вы можете писать код, использующий модуль `RegExpr`.
- Не забудьте добавить `#include “RegExpr.hpp”` там, где это необходимо.
- Don't forget to replace all `\` in regular expressions with `\\` or
  redefined [EscChar](tregexpr.md#escchar) const.

## Почему многие примеры (включая примеры из документации) работают неправильно в Borland C ++ Builder?

**Ответ**

The hint is in the previous question ;) Symbol `\` has special meaning
in `C++`, so you have to `escape` it (as described in previous answer).
But if you don't like r.e. like `\\w+\\\\w+\\.\\w+` you can redefine the
constant `EscChar` (in `RegExpr.pas`). For example `EscChar = "/"`. Then
you can write `/w+/w+/./w+`, looks unusual but more readable.
