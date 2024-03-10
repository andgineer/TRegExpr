|     |        |                                                           |                                                             |                                                              |                                                              |                                                              |
|-----|--------|-----------------------------------------------------------|-------------------------------------------------------------|--------------------------------------------------------------|--------------------------------------------------------------|--------------------------------------------------------------|
|     | Inglés | [Ruso](https://regex.sorokin.engineer/ru/latest/faq.html) | [Alemán](https://regex.sorokin.engineer/de/latest/faq.html) | [Búlgaro](https://regex.sorokin.engineer/bg/latest/faq.html) | [Francés](https://regex.sorokin.engineer/fr/latest/faq.html) | [Español](https://regex.sorokin.engineer/es/latest/faq.html) |

# Preguntas más frecuentes

## Encontré un error grave: ¡TRegExpr genera la excepción de violación de acceso!

**Responder**

You must create the object before usage. So, after you declared
something like:

``` pascal
r: TRegExpr
```

No te olvides de crear la instancia de objeto:

``` pascal
r: = TRegExpr.Create. 
```

## Las expresiones regulares con (? = ...) no funcionan

Look ahead is not implemented in the TRegExpr. But in many cases you can
easily [replace it with simple
subexpressions](regular_expressions.html#lookahead).

## ¿Es compatible con Unicode?

**Responder**

[Cómo usar Unicode](tregexpr.html#unicode)

## ¿Por qué TRegExpr devuelve más de una línea?

For example, r.e. `<font .\*>` returns the first `<font`, then the rest
of the file including last `</html>`.

**Responder**

For backward compatibility, [modifier
/s](regular_expressions.html#modifier_s) is `On` by default.

Switch it Off and `.` will match any but [Line
separators](regular_expressions.html#syntax_line_separators) - exactly
as you wish.

Por cierto, recomiendo `<font ([^\n>] *)>`, en `Match [1]` se convierte
en la URL.\</font\>.

## ¿Por qué TRegExpr regresa más de lo que esperaba?

For example r.e. `<p>(.+)</p>` applyed to string `<p>a</p><p>b</p>`
returns `a</p><p>b` but not `a` as I expected.

**Responder**

By default all operators works in `greedy` mode, so they match as more
as it possible.

If you want `non-greedy` mode you can use `non-greedy` operators like
`+?` and so on or switch all operators into `non-greedy` mode with help
of modifier `g` (use appropriate TRegExpr properties or operator `?(-g)`
in r.e.).

## Cómo analizar fuentes como HTML con la ayuda de TRegExpr?

**Responder**

Lo siento amigos, pero es casi imposible!

Of course, you can easily use TRegExpr for extracting some information
from HTML, as shown in my examples, but if you want accurate parsing you
have to use real parser, not r.e.

You can read full explanation in Tom Christiansen and Nathan Torkington
`Perl Cookbook`, for example.

In short - there are many structures that can be easy parsed by real
parser but cannot at all by r.e., and real parser is much faster to do
the parsing, because r.e. doesn't simply scan input stream, it performs
optimization search that can take a lot of time.

## ¿Hay alguna forma de obtener varias coincidencias de un patrón en TRegExpr?

**Responder**

Puede iterar coincidencias con el método ExecNext.

If you want some example, please take a look at `TRegExpr.Replace`
method implementation or at the examples for
[HyperLinksDecorator](demos.html)

## Estoy comprobando la entrada del usuario. ¿Por qué TRegExpr devuelve `True` para las cadenas de entrada incorrectas?

**Responder**

In many cases TRegExpr users forget that regular expression is for
**search** in input string.

So, for example if you use `\d{4,4}` expression, you will get success
for wrong user inputs like `12345` or `any letters 1234`.

You have to check from line start to line end to ensure there are no
anything else around: `^\d{4,4}$`.

<a name="nongreedyoptimization"></a>

## ¿Por qué los iteradores no codiciosos a veces funcionan como en modo codicioso?

For example, the r.e. `a+?,b+?` applied to string `aaa,bbb` matches
`aaa,b`, but should it not match `a,b` because of non-greediness of
first iterator?

**Responder**

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

## ¿Cómo puedo usar TRegExpr con Borland C ++ Builder?

Tengo un problema porque no hay ningún archivo de encabezado (`.h` o
`.hpp`) disponible.

**Responder**

- Agregue `RegExpr.pas` al proyecto `bcb`.
- Compilar proyecto. Esto genera el archivo de encabezado `RegExpr.hpp`.
- Ahora puede escribir código que usa la unidad `RegExpr`.
- No olvide agregar `#include “RegExpr.hpp”` donde sea necesario.
- Don't forget to replace all `\` in regular expressions with `\\` or
  redefined [EscChar](tregexpr.html#escchar) const.

## ¿Por qué muchos re (incluyendo re desde TRegExpr help y demo) funcionan mal en Borland C ++ Builder?

**Responder**

The hint is in the previous question ;) Symbol `\` has special meaning
in `C++`, so you have to `escape` it (as described in previous answer).
But if you don't like r.e. like `\\w+\\\\w+\\.\\w+` you can redefine the
constant `EscChar` (in `RegExpr.pas`). For example `EscChar = "/"`. Then
you can write `/w+/w+/./w+`, looks unusual but more readable.
