# Preguntas Frecuentes

## Encontré un error terrible: ¡TRegExpr provoca una excepción de violación de acceso!

**Respuesta**

Debe crear el objeto antes de usarlo. Por lo tanto, después de haber declarado algo como:

``` pascal
r : TRegExpr
```

no olvide crear la instancia del objeto:

``` pascal
r := TRegExpr.Create.
```

## ¿Soporta Unicode?

**Respuesta**

[Cómo usar Unicode](tregexpr.md#unicode)

## ¿Por qué TRegExpr devuelve más de una línea?

Por ejemplo, r.e. `<font .\*>` devuelve el primer `<font`, luego el resto del archivo incluyendo el último `</html>`.

**Respuesta**

Por compatibilidad hacia atrás, [el modificador /s](regular_expressions.md#s) está `Activado` por defecto.

Desactívalo y `.` coincidirá con cualquier cosa excepto [separadores de línea](regular_expressions.md#lineseparators) - exactamente como deseas.

Por cierto, sugiero `<font ([^\n>]*)>`, en `Match[1]` estará la URL.

## ¿Por qué TRegExpr devuelve más de lo que espero?

Por ejemplo, r.e. `<p>(.+)</p>` aplicado a la cadena `<p>a</p><p>b</p>` devuelve `a</p><p>b` pero no `a` como esperaba.

**Respuesta**

Por defecto, todos los operadores trabajan en modo `voraz`, por lo que coinciden tanto como sea posible.

Si deseas un modo `no voraz` puedes usar operadores no voraces como `+?` y demás o cambiar todos los operadores a modo no voraz con la ayuda del modificador `g` (usa las propiedades apropiadas de TRegExpr o el operador `?(-g)` en r.e.).

## ¿Cómo analizar fuentes como HTML con la ayuda de TRegExpr?

**Respuesta**

Lo siento, gente, pero es casi imposible!

Por supuesto, puedes usar fácilmente TRegExpr para extraer alguna información de HTML, como se muestra en mis ejemplos, pero si quieres un análisis preciso tienes que usar un verdadero analizador, no r.e.

Puedes leer la explicación completa en el `Perl Cookbook` de Tom Christiansen y Nathan Torkington, por ejemplo.

En resumen - hay muchas estructuras que pueden ser fácilmente analizadas por un verdadero analizador pero de ninguna manera por r.e., y el verdadero analizador es mucho más rápido para hacer el análisis, porque r.e. no simplemente escanea la corriente de entrada, realiza una búsqueda de optimización que puede llevar mucho tiempo.

## ¿Hay alguna manera de obtener múltiples coincidencias de un patrón en TRegExpr?

**Respuesta**

Puedes iterar coincidencias con el método ExecNext.

Si deseas algún ejemplo, por favor, echa un vistazo a la implementación del método `TRegExpr.Replace` o a los ejemplos para [HyperLinksDecorator](demos.md)

## Estoy comprobando la entrada del usuario. ¿Por qué TRegExpr devuelve `True` para cadenas de entrada incorrectas?

**Respuesta**

En muchos casos, los usuarios de TRegExpr olvidan que la expresión regular es para **buscar** en la cadena de entrada.

Así que, por ejemplo, si usas la expresión `\d{4,4}`, tendrás éxito para entradas de usuario incorrectas como `12345` o `cualquier letra 1234`.

Tienes que verificar desde el inicio hasta el final de la línea para asegurarte de que no haya nada más alrededor: `^\d{4,4}$`.

## ¿Por qué los iteradores no voraces a veces trabajan como en modo voraz?

Por ejemplo, el r.e. `a+?,b+?` aplicado a la cadena `aaa,bbb` coincide con `aaa,b`, pero ¿no debería coincidir con `a,b` debido a la no voracidad del primer iterador?

**Respuesta**

Esto se debe a la forma de trabajar de TRegExpr. De hecho, muchos otros motores de r.e. trabajan exactamente igual: solo realizan una `simple` optimización de búsqueda, y no intentan hacer la mejor optimización.

En algunos casos es malo, pero en general es más una ventaja que una limitación, por razones de rendimiento y previsibilidad.

La regla principal - r.e. primero intenta coincidir desde el lugar actual y solo si eso es completamente imposible avanza un carácter e intenta de nuevo desde la siguiente posición en el texto.

Así que, si usas `a,b+?` coincidirá con `a,b`. En el caso de `a+?,b+?` ahora no se recomienda (añadimos modificador no voraz) pero aún es posible coincidir con más de una `a`, así que TRegExpr lo hará.

TRegExpr como los r.e. de Perl o Unix no intentan avanzar y verificar - sería una "mejor" coincidencia. En primer lugar, simplemente porque no hay forma de decir que una coincidencia es mejor o peor.

## ¿Cómo puedo usar TRegExpr con Borland C++ Builder?

Tengo un problema ya que no hay disponible un archivo de cabecera (`.h` o `.hpp`).

**Respuesta**

- Agrega `RegExpr.pas` al proyecto `bcb`.
- Compila el proyecto. Esto genera el archivo de cabecera `RegExpr.hpp`.
- Ahora puedes escribir código que use la unidad `RegExpr`.
- No olvides añadir `#include “RegExpr.hpp”` donde sea necesario.
- No olvides reemplazar todos los `\` en las expresiones regulares con `\\` o redefinir la constante [EscChar](tregexpr.md#escchar).

## ¿Por qué muchos r.e. (incluyendo r.e. de la ayuda y demo de TRegExpr) funcionan mal en Borland C++ Builder?

**Respuesta**

La pista está en la pregunta anterior ;) El símbolo `\` tiene un significado especial en `C++`, por lo que tienes que `escaparlo` (como se describió en la respuesta anterior). Pero si no te gustan los r.e. como `\\w+\\w+\\.\\w+` puedes redefinir la constante `EscChar` (en `RegExpr.pas`). Por ejemplo `EscChar = "/"`. Entonces puedes escribir `/w+/w+/./w+`, parece inusual pero es más legible.
