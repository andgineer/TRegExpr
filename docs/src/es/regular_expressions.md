# Expresiones regulares (RegEx)

## Introducción

Las expresiones regulares son una forma práctica de especificar patrones de texto.

Con las expresiones regulares puedes validar entradas de usuario, buscar patrones como correos electrónicos o números de teléfono en páginas web o en documentos, y mucho más.

A continuación, se encuentra la hoja de trucos completa de expresiones regulares.

## Caracteres

### Coincidencias simples

Cualquier carácter individual (excepto los caracteres especiales de regex) coincide consigo mismo. Una
serie de caracteres (no especiales) coincide con esa serie de caracteres en la cadena de entrada.

| RegEx    | Coincidencias |
|----------|---------------|
| `foobar` | `foobar`      |

### Caracteres no imprimibles (códigos de escape)

Para especificar un carácter por su código Unicode, usa el prefijo `\x` seguido
por el código hexadecimal. Para códigos de 3-4 dígitos (después de U+00FF), encierra el código
entre llaves.

| RegEx        | Coincidencias                             |
|--------------|------------------------------------------|
| `\xAB`       | carácter con código hexadecimal `AB` de 2 dígitos     |
| `\x{AB20}`   | carácter con código hexadecimal `AB20` de 1..4 dígitos |
| `foo\x20bar` | `foo bar` (nota el espacio en el medio)  |

Hay una serie de códigos de escape predefinidos para caracteres no imprimibles, como en el lenguaje C:

| RegEx       | Coincidencias                             |
|-------------|------------------------------------------|
| `\t`        | tabulador (HT/TAB), igual a `\x09`       |
| `\n`        | salto de línea (LF), igual a `\x0a`      |
| `\r`        | retorno de carro (CR), igual a `\x0d`    |
| `\f`        | avance de página (FF), igual a `\x0c`    |
| `\a`        | alarma (BEL), igual a `\x07`             |
| `\e`        | escape (ESC), igual a `\x1b`             |
| `\cA` ... `\cZ` | chr(0) a chr(25). Por ejemplo, `\cI` coincide con el carácter de tabulación. Las letras minúsculas "a"..."z" también son compatibles.|

### Escapando

Para representar un carácter especial de regex (uno de `.+*?|\()[]{}^$`), ponle un prefijo
con una barra invertida `\`. La barra invertida literal también debe ser escapada.

| RegEx         | Coincidencias                                                      |
|---------------|--------------------------------------------------------------------|
| `\^FooBarPtr` | `^FooBarPtr`, esto es `^` y no [inicio de línea](#lineseparators)  |
| `\[a\]`       | `[a]`, esto no es una [clase de carácter](#userclass)             |

## Clases de caracteres

### Clases de caracteres definidas por el usuario

Una clase de caracteres es una lista de caracteres dentro de corchetes `[]`. La
clase coincide con cualquier **carácter individual** listado en esta clase.

| RegEx            | Coincidencias                                  |
|------------------|------------------------------------------------|
| `foob[aeiou]r`   | `foobar`, `foober`, etc. pero no `foobbr`, `foobcr`, etc. |

Puedes "invertir" la clase: si el primer carácter después del `[` es
`^`, entonces la clase coincide con cualquier carácter **excepto** los caracteres
listados en la clase.

| RegEx           | Coincidencias                                  |
|-----------------|------------------------------------------------|
| `foob[^aeiou]r` | `foobbr`, `foobcr`, etc. pero no `foobar`, `foober`, etc. |

Dentro de una lista, el carácter guion `-` se usa para especificar un rango, de modo
que `a-z` representa todos los caracteres entre `a` y `z`, inclusivos.

Si quieres que el guion `-` sea miembro de una clase, colócalo al
inicio o al final de la lista, o [escápalo](#escape) con una barra invertida.

Si quieres que `]` sea parte de la clase, puedes colocarlo al inicio de
la lista o [escaparlo](#escape) con una barra invertida.

| RegEx       | Coincidencias                         |
|-------------|---------------------------------------|
| `[-az]`     | `a`, `z` y `-`                        |
| `[az-]`     | `a`, `z` y `-`                        |
| `[a\-z]`    | `a`, `z` y `-`                        |
| `[a-z]`     | caracteres de `a` a `z`               |
| `[\n-\x0D]` | caracteres de chr(10) a chr(13)       |

### Meta-carácter punto

El meta-carácter `.` (punto) por defecto coincide con cualquier carácter. Pero si desactivas
el [modificador /s](#s), entonces no coincidirá con caracteres de salto de línea.

El `.` no actúa como meta-clase dentro de [clases de caracteres definidas por el usuario](#user-character-classes). `[.]` significa un "." literal.

### Meta-clases

Hay una serie de clases de caracteres predefinidas que hacen que las expresiones regulares sean más compactas, "meta-clases":

| RegEx | Coincidencias                                   |
|-------|--------------------------------------------------|
| `\w`  | un carácter alfanumérico, incluyendo `_`        |
| `\W`  | un carácter no alfanumérico                     |
| `\d`  | un carácter numérico (igual a `[0-9]`)          |
| `\D`  | un carácter no numérico                         |
| `\s`  | cualquier espacio (igual a `[ \t\n\r\f]`)       |
| `\S`  | un no-espacio                                   |
| `\h`  | espacio horizontal: el tabulador y todos los caracteres en la categoría "separador de espacio" de Unicode |
| `\H`  | no un espacio horizontal                        |
| `\v`  | espacio vertical: todos los caracteres tratados como saltos de línea en el estándar Unicode |
| `\V`  | no un espacio vertical                          |
| `\R`  | salto de línea Unicode: LF, par CR LF, CR, FF (avance de página), VT (tabulador vertical), U+0085, U+2028, U+2029 |

Puedes usar todas las meta-clases mencionadas en la tabla anterior dentro de [clases de caracteres definidas por el usuario](#user-character-classes).

| RegEx         | Coincidencias                                                      |
|---------------|--------------------------------------------------------------------|
| `foob\dr`     | `foob1r`, `foob6r`, etc. pero no `foobar`, `foobbr`, etc.          |
| `foob[\w\s]r` | `foobar`, `foob r`, `foobbr`, etc. pero no `foob1r`, `foob=r`, etc.|

> [TRegExpr](tregexpr.md)
>
> Las propiedades [SpaceChars](tregexpr.md#spacechars) y
> [WordChars](tregexpr.md#wordchars) definen las clases de caracteres `\w`,
> `\W`, `\s`, `\S`.
>
> Así puedes redefinir estas clases.

## Límites

### Límites de línea

| Meta-carácter | Coincidencias                                          |
|---------------|--------------------------------------------------------|
| `^`           | coincidencia de longitud cero al inicio de la línea    |
| `$`           | coincidencia de longitud cero al final de la línea     |
| `\A`          | coincidencia de longitud cero en el inicio absoluto    |
| `\z`          | coincidencia de longitud cero en el final absoluto     |
| `\Z`          | como `\z` pero también coincide antes del salto de línea final |
| `\G`          | coincidencia de longitud cero en la posición final de la coincidencia anterior |

Ejemplos:

| RegEx      | Coincidencias                                        |
|------------|------------------------------------------------------|
| `^foobar`  | `foobar` solo si está al inicio de la línea         |
| `foobar$`  | `foobar` solo si está al final de la línea          |
| `^foobar$` | `foobar` solo si es la única cadena en la línea     |
| `foob.r`   | `foobar`, `foobbr`, `foob1r`, etc.                  |

El meta-carácter `^` coincide con una posición de longitud cero al inicio de la cadena de entrada. `$` - al final. Si el [modificador /m](#m) está **activado**, también coinciden al inicio/final de líneas individuales en el texto de varias líneas.

Nota que no hay una línea vacía dentro de la secuencia `\x0D\x0A`.

> [TRegExpr](tregexpr.md)
>
> Si estás usando la [versión Unicode](tregexpr.md#unicode), entonces
> `^`/`$` también coinciden con `\x2028`, `\x2029`, `\x0B`, `\x0C` o `\x85`.

El metacarácter `\A` coincide con la posición de longitud cero justo al principio del
texto de entrada, `\z` - al final absoluto. Ignoran el [modificador /m](#m).
`\Z` es como `\z`, pero también coincide antes del salto de línea final (LF y
CR LF). El comportamiento de `\A`, `\z`, `\Z` está hecho como en la mayoría de los principales
motores de regex (Perl, PCRE, etc.).

Nota que `^.*$` no coincide con un texto entre `\x0D\x0A`, porque
esto es un separador de línea inquebrantable. Pero coincide con el texto vacío
dentro de la secuencia `\x0A\x0D` porque esto son 2 saltos de línea en el
orden incorrecto.

> [TRegExpr](tregexpr.md)
>
> El procesamiento de múltiples líneas puede ser ajustado por las propiedades
> [LineSeparators](tregexpr.md#lineseparators) y
> [UseLinePairedBreak](tregexpr.md#linepairedseparator).
>
> Así puedes usar separadores al estilo Unix `\n` o al estilo DOS/Windows `\r\n`
> o mezclarlos juntos (como en el comportamiento predeterminado descrito arriba).

Si prefieres una descripción matemáticamente correcta, puedes encontrarla en
[www.unicode.org](http://www.unicode.org/unicode/reports/tr18/).

### Límites de palabra

| RegEx | Coincidencias        |
|-------|----------------------|
| `\b`  | un límite de palabra |
| `\B`  | un no-límite de palabra |

Un límite de palabra `\b` es un punto entre dos caracteres que tiene un `\w` en un lado y un `\W` en el otro lado (en cualquier orden).

## Cuantificación

### Cuantificadores

Cualquier elemento de una expresión regular puede ser seguido por un cuantificador.
Un cuantificador especifica el número de repeticiones del elemento.

| RegEx    | Coincidencias                                          |
|----------|--------------------------------------------------------|
| `{n}`    | exactamente `n` veces                                 |
| `{n,}`   | al menos `n` veces                                    |
| `{,m}`   | no más de `m` veces (solo con AllowBraceWithoutMin)   |
| `{n,m}`  | al menos `n` pero no más de `m` veces                 |
| `*`      | cero o más veces, similar a `{0,}`                    |
| `+`      | una o más veces, similar a `{1,}`                     |
| `?`      | cero o una vez, similar a `{0,1}`                     |

Así, los dígitos entre llaves `{n,m}` especifican el número mínimo de veces `n` y el máximo `m`.

`{n}` es equivalente a `{n,n}` y coincide exactamente `n` veces. `{n,}` coincide `n` o más veces.

La variante `{,m}` solo se admite si se establece la propiedad AllowBraceWithoutMin.

No hay un límite práctico para los valores n y m (el límite es el valor máximo de 32 bits con signo).

Usar `{` sin un rango correcto dará un error. Este comportamiento puede cambiarse estableciendo la propiedad AllowLiteralBraceWithoutRange, que aceptará `{` como un carácter literal, si no está seguido por un rango. Un rango con un valor bajo mayor que el alto siempre dará un error.

| RegEx            | Coincidencias                                                     |
|------------------|-------------------------------------------------------------------|
| `foob.*r`        | `foobar`,  `foobalkjdflkj9r` y `foobr`                            |
| `foob.+r`        | `foobar`, `foobalkjdflkj9r` pero no `foobr`                       |
| `foob.?r`        | `foobar`, `foobbr` y `foobr` pero no `foobalkj9r`                 |
| `fooba{2}r`      | `foobaar`                                                         |
| `fooba{2,}r`     | `foobaar`, `foobaaar`, `foobaaaar`, etc.                          |
| `fooba{2,3}r`    | `foobaar`, o `foobaaar`  pero no `foobaaaar`                      |
| `(foobar){8,10}` | 8...10 instancias de `foobar` (`()` es [grupo](#subexpression))  |

<a name="greedy"></a>

### Codicia

Los [Cuantificadores](#iterator) en modo "codicioso" toman tanto como sea posible, en
modo "perezoso" - lo menos posible.

Por defecto todos los cuantificadores son "codiciosos". Agrega el carácter `?` para
hacer cualquier cuantificador "perezoso".

Para la cadena `abbbbc`:

| RegEx     | Coincidencias |
|-----------|---------------|
| `b+`      | `bbbb`        |
| `b+?`     | `b`           |
| `b*?`     | cadena vacía  |
| `b{2,3}?` | `bb`          |
| `b{2,3}`  | `bbb`         |

Puedes cambiar todos los cuantificadores a modo "perezoso" ([modificador /g](#g),
abajo usamos [cambio de modificador en línea](#inlinemodifiers)).

| RegEx     | Coincidencias |
|-----------|---------------|
| `(?-g)b+` | `b`           |

### Cuantificador Posesivo

La sintaxis es: `a++`, `a*+`, `a?+`, `a{2,4}+`. Actualmente solo se soporta
para llaves simples, pero no para llaves después de un grupo como
`(foo|bar){3,5}+`.

Esta característica de regex está [descrita
aquí.](https://regular-expressions.mobi/possessive.html?wlr=1) En resumen,
el cuantificador posesivo acelera la coincidencia en casos complejos.

## Elección

Las expresiones en la elección se separan por la barra vertical `|`.

Entonces `fee|fie|foe` coincidirá con cualquiera de `fee`, `fie`, o `foe` en la cadena objetivo
(como lo haría `f(e|i|o)e`).

La primera expresión incluye todo desde el último delimitador de patrón
(`(`, `[`, o el comienzo del patrón) hasta el primer `|`, y la
última expresión contiene todo desde el último `|` hasta el siguiente
delimitador de patrón.

Suena un poco complicado, por lo que es práctica común incluir la
elección en paréntesis, para minimizar la confusión sobre dónde comienza y
termina.

Las expresiones en la elección se prueban de izquierda a derecha, por lo que la primera
expresión que coincide, es la que se elige.

Por ejemplo, la expresión regular `foo|foot` en la cadena `barefoot` coincidirá
con `foo`. Solo la primera expresión que coincide.

También recuerda que `|` se interpreta como literal dentro de
corchetes, así que si escribes `[fee|fie|foe]` realmente solo estás coincidiendo
con `[feio|]`.

| RegEx          | Coincidencias         |
|----------------|-----------------------|
| `foo(bar|foo)` | `foobar` o `foofoo`   |

<a name="subexpression"></a>

## Grupos

Los paréntesis `()` se usan para definir grupos (es decir, subexpresiones).

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Las posiciones de los grupos, longitudes y valores actuales estarán en
> [MatchPos](tregexpr.md#matchpos), [MatchLen](tregexpr.md#matchlen)
> y [Match](tregexpr.md#match).
>
> Puedes sustituirlos con [Substitute](tregexpr.md#substitute).

Los grupos se numeran de izquierda a derecha por su paréntesis de apertura
(incluyendo grupos anidados). El primer grupo tiene el índice 1. El regex completo tiene
el índice 0.

> | Grupo | Valor    |
> |-------|----------|
> | 0     | `foobar` |
> | 1     | `foobar` |
> | 2     | `bar`    |

## Retroreferencias

Los metacaracteres `\1` hasta `\9` se interpretan como retroreferencias a
grupos capturados. Coinciden con el grupo encontrado previamente con el índice especificado.

El metacarácter `\g` seguido por un número también se interpreta como
retroreferencias a grupos capturados. Puede ser seguido por un número de varios dígitos.

| RegEx      | Coincidencias                |
|------------|------------------------------|
| `(.)\1+`   | `aaaa` y `cc`                |
| `(.+)\1+`  | también `abab` y `123123`    |
| `(.)\g1+`  | `aaaa` y `cc`                |

RegEx `(['"]?)(\d+)\1` coincide con `"13"` (entre comillas dobles), o `'4'` (en
comillas simples) o `77` (sin comillas), etc.

## Grupos Nombrados y Retroreferencias

Los grupos nombrados en expresiones regulares te permiten etiquetar una parte de tu patrón. 
Esto hace que tus patrones sean más fáciles de entender y actualizar.

Para crear un grupo nombrado, usa `(?<name>pattern)` o `(?'name'pattern)`, 
donde `name` es el nombre del grupo y `pattern` es el patrón regex que quieres capturar.

Las referencias hacia atrás te permiten coincidir con el mismo texto que un grupo capturó anteriormente. 
Las referencias hacia atrás nombradas usan `\k<name>`, donde `name` es el nombre del grupo que quieres coincidir nuevamente.

TRegExpr también admite la versión de Perl: `(?P<name>pattern)` para definir un grupo nombrado y `(?P=name)` 
para referencias hacia atrás.

Ejemplo

| RegEx                    | Coincidencias         |
|--------------------------|-----------------------|
| `(?P<qq>['"])\w+(?P=qq)` | `"palabra"` y `'palabra'` |

## Resultado Coincidente

El inicio de la coincidencia reportada se puede establecer usando `\K`.

Por defecto, todo el texto cubierto por un patrón se considera coincidente.
Sin embargo, es posible establecer explícitamente lo que se informará.

El patrón `a\Kb` requerirá que el texto contenga "ab". Pero solo el
"b" se informará como coincidente. Puede haber varios `\K`
en un patrón, El último establecerá la posición de inicio de la coincidencia. Solo `\K`
en partes activas del patrón se consideran. Por ejemplo, `a(\Kb)?` no
considerará `\K` si no hay "b". Las capturas pueden existir fuera del conjunto de coincidencias
establecido por `\K`.

Si se usa en otras construcciones que pueden aplicarse fuera de la coincidencia informada
(como la anticipación positiva), entonces la posición marcada por `\K` debe estar antes o en
el final reportado de la coincidencia. Si la posición se marca más tarde, la
coincidencia se considera fallida.

`\K` es algo similar a una mirada atrás. A diferencia de una mirada atrás, la parte
del patrón antes del `\K` debe estar después de la posición de inicio de la
coincidencia, si el patrón se aplica desde una posición de desplazamiento dentro del
texto.

## Modificadores

Los modificadores son para cambiar el comportamiento de las expresiones regulares.

Puedes establecer modificadores globalmente en tu sistema o cambiar dentro de la
expresión regular usando [(?imsxr-imsxr)](#inlinemodifiers).

> [TRegExpr](tregexpr.md)
>
> Para cambiar modificadores usa [ModifierStr](tregexpr.md#modifierstr) o
> las propiedades apropiadas de `TRegExpr`
> [Modifier\*](tregexpr.md#modifieri).
>
> Los valores predeterminados están definidos en [variables
> globales](tregexpr.md#global-constants). Por ejemplo, la variable global `RegExprModifierX` define el valor predeterminado para la propiedad `ModifierX`.

<a name="i"></a>

### i, insensible a mayúsculas

Insensible a mayúsculas. Usa la configuración de localización instalada en tu sistema, ver también
[InvertCase](tregexpr.md#invertcase).

<a name="m"></a>

### m, cadenas multilínea

Trata la cadena como múltiples líneas. Así `^` y `$` coinciden con el inicio o fin
de cualquier línea en cualquier lugar dentro de la cadena.

Ver también [Límites de Línea](#lineseparators).

<a name="s"></a>

### s, cadenas de una sola línea

Trata la cadena como una sola línea. Así `.` coincide con cualquier carácter, incluso un separador de líneas.

Ver también [Límites de Línea](#lineseparators), con el que normalmente no coincidiría.

<a name="g"></a>

### g, codicia

> [TRegExpr](tregexpr.md) solo modificador.

Al desactivarlo cambiarás [cuantificadores](#iterator) a
modo [no codicioso](#greedy).

Entonces, si el modificador `/g` está `Off` entonces `+` funciona como `+?`, `*` como `*?` y así sucesivamente.

Por defecto este modificador está `On`.

<a name="x"></a>

### x, sintaxis extendida

Permite comentar expresiones regulares y dividirlas en múltiples
líneas.

Si el modificador está `On` ignoramos todos los espacios en blanco que no estén
escapados ni dentro de una clase de caracteres.

Y el carácter `#` separa los comentarios.

Nota que puedes usar líneas vacías para formatear expresiones regulares para
mejor legibilidad:

``` text
(
(abc) # comentario 1
#
(efg) # comentario 2
)
```

Esto también significa que si quieres espacios en blanco reales o caracteres `#` en
el patrón (fuera de una clase de caracteres, donde no se ven afectados por
`/x`), tendrás que escaparlos o codificarlos usando escapes octales o
hexadecimales.

<a name="r"></a>

### r, rangos rusos

> [TRegExpr](tregexpr.md) solo modificador.

En la tabla ASCII rusa los caracteres `ё`/`Ё` están colocados separadamente de
otros.

Los caracteres rusos grandes y pequeños están en rangos separados, esto es
lo mismo que con los caracteres ingleses pero aun así quería alguna forma
corta.

Con este modificador en lugar de `[а-яА-ЯёЁ]` puedes escribir `[а-Я]` si necesitas todos los caracteres rusos.

Cuando el modificador está `On`:

| RegEx | Coincidencias                       |
|-------|-------------------------------------|
| `а-я` | caracteres de `а` a `я` y `ё`      |
| `А-Я` | caracteres de `А` a `Я` y `Ё`      |
| `а-Я` | todos los símbolos rusos            |

El modificador se establece <span class="title-ref">On</span> por defecto.

## Aserciones (anticipación positiva, anticipación negativa)

<a name="assertions"></a>

Aserción de anticipación positiva: `foo(?=bar)` coincide con "foo" solo antes de
"bar", y "bar" se excluye de la coincidencia.

Aserción de anticipación negativa: `foo(?!bar)` coincide con "foo" solo si no está seguido por "bar".

Aserción de retrospectiva positiva: `(?<=foo)bar` coincide con "bar" solo después de
"foo", y "foo" se excluye de la coincidencia.

Aserción de retrospectiva negativa: `(?<!foo)bar` coincide con "bar" solo si no está precedido por "foo".

Limitaciones:

- Las retrospectivas de longitud variable no están permitidas para contener grupos de captura.
  Esto puede ser permitido configurando la propiedad `AllowUnsafeLookBehind`.
  Si esto está habilitado y hay más de una coincidencia en el texto que
  el grupo podría capturar, entonces la coincidencia incorrecta puede ser capturada. Esto
  no afecta la corrección de la aserción en general. (Es decir, la
  retrospectiva devolverá correctamente si el texto antes coincidía con el
  patrón).
- Las retrospectivas de longitud variable pueden ser lentas en ejecutarse, si no
  coinciden.

## Grupos No Capturadores

La sintaxis es así: `(?:expr)`.

Tales grupos no tienen "índice" y son invisibles para
retroreferencias. Los grupos no capturadores se utilizan cuando quieres agrupar una
subexpresión, pero no quieres guardarla como una parte capturada/coincidente de la cadena. Entonces esto es solo una forma de organizar tu regex en
subexpresiones sin el sobrecosto de capturar el resultado:

| RegEx                          | Coincidencias                                                        |
|--------------------------------|----------------------------------------------------------------------|
| `(https?|ftp)://([^/\r\n]+)`   | en `https://sorokin.engineer` coincide `https` y `sorokin.engineer` |
| `(?:https?|ftp)://([^/\r\n]+)` | en `https://sorokin.engineer` solo coincide `sorokin.engineer`      |

## Grupos Atómicos

La sintaxis es así: `(?>expr|expr|...)`.

Los grupos atómicos son un caso especial de grupos no capturadores. [Descripción de
ellos.](https://regular-expressions.mobi/atomic.html?wlr=1)

## Modificadores En Línea

<a name="inlinemodifiers"></a>

Sintaxis para un modificador: `(?i)` para activar, y `(?-i)` para desactivar.
Se permiten muchos modificadores así: `(?msgxr-imsgxr)`.

Puedes usarlo dentro de la expresión regular para modificar los modificadores
en tiempo real. Esto puede ser especialmente útil porque tiene alcance local en una
expresión regular. Solo afecta esa parte de la expresión regular que
sigue al operador `(?imsgxr-imsgxr)`.

Y si está dentro de un grupo, afectará solo a este grupo - específicamente
la parte del grupo que sigue a los modificadores. Entonces en
`((?i)Saint)-Petersburg` afecta solo al grupo `((?i)Saint)` por lo que coincidirá con `saint-Petersburg` pero no con `saint-petersburg`.

Los modificadores en línea también se pueden dar como parte de un grupo no capturador:
`(?i:patrón)`.

| RegEx                        | Coincidencias                                      |
|------------------------------|----------------------------------------------------|
| `(?i)Saint-Petersburg`       | `Saint-petersburg` y `Saint-Petersburg`            |
| `(?i)Saint-(?-i)Petersburg`  | `Saint-Petersburg` pero no `Saint-petersburg`      |
| `(?i)(Saint-)?Petersburg`    | `Saint-petersburg` y `saint-petersburg`            |
| `((?i)Saint-)?Petersburg`    | `saint-Petersburg`, pero no `saint-petersburg`     |

## Comentarios

La sintaxis es así: `(?#texto)`. El texto dentro de los corchetes se ignora.

Nota que el comentario se cierra por el `)` más cercano, por lo que no hay forma
de poner un `)` literal en el comentario.

## Recursión

La sintaxis es `(?R)`, el alias es `(?0)`.

El regex `a(?R)?z` coincide con una o más letras "a" seguidas por exactamente
el mismo número de letras "z".

El propósito principal de la recursión es coincidir con construcciones equilibradas o anidadas. El regex genérico es `b(?:m|(?R))*e` donde "b" es lo que
comienza la construcción, "m" es lo que puede ocurrir en medio de la
construcción, y "e" es lo que ocurre al final de la construcción.

Si lo que puede aparecer en medio de la construcción equilibrada también
puede aparecer por sí solo sin las partes de inicio y final, entonces el regex genérico es `b(?R)*e|m`.

## Llamadas a Subrutinas

Sintaxis para llamar a grupos numerados: `(?1)` ... `(?90)` (el índice máximo está
limitado por el código).

Sintaxis para llamar a grupos nombrados: `(?P>nombre)`. También se soporta la sintaxis `(?&name)`, `\g<name>` and `\g'name'`.

# Sintaxis soportadas

`(?number)` `(?P>nombre)` `(?&nombre)` `\g<nombre>` `\g'nombre'` ============

Esto es como la recursión pero solo llama al código del grupo de captura con
el índice especificado.

## Categorías Unicode

El estándar Unicode tiene nombres para categorías de caracteres. Estas son cadenas de 2 letras. Por ejemplo, "Lu" son letras mayúsculas, "Ll" son letras minúsculas. Y la categoría más grande de 1 letra "L" es para todas las letras.

- Cc - Control
- Cf - Formato
- Co - Uso Privado
- Cs - Sustituto
- Ll - Letra Minúscula
- Lm - Letra Modificadora
- Lo - Otra Letra
- Lt - Letra de Título
- Lu - Letra Mayúscula
- Mc - Marca de Espaciado
- Me - Marca de Encerramiento
- Mn - Marca No Espaciadora
- Nd - Número Decimal
- Nl - Número de Letra
- No - Otro Número
- Pc - Puntuación de Conector
- Pd - Puntuación de Guion
- Pe - Puntuación de Cierre
- Pf - Puntuación Final
- Pi - Puntuación Inicial
- Po - Otra Puntuación
- Ps - Puntuación de Apertura
- Sc - Símbolo de Moneda
- Sk - Símbolo Modificador
- Sm - Símbolo Matemático
- So - Otro Símbolo
- Zl - Separador de Línea
- Zp - Separador de Párrafo
- Zs - Separador de Espacio

El metacarácter `\p` denota un carácter Unicode de la categoría especificada.
Sintaxis: `\pL` y `\p{L}` para nombre de 1 letra, `\p{Lu}` para nombres
de 2 letras.

El metacarácter `\P` es inverso, denota un carácter Unicode **no** en
la categoría especificada.

Estos metacaracteres también se soportan dentro de clases de caracteres.

## Palabras Finales

En este [antiguo post de blog del siglo pasado](https://sorokin.engineer/posts/en/text_processing_from_birds_eye_view.html)
ilustro algunos usos de las expresiones regulares.
