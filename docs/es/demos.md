|     |        |                                                             |                                                               |                                                                |                                                                |                                                                |
|-----|--------|-------------------------------------------------------------|---------------------------------------------------------------|----------------------------------------------------------------|----------------------------------------------------------------|----------------------------------------------------------------|
|     | Inglés | [Ruso](https://regex.sorokin.engineer/ru/latest/demos.html) | [Alemán](https://regex.sorokin.engineer/de/latest/demos.html) | [Búlgaro](https://regex.sorokin.engineer/bg/latest/demos.html) | [Francés](https://regex.sorokin.engineer/fr/latest/demos.html) | [Español](https://regex.sorokin.engineer/es/latest/demos.html) |

# Población

Código de demostración para [TRegExpr](index.html)

## Introducción

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.html).

TRegExpr interface described in [TRegExpr interface](tregexpr.html).

## Text2HTML

[Fuentes
Text2HTML](https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML)

Publicar texto plano como HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Esta unidad contiene funciones para decorar
hipervínculos.

For example, replaces `www.masterAndrey.com` with
`<a href="http://www.masterAndrey.com">www.masterAndrey.com</a>` or
`filbert@yandex.ru` with
`<a href="mailto:filbert@yandex.ru">filbert@yandex.ru</a>`.   ..
code-block:: pascal

> function DecorateURLs (  
> const AText : string; AFlags : TDecorateURLsFlagSet = \[durlAddr,
> durlPath\]
>
> ) : string;
>
> type TDecorateURLsFlags = ( durlProto, durlAddr, durlPort, durlPath,
> durlBMark, durlParam);
>
> TDecorateURLsFlagSet = set of TDecorateURLsFlags;
>
> function DecorateEMails (const AText : string) : string;  

| Valor      | Sentido                                                     |
|------------|-------------------------------------------------------------|
| durlProto  | Protocolo (como `ftp: //` o `http: //`)                     |
| durlAddr   | Dirección TCP o nombre de dominio (como `masterAndrey.com`) |
| durlPort   | Número de puerto si se especifica (como `: 8080`)           |
| durlPath   | Ruta al documento (como `index.html`)                       |
| durlBMark  | Marca de libro (como `# mark`)                              |
| durlParam  | Parámetros de URL (como `? ID = 2 &amp; User = 13`)         |

Devuelve el texto de entrada `AText` con hipervínculos decorados.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.masterAndrey.com/contacts.htm` will be decorated as
`<a href="www.masterAndrey.com/contacts.htm">www.masterAndrey.com</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Ejemplos muy simples, ver comentarios dentro de la unidad.

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Ejemplos un poco más complejos, ver comentarios dentro de la unidad
