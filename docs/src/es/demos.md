# Población

Código de demostración para [TRegExpr](tregexpr.md)

## Introducción

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.md).

TRegExpr interface described in [TRegExpr interface](tregexpr.md).

## Text2HTML

[Fuentes
Text2HTML](https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML)

Publicar texto plano como HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Esta unidad contiene funciones para decorar
hipervínculos.

For example, replaces `www.sorokin.engineer` with
`<a href="http://www.sorokin.engineer">www.sorokin.engineer</a>` or
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
| durlAddr   | Dirección TCP o nombre de dominio (como `sorokin.engineer`) |
| durlPort   | Número de puerto si se especifica (como `: 8080`)           |
| durlPath   | Ruta al documento (como `index.html`)                       |
| durlBMark  | Marca de libro (como `# mark`)                              |
| durlParam  | Parámetros de URL (como `? ID = 2 &amp; User = 13`)         |

Devuelve el texto de entrada `AText` con hipervínculos decorados.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.sorokin.engineer/contacts.htm` will be decorated as
`<a href="www.sorokin.engineer/contacts.htm">www.sorokin.engineer</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Ejemplos muy simples, ver comentarios dentro de la unidad.

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Ejemplos un poco más complejos, ver comentarios dentro de la unidad
