# Demos

Demo-Code für [TRegExpr](tregexpr.md)

## Einführung

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.md).

TRegExpr interface described in [TRegExpr interface](tregexpr.md).

## Text2HTML

\`Text2HTML-Quellen
\<<https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML>\>
\_

Veröffentlichen Sie Nur-Text als HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Diese Einheit enthält Funktionen zum
Verzieren von Hyperlinks.

For example, replaces `www.sorokin.engineer` with
`<a href="http://www.sorokin.engineer">www.sorokin.engineer</a>` or
`filbert@yandex.ru` with
`<a href="mailto:filbert@yandex.ru">filbert@yandex.ru</a>`.   ..
code-block:: pascal

> function DecorateURLs (  
> const AText : string; AFlags : TDecorateURLsFlagSet = \[durlAddr,
> DurlPath\]
>
> ) : string;
>
> type TDecorateURLsFlags = ( durlProto, durlAddr, durlPort, DurlPath,
> durlBMark, DurlParam);
>
> TDecorateURLsFlagSet = set of TDecorateURLsFlags;
>
> function DecorateEMails (const AText : string) : string;  

| Wert B     | edeutung                                                                                                    |
|------------|-------------------------------------------------------------------------------------------------------------|
| durlProto  | Protokoll (wie \`<span class="title-ref"> ftp: // \`oder</span> <span class="title-ref">http: //</span> \`) |
| durlAddr   | TCP-Adresse oder Domänenname (wie `sorokin.engineer`)                                                       |
| durlPort   | Portnummer, falls angegeben (wie `: 8080`)                                                                  |
| DurlPath   | Pfad zum Dokument (wie `index.html`)                                                                        |
| durlBMark  | Buchmarke (wie `#mark`)                                                                                     |
| DurlParam  | URL-Parameter (wie `? ID = 2 &amp; User = 13`)                                                              |

Gibt den eingegebenen Text &quot;AText&quot; mit verzierten Hyperlinks
zurück.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.sorokin.engineer/contacts.htm` will be decorated as
`<a href="www.sorokin.engineer/contacts.htm">www.sorokin.engineer</a>`.

## \`TRegExprRoutines \<<https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines>\> \_

Sehr einfache Beispiele, siehe Kommentare im Gerät

## \`TRegExprClass \<<https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass>\> \_

Etwas komplexere Beispiele, siehe Kommentare innerhalb der Einheit
