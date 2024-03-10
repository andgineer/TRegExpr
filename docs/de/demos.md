|     |                                                                |                                                                |         |                                                                  |                                                                 |                                                                |
|-----|----------------------------------------------------------------|----------------------------------------------------------------|---------|------------------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------|
|     | [English](https://regex.sorokin.engineer/en/latest/demos.html) | [Русский](https://regex.sorokin.engineer/ru/latest/demos.html) | Deutsch | [Български](https://regex.sorokin.engineer/bg/latest/demos.html) | [Français](https://regex.sorokin.engineer/fr/latest/demos.html) | [Español](https://regex.sorokin.engineer/es/latest/demos.html) |

# Demos

Demo-Code für \`TRegExpr \<index.html\> \_\_

## Einführung

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.html).

TRegExpr interface described in [TRegExpr interface](tregexpr.html).

## Text2HTML

\`Text2HTML-Quellen
\<<https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML>\>
\_

Veröffentlichen Sie Nur-Text als HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Diese Einheit enthält Funktionen zum
Verzieren von Hyperlinks.

For example, replaces `www.masterAndrey.com` with
`<a href="http://www.masterAndrey.com">www.masterAndrey.com</a>` or
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
| durlAddr   | TCP-Adresse oder Domänenname (wie `masterAndrey.com`)                                                       |
| durlPort   | Portnummer, falls angegeben (wie `: 8080`)                                                                  |
| DurlPath   | Pfad zum Dokument (wie `index.html`)                                                                        |
| durlBMark  | Buchmarke (wie `#mark`)                                                                                     |
| DurlParam  | URL-Parameter (wie `? ID = 2 &amp; User = 13`)                                                              |

Gibt den eingegebenen Text &quot;AText&quot; mit verzierten Hyperlinks
zurück.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.masterAndrey.com/contacts.htm` will be decorated as
`<a href="www.masterAndrey.com/contacts.htm">www.masterAndrey.com</a>`.

## \`TRegExprRoutines \<<https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines>\> \_

Sehr einfache Beispiele, siehe Kommentare im Gerät

## \`TRegExprClass \<<https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass>\> \_

Etwas komplexere Beispiele, siehe Kommentare innerhalb der Einheit
