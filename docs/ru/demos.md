|     |                                                             |         |                                                                |                                                                  |                                                                 |                                                                |
|-----|-------------------------------------------------------------|---------|----------------------------------------------------------------|------------------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------|
|     | [English](https://regex.sorokin.engineer/demos/) | Русский | [Deutsch](https://regex.sorokin.engineer/de/demos/) | [Български](https://regex.sorokin.engineer/bg/demos/) | [Français](https://regex.sorokin.engineer/fr/demos/) | [Español](https://regex.sorokin.engineer/es/demos/) |

# Demos

Демо-код для [TRegExpr](../tregexpr/)

## Вступление

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](../regular_expressions/).

TRegExpr interface described in [TRegExpr interface](../tregexpr/).

## Text2HTML

[Text2HTML
исходники](https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML)

Преобразует текст в HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Этот блок содержит функции для оформления
гиперссылок.

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

| Значение   | Имея в виду                                               |
|------------|-----------------------------------------------------------|
| durlProto  | Протокол (например, `ftp://` или `http://`)               |
| durlAddr   | TCP-адрес или доменное имя (например, `sorokin.engineer`) |
| durlPort   | Номер порта, если указан (например, `: 8080`)             |
| durlPath   | Путь к документу (например, `index.html`)                 |
| durlBMark  | Закладка (например, `# mark`)                             |
| durlParam  | Параметры URL (например, `? ID = 2 &amp; User = 13`)      |

Возвращает введенный текст `AText` с оформленными гиперссылками.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.sorokin.engineer/contacts.htm` will be decorated as
`<a href="www.sorokin.engineer/contacts.htm">www.sorokin.engineer</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Очень простые примеры, см. Комментарии внутри блока

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Чуть более сложные примеры, см. Комментарии внутри блока
