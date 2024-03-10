|     |                                                                |         |                                                                |                                                                  |                                                                 |                                                                |
|-----|----------------------------------------------------------------|---------|----------------------------------------------------------------|------------------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------|
|     | [English](https://regex.sorokin.engineer/en/latest/demos.html) | Русский | [Deutsch](https://regex.sorokin.engineer/de/latest/demos.html) | [Български](https://regex.sorokin.engineer/bg/latest/demos.html) | [Français](https://regex.sorokin.engineer/fr/latest/demos.html) | [Español](https://regex.sorokin.engineer/es/latest/demos.html) |

# Demos

Демо-код для [TRegExpr](index.html)

## Вступление

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.html).

TRegExpr interface described in [TRegExpr interface](tregexpr.html).

## Text2HTML

[Text2HTML
исходники](https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML)

Преобразует текст в HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Этот блок содержит функции для оформления
гиперссылок.

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

| Значение   | Имея в виду                                               |
|------------|-----------------------------------------------------------|
| durlProto  | Протокол (например, `ftp://` или `http://`)               |
| durlAddr   | TCP-адрес или доменное имя (например, `masterAndrey.com`) |
| durlPort   | Номер порта, если указан (например, `: 8080`)             |
| durlPath   | Путь к документу (например, `index.html`)                 |
| durlBMark  | Закладка (например, `# mark`)                             |
| durlParam  | Параметры URL (например, `? ID = 2 &amp; User = 13`)      |

Возвращает введенный текст `AText` с оформленными гиперссылками.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.masterAndrey.com/contacts.htm` will be decorated as
`<a href="www.masterAndrey.com/contacts.htm">www.masterAndrey.com</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Очень простые примеры, см. Комментарии внутри блока

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Чуть более сложные примеры, см. Комментарии внутри блока
