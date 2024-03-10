|     |         |                                                                |                                                                |                                                                  |                                                                 |                                                                |
|-----|---------|----------------------------------------------------------------|----------------------------------------------------------------|------------------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------|
|     | English | [Русский](https://regex.sorokin.engineer/ru/latest/demos.html) | [Deutsch](https://regex.sorokin.engineer/de/latest/demos.html) | [Български](https://regex.sorokin.engineer/bg/latest/demos.html) | [Français](https://regex.sorokin.engineer/fr/latest/demos.html) | [Español](https://regex.sorokin.engineer/es/latest/demos.html) |

# Демонстрации

Демо код за [TRegExpr](index.html)

## Въведение

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.html).

TRegExpr interface described in [TRegExpr interface](tregexpr.html).

## Text2HTML

Източници Text2HTML
\<<https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML>\>\`\_

Публикувайте обикновен текст като HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Тази единица съдържа функции за декориране
на хипер-връзки.

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

| стойност   | значение                                                     |
|------------|--------------------------------------------------------------|
| durlProto  | Протокол (като `ftp: //` или `http: //`)                     |
| durlAddr   | TCP адрес или име на домейн (като `masterAndrey.com`)        |
| durlPort   | Номер на порт, ако е посочен (като &quot;: 8080&quot;)       |
| durlPath   | Път до документа (като ,, index.html &quot;)                 |
| durlBMark  | Маркиране на книга (като \`\` \# знак &#39;&#39;)            |
| durlParam  | URL адреси (като &quot;? ID = 2 &amp; Потребител = 13&quot;) |

Връща входния текст `AText` с украсени хипервръзки.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.masterAndrey.com/contacts.htm` will be decorated as
`<a href="www.masterAndrey.com/contacts.htm">www.masterAndrey.com</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Много прости примери, вижте коментарите вътре в устройството

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Малко по-сложни примери, вижте коментарите вътре в устройството
