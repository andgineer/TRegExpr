# Демонстрации

Демо код за [TRegExpr](tregexpr.md)

## Въведение

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.md).

TRegExpr interface described in [TRegExpr interface](tregexpr.md).

## Text2HTML

Източници Text2HTML
\<<https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML>\>\`\_

Публикувайте обикновен текст като HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Тази единица съдържа функции за декориране
на хипер-връзки.

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

| стойност   | значение                                                     |
|------------|--------------------------------------------------------------|
| durlProto  | Протокол (като `ftp: //` или `http: //`)                     |
| durlAddr   | TCP адрес или име на домейн (като `sorokin.engineer`)        |
| durlPort   | Номер на порт, ако е посочен (като &quot;: 8080&quot;)       |
| durlPath   | Път до документа (като ,, index.html &quot;)                 |
| durlBMark  | Маркиране на книга (като \`\` \# знак &#39;&#39;)            |
| durlParam  | URL адреси (като &quot;? ID = 2 &amp; Потребител = 13&quot;) |

Връща входния текст `AText` с украсени хипервръзки.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.sorokin.engineer/contacts.htm` will be decorated as
`<a href="www.sorokin.engineer/contacts.htm">www.sorokin.engineer</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Много прости примери, вижте коментарите вътре в устройството

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Малко по-сложни примери, вижте коментарите вътре в устройството
