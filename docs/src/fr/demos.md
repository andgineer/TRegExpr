# Démos

Code de démonstration pour [TRegExpr](tregexpr.md)

## introduction

If you don't familiar with regular expression, please, take a look at
the [r.e.syntax](regular_expressions.md).

TRegExpr interface described in [TRegExpr interface](tregexpr.md).

## Text2HTML

[Sources
Text2HTML](https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML)

Publier du texte brut au format HTML

Uses unit
[HyperLinksDecorator](https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas)
that is based on TRegExpr.   Cette unité contient des fonctions pour
décorer des hyperliens.

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

| Valeur     | Sens                                                     |
|------------|----------------------------------------------------------|
| durlProto  | Protocole (comme `ftp: //` ou `http: //`)                |
| durlAddr   | Adresse TCP ou nom de domaine (comme `sorokin.engineer`) |
| durlPort   | Numéro de port si spécifié (comme `: 8080`)              |
| durlPath   | Chemin du document (comme `index.html`)                  |
| durlBMark  | Marque-page (comme `# mark`)                             |
| durlParam  | Paramètres d&#39;URL (comme `? ID = 2 &amp; User = 13`)  |

Retourne le texte saisi `AText` avec des hyperliens décorés.

`AFlags` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if <span class="title-ref">AFlags</span> is `[durlAddr]`
then hyper link `www.sorokin.engineer/contacts.htm` will be decorated as
`<a href="www.sorokin.engineer/contacts.htm">www.sorokin.engineer</a>`.

## [TRegExprRoutines](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines)

Exemples très simples, voir les commentaires à l&#39;intérieur de
l&#39;unité

## [TRegExprClass](https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass)

Exemples légèrement plus complexes, voir les commentaires à
l&#39;intérieur de l&#39;unité
