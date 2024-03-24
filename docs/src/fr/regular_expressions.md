# Expressions régulières (RegEx)

## Introduction

Les expressions régulières sont un moyen pratique de spécifier des modèles de texte.

Avec les expressions régulières, vous pouvez valider des entrées utilisateur, rechercher certains motifs comme des emails ou des numéros de téléphone sur des pages web ou dans des documents, etc.

Ci-dessous se trouve la feuille de triche complète pour les expressions régulières.

## Caractères

### Correspondances simples

Tout caractère unique (à l'exception des caractères spéciaux de regex) correspond à lui-même. Une série de caractères (non spéciaux) correspond à cette série de caractères dans la chaîne d'entrée.

| RegEx    | Correspondances |
|----------|-----------------|
| `foobar` | `foobar`        |

### Caractères non imprimables (codes d'échappement)

Pour spécifier un caractère par son code Unicode, utilisez le préfixe `\x` suivi du code hexadécimal. Pour un code de 3-4 chiffres (après U+00FF), encadrez le code entre accolades.

| RegEx        | Correspondances                               |
|--------------|-----------------------------------------------|
| `\xAB`       | caractère avec le code hexadécimal `AB` de 2 chiffres |
| `\x{AB20}`   | caractère avec le code hexadécimal `AB20` de 1..4 chiffres |
| `foo\x20bar` | `foo bar` (notez l'espace au milieu)          |

Il existe un certain nombre de codes d'échappement prédéfinis pour les caractères non imprimables, comme dans le langage C :

| RegEx       | Correspondances                                 |
|-------------|-------------------------------------------------|
| `\t`        | tabulation (HT/TAB), équivalent à `\x09`        |
| `\n`        | saut de ligne (LF), équivalent à `\x0a`         |
| `\r`        | retour chariot (CR), équivalent à `\x0d`        |
| `\f`        | alimentation en formulaire (FF), équivalent à `\x0c` |
| `\a`        | alarme (BEL), équivalent à `\x07`               |
| `\e`        | échappement (ESC), équivalent à `\x1b`          |
| `\cA` ... `\cZ` | chr(0) à chr(25). Par exemple, `\cI` correspond au caractère de tabulation. Les lettres minuscules "a"..."z" sont également prises en charge. |

### Échappement

Pour représenter un caractère spécial de regex (un de `.+*?|\()[]{}^$`), préfixez-le avec un antislash `\`. L'antislash littéral doit également être échappé.

| RegEx         | Correspondances                                                     |
|---------------|---------------------------------------------------------------------|
| `\^FooBarPtr` | `^FooBarPtr`, ceci est `^` et non le [début de ligne](#lineseparators) |
| `\[a\]`       | `[a]`, ceci n'est pas une [classe de caractère](#userclass)        |

## Classes de caractères

### Classes de caractères utilisateur

Une classe de caractères est une liste de caractères à l'intérieur des crochets `[]`. La classe correspond à tout **caractère unique** listé dans cette classe.

| RegEx            | Correspondances                                     |
|------------------|-----------------------------------------------------|
| `foob[aeiou]r`   | `foobar`, `foober`, etc. mais pas `foobbr`, `foobcr` |

Vous pouvez "inverser" la classe - si le premier caractère après le `[` est `^`, alors la classe correspond à tout caractère **sauf** les caractères listés dans la classe.

| RegEx           | Correspondances                                     |
|-----------------|-----------------------------------------------------|
| `foob[^aeiou]r` | `foobbr`, `foobcr`, etc. mais pas `foobar`, `foober` |

Dans une liste, le caractère tiret `-` est utilisé pour spécifier une plage, de sorte que `a-z` représente tous les caractères entre `a` et `z`, inclus.

Si vous voulez que le tiret `-` lui-même soit membre d'une classe, placez-le au début ou à la fin de la liste, ou [échappez](#escape) le avec un antislash.

Si vous voulez que `]` fasse partie de la classe, vous pouvez le placer au début de la liste ou [l'échapper](#escape) avec un antislash.

| RegEx       | Correspondances                      |
|-------------|--------------------------------------|
| `[-az]`     | `a`, `z` et `-`                      |
| `[az-]`     | `a`, `z` et `-`                      |
| `[a\-z]`    | `a`, `z` et `-`                      |
| `[a-z]`     | caractères de `a` à `z`              |
| `[\n-\x0D]` | caractères de chr(10) à chr(13)      |

### Méta-caractère point

Le méta-caractère `.` (point) correspond par défaut à tout caractère. Mais si vous désactivez le [modificateur /s](#s), alors il ne correspondra pas aux caractères de saut de ligne.

Le `.` ne fonctionne pas comme méta-classe à l'intérieur des [classes de caractères utilisateur](#user-character-classes). `[.]` signifie un "." littéral.

### Méta-classes

Il existe un certain nombre de classes de caractères prédéfinies qui rendent les expressions régulières plus compactes, "méta-classes" :

| RegEx | Correspondances                                   |
|-------|---------------------------------------------------|
| `\w`  | un caractère alphanumérique, y compris `_`        |
| `\W`  | un non-alphanumérique                             |
| `\d`  | un caractère numérique (comme `[0-9]`)            |
| `\D`  | un non-numérique                                  |
| `\s`  | un espace (comme `[ \t\n\r\f]`)                   |
| `\S`  | un non-espace                                     |
| `\h`  | espace horizontal : la tabulation et tous les caractères dans la catégorie "séparateur d'espace" de Unicode |
| `\H`  | pas un espace horizontal                          |
| `\v`  | espace vertical : tous les caractères traités comme des sauts de ligne dans la norme Unicode |
| `\V`  | pas un espace vertical                            |
| `\R`  | saut de ligne Unicode : LF, paire CR LF, CR, FF (saut de page), VT (tabulation verticale), U+0085, U+2028, U+2029 |

Vous pouvez utiliser toutes les méta-classes mentionnées dans le tableau ci-dessus dans les [classes de caractères utilisateur](#user-character-classes).

| RegEx         | Correspondances                                                    |
|---------------|--------------------------------------------------------------------|
| `foob\dr`     | `foob1r`, `foob6r`, etc. mais pas `foobar`, `foobbr`, etc.         |
| `foob[\w\s]r` | `foobar`, `foob r`, `foobbr`, etc. mais pas `foob1r`, `foob=r`, etc. |

> [TRegExpr](tregexpr.md)
>
> Les propriétés [SpaceChars](tregexpr.md#spacechars) et
> [WordChars](tregexpr.md#wordchars) définissent les classes de caractères `\w`,
> `\W`, `\s`, `\S`.
>
> Vous pouvez donc redéfinir ces classes.

## Limites

### Limites de ligne

| Méta-caractère | Correspondances                                        |
|----------------|--------------------------------------------------------|
| `^`            | correspondance de longueur zéro au début de ligne      |
| `$`            | correspondance de longueur zéro à la fin de ligne      |
| `\A`           | correspondance de longueur zéro tout au début          |
| `\z`           | correspondance de longueur zéro tout à la fin          |
| `\Z`           | comme `\z` mais correspond aussi avant le dernier saut de ligne |
| `\G`           | correspondance de longueur zéro à la position de fin de la correspondance précédente |

Exemples :

| RegEx      | Correspondances                                    |
|------------|----------------------------------------------------|
| `^foobar`  | `foobar` seulement s'il est au début de ligne      |
| `foobar$`  | `foobar` seulement s'il est à la fin de ligne      |
| `^foobar$` | `foobar` seulement s'il est la seule chaîne en ligne |
| `foob.r`   | `foobar`, `foobbr`, `foob1r`, etc.                 |

Le méta-caractère `^` correspond à une position de longueur zéro au début de la chaîne d'entrée. `$` - à la fin. Si le [modificateur /m](#m) est **activé**, ils correspondent également au début/à la fin de lignes individuelles dans le texte multiligne.

Notez qu'il n'y a pas de ligne vide dans la séquence `\x0D\x0A`.

> [TRegExpr](tregexpr.md)
>
> Si vous utilisez la [version Unicode](tregexpr.md#unicode), alors
> `^`/`$` correspondent également à `\x2028`, `\x2029`, `\x0B`, `\x0C` ou `\x85`.

Le méta-caractère `\A` correspond à la position de longueur zéro tout au début de
la chaîne d'entrée, `\z` - à tout à la fin. Ils ignorent le [modificateur /m](#m).
`\Z` est comme `\z`, mais correspond aussi avant le dernier saut de ligne (LF et
CR LF). Le comportement de `\A`, `\z`, `\Z` est conçu comme dans la plupart des grands
moteurs de regex (Perl, PCRE, etc).

Notez que `^.*$` ne correspond pas à une chaîne entre `\x0D\x0A`, car
c'est un séparateur de ligne ininterrompable. Mais il correspond à la chaîne vide
dans la séquence `\x0A\x0D` parce que c'est 2 sauts de ligne dans le
mauvais ordre.

> [TRegExpr](tregexpr.md)
>
> Le traitement multi-ligne peut être ajusté par les propriétés
> [LineSeparators](tregexpr.md#lineseparators) et
> [UseLinePairedBreak](tregexpr.md#linepairedseparator).
>
> Ainsi, vous pouvez utiliser des séparateurs de style Unix `\n` ou de style DOS/Windows `\r\n`
> ou les mélanger ensemble (comme dans le comportement par défaut décrit ci-dessus).

Si vous préférez une description mathématiquement correcte, vous pouvez la trouver sur
[www.unicode.org](http://www.unicode.org/unicode/reports/tr18/).

### Limites de mots

| RegEx | Correspondances       |
|-------|-----------------------|
| `\b`  | une limite de mot     |
| `\B`  | une non-limite de mot |

Une limite de mot `\b` est un point entre deux caractères qui a un `\w` d'un côté et un `\W` de l'autre côté (dans un ordre quelconque).

## Quantification

### Quantificateurs

Tout élément d'une expression régulière peut être suivi d'un quantificateur. Le quantificateur spécifie le nombre de répétitions de l'élément.

| RegEx    | Correspondances                                                  |
|----------|------------------------------------------------------------------|
| `{n}`    | exactement `n` fois                                             |
| `{n,}`   | au moins `n` fois                                                |
| `{,m}`   | pas plus de `m` fois (seulement avec AllowBraceWithoutMin)       |
| `{n,m}`  | au moins `n` mais pas plus de `m` fois                           |
| `*`      | zéro ou plus, similaire à `{0,}`                                 |
| `+`      | un ou plus, similaire à `{1,}`                                   |
| `?`      | zéro ou un, similaire à `{0,1}`                                  |

Les chiffres entre accolades `{n,m}` spécifient le nombre minimum de fois à correspondre `n` et le maximum `m`.

`{n}` est équivalent à `{n,n}` et correspond exactement `n` fois. `{n,}` correspond `n` fois ou plus.

La variante `{,m}` n'est prise en charge que si la propriété AllowBraceWithoutMin est définie.

Il n'y a pas de limite pratique aux valeurs de n et m (la limite est la valeur maximale entière signée de 32 bits).

Utiliser `{` sans une plage correcte donnera une erreur. Ce comportement peut être modifié en définissant la propriété AllowLiteralBraceWithoutRange, qui acceptera `{` comme un caractère littéral, s'il n'est pas suivi d'une plage. Une plage avec une valeur basse plus grande que la valeur haute donnera toujours une erreur.

| RegEx            | Correspondances                                                      |
|------------------|----------------------------------------------------------------------|
| `foob.*r`        | `foobar`,  `foobalkjdflkj9r` et `foobr`                              |
| `foob.+r`        | `foobar`, `foobalkjdflkj9r` mais pas `foobr`                         |
| `foob.?r`        | `foobar`, `foobbr` et `foobr` mais pas `foobalkj9r`                  |
| `fooba{2}r`      | `foobaar`                                                            |
| `fooba{2,}r`     | `foobaar'`, `foobaaar`, `foobaaaar`, etc.                            |
| `fooba{2,3}r`    | `foobaar`, ou `foobaaar`  mais pas `foobaaaar`                       |
| `(foobar){8,10}` | 8...10 instances de `foobar` (`()` est un [groupe](#subexpression)) |

<a name="greedy"></a>

### Gourmandise

Les [Quantificateurs](#iterator) en mode "gourmand" prennent autant que possible, en
mode "paresseux" - le moins possible.

Par défaut, tous les quantificateurs sont "gourmands". Ajoutez le caractère `?` pour
rendre tout quantificateur "paresseux".

Pour la chaîne `abbbbc` :

| RegEx     | Correspondances |
|-----------|-----------------|
| `b+`      | `bbbb`          |
| `b+?`     | `b`             |
| `b*?`     | chaîne vide     |
| `b{2,3}?` | `bb`            |
| `b{2,3}`  | `bbb`           |

Vous pouvez passer tous les quantificateurs en mode "paresseux" ([modificateur /g](#g),
ci-dessous nous utilisons [changement de modificateur en ligne](#inlinemodifiers)).

| RegEx     | Correspondances |
|-----------|-----------------|
| `(?-g)b+` | `b`             |

### Quantificateur Possessif

La syntaxe est : `a++`, `a*+`, `a?+`, `a{2,4}+`. Actuellement, elle est prise en charge
seulement pour les accolades simples, mais pas pour les accolades après un groupe comme
`(foo|bar){3,5}+`.

Cette fonctionnalité regex est [décrite
ici.](https://regular-expressions.mobi/possessive.html?wlr=1) En bref,
le quantificateur possessif accélère la correspondance dans les cas complexes.

## Choix

Les expressions dans le choix sont séparées par une barre verticale `|`.

Ainsi `fee|fie|foe` correspondra à l'une des `fee`, `fie`, ou `foe` dans la chaîne cible
(comme le ferait `f(e|i|o)e`).

La première expression inclut tout depuis le dernier délimiteur de motif
(`(`, `[`, ou le début du motif) jusqu'au premier `|`, et la
dernière expression contient tout depuis le dernier `|` jusqu'au prochain
délimiteur de motif.

Cela semble un peu compliqué, il est donc courant d'inclure le
choix entre parenthèses, pour minimiser la confusion sur son début et sa fin.

Les expressions dans le choix sont testées de gauche à droite, donc la première
expression qui correspond, est celle qui est choisie.

Par exemple, l'expression régulière `foo|foot` dans la chaîne `barefoot` correspondra
à `foo`. Juste la première expression qui correspond.

Rappelez-vous aussi que `|` est interprété comme un littéral à l'intérieur des crochets carrés, donc si vous écrivez `[fee|fie|foe]` vous ne faites vraiment correspondre que
`[feio|]`.

| RegEx          | Correspondances       |
|----------------|-----------------------|
| `foo(bar|foo)` | `foobar` ou `foofoo`  |

<a name="subexpression"></a>

## Groupes

Les crochets `()` sont utilisés pour définir des groupes (c'est-à-dire des sous-expressions).

> [!NOTE]
> [TRegExpr](tregexpr.md)
>
> Les positions des groupes, les longueurs et les valeurs réelles seront dans
> [MatchPos](tregexpr.md#matchpos), [MatchLen](tregexpr.md#matchlen)
> et [Match](tregexpr.md#match).
>
> Vous pouvez les remplacer par [Substitute](tregexpr.md#substitute).

Les groupes sont numérotés de gauche à droite par leur parenthèse ouvrante
(y compris les groupes imbriqués). Le premier groupe a l'indice 1. Le regex entier a
l'indice 0.

> | Groupe | Valeur    |
> |--------|-----------|
> | 0      | `foobar`  |
> | 1      | `foobar`  |
> | 2      | `bar`     |

## Références Arrière

Les métacaractères `\1` à `\9` sont interprétés comme des références arrière aux
groupes de capture. Ils correspondent au groupe trouvé précédemment avec l'indice spécifié.

Le métacaractère `\g` suivi d'un nombre est également interprété comme
des références arrière aux groupes de capture. Il peut être suivi d'un nombre à plusieurs chiffres.

| RegEx      | Correspondances           |
|------------|---------------------------|
| `(.)\1+`   | `aaaa` et `cc`            |
| `(.+)\1+`  | également `abab` et `123123` |
| `(.)\g1+`  | `aaaa` et `cc`            |

RegEx `(['"]?)(\d+)\1` correspond à `"13"` (entre guillemets doubles), ou `'4'` (entre
guillemets simples) ou `77` (sans guillemets), etc.

## Groupes Només et Références Arrière

Les groupes nommés dans les expressions régulières vous permettent d'étiqueter une partie de votre motif. 
Cela rend vos motifs plus faciles à comprendre et à mettre à jour.

Pour créer un groupe nommé, utilisez `(?<name>pattern)` ou `(?'name'pattern)`, 
où `name` est le nom du groupe et `pattern` est le motif regex que vous souhaitez capturer.

Les références arrière vous permettent de faire correspondre le même texte qu'un groupe a capturé précédemment. 
Les références arrière nommées utilisent `\k<name>`, où `name` est le nom du groupe que vous souhaitez faire correspondre à nouveau.

TRegExpr prend également en charge la version Perl : `(?P<name>pattern)` pour définir un groupe nommé et `(?P=name)` 
pour les références arrière.


Exemple

| RegEx                    | Correspondances        |
|--------------------------|------------------------|
| `(?P<qq>['"])\w+(?P=qq)` | `"mot"` et `'mot'`     |

## Résultat Correspondant

Le début de la correspondance signalée peut être défini en utilisant `\K`.

Par défaut, l'ensemble du texte couvert par un motif est considéré comme correspondant.
Cependant, il est possible de définir explicitement ce qui sera signalé.

Le motif `a\Kb` nécessitera que le texte contienne "ab". Mais seulement le
"b" sera signalé comme ayant correspondu. Il peut y avoir plusieurs `\K`
dans un motif, Le dernier définira la position de début de correspondance. Seuls les `\K`
dans les parties actives du motif sont considérés. Par ex. `a(\Kb)?` ne
considérera pas `\K` s'il n'y a pas de "b". Les captures peuvent exister en dehors de la correspondance
définie par `\K`.

Si utilisé dans d'autres constructions qui peuvent s'appliquer en dehors de la correspondance signalée
(comme l'anticipation positive), alors la position marquée par `\K` doit être avant ou à
la fin signalée de la correspondance. Si la position est marquée plus tard, la
correspondance est considérée comme échouée.

`\K` est quelque peu similaire à un regard en arrière. Contrairement à un regard en arrière, la partie
du motif avant le `\K` doit être après la position de début de la
correspondance, si le motif est appliqué à partir d'une position décalée dans le
texte.

## Modificateurs

Les modificateurs servent à changer le comportement des expressions régulières.

Vous pouvez définir des modificateurs globalement dans votre système ou les changer à l'intérieur de
l'expression régulière en utilisant [(?imsxr-imsxr)](#inlinemodifiers).

> [TRegExpr](tregexpr.md)
>
> Pour changer les modificateurs, utilisez [ModifierStr](tregexpr.md#modifierstr) ou
> les propriétés appropriées de `TRegExpr`
> [Modifier\*](tregexpr.md#modifieri).
>
> Les valeurs par défaut sont définies dans [variables
> globales](tregexpr.md#global-constants). Par exemple, la variable globale `RegExprModifierX` définit la valeur par défaut pour la propriété `ModifierX`.

<a name="i"></a>

### i, insensible à la casse

Insensible à la casse. Utilisez les paramètres de locale installés dans votre système, voir aussi
[InvertCase](tregexpr.md#invertcase).

<a name="m"></a>

### m, chaînes multilignes

Traitez la chaîne comme plusieurs lignes. Ainsi `^` et `$` correspondent au début ou à la fin
de n'importe quelle ligne n'importe où dans la chaîne.

Voir aussi [Limites de Ligne](#lineseparators).

<a name="s"></a>

### s, chaînes sur une seule ligne

Traitez la chaîne comme une seule ligne. Ainsi `.` correspond à n'importe quel caractère,
même un séparateur de lignes.

Voir aussi [Limites de Ligne](#lineseparators), auxquels il ne correspondrait normalement pas.

<a name="g"></a>

### g, gourmandise

> [TRegExpr](tregexpr.md) seulement modificateur.

En le désactivant, vous passerez [quantificateurs](#iterator) en mode
[non-gourmand](#greedy).

Ainsi, si le modificateur `/g` est `Off`, alors `+` fonctionne comme `+?`, `*` comme `*?` et ainsi
de suite.

Par défaut, ce modificateur est `On`.

<a name="x"></a>

### x, syntaxe étendue

Permet de commenter l'expression régulière et de la diviser en plusieurs
lignes.

Si le modificateur est `On`, nous ignorons tous les espaces blancs qui ne sont ni
échappés ni dans une classe de caractères.

Et le caractère `#` sépare les commentaires.

Notez que vous pouvez utiliser des lignes vides pour formater l'expression régulière pour
une meilleure lisibilité :

``` text
(
(abc) # commentaire 1
#
(efg) # commentaire 2
)
```

Cela signifie également que si vous voulez de vrais espaces blancs ou des caractères `#` dans
le motif (en dehors d'une classe de caractères, où ils ne sont pas affectés par
`/x`), vous devrez soit les échapper soit les encoder en utilisant des échappements octaux ou
hexadécimaux.

<a name="r"></a>

### r, gammes russes

> [TRegExpr](tregexpr.md) seul modificateur.

Dans la table ASCII russe, les caractères `ё`/`Ё` sont placés séparément des
autres.

Les caractères russes grands et petits sont dans des plages séparées, c'est le
même que pour les caractères anglais mais néanmoins je voulais une forme courte.

Avec ce modificateur au lieu de `[а-яА-ЯёЁ]` vous pouvez écrire `[а-Я]` si vous
avez besoin de tous les caractères russes.

Lorsque le modificateur est `On` :

| RegEx | Correspondances                |
|-------|--------------------------------|
| `а-я` | caractères de `а` à `я` et `ё` |
| `А-Я` | caractères de `А` à `Я` et `Ё` |
| `а-Я` | tous les symboles russes       |

Le modificateur est réglé sur <span class="title-ref">On</span> par défaut.

## Assertions (anticipation positive, anticipation négative)

<a name="assertions"></a>

Assertion d'anticipation positive : `foo(?=bar)` correspond à "foo" seulement avant
"bar", et "bar" est exclu de la correspondance.

Assertion d'anticipation négative : `foo(?!bar)` correspond à "foo" seulement s'il n'est pas suivi par "bar".

Assertion de rétrospection positive : `(?<=foo)bar` correspond à "bar" seulement après
"foo", et "foo" est exclu de la correspondance.

Assertion de rétrospection négative : `(?<!foo)bar` correspond à "bar" seulement s'il n'est pas précédé de "foo".

Limitations :

- Les rétrospections de longueur variable ne sont pas autorisées à contenir des groupes de capture.
  Cela peut être autorisé en définissant la propriété `AllowUnsafeLookBehind`.
  Si cela est activé et qu'il y a plus d'une correspondance dans le texte que
  le groupe pourrait capturer, alors la mauvaise correspondance peut être capturée. Cela
  n'affecte pas la justesse de l'assertion globale. (C'est-à-dire, la
  rétrospection retournera correctement si le texte avant correspondait au
  motif).
- Les rétrospections de longueur variable peuvent être lentes à exécuter, si elles ne
  correspondent pas.

## Groupes non capturants

La syntaxe est comme ceci : `(?:expr)`.

De tels groupes n'ont pas d'"index" et sont invisibles pour
les références arrière. Les groupes non capturants sont utilisés lorsque vous voulez grouper une
sous-expression, mais vous ne voulez pas la sauvegarder comme une partie correspondante/capturée de la chaîne. C'est donc juste une façon d'organiser votre regex en
sous-expressions sans surcharge de capturer le résultat :

| RegEx                          | Correspondances                                                     |
|--------------------------------|---------------------------------------------------------------------|
| `(https?|ftp)://([^/\r\n]+)`   | dans `https://sorokin.engineer` correspond à `https` et `sorokin.engineer` |
| `(?:https?|ftp)://([^/\r\n]+)` | dans `https://sorokin.engineer` correspond seulement à `sorokin.engineer` |

## Groupes atomiques

La syntaxe est comme ceci : `(?>expr|expr|...)`.

Les groupes atomiques sont un cas spécial de groupes non capturants. [Description de
eux.](https://regular-expressions.mobi/atomic.html?wlr=1)

## Modificateurs en ligne

<a name="inlinemodifiers"></a>

Syntaxe pour un modificateur : `(?i)` pour activer, et `(?-i)` pour désactiver.
Plusieurs modificateurs sont autorisés comme ceci : `(?msgxr-imsgxr)`.

Vous pouvez l'utiliser à l'intérieur de l'expression régulière pour modifier les modificateurs
à la volée. Cela peut être particulièrement pratique car cela a une portée locale dans une
expression régulière. Cela n'affecte que cette partie de l'expression régulière qui
suit l'opérateur `(?imsgxr-imsgxr)`.

Et s'il est à l'intérieur d'un groupe, cela n'affectera que ce groupe - spécifiquement
la partie du groupe qui suit les modificateurs. Ainsi dans
`((?i)Saint)-Petersburg` cela n'affecte que le groupe `((?i)Saint)` donc cela correspondra
à `saint-Petersburg` mais pas à `saint-petersburg`.

Les modificateurs en ligne peuvent également être donnés dans le cadre d'un groupe non capturant :
`(?i:modèle)`.

| RegEx                        | Correspondances                                   |
|------------------------------|---------------------------------------------------|
| `(?i)Saint-Petersburg`       | `Saint-petersburg` et `Saint-Petersburg`          |
| `(?i)Saint-(?-i)Petersburg`  | `Saint-Petersburg` mais pas `Saint-petersburg`    |
| `(?i)(Saint-)?Petersburg`    | `Saint-petersburg` et `saint-petersburg`          |
| `((?i)Saint-)?Petersburg`    | `saint-Petersburg`, mais pas `saint-petersburg`   |

## Commentaires

La syntaxe est comme ceci : `(?#texte)`. Le texte à l'intérieur des crochets est ignoré.

Notez que le commentaire est fermé par le `)` le plus proche, donc il n'y a aucun moyen
de mettre un `)` littéral dans le commentaire.

## Récursion

La syntaxe est `(?R)`, l'alias est `(?0)`.

Le regex `a(?R)?z` correspond à une ou plusieurs lettres "a" suivies par exactement
le même nombre de lettres "z".

Le principal objectif de la récursion est de correspondre à des constructions équilibrées ou imbriquées. Le regex générique est `b(?:m|(?R))*e` où "b" est ce qui
commence la construction, "m" est ce qui peut se produire au milieu de la
construction, et "e" est ce qui se produit à la fin de la construction.

Si ce qui peut apparaître au milieu de la construction équilibrée peut également
apparaître seul sans les parties de début et de fin, alors le regex
générique est `b(?R)*e|m`.

## Appels de sous-routine

Syntaxe pour l'appel aux groupes numérotés : `(?1)` ... `(?90)` (l'indice maximal est
limité par le code).

Syntaxe pour l'appel aux groupes nommés : `(?P>name)`. La syntaxe Perl est également
prise en charge: `(?&name)`, `\g<name>` and `\g'name'`

C'est comme la récursion, mais appelle seulement le code du groupe de capture avec
l'indice spécifié.

## Catégories Unicode

La norme Unicode a des noms pour les catégories de caractères. Ce sont des chaînes de 2 lettres. Par exemple, "Lu" est pour les lettres majuscules, "Ll" est pour les lettres minuscules. Et la catégorie plus grande d'une lettre "L" est pour toutes les lettres.

- Cc - Contrôle
- Cf - Format
- Co - Utilisation Privée
- Cs - Surrogat
- Ll - Lettre Minuscule
- Lm - Lettre Modificatrice
- Lo - Autre Lettre
- Lt - Lettre Majuscule et Minuscule
- Lu - Lettre Majuscule
- Mc - Marque d'Espacement
- Me - Marque d'Encadrement
- Mn - Marque Non-espacée
- Nd - Nombre Décimal
- Nl - Nombre de Lettre
- No - Autre Nombre
- Pc - Ponctuation de Connexion
- Pd - Ponctuation Tiret
- Pe - Ponctuation Fermante
- Pf - Ponctuation Finale
- Pi - Ponctuation Initiale
- Po - Autre Ponctuation
- Ps - Ponctuation Ouvrante
- Sc - Symbole Monétaire
- Sk - Symbole Modificateur
- Sm - Symbole Mathématique
- So - Autre Symbole
- Zl - Séparateur de Ligne
- Zp - Séparateur de Paragraphe
- Zs - Séparateur d'Espace

Le métacaractère `\p` désigne un caractère Unicode de la catégorie spécifiée.
Syntaxe : `\pL` et `\p{L}` pour un nom à 1 lettre, `\p{Lu}` pour les noms à 2 lettres.

Le métacaractère `\P` est inversé, il désigne un caractère Unicode **non** dans
la catégorie spécifiée.

Ces métacaractères sont également pris en charge dans les classes de caractères.

## Postface

Dans cet [ancien article de blog du siècle précédent](https://sorokin.engineer/posts/en/text_processing_from_birds_eye_view.html)
j'illustre quelques utilisations des expressions régulières.
