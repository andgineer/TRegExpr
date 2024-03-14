# FAQ

## J'ai trouvé un bug terrible : TRegExpr provoque une exception de violation d'accès !

**Réponse**

Vous devez créer l'objet avant de l'utiliser. Ainsi, après avoir déclaré quelque chose comme :

``` pascal
r : TRegExpr
```

n'oubliez pas de créer l'instance de l'objet :

``` pascal
r := TRegExpr.Create.
```

## Est-ce qu'il supporte l'Unicode ?

**Réponse**

[Comment utiliser l'Unicode](tregexpr.md#unicode)

## Pourquoi TRegExpr retourne plus d'une ligne ?

Par exemple, r.e. `<font .\*>` retourne le premier `<font`, puis le reste du fichier incluant le dernier `</html>`.

**Réponse**

Pour la compatibilité ascendante, le [modificateur /s](regular_expressions.md#s) est `Activé` par défaut.

Désactivez-le et `.` correspondra à tout sauf aux [séparateurs de ligne](regular_expressions.md#lineseparators) - exactement comme vous le souhaitez.

D'ailleurs, je suggère `<font ([^\n>]*)>`, dans `Match[1]` sera l'URL.

## Pourquoi TRegExpr retourne plus que ce que j'attends ?

Par exemple, r.e. `<p>(.+)</p>` appliqué à la chaîne `<p>a</p><p>b</p>` retourne `a</p><p>b` mais pas `a` comme je l'attendais.

**Réponse**

Par défaut, tous les opérateurs fonctionnent en mode `gourmand`, donc ils correspondent autant que possible.

Si vous voulez un mode `non gourmand`, vous pouvez utiliser des opérateurs non gourmands comme `+?` etc. ou passer tous les opérateurs en mode non gourmand avec l'aide du modificateur `g` (utilisez les propriétés appropriées de TRegExpr ou l'opérateur `?(-g)` dans r.e.).

## Comment parser des sources comme HTML avec l'aide de TRegExpr ?

**Réponse**

Désolé, mais c'est presque impossible !

Bien sûr, vous pouvez facilement utiliser TRegExpr pour extraire certaines informations de HTML, comme montré dans mes exemples, mais si vous voulez un parsing précis, vous devez utiliser un vrai parseur, pas r.e.

Vous pouvez lire l'explication complète dans le `Perl Cookbook` de Tom Christiansen et Nathan Torkington, par exemple.

En bref - il y a de nombreuses structures qui peuvent être facilement parsées par un vrai parseur mais pas du tout par r.e., et le vrai parseur est beaucoup plus rapide pour faire le parsing, car r.e. ne se contente pas de scanner le flux d'entrée, il effectue une recherche d'optimisation qui peut prendre beaucoup de temps.

## Y a-t-il un moyen d'obtenir plusieurs correspondances d'un motif sur TRegExpr ?

**Réponse**

Vous pouvez itérer les correspondances avec la méthode ExecNext.

Si vous voulez un exemple, veuillez consulter l'implémentation de la méthode `TRegExpr.Replace` ou les exemples pour [HyperLinksDecorator](demos.md)

## Je vérifie l'entrée de l'utilisateur. Pourquoi TRegExpr retourne `Vrai` pour des chaînes d'entrée incorrectes ?

**Réponse**

Dans de nombreux cas, les utilisateurs de TRegExpr oublient que l'expression régulière est destinée à **rechercher** dans la chaîne d'entrée.

Donc, par exemple, si vous utilisez l'expression `\d{4,4}`, vous aurez du succès pour des entrées utilisateur incorrectes comme `12345` ou `n'importe quelles lettres 1234`.

Vous devez vérifier du début à la fin de la ligne pour vous assurer qu'il n'y a rien d'autre autour : `^\d{4,4}$`.

## Pourquoi les itérateurs non gourmands fonctionnent parfois en mode gourmand ?

Par exemple, le r.e. `a+?,b+?` appliqué à la chaîne `aaa,bbb` correspond à `aaa,b`, mais ne devrait-il pas correspondre à `a,b` en raison de la non-gourmandise du premier itérateur ?

**Réponse**

Cela est dû à la manière de fonctionner de TRegExpr. En fait, beaucoup d'autres moteurs r.e. fonctionnent exactement de la même manière : ils n'effectuent qu'une `simple` optimisation de recherche, et n'essaient pas de faire la meilleure optimisation.

Dans certains cas, c'est mauvais, mais en général, c'est plutôt un avantage qu'une limitation, pour des raisons de performance et de prévisibilité.

La règle principale - r.e. essaie d'abord de correspondre à partir de l'endroit actuel et seulement si cela est complètement impossible, il avance d'un caractère et essaie à nouveau à partir de la position suivante dans le texte.

Ainsi, si vous utilisez `a,b+?` cela correspondra à `a,b`. Dans le cas de `a+?,b+?` c'est maintenant déconseillé (nous ajoutons le modificateur non gourmand) mais il est toujours possible de correspondre à plus d'un `a`, donc TRegExpr le fera.

TRegExpr comme les r.e. de Perl ou d'Unix n'essaient pas d'avancer et de vérifier - serait-ce une meilleure correspondance. Tout d'abord, simplement parce qu'il n'y a aucun moyen de dire qu'une correspondance est meilleure ou pire.

## Comment puis-je utiliser TRegExpr avec Borland C++ Builder ?

J'ai un problème car aucun fichier d'en-tête (`.h` ou `.hpp`) n'est disponible.

**Réponse**

- Ajoutez `RegExpr.pas` au projet `bcb`.
- Compilez le projet. Cela génère le fichier d'en-tête `RegExpr.hpp`.
- Maintenant, vous pouvez écrire du code qui utilise l'unité `RegExpr`.
- N'oubliez pas d'ajouter `#include “RegExpr.hpp”` là où c'est nécessaire.
- N'oubliez pas de remplacer tous les `\` dans les expressions régulières par `\\` ou de redéfinir la constante [EscChar](tregexpr.md#escchar).

## Pourquoi de nombreux r.e. (y compris les r.e. de l'aide et de la démo de TRegExpr) fonctionnent mal dans Borland C++ Builder ?

**Réponse**

L'astuce est dans la question précédente ;) Le symbole `\` a une signification 
spéciale en `C++`, donc vous devez l'`échapper` (comme décrit dans la réponse précédente). 
Mais si vous n'aimez pas les r.e. comme `\\w+\\w+\\.\\w+` vous pouvez redéfinir la 
constante `EscChar` (dans `RegExpr.pas`). 

Par exemple `EscChar = "/"`. 
Alors vous pouvez écrire `/w+/w+/./w+`, cela semble inhabituel mais est plus lisible.