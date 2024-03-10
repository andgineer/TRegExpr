|     |         |                                                              |                                                              |                                                                |                                                               |                                                              |
|-----|---------|--------------------------------------------------------------|--------------------------------------------------------------|----------------------------------------------------------------|---------------------------------------------------------------|--------------------------------------------------------------|
|     | English | [Русский](https://regex.sorokin.engineer/ru/latest/faq.html) | [Deutsch](https://regex.sorokin.engineer/de/latest/faq.html) | [Български](https://regex.sorokin.engineer/bg/latest/faq.html) | [Français](https://regex.sorokin.engineer/fr/latest/faq.html) | [Español](https://regex.sorokin.engineer/es/latest/faq.html) |

# FAQ

## J&#39;ai trouvé un bug terrible: TRegExpr déclenche une exception de violation d&#39;accès!

**Réponse**

You must create the object before usage. So, after you declared
something like:

``` pascal
r: TRegExpr
```

n&#39;oubliez pas de créer l&#39;instance d&#39;objet:

``` pascal
r: = TRegExpr.Create. 
```

## Les expressions régulières avec (? = ...) ne fonctionnent pas

Look ahead is not implemented in the TRegExpr. But in many cases you can
easily [replace it with simple
subexpressions](regular_expressions.html#lookahead).

## Est-ce qu&#39;il prend en charge Unicode?

**Réponse**

[Comment utiliser Unicode](tregexpr.html#unicode)

## Pourquoi TRegExpr renvoie-t-il plus d&#39;une ligne?

For example, r.e. `<font .\*>` returns the first `<font`, then the rest
of the file including last `</html>`.

**Réponse**

For backward compatibility, [modifier
/s](regular_expressions.html#modifier_s) is `On` by default.

Switch it Off and `.` will match any but [Line
separators](regular_expressions.html#syntax_line_separators) - exactly
as you wish.

BTW, je suggère &quot;\` \<font (\[^n\>\] \*)&gt; &quot;, dans&quot;
Match \[1\] &quot;sera l&#39;URL.\</font\>

## Pourquoi TRegExpr renvoie-t-il plus que ce à quoi je m&#39;attendais?

For example r.e. `<p>(.+)</p>` applyed to string `<p>a</p><p>b</p>`
returns `a</p><p>b` but not `a` as I expected.

**Réponse**

By default all operators works in `greedy` mode, so they match as more
as it possible.

If you want `non-greedy` mode you can use `non-greedy` operators like
`+?` and so on or switch all operators into `non-greedy` mode with help
of modifier `g` (use appropriate TRegExpr properties or operator `?(-g)`
in r.e.).

## Comment analyser des sources comme HTML avec l&#39;aide de TRegExpr?

**Réponse**

Désolé les gars, mais c&#39;est presque impossible!

Of course, you can easily use TRegExpr for extracting some information
from HTML, as shown in my examples, but if you want accurate parsing you
have to use real parser, not r.e.

You can read full explanation in Tom Christiansen and Nathan Torkington
`Perl Cookbook`, for example.

In short - there are many structures that can be easy parsed by real
parser but cannot at all by r.e., and real parser is much faster to do
the parsing, because r.e. doesn't simply scan input stream, it performs
optimization search that can take a lot of time.

## Existe-t-il un moyen d&#39;obtenir plusieurs correspondances d&#39;un modèle sur TRegExpr?

**Réponse**

Vous pouvez itérer des correspondances avec la méthode ExecNext.

If you want some example, please take a look at `TRegExpr.Replace`
method implementation or at the examples for
[HyperLinksDecorator](demos.html)

## Je vérifie les entrées de l&#39;utilisateur. Pourquoi TRegExpr renvoie-t-il `True` pour les chaînes d&#39;entrée incorrectes?

**Réponse**

In many cases TRegExpr users forget that regular expression is for
**search** in input string.

So, for example if you use `\d{4,4}` expression, you will get success
for wrong user inputs like `12345` or `any letters 1234`.

You have to check from line start to line end to ensure there are no
anything else around: `^\d{4,4}$`.

<a name="nongreedyoptimization"></a>

## Pourquoi les itérateurs non-gourmands fonctionnent-ils parfois comme en mode gourmand?

For example, the r.e. `a+?,b+?` applied to string `aaa,bbb` matches
`aaa,b`, but should it not match `a,b` because of non-greediness of
first iterator?

**Réponse**

This is because of TRegExpr way to work. In fact many others r.e.
engines work exactly the same: they performe only `simple` search
optimization, and do not try to do the best optimization.

In some cases it's bad, but in common it's rather advantage then
limitation, because of performance and predictability reasons.

The main rule - r.e. first of all try to match from current place and
only if that's completely impossible move forward by one char and try
again from next position in the text.

So, if you use `a,b+?` it'll match `a,b`. In case of `a+?,b+?` it's now
not recommended (we add non-greedy modifyer) but still possible to match
more then one `a`, so TRegExpr will do it.

TRegExpr like Perl's or Unix's r.e. doesn't attempt to move forward and
check - would it will be "better" match. Fisrt of all, just because
there is no way to say it's more or less good match.

## Comment utiliser TRegExpr avec Borland C ++ Builder?

J&#39;ai un problème car aucun fichier d&#39;en-tête (`.h` ou `.hpp`)
n&#39;est disponible.

**Réponse**

- Ajoutez `RegExpr.pas` au projet `bcb`.
- Compiler le projet. Cela génère le fichier d’en-tête `RegExpr.hpp`.
- Vous pouvez maintenant écrire du code utilisant l’unité `RegExpr`.
- N&#39;oubliez pas d&#39;ajouter «#include« RegExpr.hpp »\` \`si
  nécessaire.
- Don't forget to replace all `\` in regular expressions with `\\` or
  redefined [EscChar](tregexpr.html#escchar) const.

## Pourquoi beaucoup de solutions (y compris de l&#39;aide et de la démonstration TRegExpr) fonctionnent-elles mal dans Borland C ++ Builder?

**Réponse**

The hint is in the previous question ;) Symbol `\` has special meaning
in `C++`, so you have to `escape` it (as described in previous answer).
But if you don't like r.e. like `\\w+\\\\w+\\.\\w+` you can redefine the
constant `EscChar` (in `RegExpr.pas`). For example `EscChar = "/"`. Then
you can write `/w+/w+/./w+`, looks unusual but more readable.
