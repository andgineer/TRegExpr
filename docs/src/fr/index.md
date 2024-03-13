<table>
  <tr>
    <td><a href="https://regex.sorokin.engineer/">English</a></td>
    <td><a href="https://regex.sorokin.engineer/ru/">Русский</a></td>
    <td><a href="https://regex.sorokin.engineer/de/">Deutsch</a></td>
    <td><a href="https://regex.sorokin.engineer/bg/">Български</a></td>
    <td>Français</td>
    <td><a href="https://regex.sorokin.engineer/es/">Español</a></td>
  </tr>
</table>

# Introduction

La bibliothèque TRegExpr implémente [les expressions régulières](regular_expressions.md).

Les expressions régulières sont faciles à utiliser et constituent un outil puissant pour 
la recherche et la substitution sophistiquées, ainsi que pour la vérification de texte 
basée sur des modèles.

Elles sont particulièrement utiles pour la validation des saisies utilisateur dans les 
formulaires - pour valider les adresses e-mail, etc.

Vous pouvez également extraire des numéros de téléphone, des codes postaux, etc., 
à partir de pages web ou de documents, rechercher des motifs complexes dans des fichiers 
journaux et tout ce que vous pouvez imaginer. Les règles (modèles) peuvent être changées 
sans avoir à recompiler votre programme.

TRegExpr est implémenté en Pascal pur. 
Il est inclus dans le projet [Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr): 
[package](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). 
Mais il existe également en tant que bibliothèque séparée et peut être compilé avec 
Delphi 2-7, Borland C++ Builder 3-6.

[Comment bien la bibliothèque a été
rencontré](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Démarrage rapide

To use the library just add [the
sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.md).

Dans la [FAQ](faq.md) vous pouvez apprendre des problèmes des autres
utilisateurs.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

If you see any problems, please [create the bug](https://github.com/andgineer/TRegExpr/issues).

# Traductions

La documentation a été traduite en
[anglais](https://regex.sorokin.engineer/) et en
[russe](https://regexpr.sorokin.engineer/ru/).

Il existe des traductions incomplètes dans plusieurs autres langues. 
Si vous souhaitez aider à les compléter,
[contactez-moi](https://github.com/andgineer).

# Reconnaissance

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- [Alexey Torgashin](https://github.com/Alexey-T) - main contributor since 2019, e.g. 
- named groups, non-capturing groups, assertions, backward search and much more
- Guido Muehlwitz - bogue trouvé et corrigé dans le traitement d&#39;une
  grosse chaîne
- Stephan Klimek - testing in C++Builder and suggesting/implementing
  many features
- Steve Mudford - Paramètre de décalage implémenté
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) -German
  translation, usefull suggestions
- Yury Finkel - implemented Unicode support, found and fixed some bugs
- Ralf Junker - implemented some features, many optimization suggestions
- Simeon Lilov - traduction en bulgare
- Filip Jirsбk and Matthew Winter - help in implementation non-greedy
  mode
- Kit Eason - many examples for introduction help section
- Juergen Schroth - chasse aux insectes et suggestions utiles
- Martin Ledoux - traduction française
- Diego Calp, Argentine - traduction espagnole
