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

La bibliothèque TRegExpr implémente les [expressions régulières](regular_expressions.md).

Les expressions régulières sont un outil facile à utiliser et puissant pour la recherche sophistiquée et la substitution ainsi que pour la vérification de texte basée sur des modèles.

Elles sont particulièrement utiles pour la validation des saisies des utilisateurs dans les formulaires - pour valider les adresses e-mail, etc.

Vous pouvez également extraire des numéros de téléphone, des codes ZIP, etc. à partir de pages web ou de documents, rechercher des motifs complexes dans des fichiers journaux et tout ce que vous pouvez imaginer. Les règles (modèles) peuvent être changées sans devoir recompiler votre programme.

TRegExpr est implémentée en Pascal pur. Elle est incluse dans [Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr) : [paquet](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). Mais elle existe également comme bibliothèque séparée et peut être compilée par Delphi 2-7, Borland C++ Builder 3-6.

[Quel bon accueil la bibliothèque a-t-elle reçu](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Démarrage rapide

Pour utiliser la bibliothèque, ajoutez simplement [les sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas) à votre projet et utilisez la classe [TRegExpr](tregexpr.md).

Dans la [FAQ](faq.md), vous pouvez apprendre des problèmes des autres utilisateurs.

L'application Windows prête à l'emploi [REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip) vous aidera à apprendre et à déboguer les expressions régulières.

Si vous voyez des problèmes, veuillez [créer le bug](https://github.com/andgineer/TRegExpr/issues).

# Traductions

La documentation a été traduite en [anglais](https://regex.sorokin.engineer/) et en [russe](https://regexpr.sorokin.engineer/ru/).

Il existe des traductions incomplètes dans plusieurs autres langues. Si vous souhaitez aider à les compléter, [contactez-moi](https://github.com/andgineer).

# Gratitude

De nombreuses fonctionnalités ont été suggérées et beaucoup de bugs ont été trouvés (et même corrigés) par les contributeurs de TRegExpr.

Je ne peux pas tous les énumérer ici, mais j'apprécie tous les rapports de bug, les suggestions de fonctionnalités et les questions que je reçois de votre part.

- [Alexey Torgashin](https://github.com/Alexey-T) - principal contributeur depuis 2019, par ex. 
- groupes nommés, groupes non capturants, assertions, recherche arrière et bien plus
- Guido Muehlwitz - a trouvé et corrigé un vilain bug dans le traitement des grandes chaînes
- Stephan Klimek - tests dans C++Builder et suggestion/mise en œuvre de nombreuses fonctionnalités
- Steve Mudford - a implémenté le paramètre Offset
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) - traduction en allemand, suggestions utiles
- Yury Finkel - a implémenté le support Unicode, trouvé et corrigé certains bugs
- Ralf Junker - a implémenté certaines fonctionnalités, de nombreuses suggestions d'optimisation
- Simeon Lilov - traduction bulgare
- Filip Jirsбk et Matthew Winter - aide à l'implémentation du mode non gourmand
- Kit Eason - de nombreux exemples pour la section d'aide à l'introduction
- Juergen Schroth - chasse aux bugs et suggestions utiles
- Martin Ledoux - traduction en français
- Diego Calp, Argentine - traduction en espagnol
