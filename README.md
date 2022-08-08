# SimBEL

## Résumé

SimBEL est un modèle de simulation Monte-Carlo s'appuyant sur une projection d'un canton (actif et passif) permettant l'évaluation des provisions best estimate d'un contrat d'épargne et retraite francais en euros. Plusieur chocs de la formule standard peuvent etre effectués.

## Installation

1.	Avoir au moins la version 4.2.0 de [R](https://www.r-project.org/).
2.	Installer la library devtools.
3.	Exécuter les commandes suivantes :

``` r
dev_mode(on = TRUE)

install_github("qguibert/SimBEL")

dev_mode(on = FALSE)

library(SimBEL)
```

## Environnement de travail

L'environnement contenant les formats de données en entrée et les templates en sortie est disponible dans ce [dépôt]( https://github.com/primact/Environnement).
