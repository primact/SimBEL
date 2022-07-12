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



## Historique des principales versions diffusées

Version 1.0.0 : Initialisation du projet.

Verison 1.1.0 : Corrections mineures (mise en forme, documentation, ajustement erreurs variation VNC).

Version 1.1.5 : Re-ajustement erreurs variation VNC. Correction d'erreurs sur les chocs d'inflation et de spread.

Verison 1.2.0 : Gestion de la regle des 8 ans.

Version 2.0.0 : Prise en compte des retraites en phase de restitution.

Version 2.5.0 : Optimisation du package : 1. Stockage des tables de probabilites, 2. Optimisation Rccp, 3. Boucle sur les tables
differentes plutot que sur les model points, 4. Gestion des appels de data.frame, 5. Optimisation mineures.

Version 2.5.1 : Corrections mineures dues a des regressions dans le code.

Version 3.0.0 : Ajout de fonctionnalites de reporting en passant par un stockage via une base de donnees SQLite. Ajout de template Excel pour la visualisation des resultats. Decomposition des fonctions de chargement de donnees. 

Version 3.0.1 : Correction du choc inflation (erreur methodologique) et import du choc inflation qui ne fonctionnait pas avec la version 3.5.0 de R. 

Version 3.1.0 : Prise en compte de l'ecoulement de la PPB initiale dans le calcul de la FDB.

Version 3.2.0 : Correction mineure sur la calcul de la valeur moyenne de l'actif. Ajout des chocs de devise et du choc rachat massif. 

Version 3.2.1 : Correction mineure sur la calcul du choc rachat massif.

Version 3.2.2 : Passage à la version de R 4.0.0.

Version 3.2.3 : Passage à la version de R 4.2.0.
