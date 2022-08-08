# SimBEL 3.3.1

-   Ajout des tests unitaires sur Github.
-   Correction d'un bug mineur sur le choc longevite.

# SimBEL 3.3.0

Integration du calcul d'une fonction 'calc_bscr' permettant de calculer le BSCR.

# SimBEL 3.2.3

Il s'agit d'une mise a jour permettant d'executer 'SimBEL' avec la version R 4.2.0 et supérieures.

# SimBEL 3.2.2

Il s'agit d'une mise a jour permettant d'executer 'SimBEL' avec la version R 4.0.0 et supérieures.

# SimBEL 3.2.1

Il s'agit d'une correction mineure sur le calcul du choc rachat massif.

# SimBEL 3.2.0

-   Correction mineure sur le calcul de la valeur moyenne de l'actif.
-   Ajout des chocs de devise.
-   Ajout du choc rachat massif.

# SimBEL 3.1.0

Il s'agit d'une mise à jour permettant de prendre en compte la PPB initiale dans le calcul de la FDB.

# SimBEL 3.0.1

-   Correction mineure du choc inflation (erreur methodologique)
-   Correction de l'import du choc inflation qui ne fonctionnait pas avec la version 3.5.0 de R.

# SimBEL 3.0.0

Il s'agit de l'ajout de nouvelles fonctionnalités de reporting en passant par un stockage via une base de données SQLite.

## Fonctionnalités

-   Ajout d'une base de données SQLLite.
-   Chargements de variables du compte de résultat dans cette base
-   Création d'un template Excel connecté à la base SQLLite pour la visualisation les résultats.
-   Décomposition des fonctions de chargement de donnees.

# SimBEL 2.5.1

Il s'agit de corrections mineures dues à des regressions dans le code.

# SimBEL 2.5.0

Il s'agit d'une réécrite importante du package pour permettre une optimisation des temps de calculs.

## Fonctionnalités

-   Stockage des tables de probabilites lors du premier run
-   Optimisation Rccp des boucles
-   Boucle sur les tables differentes plutot que sur les model points
-   Gestion optimisés des appels de data.frame
-   Optimisation mineures.

# SimBEL 2.0.0

Il s'agit de l'ajout d'un nouveau type de produit avec la prise en compte des rentes en phase de restitution.

# SimBEL 1.2.0

Il s'agit de l'ajout de la règle des 8 ans pour la gestion de la PPB

# SimBEL 1.1.5

Il s'agit de la correction de divers bugs : \* Re-ajustement erreurs variation VNC. \* Correction d'erreurs sur les chocs d'inflation et de spread

# SimBEL 1.1.0

Il s'agit de la correction et améliorations mineures : \* mise en forme du code. \* documentation. \* ajustement erreurs variation VNC.

# SimBEL 1.0.0

Initialisation du projet.
