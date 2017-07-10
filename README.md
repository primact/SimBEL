# SimBEL

Un modele de simulation Monte-Carlo s'appuyant sur une projection d'un canton (actif et passif) permettant l'evaluation des provisions best estimate d'un contrat d'epargne et retraite francais en euros. 

Plusieurs chocs de la formule standard peuvent etre effectues.



Version 1.0.0 : Initialisation du projet.
Verison 1.1.0 : Corrections mineures (mise en forme, documentation, ajustement erreurs variation VNC).
Version 1.1.5 : Re-ajustement erreurs variation VNC. Correction d'erreurs sur les chocs d'inflation et de spread.
Verison 1.2.0 : Gestion de la regle des 8 ans.
Version 2.0.0 : Prise en compte des retraites en phase de restitution.
Version 2.5.0 : Optimisation du package : 1. Stockage des tables de probabilites, 2. Optimisation Rccp, 3. Boucle sur les tables
differentes plutot que sur les model points, 4. Gestion des appels de data.frame, 5. Optimisation mineures.
Version 2.5.1 : Corrections mineures dues a des regressions dans le code.


``` r
## basic example code
```
