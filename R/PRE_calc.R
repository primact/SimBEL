
setGeneric(name = "calc_PRE", def = function(x, pmvl_action_immo){standardGeneric("calc_PRE")})
setMethod(
    f = "calc_PRE",
    signature = c(x = "PRE", pmvl_action_immo = "numeric"),
    definition = function(x, pmvl_action_immo){
        # Verification des inputs
        if(length(pmvl_action_immo) != 1) {stop("[PRE : calc_PRE] : Les inputs ne sont pas de la bonne dimension. \n")}
        # Calcul de la PRE
        if(pmvl_action_immo  <= 0){
            # Les actifs actions et immos (hors oblig) sont en situations de moins values latentes sur l'exercice par rapport au stock initial
            # La PRE est augmentee de la moins value latentes hors produits obligataires

            # Variation de PRE
            var_pre <- min(x@val_debut + 1 / x@ryth_dot * max(- pmvl_action_immo, 0),
                           max(- pmvl_action_immo, 0)) - x@val_debut

            # Valeur courante
            val_courante <- x@val_debut + var_pre
        }else{ # Reprise de la PRE
            val_courante <- 0
            var_pre <- val_courante - x@val_debut
        }
        return (list(pre_courante = val_courante, var_pre = var_pre))
    }
)
