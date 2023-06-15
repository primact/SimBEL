#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "EpEuroInd",
    definition = function(.Object, mp, tab, tab_proba) {
        if (!missing(mp) & !missing(tab) & !missing(tab_proba)) {
            .Object@mp <- data.frame(
                num_mp = mp[, "num_mp"],
                num_canton = mp[, "num_canton"],
                num_prod = mp[, "num_prod"],
                age = mp[, "age"],
                gen = mp[, "gen"],
                num_tab_mort = mp[, "num_tab_mort"],
                chgt_enc = mp[, "chgt_enc"],
                ind_chgt_enc_pos = mp[, "ind_chgt_enc_pos"],
                pm = mp[, "pm"],
                nb_contr = mp[, "nb_contr"],
                anc = mp[, "anc"],
                terme = mp[, "terme"],
                type_cot = mp[, "type_cot"],
                periode_cot = mp[, "periode_cot"],
                tx_cible = mp[, "tx_cible"],
                chgt_prime = mp[, "chgt_prime"],
                prime = mp[, "prime"],
                tx_tech = mp[, "tx_tech"],
                terme_tx_tech = mp[, "terme_tx_tech"],
                tmg = mp[, "tmg"],
                terme_tmg = mp[, "terme_tmg"],
                num_rach_tot = mp[, "num_rach_tot"],
                num_rach_part = mp[, "num_rach_part"],
                num_rach_dyn_tot = mp[, "num_rach_dyn_tot"],
                num_rach_dyn_part = mp[, "num_rach_dyn_part"],
                chgt_rach = mp[, "chgt_rach"],
                pm_gar = mp[, "pm"],
                tx_revalo_prec = mp[, "tx_revalo_prec"],
                tx_cible_prec = mp[, "tx_cible_prec"]
            )
            .Object@tab <- tab
            .Object@tab_proba <- tab_proba
            validObject(.Object)
        } else {
            # Traitement du cas vide
            .Object@tab <- new("TabEpEuroInd")
            .Object@tab_proba <- new("TabProbaEpEuroInd")
            .Object@mp <- data.frame(
                num_mp = integer(),
                num_canton = integer(),
                num_prod = integer(),
                age = integer(),
                gen = integer(),
                num_tab_mort = factor(),
                chgt_enc = numeric(),
                ind_chgt_enc_pos = logical(),
                pm = numeric(),
                nb_contr = numeric(),
                anc = integer(),
                terme = integer(),
                type_cot = factor(),
                periode_cot = factor(),
                tx_cible = factor(),
                chgt_prime = numeric(),
                prime = numeric(),
                tx_tech = numeric(),
                terme_tx_tech = integer(),
                tmg = numeric(),
                terme_tmg = integer(),
                num_rach_tot = factor(),
                num_rach_part = factor(),
                num_rach_dyn_tot = factor(),
                num_rach_dyn_part = factor(),
                chgt_rach = numeric(),
                pm_gar = numeric(),
                tx_revalo_prec = numeric(),
                tx_cible_prec = numeric()
            )
        }
        return(.Object)
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "EpEuroInd",
    definition = function(x, i) {
        switch(EXPR = i,
            "mp" = {
                return(x@mp)
            },
            "tab" = {
                return(x@tab)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "EpEuroInd",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "mp" = {
                x@mp <- value
            },
            "tab" = {
                x@tab <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        return(x)
    }
)
