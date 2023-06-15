#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "RetraiteEuroRest",
    definition = function(.Object, mp, tab, tab_proba) {
        if (!missing(mp) & !missing(tab) & !missing(tab_proba)) {
            .Object@mp <- data.frame(
                num_mp = mp[, "num_mp"],
                num_canton = mp[, "num_canton"],
                num_prod = mp[, "num_prod"],
                age = mp[, "age"],
                gen = mp[, "gen"],
                num_tab_mort = mp[, "num_tab_mort"],
                pm = mp[, "pm"],
                nb_contr = mp[, "nb_contr"],
                statut_rvs = mp[, "statut_rvs"],
                age_rvs = mp[, "age_rvs"],
                gen_rvs = mp[, "gen_rvs"],
                num_tab_mort_rvs = mp[, "num_tab_mort_rvs"],
                tx_rvs = mp[, "tx_rvs"],
                tx_tech = mp[, "tx_tech"],
                tx_cible = mp[, "tx_cible"],
                freq_rente = mp[, "freq_rente"],
                rente = mp[, "rente"],
                rente_gar = mp[, "rente"],
                ch_arr = mp[, "ch_arr"],
                echu = mp[, "echu"],
                tx_cible_prec = mp[, "tx_cible_prec"],
                diff = mp[, "diff"]
            )
            .Object@tab <- tab
            .Object@tab_proba <- tab_proba
            validObject(.Object)
        } else {
            # Traitement du cas vide
            .Object@tab <- new("TabRetEuroRest")
            .Object@tab_proba <- new("TabProbaRetEuroRest")
            .Object@mp <- data.frame(
                num_mp = integer(),
                num_canton = integer(),
                num_prod = integer(),
                age = integer(),
                gen = integer(),
                num_tab_mort = factor(),
                pm = numeric(),
                nb_contr = numeric(),
                statut_rvs = integer(),
                age_rvs = integer(),
                gen_rvs = integer(),
                num_tab_mort_rvs = factor(),
                tx_rvs = numeric(),
                tx_tech = numeric(),
                tx_cible = factor(),
                freq_rente = numeric(),
                rente = numeric(),
                rente_gar = numeric(),
                ch_arr = numeric(),
                echu = logical(),
                tx_cible_prec = numeric(),
                diff = integer()
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
    signature = "RetraiteEuroRest",
    definition = function(x, i) {
        switch(EXPR = i,
            "mp" = {
                return(x@mp)
            },
            "tab" = {
                return(x@tab)
            },
            "tab_proba" = {
                return(x@tab_proba)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "RetraiteEuroRest",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "mp" = {
                x@mp <- value
            },
            "tab" = {
                x@tab <- value
            },
            "tab_proba" = {
                x@tab_proba <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        return(x)
    }
)
