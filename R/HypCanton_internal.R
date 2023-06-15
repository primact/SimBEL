# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement

setMethod(
    f = "initialize",
    signature = "HypCanton",
    definition = function(.Object,
                          tx_soc = numeric(), tx_import = numeric(), method_taux_cible = character()) {
        .Object@tx_soc <- tx_soc
        .Object@tx_import <- tx_import
        .Object@method_taux_cible <- method_taux_cible
        validObject(.Object)
        return(.Object)
    }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "HypCanton",
    definition = function(x, i) {
        switch(EXPR = i,
            "tx_soc" = {
                return(x@tx_soc)
            },
            "tx_import" = {
                return(x@tx_import)
            },
            "method_taux_cible" = {
                return(x@method_taux_cible)
            },
            stop("[HypCanton] : Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "HypCanton",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "tx_soc" = {
                x@tx_soc <- value
            },
            "tx_import" = {
                x@tx_import <- value
            },
            "method_taux_cible" = {
                x@method_taux_cible <- value
            },
            stop("[HypCanton] : Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
