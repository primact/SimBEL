#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe TabProbaEpEuroInd
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "TabProbaEpEuroInd",
    definition = function(.Object, num_mp = "numeric") {
        .Object@qx_rach_tot <- data.frame(num_mp)
        .Object@qx_rach_part <- data.frame(num_mp)
        .Object@qx_dc <- data.frame(num_mp)
        return(.Object)
    }
)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "TabProbaEpEuroInd",
    definition = function(x, i) {
        switch(EXPR = i,
            "qx_rach_tot" = {
                return(x@qx_rach_tot)
            },
            "qx_rach_part" = {
                return(x@qx_rach_part)
            },
            "qx_dc" = {
                return(x@qx_dc)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)


# Setteur
setReplaceMethod(
    f = "[",
    signature = "TabProbaEpEuroInd",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "qx_rach_tot" = {
                x@qx_rach_tot <- value
            },
            "qx_rach_part" = {
                x@qx_rach_part <- value
            },
            "qx_dc" = {
                x@qx_dc <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
