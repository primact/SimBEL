#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "ChocSolvabilite2",
    definition = function(x, i) {
        switch(EXPR = i,
            "scenario" = {
                return(x@scenario)
            },
            "param_choc_mket" = {
                return(x@param_choc_mket)
            },
            "param_choc_sousc" = {
                return(x@param_choc_sousc)
            },
            "matrice_choc_action" = {
                return(x@matrice_choc_action)
            },
            "matrice_choc_mket" = {
                return(x@matrice_choc_mket)
            },
            "matrice_choc_sousc" = {
                return(x@matrice_choc_sousc)
            },
            "matrice_choc_bscr" = {
                return(x@matrice_choc_bscr)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "ChocSolvabilite2",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "scenario" = {
                x@scenario <- value
            },
            "param_choc_mket" = {
                x@param_choc_mket <- value
            },
            "param_choc_sousc" = {
                x@param_choc_sousc <- value
            },
            "matrice_choc_action" = {
                x@matrice_choc_action <- value
            },
            "matrice_choc_mket" = {
                x@matrice_choc_mket <- value
            },
            "matrice_choc_sousc" = {
                x@matrice_choc_sousc <- value
            },
            "matrice_choc_bscr" = {
                x@matrice_choc_bscr <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
