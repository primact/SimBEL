#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "ModelPointESG",
    definition = function(x, i) {
        switch(EXPR = i,
            "annee" = {
                return(x@annee)
            },
            "num_traj" = {
                return(x@num_traj)
            },
            "indice_action" = {
                return(x@indice_action)
            },
            "indice_immo" = {
                return(x@indice_immo)
            },
            "indice_inflation" = {
                return(x@indice_inflation)
            },
            "yield_curve" = {
                return(x@yield_curve)
            },
            "deflateur" = {
                return(x@deflateur)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "ModelPointESG",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "annee" = {
                x@annee <- value
            },
            "num_traj" = {
                x@num_traj <- value
            },
            "indice_action" = {
                x@indice_action <- value
            },
            "indice_immo" = {
                x@indice_immo <- value
            },
            "indice_inflation" = {
                x@indice_inflation <- value
            },
            "yield_curve" = {
                x@yield_curve <- value
            },
            "deflateur" = {
                x@deflateur <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
