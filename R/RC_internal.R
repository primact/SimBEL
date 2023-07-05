#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "RC",
    definition = function(x, i) {
        switch(EXPR = i,
            # Data frame Financier
            "val_debut" = {
                return(x@val_debut)
            },
            "val_courante" = {
                return(x@val_courante)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "RC",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "val_debut" = {
                x@val_debut <- value
            },
            "val_courante" = {
                x@val_courante <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
