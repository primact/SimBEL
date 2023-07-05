#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "Oblig",
    definition = function(x, i) {
        switch(EXPR = i,
            # Data frame Financier
            "ptf_oblig" = {
                return(x@ptf_oblig)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "Oblig",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "ptf_oblig" = {
                x@ptf_oblig <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
