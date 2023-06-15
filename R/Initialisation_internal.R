#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "Initialisation",
    definition = function(x, i) {
        switch(EXPR = i,
            # Data frame Financier
            "root_address" = {
                return(x@root_address)
            },
            "address" = {
                return(x@address)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "Initialisation",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "root_address" = {
                x@root_address <- value
            },
            "address" = {
                x@address <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
