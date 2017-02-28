#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "FraisFin",
    definition = function(x,i){
        switch(EXPR = i,
               # Data frame Financier
               "tx_chargement" = {return(x@tx_chargement)},
               "indicatrice_inflation" = {return(x@indicatrice_inflation)},
               stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "FraisFin",
    definition = function(x,i,value){
        switch(EXPR = i,
               "tx_chargement" = {x@tx_chargement <- value},
               "indicatrice_inflation" = {x@indicatrice_inflation <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
