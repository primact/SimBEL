#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "Immo",
    definition = function(x,i){
        switch(EXPR = i,
               # Data frame Financier
               "ptf_immo" = {return(x@ptf_immo)},
               stop("Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "Immo",
    definition = function(x,i,value){
        switch(EXPR = i,
               "ptf_immo" = {x@ptf_immo <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
