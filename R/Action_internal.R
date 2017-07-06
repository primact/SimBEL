#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "Action",
    definition = function(x,i){
        switch(EXPR = i,
               # Data frame Financier
               "ptf_action" = {return(x@ptf_action)},
               stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "Action",
    definition = function(x,i,value){
        switch(EXPR = i,
               "ptf_action" = {x@ptf_action <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
