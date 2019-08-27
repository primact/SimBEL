#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "ChocSolvabilite2",
    definition = function(x,i){
        switch(EXPR = i,
               "scenario"         = {return(x@scenario)},
               "param_choc_mket"  = {return(x@param_choc_mket)},
               "param_choc_sousc" = {return(x@param_choc_sousc)},
               stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "ChocSolvabilite2",
    definition = function(x,i,value){
        switch(EXPR = i,
               "scenario"         = {x@scenario <- value},
               "param_choc_mket"  = {x@param_choc_mket <- value},
               "param_choc_sousc" = {x@param_choc_sousc <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
