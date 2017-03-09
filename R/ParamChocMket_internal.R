#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "ParamChocMket",
    definition = function(x, i){
        switch(EXPR = i,
               "table_choc_action" = {return(x@table_choc_action)},
               "table_choc_immo"   = {return(x@table_choc_immo)},
               "table_choc_spread" = {return(x@table_choc_spread)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


# Setteur
setReplaceMethod(
    f = "[",
    signature = "ParamChocMket",
    definition = function(x, i, value){
        switch(EXPR = i,
               "table_choc_action" = {x@table_choc_action <- value},
               "table_choc_immo"   = {x@table_choc_immo <- value},
               "table_choc_spread" = {x@table_choc_spread <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
