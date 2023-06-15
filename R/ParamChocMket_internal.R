#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "ParamChocMket",
    definition = function(x, i) {
        switch(EXPR = i,
            "table_choc_action_type1" = {
                return(x@table_choc_action_type1)
            },
            "table_choc_action_type2" = {
                return(x@table_choc_action_type2)
            },
            "table_choc_immo" = {
                return(x@table_choc_immo)
            },
            "table_choc_spread" = {
                return(x@table_choc_spread)
            },
            "table_choc_currency" = {
                return(x@table_choc_currency)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)


# Setteur
setReplaceMethod(
    f = "[",
    signature = "ParamChocMket",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "table_choc_action_type1" = {
                x@table_choc_action_type1 <- value
            },
            "table_choc_action_type2" = {
                x@table_choc_action_type2 <- value
            },
            "table_choc_immo" = {
                x@table_choc_immo <- value
            },
            "table_choc_spread" = {
                x@table_choc_spread <- value
            },
            "table_choc_currency" = {
                x@table_choc_currency <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
