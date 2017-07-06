#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Getteur
setMethod(
    f = "[",
    signature = "Be",
    definition = function(x,i){
        switch(EXPR = i,
               "param_be" = {return(x@param_be)},
               "canton" = {return(x@canton)},
               "esg" = {return(x@esg)},
               "tab_flux" = {return(x@tab_flux)},
               "tab_be" = {return(x@tab_be)},
               stop("[Be] : Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "Be",
    definition = function(x,i,value){
        switch(EXPR = i,
               "param_be" = {x@param_be <- value},
               "canton" = {x@canton <- value},
               "esg" = {x@esg <- value},
               "tab_flux" = {x@tab_flux <- value},
               "tab_be" = {x@tab_be <- value},
               stop("[Be] : Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
