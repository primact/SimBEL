#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "ParamAlmEngine",
    definition = function(x, i) {
        switch(EXPR = i,
            # Data frame Financier
            "ptf_reference" = {
                return(x@ptf_reference)
            },
            "alloc_cible" = {
                return(x@alloc_cible)
            },
            "seuil_realisation_PVL" = {
                return(x@seuil_realisation_PVL)
            },
            stop("[ParamAlmEngine] : Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "ParamAlmEngine",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "ptf_reference" = {
                x@ptf_reference <- value
            },
            "alloc_cible" = {
                x@alloc_cible <- value
            },
            "seuil_realisation_PVL" = {
                x@seuil_realisation_PVL <- value
            },
            stop("[ParamAlmEngine] : Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
