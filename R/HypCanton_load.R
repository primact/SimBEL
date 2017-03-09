# Script permettant de charger la valeur initiale des ppb dans un objet de type Ppb

setGeneric(name = "hyp_canton_load", def = function(file_hyp_canton_address){standardGeneric("hyp_canton_load")})
setMethod(
    f = "hyp_canton_load",
    signature = "character",
    definition = function(file_hyp_canton_address){
        temp <- read.csv2(file_hyp_canton_address)
        hyp_canton  <- new("HypCanton",
                           tx_soc            = temp[,"tx_soc"],
                           tx_import         = temp[,"tx_import"],
                           method_taux_cible = as.character(temp[,"method_taux_cible"]))
        return(hyp_canton)
    }
)