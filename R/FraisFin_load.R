# Script permettant de charger la valeur initiale des frais financiers dans un objet de type Frais

setGeneric(name = "frais_fin_load", def = function(file_frais_fin_address){standardGeneric("frais_fin_load")})
setMethod(
    f = "frais_fin_load",
    signature = "character",
    definition = function(file_frais_fin_address){
        temp <- read.csv2(file_frais_fin_address)
        frais_fin <- new("FraisFin", tx_chargement = temp[,"tx_chargement"], indicatrice_inflation = temp[,"indicatrice_inflation"])
        return(frais_fin)
    }
)