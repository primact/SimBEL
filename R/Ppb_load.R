# Script permettant de charger la valeur initiale des ppb dans un objet de type Ppb

setGeneric(name = "ppb_load", def = function(file_ppb_address){standardGeneric("ppb_load")})
setMethod(
    f = "ppb_load",
    signature = "character",
    definition = function(file_ppb_address){
        temp <- read.csv2(file_ppb_address)
        ppb  <- new("Ppb",
                    valeur_ppb = temp[,"valeur_ppb"],
                    ppb_debut  = temp[,"ppb_debut"],
                    seuil_rep  = temp[,"seuil_rep"],
                    seuil_dot  = temp[,"seuil_dot"],
                    compte_rep = temp[,"compte_rep"],
                    compte_dot = temp[,"compte_dot"])
        return(ppb)
    }
)