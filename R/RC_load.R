# Script permettant de charger la valeur initiale de RC dans un objet de type RC

setGeneric(name = "rc_load", def = function(file_RC_address){standardGeneric("rc_load")})
setMethod(
    f = "rc_load",
    signature = "character",
    definition = function(file_RC_address){
        temp <- read.csv2(file_RC_address)
        rc <- new("RC", 
                   val_debut    = temp[,"rc_init"],
                   val_courante = temp[,"rc_init"])
        return(rc)
    }
)