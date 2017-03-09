# Script permettant de charger la valeur initiale des param revalo dans un objet de type ParamRevaloEngine

setGeneric(name = "param_revalo_load", def = function(file_revalo_address){standardGeneric("param_revalo_load")})
setMethod(
    f = "param_revalo_load",
    signature = "character",
    definition = function(file_revalo_address){
        temp <- read.csv2(file_revalo_address)
        param_revalo  <- new("ParamRevaloEngine",
                             taux_pb_fi   = temp[,"taux_pb_fi"],
                             taux_pb_tech = temp[,"taux_pb_tech"],
                             tx_marge_min = temp[,"tx_marge_min"])
        return(param_revalo)
    }
)