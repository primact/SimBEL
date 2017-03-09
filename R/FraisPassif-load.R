setGeneric(name = "frais_passif_load", def = function(file_frais_passif_address){standardGeneric("frais_passif_load")})
setMethod(
    f = "frais_passif_load",
    signature = "character",
    definition = function(file_frais_passif_address){
        temp           <- read.csv2(file_frais_passif_address)
        frais_passifs  <- new(Class = "FraisPassif", mp = temp)
        return(frais_passifs)
    }
)