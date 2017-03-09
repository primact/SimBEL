setGeneric(name = "autres_passif_load", def = function(file_autres_passif_address){standardGeneric("autres_passif_load")})
setMethod(
    f = "autres_passif_load",
    signature = "character",
    definition = function(file_autres_passif_address){
        temp           <- read.csv2(file_autres_passif_address)
        autres_passifs <- new("AutresPassifs", mp = temp)
        return(autres_passifs)
    }
)