# file_autres_reserves_address <- paste(x@address[["data"]][["passif"]], "autres_reserves.csv", sep = "/")
setGeneric(name = "autres_reserves_load", def = function(file_autres_reserves_address){standardGeneric("autres_reserves_load")})
setMethod(
    f = "autres_reserves_load",
    signature = "character",
    definition = function(file_autres_reserves_address){
        temp            <- read.csv2(file_autres_reserves_address)
        autres_reserves <- new("AutresReserves",
                               pgg_debut      = temp[,"pgg_debut"],
                               psap_debut     = temp[,"psap_debut"],
                               pgg_valeur     = temp[,"pgg_valeur"],
                               psap_valeur    = temp[,"psap_valeur"],
                               tx_pgg_ep      = temp[,"tx_pgg_ep"],
                               tx_pgg_autres  = temp[,"tx_pgg_autres"],
                               tx_psap_ep     = temp[,"tx_psap_ep"],
                               tx_psap_autres = temp[,"tx_psap_autres"])
        return(autres_reserves)
    }
)