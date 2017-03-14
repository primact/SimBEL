# Ce script charge dans une liste l'ensemble des scenarios de choc S2 marche
# folder_chocs_address <- racine@address[["param"]][["chocs"]]

setGeneric(name = "chargement_choc", def = function(x, folder_chocs_address){standardGeneric("chargement_choc")})
setMethod(
    f = "chargement_choc",
    signature = c("ChocSolvabilite2", "character"),
    definition = function(x, folder_chocs_address){
    # Lecture des chocs Action, Immo, Spread
    table_choc_action_type1 <- read.csv2(paste(folder_chocs_address, "param_choc_mket_action_type1.csv", sep = "/"), colClasses = c("integer", "numeric"))
    table_choc_action_type2 <- read.csv2(paste(folder_chocs_address, "param_choc_mket_action_type2.csv", sep = "/"), colClasses = c("integer", "numeric"))
    table_choc_immo   <- read.csv2(paste(folder_chocs_address, "param_choc_mket_immo.csv", sep = "/"), colClasses = c("integer", "numeric"))
    table_choc_spread <- read.csv2(paste(folder_chocs_address, "param_choc_mket_spread.csv", sep = "/"), colClasses = c("integer", "character", "numeric", "numeric"))
    
    x@param_choc_mket <- new("ParamChocMket",
                             table_choc_action_type1 = table_choc_action_type1,
                             table_choc_action_type2 = table_choc_action_type2,
                             table_choc_immo         = table_choc_immo,
                             table_choc_spread       = table_choc_spread)
    x@param_choc_sousc <- new("ParamChocSousc",
                              read.csv2(paste(folder_chocs_address, "param_choc_sousc.csv", sep = "/")))
    
    return(x)    
    }
)
