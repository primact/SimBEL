#file_lancement_address <- paste(x@address[["param"]][["lancement"]], "param_lancement.csv", sep = "/")
setGeneric(name = "initialisation_load", def = function(x, file_lancement_address){standardGeneric("initialisation_load")})
setMethod(
    f = "initialisation_load",
    signature = c("Initialisation","character"),
    definition = function(x, file_lancement_address){
        temp            <- read.csv2(file_lancement_address)
        # Verification des inputs
        if(temp[,"nb_simu"] != round(temp[,"nb_simu"]) | temp[,"nb_annee_trajectoire"] != round(temp[,"nb_annee_trajectoire"])) stop("[Initialisation : load] Les inputs du fichier param_lancement.csv doivent etre entiers. \n")
        x@nb_simu       <- as.integer(temp[,"nb_simu"])
        x@nb_annee_proj <- as.integer(temp[,"nb_annee_trajectoire"])
        return(x)
    }
)