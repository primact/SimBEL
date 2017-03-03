##' Definition de l'architecture d'un workspace.
##'
##' \code{set_architecture}.
##' @name set_architecture
##' @docType methods
##' @param x un objet de la classe \code{Initialisation}.
##' @return Objet mis a jour des chemins.
##' @author Prim'Act
##' @export
##' @aliases Initialisation
setGeneric(name = "set_architecture", def = function(x){standardGeneric("set_architecture")})
setMethod(
    f = "set_architecture",
    signature = "Initialisation",
    definition = function(x){
        # x@address$scripts_actif       <- paste(x@root_address, "internal_ws/script/actif", sep="/") 
        # x@address$scripts_be          <- paste(x@root_address, "internal_ws/script/be", sep="/") 
        # x@address$scripts_canton      <- paste(x@root_address, "internal_ws/script/canton", sep="/") 
        # x@address$scripts_passif      <- paste(x@root_address, "internal_ws/script/passif", sep="/") 
        # x@address$scripts_init        <- paste(x@root_address, "internal_ws/script/initialisation", sep="/") 
        x@address$init_save_folder    <- paste(x@root_address, "internal_ws/data/init", sep="/")
        x@address$init_save_folder_central    <- paste(x@root_address, "internal_ws/data/scenario/central", sep="/")
        x@address$data_actif          <- paste(x@root_address, "input/donnees/actif", sep="/") 
        x@address$data_ESG            <- paste(x@root_address, "input/donnees/actif/ESG", sep="/") 
        x@address$data_Ptf_reference  <- paste(x@root_address, "input/donnees/actif/Portefeuille_reference", sep="/")
        x@address$data_passif_chgmt   <- paste(x@root_address, "input/donnees/location", sep="/")
        x@address$data_passif         <- paste(x@root_address, "input/donnees/passif", sep="/")
        
        return(x)
    }
)
# chemin_script_passif <- paste(chemin_env,"internal_ws/script/passif",sep="/")
# chemin_script_canton <- paste(chemin_env,"internal_ws/script/canton/R",sep="/")
# chemin_script_be     <- paste(chemin_env,"internal_ws/script/be/R",sep="/")
# chemin_script_actif  <- paste(chemin_env,"internal_ws/script/actif",sep="/")
# chemin_script_init   <- paste(chemin_env,"internal_ws/script/initialisation",sep="/")
# chemin_init          <- paste(chemin_env,"internal_ws/data",sep="/")
# chemin_inputs_passif <- paste(chemin_env,"input/donnees/passif",sep="/")
# inputs_location      <- paste(chemin_env,"input/donnees/location",sep="/")
# chemin_load          <- paste(chemin_env,"internal_ws/script/donnees pour MT",sep="/")