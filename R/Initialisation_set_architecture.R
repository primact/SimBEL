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
        # Adresse des scripts : paragraphe a mettre en commentaire lors du passage en scripts
        x@address[["scripts"]] <- list()
            x@address[["scripts"]][["actif"]]       <- paste(x@root_address, "internal_ws/script/actif", sep="/") 
            x@address[["scripts"]][["be"]]          <- paste(x@root_address, "internal_ws/script/be", sep="/") 
            x@address[["scripts"]][["canton"]]      <- paste(x@root_address, "internal_ws/script/canton", sep="/") 
            x@address[["scripts"]][["passif"]]      <- paste(x@root_address, "internal_ws/script/passif", sep="/") 
            x@address[["scripts"]][["init"]]        <- paste(x@root_address, "internal_ws/script/initialisation", sep="/") 
        
        # Adresses sauvegardes
        x@address[["save_folder"]] <- list()
            # Photo initiale
            x@address[["save_folder"]][["init"]]        <- paste(x@root_address, "internal_ws/data/init", sep="/")
            # Scenario central
            x@address[["save_folder"]][["central"]]     <- paste(x@root_address, "internal_ws/data/scenario/central", sep="/")
            # Choc marche
            x@address[["save_folder"]][["action"]]      <- paste(x@root_address, "internal_ws/data/scenario/action", sep="/")
            x@address[["save_folder"]][["taux_up"]]     <- paste(x@root_address, "internal_ws/data/scenario/taux_up", sep="/")
            x@address[["save_folder"]][["taux_down"]]   <- paste(x@root_address, "internal_ws/data/scenario/taux_down", sep="/")
            x@address[["save_folder"]][["immo"]]        <- paste(x@root_address, "internal_ws/data/scenario/immobilier", sep="/")
            x@address[["save_folder"]][["spread"]]      <- paste(x@root_address, "internal_ws/data/scenario/spread", sep="/")
            # Choc souscription
            x@address[["save_folder"]][["frais"]]       <- paste(x@root_address, "internal_ws/data/scenario/frais", sep="/")
            x@address[["save_folder"]][["mortalite"]]   <- paste(x@root_address, "internal_ws/data/scenario/mortalite", sep="/")
            x@address[["save_folder"]][["longevite"]]   <- paste(x@root_address, "internal_ws/data/scenario/longevite", sep="/")
            x@address[["save_folder"]][["rachat_up"]]   <- paste(x@root_address, "internal_ws/data/scenario/rachat_up", sep="/")
            x@address[["save_folder"]][["rachat_down"]] <- paste(x@root_address, "internal_ws/data/scenario/rachat_down", sep="/")
        
        # Adresses donnees
        x@address[["data"]] <- list()
            x@address[["data"]][["actif"]]           <- paste(x@root_address, "input/donnees/actif", sep="/")
            x@address[["data"]][["ptf_reference"]]   <- paste(x@root_address, "input/donnees/actif/Portefeuille_reference", sep="/")
            x@address[["data"]][["passif_chgmt"]]    <- paste(x@root_address, "input/donnees/location", sep="/")
            x@address[["data"]][["passif"]]          <- paste(x@root_address, "input/donnees/passif", sep="/")
        
        # Adresses parametres
        x@address[["param"]] <- list()
            # Chargement des ESG
            # Sans VA
            x@address[["param"]][["ESG"]]                 <- paste(x@root_address, "input/parametres/esg/ESG", sep="/") 
            x@address[["param"]][["ESG_up"]]              <- paste(x@root_address, "input/parametres/esg/ESG_up", sep="/") 
            x@address[["param"]][["ESG_down"]]            <- paste(x@root_address, "input/parametres/esg/ESG_down", sep="/") 
            # Avec VA
            x@address[["param"]][["ESG_VA"]]              <- paste(x@root_address, "input/parametres/esg/ESG_VA", sep="/") 
            x@address[["param"]][["ESG_up_VA"]]           <- paste(x@root_address, "input/parametres/esg/ESG_up_VA", sep="/") 
            x@address[["param"]][["ESG_down_VA"]]         <- paste(x@root_address, "input/parametres/esg/ESG_down_VA", sep="/")
            # Autres param
            x@address[["param"]][["alm"]]                 <- paste(x@root_address, "input/parametres/alm", sep="/")
            x@address[["param"]][["chocs"]]               <- paste(x@root_address, "input/parametres/chocs", sep="/")
            x@address[["param"]][["frais"]]               <- paste(x@root_address, "input/parametres/frais", sep="/")
            x@address[["param"]][["hyp_canton"]]          <- paste(x@root_address, "input/parametres/hyp_canton", sep="/")
            x@address[["param"]][["lancement"]]           <- paste(x@root_address, "input/parametres/lancement", sep="/")
            x@address[["param"]][["pb_contractuelle"]]    <- paste(x@root_address, "input/parametres/pb_contractuelle", sep="/") 
            x@address[["param"]][["ppb"]]                 <- paste(x@root_address, "input/parametres/ppb", sep="/") 
            x@address[["param"]][["ptf_reinvestissement"]]<- paste(x@root_address, "input/parametres/ptf_reinvestissement", sep="/") 
            x@address[["param"]][["revalo"]]              <- paste(x@root_address, "input/parametres/revalo", sep="/") 
            x@address[["param"]][["tables"]]              <- paste(x@root_address, "input/parametres/tables", sep="/") 
            
        # Chargement des valeurs nb_simu et nb_annee_proj de l'objet Initialisation
        x <- initialisation_load(x, paste(x@address[["param"]][["lancement"]], "param_lancement.csv", sep = "/"))
       return(x)
    }
)