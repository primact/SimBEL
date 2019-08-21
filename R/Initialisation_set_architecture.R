##' Definition de l'architecture d'un workspace.
##'
##' \code{set_architecture}.
##' @name set_architecture
##' @docType methods
##' @param x un objet de la classe \code{\link{Initialisation}}.
##' @return Objet mis a jour de l'ensemble des chemins du workspace,
##' ceux ci sont stockes sous forme de liste dans l'attribut \code{address}
##' de l'objet \code{\link{Initialisation}} renseigne en input.
##' @author Prim'Act
##' @export
##' @include Initialisation_class.R

setGeneric(name = "set_architecture", def = function(x){standardGeneric("set_architecture")})
setMethod(
    f = "set_architecture",
    signature = "Initialisation",
    definition = function(x){
        # Adresses sauvegardes
        x@address[["save_folder"]] <- list()
            # Photo initiale
            x@address[["save_folder"]][["init"]]        <- paste(x@root_address, "internal_ws/data/init", sep="/")
            # Scenario central
            x@address[["save_folder"]][["central"]]     <- paste(x@root_address, "internal_ws/data/scenario/central", sep="/")
            # Choc marche
            x@address[["save_folder"]][["action_type1"]]<- paste(x@root_address, "internal_ws/data/scenario/action_type1", sep="/")
            x@address[["save_folder"]][["action_type2"]]<- paste(x@root_address, "internal_ws/data/scenario/action_type2", sep="/")
            x@address[["save_folder"]][["taux_up"]]     <- paste(x@root_address, "internal_ws/data/scenario/taux_up", sep="/")
            x@address[["save_folder"]][["taux_down"]]   <- paste(x@root_address, "internal_ws/data/scenario/taux_down", sep="/")
            x@address[["save_folder"]][["immo"]]        <- paste(x@root_address, "internal_ws/data/scenario/immobilier", sep="/")
            x@address[["save_folder"]][["spread"]]      <- paste(x@root_address, "internal_ws/data/scenario/spread", sep="/")
            x@address[["save_folder"]][["currency_up"]]      <- paste(x@root_address, "internal_ws/data/scenario/currency_up", sep="/")
            x@address[["save_folder"]][["currency_down"]]      <- paste(x@root_address, "internal_ws/data/scenario/currency_down", sep="/")
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
            x@address[["data"]][["passif"]]          <- paste(x@root_address, "input/donnees/passif", sep="/")
            x@address[["data"]][["autres_passifs_choc"]] <- paste(x@root_address, "input/donnees/passif/autres_passifs_choc", sep="/")

        # Adresses parametres
        x@address[["param"]] <- list()
            # Chargement des ESG
            # Sans VA
            x@address[["param"]][["ESG"]]                 <- paste(x@root_address, "input/parametres/esg/ESG", sep="/")
            x@address[["param"]][["ESG_up"]]              <- paste(x@root_address, "input/parametres/esg/ESG_up", sep="/")
            x@address[["param"]][["ESG_down"]]            <- paste(x@root_address, "input/parametres/esg/ESG_down", sep="/")
            # Autres param
            x@address[["param"]][["alm"]]                 <- paste(x@root_address, "input/parametres/alm", sep="/")
            x@address[["param"]][["base"]]                <- paste(x@root_address, "input/parametres/base", sep="/")
            x@address[["param"]][["chocs"]]               <- paste(x@root_address, "input/parametres/chocs", sep="/")
            x@address[["param"]][["hyp_canton"]]          <- paste(x@root_address, "input/parametres/hyp_canton", sep="/")
            x@address[["param"]][["lancement"]]           <- paste(x@root_address, "input/parametres/lancement", sep="/")
            x@address[["param"]][["ppb"]]                 <- paste(x@root_address, "input/parametres/ppb", sep="/")
            x@address[["param"]][["revalo"]]              <- paste(x@root_address, "input/parametres/revalo", sep="/")
            x@address[["param"]][["tables"]]              <- paste(x@root_address, "input/parametres/tables", sep="/")

        # Chargement des valeurs nb_simu et nb_annee_proj de l'objet Initialisation
        x <- initialisation_load(x, paste(x@address[["param"]][["lancement"]], "param_lancement.csv", sep = "/"))
       return(x)
    }
)
