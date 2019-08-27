#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement de la classe PortPassif
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger et d'instancier un portfeuille de passif.
##'
##' \code{load_pp} est une methode permettant de charger les parametres et les donnees associees a un
##' objet de classe \code{\link{PortPassif}}.
##' @name load_pp
##' @docType methods
##' @param x est un objet de la classe \code{\link{Initialisation}} qui est utilise pour renseigner le chemin
##' d'acces de tous les parametres et les donnees necessaires.
##' @return L'objet de la classe \code{\link{PortPassif}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l'input.
##' @export
##' @include PortPassif-class.R
##'

setGeneric(name = "load_pp", def = function(x){standardGeneric("load_pp")})
setMethod(
    f = "load_pp",
    signature = c(x = "Initialisation"),
    def = function(x){

        ## 1 - Gestion des passifs epargne
        # Chargement des produits epargne
        input_epeuroind <- read.csv2(paste(x@address[["data"]][["passif"]],"empl_tables_epeuroind.csv",sep="/"),
                                     header = TRUE, colClasses = c("character", "character"))
        nb_epeuroind <- nrow(input_epeuroind)

        # Initilisation de la liste
        list_epeuroind <- list()

        if (nb_epeuroind > 0L) {

            for (i in 1L:nb_epeuroind) {

                # Chargement de l'objet
                list_epeuroind[[i]] <- load_epeuroind(paste(x@address[["data"]][["passif"]], input_epeuroind[i,"nom_table_csv"], sep="/"))
            }

            # Nommage de la liste
            names(list_epeuroind) <- input_epeuroind[,"nom_table_R"]
        }



        ## 2 - Gestion des passifs retraite
        # Chargement des produits de retraite
        input_retraiteeurorest <- read.csv2(paste(x@address[["data"]][["passif"]],"empl_tables_reteurorest.csv",sep="/"),
                                            header = TRUE, colClasses = c("character", "character"))
        nb_retraiteeurorest <- nrow(input_retraiteeurorest)

        # Initialisation de la liste
        list_retraiteeurorest <- list()

        if (nb_retraiteeurorest > 0L) {

            for (i in 1L:nb_retraiteeurorest) {

                # Chargement de l'objet
                list_retraiteeurorest[[i]] <- load_reteurorest(paste(x@address[["data"]][["passif"]], input_retraiteeurorest[i,"nom_table_csv"], sep="/"))
            }

            # Nommage de la liste
            names(list_retraiteeurorest) <- input_retraiteeurorest[,"nom_table_R"]
        }



        # Chargement  autres passifs
        autres_passifs <- autres_passif_load(paste(x@address[["data"]][["passif"]], "autres_passifs.csv", sep = "/"))


        # Chargement  autres reserves
        autres_reserves <- autres_reserves_load(paste(x@address[["data"]][["passif"]], "autres_reserves.csv", sep = "/"))

        # Chargement TauxPB
        taux_pb <- tauxpb_load(paste(x@address[["data"]][["passif"]],"taux_pb.csv",sep="/"))

        # Chargement et instanciation des tables de frais passif
        frais_passif <- frais_passif_load(paste(x@address[["data"]][["passif"]],"frais_passif.csv",sep="/"))


        # Instanciation de la classe PortPassif
        return(new(Class="PortPassif", 0L, list_epeuroind, list_retraiteeurorest, names_class_prod = c(if(nb_epeuroind > 0L) "eei", if(nb_retraiteeurorest > 0L) "rer"),
                   load_ht(x), frais_passif, taux_pb, autres_passifs, autres_reserves, TRUE, choc_lapse_mass = 0, choc_mort_cat = 0))
    }
)
