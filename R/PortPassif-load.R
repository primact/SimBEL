#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement de la classe PortPassif
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger et d'instancier un portfeuille de passif.
##'
##' \code{load_pp} est une methode permettant de charger les parametres et les donnees associees a un
##' objet de classe \code{\link{PortPassifs}}.
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
  signature = "Initialisation",
  def = function(x){

    input_epeuroind <- read.csv2(paste(x@address[["data"]][["passif"]],"empl_tables_epeuroind.csv",sep="/"),header = TRUE)

    list_epeuroind <-list()

    # Chargement et instanciation des tables de taux de PB
    taux_pb_csv <- read.csv2(paste(x@address[["data"]][["passif"]],"taux_pb.csv",sep="/"),header = TRUE)
    taux_pb <- new(Class = "TauxPB", taux_pb_csv)

    # Chargement et instanciation des tables de frais passif
    frais_passif <- frais_passif_load(paste(x@address[["data"]][["passif"]],"frais_passif.csv",sep="/"))


    for (i in 1:nrow(input_epeuroind["nom_table_R"]))
    {
      temp_csv <- read.csv2(paste(x@address[["data"]][["passif"]],input_epeuroind[i,"nom_table_csv"],sep="/"),header = TRUE)
      list_epeuroind[[i]] <- new(Class ="EpEuroInd",temp_csv,new("TabEpEuroInd"))

    }
    names(list_epeuroind) <- input_epeuroind[,"nom_table_R"]
    
    input_retraiteeurorest <- read.csv2(paste(x@address[["data"]][["passif"]],"empl_tables_reteurorest.csv",sep="/"),header = TRUE)
    list_retraiteeurorest <- list()
    for (i in 1:nrow(input_retraiteeurorest["nom_table_R"]))
    {
        temp_csv <- read.csv2(paste(x@address[["data"]][["passif"]],input_retraiteeurorest[i,"nom_table_csv"],sep="/"),header = TRUE)
        list_retraiteeurorest[[i]] <- new(Class ="RetraiteEuroRest",temp_csv,new("TabRetEuroRest"))
        
    }
    names(list_retraiteeurorest) <- input_retraiteeurorest[,"nom_table_R"]
    # Chargement  autres passifs
    autres_passifs <- autres_passif_load(paste(x@address[["data"]][["passif"]], "autres_passifs.csv", sep = "/"))


    # Chargement  autres reserves
    autres_reserves <- autres_reserves_load(paste(x@address[["data"]][["passif"]], "autres_reserves.csv", sep = "/"))


    # instanciation de la classe PortPassif
    return(new(Class="PortPassif",as.integer(0), list_epeuroind, list_retraiteeurorest, names_class_prod=c("eei", "rer"), load_ht(x), frais_passif, taux_pb, autres_passifs, autres_reserves))
  }
)
