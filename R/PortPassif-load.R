#----------------------------------------------------------------------------------------------------------------------------------------------------
# load_pp : Methode de la classe de PortPassif
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' permet de charger et d'instancier la classe PortPassif
##'
##' \code{load_pp} est une methode permettant charger la class PortPassif
##' @name load_pp
##' @docType methods
##' @return 
##' @author Prim'Act
##' @export
##' @aliases PortPassif


setGeneric(name = "load_pp", def = function(x){standardGeneric("load_pp")})
setMethod(
  f = "load_pp",
  signature = "Initialisation",
  def = function(x){
    
    input_epeuroind <- read.csv2(paste(x@address$data_passif_chgmt,"empl_tables_epeuroind.csv",sep="/"),header = TRUE)
    
    list_epeuroind <-list()
    
    # Chargement et instanciation des tables de taux de PB
    taux_pb_csv <- read.csv2(paste(x@address$data_passif,"taux_pb.csv",sep="/"),header = TRUE)
    taux_pb <- new(Class = "TauxPB", taux_pb_csv)
    
    # Chargement et instanciation des tables de frais passif
    frais_passif_csv <- read.csv2(paste(x@address$data_passif,"frais_passif.csv",sep="/"),header = TRUE)
    frais_passif <- new(Class = "FraisPassif", frais_passif_csv)
    
    for (i in 1:nrow(input_epeuroind["nom_table_R"]))
    {
      temp_csv <- read.csv2(paste(x@address$data_passif,input_epeuroind[i,"nom_table_csv"],sep="/"),header = TRUE)
      list_epeuroind[[i]] <- new(Class ="EpEuroInd",temp_csv,new("TabEpEuroInd"))
      
    }
    names(list_epeuroind) <- input_epeuroind[,"nom_table_R"]
  
    
    # Chargement  autres passifs
    autres_passifs_csv <- read.csv2(paste(x@address$data_passif,"autres_passifs.csv",sep="/"),header = TRUE)
    autres_passifs <- new(Class ="AutresPassifs",autres_passifs_csv)
    
    # Chargement  autres reserves
    autres_reserves <- new(Class ="AutresReserves", pgg_debut = 0, psap_debut = 0, pgg_valeur = 0, psap_valeur = 0, tx_pgg_ep = 0, tx_pgg_autres = 0, tx_psap_ep = 0, tx_psap_autres = 0)
    
    
    # instanciation de la classe PortPassif
    return(new(Class="PortPassif",an, list_epeuroind, names_class_prod=c("eei"), load_ht(x), frais_passif, taux_pb, autres_passifs, autres_reserves))
  }
)
