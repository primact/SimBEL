#----------------------------------------------------------------------------------------------------------------------------------------------------
# load_ht : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' permet de charger et d'instancier la classe HypTech
##'
##' \code{load_ht} est une methode permettant charger la class HypTech
##' @name load_ht
##' @docType methods
##' @return 
##' @author Prim'Act
##' @export
##' @aliases HypTech


setGeneric(name = "load_ht", def = function(x){standardGeneric("load_ht")})
setMethod(
  f = "load_ht",
  signature = "Initialisation",
  def = function(x){
    input_morta <- read.csv2(paste(x@address$data_passif_chgmt,"empl_tables_morta.csv",sep="/"),header = TRUE)
    input_rach <- read.csv2(paste(x@address$data_passif_chgmt,"empl_tables_rachats.csv",sep="/"),header = TRUE)
    input_param_rach <- read.csv2(paste(x@address$data_passif_chgmt,"empl_param_rachats_conj.csv",sep="/"),header = TRUE)

    list_morta <-list()
    list_rach <-list()
    list_param_rach <-list()
    
    # Chargement et instanciation des tables de moratlite
    for (i in 1:nrow(input_morta["nom_table_R"]))
    {
      temp_csv <- read.csv2(paste(x@address$data_passif,input_morta[i,"nom_table_csv"],sep="/"),header = TRUE)
      list_morta[[i]] <- new(Class ="ParamTableMort",temp_csv)
      
    }
    names(list_morta) <- input_morta[,"nom_table_R"]
    
    
    # Chargement et instanciation des tables de rachat
    for (i in 1:nrow(input_rach["nom_table_R"]))
    {
      temp_csv <- read.csv2(paste(x@address$data_passif,input_rach[i,"nom_table_csv"],sep="/"),header = TRUE)
      list_rach[[i]] <- new(Class ="ParamTableRach",temp_csv)
      
    }
    names(list_rach) <- input_rach[,"nom_table_R"]
    
    
    # Chargement et instanciation des tables de parametres de rachats dynamique
    for (i in 1:nrow(input_param_rach["nom_table_R"]))
    {
      temp_csv <- read.csv2(paste(x@address$data_passif,input_param_rach[i,"nom_table_csv"],sep="/"),header = TRUE)
      list_param_rach[[i]] <- new(Class ="ParamRachDyn",temp_csv)
      
    }
    names(list_param_rach) <- input_param_rach[,"nom_table_R"]
    
    # Chargement et instanciation des parametres de comportement
    param_comport_csv <- read.csv2(paste(x@address$data_passif,"param_comport.csv",sep="/"),header = TRUE)
    nom_param_comport <- as.character(param_comport_csv[,"nom_param_comport"])
    mat_oblig <- as.numeric(param_comport_csv["mat_oblig"])
    alloc_mar <- as.numeric((c(param_comport_csv["alloc_mar_action"], param_comport_csv["alloc_mar_immo"], 
                               param_comport_csv["alloc_mar_tres"], param_comport_csv["alloc_mar_oblig"])))
    w_n <- as.numeric(param_comport_csv ["w_n"])
    marge_mar <- as.numeric(param_comport_csv ["marge_mar"])
    ch_enc_mar <- as.numeric(param_comport_csv ["ch_enc_mar"])
    ind_ref_action <- as.numeric(param_comport_csv ["ind_ref_action"])
    ind_ref_immo <- as.numeric(param_comport_csv ["ind_ref_immo"])
    # instanciation de la classe ParamComport
    param_comport <- new(Class = "ParamComport",mat_oblig , alloc_mar , w_n , marge_mar, ch_enc_mar, ind_ref_action , ind_ref_immo )
    list_param_comport = list(param_comport)
    names(list_param_comport) <- nom_param_comport
    # instanciation de la classe HypTech
    return(new(Class="HypTech",list_morta,list_rach,list_param_rach,list_param_comport))
  }
)

    
  







