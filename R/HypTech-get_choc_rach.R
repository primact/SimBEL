#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_choc_rach : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' donne le résulat d'un choc de rachat sur toutes les tables de rachat d'un objet HypTech
##'
##' \code{get_choc_rach}
##' @name get_choc_rach
##' @docType methods
##' @author Prim'Act
##' @export
##' @aliases HypTech


setGeneric("get_choc_rach", function(x, type_choc_rach, choc, choc_lim){standardGeneric("get_choc_rach")})
setMethod(
  f = "get_choc_rach",
  signature = c(x = "HypTech",type_choc_rach="character", choc = "numeric",choc_lim = "numeric"),
  def = function(x, type_choc_rach, choc, choc_lim){
    
    tables <- x["tables_rach"]
    nom_tables <- names(tables)
    nb_tables <- length(tables)
    
    for(i in 1: nb_tables){
      param_rach <- tables[[nom_tables[i]]]
      df_rach <- param_rach["table"]
      ancmin <- param_rach["anc_min"]
      ancmax <- param_rach["anc_max"]
      agemin <- param_rach["age_min"]
      agemax <- param_rach["age_max"]
      
      for (anc in ancmin : ancmax)
      {
        for(age in agemin : agemax){
          
          if (type_choc_rach == "up"){qx_choc <- min((calc_rach(param_rach,age,anc)*(1+choc)),choc_lim)}
          if (type_choc_rach == "down"){qx_choc <- max(calc_rach(param_rach,age,anc)*(1+choc),
                                                    calc_rach(param_rach,age,anc)+choc_lim)}
          
          df_rach[df_rach$anc == anc & df_rach$age == age,"taux_rachat"] <- qx_choc  
        }
      }
      
      x["tables_rach"][[nom_tables[i]]]["table"] <-  df_rach 
    }
    
    return (x)}
)


  







	


