#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_choc_table : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' donne le résulat d'un choc sur toutes les tables d'un objet HypTech
##'
##' \code{get_choc_table}
##' @name get_choc_table
##' @docType methods
##' @author Prim'Act
##' @export
##' @aliases HypTech


setGeneric("get_choc_table", function(x, choc){standardGeneric("get_choc_table")})
setMethod(
  f = "get_choc_table",
  signature = c(x = "HypTech",  choc = "numeric"),
  def = function(x, choc){
    
    tables <- x["tables_mort"]
    nom_tables <- names(tables)
    nb_tables <- length(tables)
    
    for(i in 1: nb_tables){
      
      param_mort <- tables[[nom_tables[i]]]
      df_mort <- param_mort["table"]
      genmin <- param_mort["gen_min"]
      genmax <- param_mort["gen_max"]
      agemin <- param_mort["age_min"]
      agemax <- param_mort["age_max"]
      
      for (gen in genmin : genmax)
      {
        for(age in (agemin+1) : agemax){
          
          lx_anc <- df_mort[df_mort$gen == gen & df_mort$age == age-1,"lx"]
          lx_anc_central <- param_mort["table"][df_mort$gen == gen & df_mort$age == age-1,"lx"]
          if (lx_anc == 0 || lx_anc_central == 0) {
            df_mort[df_mort$gen == gen & df_mort$age == age,"lx"] <- 0
          }else{
            qx_choc <- min((calc_qx(param_mort,age-1,gen)*(1+choc)),1)
            df_mort[df_mort$gen == gen & df_mort$age == age,"lx"] <- lx_anc * (1-qx_choc)
          }  
        }
      } 
      
      x["tables_mort"][[nom_tables[i]]]["table"] <-  df_mort 
    }
    
    return (x)}
)
