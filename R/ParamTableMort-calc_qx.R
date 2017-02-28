

#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_qx : Methode de la classe ParamTableMort
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de mortalite
##'
##' \code{calc_qx} est une methode permettant de calculer le taux de mortalite.
##' @name calc_qx
##' @docType methods
##' @param table_mort un objet de la classe \code{ParamTableMort} contenant les tables de mortalite.
##' @param age : en numerique
##' @param gen : generation en numerique
##' @return le taux de mortalite
##' @author Prim'Act
##' @export
##' @aliases ParamTableMort
##' 
##' 
setGeneric("calc_qx",function(table_mort,age, gen){standardGeneric("calc_qx")})
setMethod(
  f = "calc_qx",
  signature = c(table_mort="ParamTableMort",age="numeric",gen="numeric"),
  def = function(table_mort,age,gen){
    
    # Ajout de test sur le format
    if(age < table_mort@age_min){
      stop("L age doit etre superieur a l age minimum de la table")
    }
    # Age applique
    age_app <- min(age,table_mort@age_max-1)
    
    # Generation applique
    gen_app <- max(min(gen,table_mort@gen_max),table_mort@gen_min)
    
    #Calcul des probabilites de deces
    res1 <- table_mort@table[table_mort@table["gen"]==gen_app & table_mort@table["age"]==age_app,3]
    res2 <- table_mort@table[table_mort@table["gen"]==gen_app & table_mort@table["age"]==(age_app+1),3]
    
    return((res1-res2)/res1)
  }
)





	


