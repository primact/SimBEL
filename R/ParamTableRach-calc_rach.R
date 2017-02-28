

#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_rach : Methode de la classe ParamTableRach
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de rachat total ou partiel
##'
##' \code{calc_rach} est une methode permettant de calculer le taux de rachat total ou partiel.
##' @name calc_rach
##' @docType methods
##' @param table_rach un objet de la classe \code{ParamTableRach} contenant les tables de rachat.
##' @param age : en numerique
##' @param anc : anciennete en numerique
##' @return le taux de rachat total ou partiel
##' @author Prim'Act
##' @export
##' @aliases ParamTableRach
##' 
##' 
setGeneric("calc_rach",
           function(table_rach, age, anc){standardGeneric("calc_rach")})

setMethod(
  f = "calc_rach",
  signature = c(table_rach = "ParamTableRach", age = "numeric", anc = "numeric"),
  def = function(table_rach, age, anc){
    # Ajout de test sur le format
    if(age < table_rach@age_min){
      stop("L age doit etre superieur a l age minimum de la table")
    }
    if(anc < table_rach@anc_min){
      stop("L anciennete doit etre superieure a l anciennete minimum de la table")
    }
    
    # Gestion des ages superieur a l age maximum de la table
    age_app <- min(age, table_rach@age_max)
    
    # Test si l age est bien present dans la table
    if (!(age_app %in%  table_rach@table$age)){
      stop("L age doit etre present dans la table")
    }
    
    # On calcule l'anciennete max pour l'age demande
    anc_max <- max(table_rach@table[table_rach@table["age"] == age, "anc"], 0)
    #on calcule l'anciennete appliquee
    anc_app <- min(anc, anc_max)
    
    # Test si l anciennete est bien presente dans la table
    if (!(anc_app %in%  table_rach@table$anc)){
      stop("L anciennete doit etre presente dans la table")
    }
    
    res1 <- table_rach@table[table_rach@table["anc"] == anc_app & table_rach@table["age"] == age_app, "taux_rachat"]
    return(res1)
  }
)







	


