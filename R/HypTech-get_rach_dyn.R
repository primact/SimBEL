#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_rach_dyn : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la composante rachats dynamique
##'
##' \code{get_rach_dyn} est une methode permettant de calculer la composante rachats dynamique.
##' @name get_rach_dyn
##' @docType methods
##' @param x un objet de la classe \code{HypTech}.
##' @param nom_table : nom de la table de parametres de rachats conjoncturels consideree
##' @param tx_cible : taux cible en numerique
##' @param tx_serv : taux servi en numerique
##' @return la composante rachats dynamique en numerique
##' @author Prim'Act
##' @export
##' @aliases HypTech


setGeneric("get_rach_dyn", function(x, nom_table,tx_cible,tx_serv){standardGeneric("get_rach_dyn")})
setMethod(
  f = "get_rach_dyn",
  signature = c(x = "HypTech",  nom_table = "character", tx_cible = "numeric", tx_serv = "numeric"),
  def = function(x, nom_table, tx_cible, tx_serv){
    
    # Ajout d un test de presence du nom
    if (! nom_table %in% names(x@param_rach_dyn)){"tables de parametres de rachats dynamique non trouve"}
    else{return(calc_rach_dyn(x@param_rach_dyn[[nom_table]],tx_cible,tx_serv))}
  }
)

    
  







	


