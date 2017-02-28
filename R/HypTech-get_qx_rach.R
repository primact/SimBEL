#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_qx_rach : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de rachat total ou partiel
##'
##' \code{get_qx_rach} est une methode permettant de calculer le taux de rachat total ou partiel.
##' @name get_qx_rach
##' @docType methods
##' @param x un objet de la classe \code{HypTech}.
##' @param nom_table : nom de la table de rachat consideree
##' @param age : age en numerique
##' @param anc : anciennete en numerique
##' @return le taux de rachat total ou partiel
##' @author Prim'Act
##' @export
##' @aliases HypTech



setGeneric("get_qx_rach", function(x, nom_table, age, anc){standardGeneric("get_qx_rach")})
setMethod(
  f = "get_qx_rach",
  signature = c(x = "HypTech",  nom_table = "character", age = "numeric", anc = "numeric"),
  def = function(x, nom_table, age, anc){
    
    # Ajout d un test de presence du nom
    if (! nom_table %in% names(x@tables_rach)){"Nom de table de rachat non trouve"}
      else {return(calc_rach(x@tables_rach[[nom_table]],age,anc))}
  }
)
