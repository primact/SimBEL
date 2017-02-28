#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_comport : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux cible.
##'
##' \code{get_comport} est une methode permettant de calculer le taux cible.
##' @name get_comport
##' @docType methods
##' @param x un objet de la classe \code{HypTech}.
##' @param nom_table un nom de la table de parametres de taux cible.
##' @param list_rd une liste de taux de rendement.
##' @param tx_cible_prec un taux cible en numerique.
##' @return La valeur du taux cible.
##' @author Prim'Act
##' @export
##' @aliases HypTech


setGeneric("get_comport", function(x, nom_table, list_rd, tx_cible_prec){standardGeneric("get_comport")})
setMethod(
  f = "get_comport",
  signature = c(x = "HypTech",  nom_table = "character", list_rd = "list", tx_cible_prec = "numeric"),
  def = function(x, nom_table, list_rd , tx_cible_prec){

    # Ajout d un test de presence du nom
    if (! nom_table %in% names(x@param_comport)){"tables de parametres de taux cible non trouve"}
    else{return(calc_tx_cible_ref_marche(x@param_comport[[nom_table]], list_rd, tx_cible_prec))}
  }
)


