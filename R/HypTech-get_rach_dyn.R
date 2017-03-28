#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_rach_dyn : Methode de calcul des taux de rachat dynamique
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recuperer les taux de rachat dynamiques calcules.
##'
##' \code{get_rach_dyn} est une methode permettant d'executer le calcul des taux de rachat dynamique.
##' @name get_rach_dyn
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table un nom de jeu de paramatre de rachat dynamique.
##' @param tx_cible est une valeur \code{numeric} correspondant taux de revalorisation cible.
##' @param tx_serv est une valeur \code{numeric} correspondant taux de revalorisation servi.
##' @return Le taux de rachat dynamique.
##' @author Prim'Act
##' @seealso Le calcul du taux de rachat dynamique \code{\link{calc_rach_dyn}}.
##' @export
##' @include HypTech-class.R
setGeneric("get_rach_dyn", function(x, nom_table,tx_cible,tx_serv){standardGeneric("get_rach_dyn")})
setMethod(
  f = "get_rach_dyn",
  signature = c(x = "HypTech",  nom_table = "character", tx_cible = "numeric", tx_serv = "numeric"),
  def = function(x, nom_table, tx_cible, tx_serv){

    # Ajout d un test de presence du nom
    if (! nom_table %in% names(x@param_rach_dyn)){"tables de parametres de rachats dynamique non trouve"}
    else{return(calc_rach_dyn(x@param_rach_dyn[[nom_table]], tx_cible, tx_serv))}
  }
)













