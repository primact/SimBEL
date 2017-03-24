#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_comport : pour le calcul du taux cible
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recuperer les taux de revalorisation cible calcules.
##'
##' \code{get_comport} est une methode permettant d'executer le calcul des taux de revalorisation cible.
##' @name get_comport
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table un nom de la table de parametres de taux cible.
##' @param list_rd une liste contenant les rendements de reference. Le format de cette liste est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @param tx_cible_prec une valeur \code{numeric} correspondant au taux cible de la periode precedente.
##' @return La valeur du taux cible.
##' @author Prim'Act
##' @seealso Le calcul du taux cible \code{\link{calc_tx_cible_ref_marche}}.
##' @export
##' @aliases HypTech
##' @include HypTech-class.R
setGeneric("get_comport", function(x, nom_table, list_rd, tx_cible_prec){standardGeneric("get_comport")})
setMethod(
  f = "get_comport",
  signature = c(x = "HypTech",  nom_table = "character", list_rd = "list", tx_cible_prec = "numeric"),
  def = function(x, nom_table, list_rd , tx_cible_prec){

    # Ajout d un test de presence du nom
    if (! nom_table %in% names(x@param_comport)){
      stop("[Hyptech : get_comport] Tables de parametres de taux cible non trouve")
      }
    else{return(calc_tx_cible_ref_marche(x@param_comport[[nom_table]], list_rd, tx_cible_prec))}
  }
)


