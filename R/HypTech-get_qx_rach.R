#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_qx_rach : Methode de calcul des taux de rachat
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recuperer les taux de rachat calcules.
##'
##' \code{get_qx_rach} est une methode permettant d'executer le calcul des taux de rachat structurel. Il
##' peut s'agir soit de taux de rachat partiels, soit de taux de rachat totaux.
##' @name get_qx_rach
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table un nom de la table de rachat.
##' @param age est la valeur \code{numeric} de l'age.
##' @param anc est la valeur \code{numeric} de l'anciennete du contrat.
##' @details Selon le nom de la table \code{nom_table}, le resultat de cette fonction sera un taux
##' de rachat partiel ou un taux de rachat total.
##' @return Le taux de rachat.
##' @author Prim'Act
##' @seealso Le calcul du taux de rachat \code{\link{calc_rach}}.
##' @export
##' @include HypTech-class.R
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
