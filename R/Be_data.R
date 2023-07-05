# Dataset pour le package SimBEL
#
##' Dataset Be.
##'
##' Dataset de la classe \code{\link{Be}} contenant des donnees correspondant aux classes :
##' \code{\link{Canton}}, \code{\link{ParamBe}}, \code{\link{ESG}}.
##'
##' @name Be_DataSet
##' @docType data
##' @usage data("be")
##' @format Objet de la classe \code{\link{Be}}.
##' @keywords datasets
##' @author Prim'Act
##' @note Il s'agit de donnees simulees, elles ont ete creees afin de tester les differents fonctions du package.
##' @examples
##' data(be)
##' param_be <- be@param_be
##' portefeuille_passif <- be@canton@ptf_passif
"be"
