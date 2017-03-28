#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de veillissement d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Veillissement d'un an des contrats epargne en euros.
##'
##' \code{vieilli_mp} est une methode permettant de vieillir
##'  les model points epargne en euros d'une peridoe.
##' @name vieilli_mp
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param pm_fin_ap_pb un vecteur de type \code{numeric} contenant par model point
##' les montants de PM revalorises apres participation aux benefices.
##' @param tx_revalo un vecteur de type \code{numeric} contenant par model point
##' les taux de revalorisation nets appliques.
##' @return l'objet \code{x} vieilli d'une periode.
##' @author Prim'Act
##' @seealso Calcul de la revalorisation des PM \code{\link{calc_revalo_pm}}.
##' @export
##' @include EpEuroInd-class.R
##'
setGeneric(name = "vieilli_mp", def = function(x, pm_fin_ap_pb, tx_revalo){standardGeneric("vieilli_mp")})
setMethod(
  f = "vieilli_mp",
  signature = c(x = "EpEuroInd", pm_fin_ap_pb = "numeric", tx_revalo = "numeric"),
  def = function(x, pm_fin_ap_pb, tx_revalo){
    # viellissement de 1 an
    x@mp$age <- x@mp$age + as.integer(1)
    x@mp$anc <- x@mp$anc + as.integer(1)

    # Ajustement du nombre de contrat, de la PM garanti (calcul FDB) et du taux cible.
    x@mp$nb_contr <- x@tab@tab[["nb_contr"]]
    x@mp$pm_gar <- x@tab@tab[["pm_gar"]]
    x@mp$tx_cible_prec <- x@tab@tab[["tx_cible"]]

    # Ajustement de PM de fin
    x@mp$pm <- pm_fin_ap_pb
    x@mp$tx_revalo_prec <- tx_revalo

    # output
    return(x)
  }
)
