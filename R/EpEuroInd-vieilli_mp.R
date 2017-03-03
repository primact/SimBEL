#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 22/01/2017. Fait par QG : initialisation
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de veillissement d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Veilli un model point.
##'
##' \code{vieilli_mp} est une methode permettant de vieillir
##'  un model point de 1 an.
##' @name vieilli_mp
##' @docType methods
##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros
##' @param pm_fin_ap_pb un vecteur de type \code{numeric} contenant par model points
##' les montants de PM revalorises.
##' @param tx_revalo un vecteur de type \code{numeric} contenant par model points
##' les taux de revalorisation nets appliques.
##' @return l'objet \code{x} vieilli d'une annee.
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd
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
