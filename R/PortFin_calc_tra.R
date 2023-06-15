#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_tra
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul du taux de rendement financier
##'
##' \code{calc_tra} est une methode permettant de calculer le taux de rendement financier du portefeuille.
##' @name calc_tra
##' @docType methods
##' @param plac_moy est un objet de type \code{numeric}, qui fournit la valeur moyenne
##'  des placements de l'annee en valeur nette comptable.
##' @param res_fin est un objet de type \code{numeric}, qui fournit le resultat financier du porfeuille.
##' @return La valeur du taux de rendement de l'actif.
##' @author Prim'Act
##' @export
##' @include PortFin_class.R

setGeneric(name = "calc_tra", def = function(plac_moy, res_fin) {
    standardGeneric("calc_tra")
})
setMethod(
    f = "calc_tra",
    signature = c(plac_moy = "numeric", res_fin = "numeric"),
    definition = function(plac_moy, res_fin) {
        # Valeur moyenne des placements
        if (plac_moy == 0) {
            tra <- 0
        } else {
            tra <- res_fin / plac_moy
        } # Calcul du TRA

        # Output
        return(tra)
    }
)
