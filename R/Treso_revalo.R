# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revalo_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille treso.
##'
##' \code{revalo_treso} est une methode permettant de calculer les valeurs de marche.
##' @name revalo_treso
##' @docType methods
##' @param Rt valeur du taux zero coupon 1 an de l'annee.
##' @param Rt_prev valeur du taux zero coupon 0 an de l'annee.
##' @return Le taux de rendement.
##' @author Prim'Act
##' @export
##' @include Treso_class.R

# ATTENTION, Rt = R(t,t+1)
# Dans le cas general cette fonction devra donc etre appelee avec Rt_prev force a 0!!!!
setGeneric(name = "revalo_treso", def = function(Rt, Rt_prev) {
    standardGeneric("revalo_treso")
})
setMethod(
    f = "revalo_treso",
    signature = c(Rt = "numeric", Rt_prev = "numeric"),
    definition = function(Rt, Rt_prev) {
        # Verification des inputs
        len_rt <- length(Rt)
        if ((len_rt != length(Rt_prev)) | (len_rt != 1L)) stop("[Treso : revalo] : Les inputs doivent etre de meme dimension (superieure a 1). \n")

        # Calcul du vecteur rdt : Prise en compte du fait que les dividendes soient reinvestis ou non
        rdt <- (1 + Rt) / (1 + Rt_prev) - 1

        # Output
        return(rdt)
    }
)
