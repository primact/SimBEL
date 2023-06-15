#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Vieillissement d'un an de la PPB.
##'
##' \code{vieillissement_ppb} est une methode permettant de vieillir d'un an la PPB. Cette methode permet de
##' reinitialiser les montants de dotation et de reprise cumules sur l'annee, de re-initialiser le montant de PPB de debut de periode,
##' et de mettre a jour le vecteur historique de la PPB.
##' @name vieillissement_ppb
##' @docType methods
##' @param x objet de la classe \code{\link{Ppb}}.
##' @return L'objet \code{x} vieilli d'une annee.
##' @author Prim'Act
##' @export
##' @include Ppb_class.R

setGeneric(name = "vieillissement_ppb", def = function(x) {
    standardGeneric("vieillissement_ppb")
})
setMethod(
    f = "vieillissement_ppb",
    signature = c(x = "Ppb"),
    def = function(x) {
        # Taille de l'historique
        l <- length(x@hist_ppb)

        # Decalage vecteur valeur_ppb
        for (i in 1L:(l - 1L)) {
            x@hist_ppb[l - (i - 1L)] <- x@hist_ppb[l - i]
        }

        # Mise a zero de l'annee 1
        x@hist_ppb[1L] <- x@compte_dot

        # Mise a jour du montant de PPB initial
        x@valeur_ppb <- sum(x@hist_ppb)
        x@ppb_debut <- sum(x@hist_ppb)

        # Remise a zero des compteurs
        x@compte_dot <- 0
        x@compte_rep <- 0

        # Output
        return(x)
    }
)
