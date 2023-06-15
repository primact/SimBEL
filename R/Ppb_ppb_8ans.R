#----------------------------------------------------------------------------------------------------------------------------------------------------
#           ppb_8ans : Methode permettant d'appliquer la regle des 8 ans
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la valeur de la PPB a distribue en appliquant la regle des 8 ans.
##'
##' \code{ppb_8ans} est une methode permettant d'appliquer la regle des 8 ans.
##' @name ppb_8ans
##' @docType methods
##' @param x un objet de la classe \code{\link{Ppb}}.
##' @return une liste contenant les parametres
##' \describe{
##' \item{\code{ppb_8} : }{la valeur \code{numeric} correspondant au montant de la ppb de l'annee t-8.}
##' \item{\code{ppb} : }{un objet \code{\link{Ppb}} correspondant a la PPB mise a jour.}
##' }
##' @author Prim'Act
##' @export
##' @include Ppb_class.R

setGeneric(name = "ppb_8ans", def = function(x) {
    standardGeneric("ppb_8ans")
})
setMethod(
    f = "ppb_8ans",
    signature = c(x = "Ppb"),
    definition = function(x) {
        # Recuperer la ppb de l'annee t-8
        ppb_8 <- x@hist_ppb[8L]

        # Mise a jour de valeur_ppb
        x@valeur_ppb <- x@valeur_ppb - ppb_8

        # Mise a jour du compteur de reprise
        x@compte_rep <- x@compte_rep + ppb_8

        # Mettre a 0 la PPB de l'annee an-8
        x@hist_ppb[8L] <- 0

        # Output
        return(list(
            ppb_8 = ppb_8,
            ppb = x
        ))
    }
)
