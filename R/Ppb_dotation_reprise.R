#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_dotation_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Dote la valeur de la PPB
##'
##' \code{calc_dotation_ppb} est une methode permettant de doter la PPB. La dotation est effectuee si les
##' limites de dotation de la PPB sur l'annee ne sont pas atteintes. La valeur de cette limite est mise a jour suite a la dotation.
##' @name calc_dotation_ppb
##' @docType methods
##' @param x objet de la classe \code{\link{Ppb}}.
##' @param montant une valeur \code{numeric} a doter.
##' @return \code{ppb} l'objet \code{x} mis a jour.
##' @return \code{dotation} le montnant de la dotation effectuee.
##' @author Prim'Act
##' @export
##' @include Ppb_class.R

setGeneric(name = "calc_dotation_ppb", def = function(x, montant) {
    standardGeneric("calc_dotation_ppb")
})
setMethod(
    f = "calc_dotation_ppb",
    signature = c(x = "Ppb", montant = "numeric"),
    definition = function(x, montant) {
        # Limite de dotation courante
        limit <- max(x@seuil_dot * x@ppb_debut - x@compte_dot, 0)

        # Montant dote
        dot <- min(limit, montant)

        # Application de la dotation
        x@valeur_ppb <- x@valeur_ppb + dot

        # Mise a jour du montant dote
        x@compte_dot <- x@compte_dot + dot

        # Output
        return(list(
            ppb = x,
            dotation = dot
        ))
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_reprise_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Reprend sur la valeur de la PPB
##'
##' \code{calc_reprise_ppb} est une methode permettant de reprendre sur la PPB.
##' La reprise est effectuee si les limites de reprise de la PPB sur l'annee ne sont pas atteintes.
##' La valeur de cette limite est mise a jour suite a la reprise
##' @name calc_reprise_ppb
##' @docType methods
##' @param x un objet de la classe \code{Ppb}.
##' @param montant la valeur \code{numeric} de la reprise.
##' @return \code{ppb} l'objet \code{x} mis a jour
##' @return \code{reprise} le montnant de la reprise effectuee.
##' @author Prim'Act
##' @export
##' @include Ppb_class.R

setGeneric(name = "calc_reprise_ppb", def = function(x, montant) {
    standardGeneric("calc_reprise_ppb")
})
setMethod(
    f = "calc_reprise_ppb",
    signature = c(x = "Ppb", montant = "numeric"),
    definition = function(x, montant) {
        # Limite de reprise courante
        limit <- max(x@seuil_rep * x@ppb_debut - x@compte_rep, 0)

        # Montant repris
        rep <- min(limit, montant, x@valeur_ppb)

        # Application de la reprise
        reste <- rep # Reste de PPB a attribue
        i <- length(x@hist_ppb) # Indice sur l'annee

        while (!(reste == 0 | i == 0)) {
            # Si en l'annee i, la PPB > reste :
            if (x@hist_ppb[i] < reste) {
                reste <- reste - x@hist_ppb[i]
                x@hist_ppb[i] <- 0
            } else { # Si en l'annee i, la PPB < reste :
                x@hist_ppb[i] <- x@hist_ppb[i] - reste
                reste <- 0
            }

            i <- i - 1L
        }

        # Mise a jour de la somme
        x@valeur_ppb <- x@valeur_ppb - rep

        # Mise a jour du montant repris
        x@compte_rep <- x@compte_rep + rep

        # Output
        return(list(
            ppb = x,
            reprise = rep
        ))
    }
)
