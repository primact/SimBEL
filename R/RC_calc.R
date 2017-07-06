#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_RC
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule de la RC.
##'
##' \code{calc_RC} est une methode permettant de calculer le montant de Reserve de Capitalisation.
##' @name calc_RC
##' @docType methods
##' @param x objet de la classe \code{\link{RC}}, necessaire pour connaitre le stock de RC initial.
##' @param pmvr_oblig est un \code{numeric} correspondant au montant global annuel de plus ou moins values realisees sur des actifs obligataires.
##' @return Le format de la liste renvoyee est :
##' \describe{
##' \item{\code{RC_courante} : }{ valeur de la RC courante initiale augmentee des plus ou moins values annuelles realisees}
##' \item{\code{var_RC} : }{ variation de la RC courante.}
##' }
##' @author Prim'Act
##' @export
##' @include RC_class.R

setGeneric(name = "calc_RC", def = function(x, pmvr_oblig){standardGeneric("calc_RC")})
setMethod(
    f = "calc_RC",
    signature = c(x = "RC", pmvr_oblig = "numeric"),
    definition = function(x, pmvr_oblig){

        # Verification des inputs
        if (length(pmvr_oblig) !=  1L) stop("[RC : calc_RC] : Le montant de plus ou moins value realise doit etre de dimension 1. \n")

        # Calcul de la valeur courante
        val_courante <- max(x@val_courante + pmvr_oblig, 0)

        # Calcul delta
        delta_RC <- val_courante - x@val_debut

        # Output
        return (list(rc_courante = val_courante, var_rc = delta_RC))
    }
)
