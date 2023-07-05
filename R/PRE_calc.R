#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_PRE
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul de la PRE.
##'
##' \code{calc_PRE} est une methode permettant de calculer le montant de PRE.
##' @name calc_PRE
##' @docType methods
##' @param x objet de la classe \code{\link{PRE}}, necessaire pour connaitre le stock de PRE initial.
##' @param pmvl_action_immo est un \code{numeric} correspondant au montant global de plus ou moins values latentes des actifs actions et immobiliers.
##' En cas de moins value latente, la PRE est abondee.
##' En cas de plus value latente, la PRE est integralement reprise.
##' @return Le format de la liste renvoyee est :
##' \describe{
##' \item{\code{pre_courante} : }{ valeur de la pre courante calculee a partir des inputs transmis}
##' \item{\code{var_pre} : }{ variation de la pre courante}
##' }
##' @author Prim'Act
##' @export
##' @include PRE_class.R

setGeneric(name = "calc_PRE", def = function(x, pmvl_action_immo) {
    standardGeneric("calc_PRE")
})
setMethod(
    f = "calc_PRE",
    signature = c(x = "PRE", pmvl_action_immo = "numeric"),
    definition = function(x, pmvl_action_immo) {
        # Verification des inputs
        if (length(pmvl_action_immo) != 1L) stop("[PRE : calc_PRE] : Les inputs ne sont pas de la bonne dimension. \n")

        # Calcul de la PRE
        if (pmvl_action_immo <= 0L) {
            # Les actifs actions et immos (hors oblig) sont en situations de moins values latentes sur l'exercice par rapport au stock initial
            # La PRE est augmentee de la moins value latentes hors produits obligataires

            # Variation de PRE
            var_pre <- min(
                x@val_debut + 1 / x@ryth_dot * max(-pmvl_action_immo, 0),
                max(-pmvl_action_immo, 0)
            ) - x@val_debut

            # Valeur courante
            val_courante <- x@val_debut + var_pre
        } else { # Reprise de la PRE
            val_courante <- 0
            var_pre <- val_courante - x@val_debut
        }

        # Output
        return(list(pre_courante = val_courante, var_pre = var_pre))
    }
)
