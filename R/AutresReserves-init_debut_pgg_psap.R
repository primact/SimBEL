#----------------------------------------------------------------------------------------------------------------------------------------------------
#           init_debut_pgg_psap
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Re-initialise un objet \code{AutresReserves} en debut d'annee.
##'
##' \code{init_debut_pgg_psap} est une methode permettant de re-initialiser les montants
##' de PGG et de PSAP de debut de periode.
##' @name init_debut_pgg_psap
##' @docType methods
##' @param x objet de la classe \code{AutresReserves}.
##' @return L'objet x reinitialise.
##' @author Prim'Act
##' @export
##' @include AutresReserves-class.R

setGeneric(name = "init_debut_pgg_psap", def = function(x) {
    standardGeneric("init_debut_pgg_psap")
})
setMethod(
    f = "init_debut_pgg_psap",
    signature = c(x = "AutresReserves"),
    definition = function(x) {
        # Mise a jour du montant de PSAP et de PGG initial
        x@psap_debut <- x@psap_valeur
        x@pgg_debut <- x@pgg_valeur

        # Output
        return(x)
    }
)
