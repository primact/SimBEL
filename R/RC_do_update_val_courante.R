#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_RC_val_courante
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur courante de RC
##'
##' \code{do_update_RC_val_courante} est une methode permettant de calculer le montant de RC.
##' @name do_update_RC_val_courante
##' @docType methods
##' @param x objet de la classe \code{\link{RC}} correspondant a la RC avant mise a jour.
##' @param val_courante est un \code{numeric} correspondant au montant de RC calcule par la fonction \code{\link{calc_RC}}.
##' @return L'objet \code{\link{RC}} mis a jour de la nouvelle valeur courante de \code{\link{RC}}.
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la RC \code{\link{calc_RC}}
##' @include RC_class.R

setGeneric("do_update_RC_val_courante", def = function(x, val_courante) {
    standardGeneric("do_update_RC_val_courante")
})
setMethod(
    f = "do_update_RC_val_courante",
    signature = c(x = "RC", val_courante = "numeric"),
    definition = function(x, val_courante) {
        # Mise a jour de la valeur
        x@val_courante <- val_courante

        # Output
        return(x)
    }
)
