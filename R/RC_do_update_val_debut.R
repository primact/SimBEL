#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_RC_val_debut
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur initiale de RC
##'
##' \code{do_update_RC_val_debut} est une methode permettant de mettre a jour le montant de debut de periode de RC.
##' @name do_update_RC_val_debut
##' @docType methods
##' @param x objet de la classe \code{\link{RC}} correspondant a la RC avant mise a jour.
##' @param val_debut est un \code{numeric} correspondant au montant de debut de periode de RC.
##' @return L'objet \code{x} mis a jour de la nouvelle valeur de debut de \code{\link{RC}}.
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la RC \code{\link{calc_RC}}
##' @include RC_class.R

setGeneric("do_update_RC_val_debut", def = function(x, val_debut) {
    standardGeneric("do_update_RC_val_debut")
})
setMethod(
    f = "do_update_RC_val_debut",
    signature = c(x = "RC", val_debut = "numeric"),
    definition = function(x, val_debut) {
        # Mise a jour de la valeur
        x@val_debut <- val_debut

        # Output
        return(x)
    }
)
