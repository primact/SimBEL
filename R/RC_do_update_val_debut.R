#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_RC_val_debut
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur courante de RC
##'
##' \code{do_update_RC_val_debut} est une methode permettant de calculer le montant de RC.
##' @name do_update_RC_val_debut
##' @docType methods
##' @param x objet de la classe \code{RC} correspondant a la RC avant mise a jour.
##' @param val_courante est un \code{numeric} correspondant au montant de RC calcule par la fonction \code{\link{calc_RC}}.
##' @return L'objet \code{RC} mis a jour de la nouvelle valeur de debut de \code{RC}
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la RC \code{\link{calc_RC}}
##' @aliases RC
##' @include RC_class.R

setGeneric("do_update_RC_val_debut", def = function(x, val_debut){standardGeneric("do_update_RC_val_debut")})
setMethod(
    f = "do_update_RC_val_debut",
    signature = c(x = "RC", val_debut = "numeric"),
    definition = function(x, val_debut){
        x["val_debut"] <- val_debut
        return(x)
    }
)

