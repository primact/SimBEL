#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_RC_val_courante
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur courante de RC
##'
##' \code{do_update_RC_val_courante} est une methode permettant de calculer le montant de RC.
##' @name do_update_RC_val_courante
##' @docType methods
##' @param x objet de la classe \code{RC}correspondant a la RC avant mise a jour.
##' @param val_courante est un \code{numeric} correspondant au montant de RC calcule par la fonction \code{\link{calc_RC}}.
##' @return L'objet \code{RC} mis a jour de la nouvelle valeur courante de \code{RC}
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la RC \code{\link{calc_RC}}
##' @aliases RC
##' @include RC_class.R

setGeneric("do_update_RC_val_courante", def = function(x, val_courante){standardGeneric("do_update_RC_val_courante")})
setMethod(
    f = "do_update_RC_val_courante",
    signature = c(x = "RC", val_courante = "numeric"),
    definition = function(x, val_courante){
        x["val_courante"] <- val_courante
        return(x)
    }
)
