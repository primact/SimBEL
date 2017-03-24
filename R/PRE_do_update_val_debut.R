#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_PRE_val_debut
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur courante de PRE
##'
##' \code{do_update_PRE_val_debut} est une methode permettant de calculer le montant de PRE.
##' @name do_update_PRE_val_debut
##' @docType methods
##' @param x objet de la classe \code{PRE} correspondant a la PRE avant mise a jour.
##' @param val_courante est un \code{numeric} correspondant au montant de PRE calcule par la fonction \code{\link{calc_PRE}}.
##' @return L'objet \code{PRE} mis a jour de la nouvelle valeur de debut de \code{PRE}
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la PRE \code{\link{calc_PRE}}
##' @aliases PRE
##' @include PRE_class.R

setGeneric("do_update_PRE_val_debut", def = function(x, val_debut){standardGeneric("do_update_PRE_val_debut")})
setMethod(
    f = "do_update_PRE_val_debut",
    signature = c(x = "PRE", val_debut = "numeric"),
    definition = function(x, val_debut){
        x@val_debut <- val_debut
        return(x)
    }
)
