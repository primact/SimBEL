#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_PRE_val_courante
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur courante de PRE.
##'
##' \code{do_update_PRE_val_courante} est une methode permettant de calculer le montant de PRE.
##' @name do_update_PRE_val_courante
##' @docType methods
##' @param x objet de la classe \code{\link{PRE}} correspondant a la PRE avant mise a jour.
##' @param val_courante est un \code{numeric} correspondant au montant de PRE calcule par la fonction \code{\link{calc_PRE}}.
##' @return L'objet \code{\link{PRE}} mis a jour de la nouvelle valeur courante de \code{\link{PRE}}
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la PRE \code{\link{calc_PRE}}
##' @include PRE_class.R

setGeneric("do_update_PRE_val_courante", def = function(x, val_courante){standardGeneric("do_update_PRE_val_courante")})
setMethod(
    f = "do_update_PRE_val_courante",
    signature = c(x = "PRE", val_courante = "numeric"),
    definition = function(x, val_courante){
        
        # Mise a jour valeur courante
        x@val_courante <- val_courante
        
        # Output
        return(x)
    }
)
