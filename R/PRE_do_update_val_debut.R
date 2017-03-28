#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_PRE_val_debut
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur de debut de periode de la PRE
##'
##' \code{do_update_PRE_val_debut} est une methode permettant de mettre a jour le montant de debut de periode de PRE.
##' @name do_update_PRE_val_debut
##' @docType methods
##' @param x objet de la classe \code{\link{PRE}} correspondant a la PRE avant mise a jour.
##' @param val_debut est un \code{numeric} correspondant au montant de debut de periode de PRE.
##' @return L'objet \code{\link{PRE}} mis a jour de la nouvelle valeur de debut de \code{\link{PRE}}
##' @author Prim'Act
##' @export
##' @seealso La methode de calcul de la PRE \code{\link{calc_PRE}}.
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
