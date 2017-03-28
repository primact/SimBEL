
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les plus et moins-values action.
##'
##' \code{calc_pmvl_action} est une methode permettant de calculer les plus et moins-values du portefeuille action.
##' @name calc_pmvl_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}} (decrivant le portefeuille d'action).
##' @return \code{pvl} correspondant a la somme des plus-values latentes actions.
##' @return \code{mvl} correspondant a la somme des moins-values latentes actions.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "calc_pmvl_action", def = function(x){standardGeneric("calc_pmvl_action")})
setMethod(
    f = "calc_pmvl_action",
    signature = "Action",
    definition = function(x){
        nom_table  <- names(x@ptf_action)
        val_marche <- which(nom_table == "val_marche")
        val_nc     <- which(nom_table == "val_nc")

        # Plus ou moins value latentes
        pmvl <-.subset2(x@ptf_action, val_marche) - .subset2(x@ptf_action, val_nc)

        pvl <- sum( pmvl * (pmvl > 0))
        mvl <- sum( pmvl * (pmvl <= 0))
        return(list(pvl = pvl, mvl = mvl))
    }
)
