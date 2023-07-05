#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe TabProbaEpEuroInd
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{TabProbaEpEuroInd}.
##'
##' Une classe pour le stockage en memoire des differentes probabilites invariantes au niveau du model point \code{\link{EpEuroInd}}.
##'
##' @name TabProbaEpEuroInd
##' @slot rachat_tot est un \code{data.frame} contenant les taux de rachat totaux pour chaque model point.
##' @slot rachat_part est un \code{data.frame} contenant les taux de rachat partiels pour chaque model point.
##' @slot qx_mort est un \code{data.frame} contenant les probabilite des deces pour chaque model point.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(
    Class = "TabProbaEpEuroInd",
    slots = c(
        qx_rach_tot = "data.frame",
        qx_rach_part = "data.frame",
        qx_dc = "data.frame"
    ),
    validity = function(object) {
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL

        # Verification du type de variables de la liste
        if (!is.data.frame(object@qx_rach_tot)) retval <- c(retval, "[TabProbaEpEuroInd] : qx_rach_tot n'est pas un data.frame\n")
        if (!is.data.frame(object@qx_rach_part)) retval <- c(retval, "[TabProbaEpEuroInd] : qx_rach_part n'est pas un data.frame\n")
        if (!is.data.frame(object@qx_dc)) retval <- c(retval, "[TabProbaEpEuroInd] : qx_dc n'est pas un data.frame\n")

        # Resultats du controle
        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)
