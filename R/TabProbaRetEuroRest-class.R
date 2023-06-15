#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe TabProbaRetEuroRest
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{TabProbaRetEuroRest}.
##'
##' Une classe pour le stockage en memoire des differentes probabilites invariantes au niveau du model point \code{\link{RetraiteEuroRest}}.
##'
##' @name TabProbaRetEuroRest
##' @slot ax est un \code{data.frame} contenant les valeurs des coefficients actuariels pour chaque model point.
##' @slot sortie_retraite est un \code{data.frame} contenant les la probabilite de sortie d'un contrat retraite pour chaque model point.
##' @slot survie_un_an est un \code{data.frame} contenant les valeurs des coefficients actuariels pour chaque model point.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export

setClass(
    Class = "TabProbaRetEuroRest",
    slots = c(
        ax = "data.frame",
        sortie_retraite = "data.frame",
        survie_un_an = "data.frame"
    ),
    validity = function(object) {
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL


        # Verification du type de variables de la liste
        if (!is.data.frame(object@ax)) retval <- c(retval, "[TabProbaRetEuroRest] : ax n'est pas un data.frame\n")
        if (!is.data.frame(object@sortie_retraite)) retval <- c(retval, "[TabProbaRetEuroRest] : sortie_retraite n'est pas un data.frame\n")
        if (!is.data.frame(object@survie_un_an)) retval <- c(retval, "[TabProbaRetEuroRest] : survie_un_an n'est pas un data.frame\n")



        # Resultats du controle
        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)
