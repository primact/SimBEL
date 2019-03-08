#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction permettant de choquer la table d'inflation de l'ESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique le choc frais de la formule standard a la table de simulation pour l'indice inflation.
##'
##' \code{get_choc_inflation_frais} est une methode permettant d'appliquer le choc frais
##'  de la formule standard a la table de simulation pour l'indice inflation.
##' @name get_choc_inflation_frais
##' @docType methods
##' @param x un objet de la classe \code{\link{ESG}}.
##' @param choc une valeur \code{numeric} correspondant au coefficient de
##' choc a appliquer en additif au taux d'inflation.
##' @author Prim'Act
##' @return L'objet \code{x} mis a jour.
##' @note L'inflation comprise dans l'ESG est suppose etre deja capitalise, i.e.
##' \eqn{indice_inflation = (1 + tx inflation)^{annee}}. Il ne s'agit pas du taux d'inflation.
##' @export
##' @include ESG_class.R
##'
setGeneric(name = "get_choc_inflation_frais", def = function(x, choc)
{standardGeneric("get_choc_inflation_frais")})
setMethod(
    f = "get_choc_inflation_frais",
    signature = c(x = "ESG", choc = "numeric"),
    def = function(x, choc){

        # Inflation
        ind_inflation <- x@ind_inflation

        # Annees pour l'actualisation (eviter les divisions par 0)
        annees <- pmax((1L:ncol(ind_inflation) - 1L), 1L)

        # Indice d'inflation avant choc (les annees sont basculees en lignes)
        fun <- function(i){(ind_inflation[i, ])^ (1 / annees) - 1}
        tx_inflation <- t(do.call("rbind", lapply(1L:nrow(ind_inflation), fun)))

        # Application du choc
        tx_inflation[2L:nrow(tx_inflation), ] <- tx_inflation[2L:nrow(tx_inflation), ] + choc


        # Remise sous forme d'indice
        fun <- function(i){(1 + tx_inflation[, i])^(annees)}
        index <- sapply(1L:nrow(ind_inflation), fun)

        # Remise au format data.frame
        index <- data.frame(t(index))

        # Mise a jour de la matrice de taux d'inflation
        x@ind_inflation <- index

        # output
        return(x)

    }
)



