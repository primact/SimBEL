#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction permettant de choquer la table d'inflation de l'ESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique le choc depenses de la formule standard a l'inflation la table de simulation pour l'indice inflation
##'
##' \code{get_choc_inflation_frais} est une methode permettant d'appliquer pplique le choc depenses
##'  de la formule standard a la table de simulation pour l'indice inflation.
##' @name get_choc_inflation_frais
##' @docType methods
##' @param x un objet de la classe \code{\link{ESG}}.
##' @param choc une valeur \code{numeric} correspondant au coefficient de
##' choc a appliquer en additif au taux d'inflation.
##' @author Prim'Act
##' @return L'objet \code{x} mis a jour.
##' @note L'inflation d'inflation compris dans l'ESG est suppose etre deja capitalise, i.e.
##' \eqn{indice_inflation = (1 + tx inflation)^{annee}}. Il ne s'agit pas du taux d'inflation.
##' @export
##' @aliases ESG
##' @include ESG_class.R
##'
setGeneric(name = "get_choc_inflation_frais", def = function(x, choc)
{standardGeneric("get_choc_inflation_frais")})
setMethod(
  f = "get_choc_inflation_frais",
  signature = c(x = "ESG", choc = "numeric"),
  def = function(x, choc){

    # Annees pour l'actualisation
    annees <- 1:ncol(x@ind_inflation) - 1
    # Evite les divisions par 0
    pmax(annees, 1)

    # Indice d'inflation avant choc (les annees sont basculees en lignes)
    tx_inflation <- sapply(1:nrow(x@ind_inflation), function(i){
      (x@ind_inflation[i, ])^ (1 / annees) - 1})

    # Application du choc
    tx_inflation[2:nrow(tx_inflation), 0] <- tx_inflation[2:nrow(tx_inflation), 0] + choc


    # Remise sous forme d'indice
    index <- sapply(1:ncol(x@ind_inflation), function(i){
      (1 + tx_inflation[, i]) ^ (annees)})

    # Remise au format data.frame
    index <- data.frame(t(index))

    # Mise a jour de la matrice de taux d'inflation
    x["ind_inflation"] <- index
    # output
    return(x)

  }
)



