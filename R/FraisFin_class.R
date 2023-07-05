# 10/03/2017 Guillaume de Kervenoael, Quentin Guibert

#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe FraisFin
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe FraisFin
##'
##' Classe pour les parametres de frais financiers d'un assureur.
##'
##' @name FraisFin
##' @slot tx_chargement est une valeur \code{numeric} correspondant au taux de frais de gestion financiere.
##' @slot indicatrice_inflation est un objet de type \code{logical}, qui permet d'indiquer si une inflation
##'  doit etre appliquee.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
setClass(
    Class = "FraisFin",
    representation = representation(
        tx_chargement = "numeric",
        indicatrice_inflation = "logical"
    )
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur :
setValidity(
    "FraisFin",
    function(object) {
        retval <- NULL
        # Verification du nombre de colonnes
        if (length(object@tx_chargement) > 1L) retval <- c(retval, "[FraisFin] : Dimension de l'attribut tx de chargement incorrecte \n")
        if (length(object@indicatrice_inflation) > 1L) retval <- c(retval, "[FraisFin] : Dimension de l'attribut indicatrice inflation incorrecte \n")

        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "FraisFin",
    definition = function(.Object, tx_chargement = numeric(), indicatrice_inflation = logical()) {
        # Traitement du cas ou tout les elements sont renseignes
        if (!missing(tx_chargement) & !missing(indicatrice_inflation)) {
            .Object@tx_chargement <- tx_chargement
            .Object@indicatrice_inflation <- indicatrice_inflation
            validObject(.Object)
        } else if (missing(tx_chargement) & missing(indicatrice_inflation)) {
            .Object@tx_chargement <- numeric()
            .Object@indicatrice_inflation <- logical()
        } else {
            stop("[FraisFin] : Veuillez renseigner un niveau de frais \n")
        }
        return(.Object)
    }
)
