#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe HypCanton
#----------------------------------------------------------------------------------------------------------------------------------------------------
##'  La classe \code{HypCanton}.
##'
##' Une class de parametres pour les parametres generaux du canton.
##' @name HypCanton
##' @slot tx_soc une valeur \code{numeric} correspondant au taux de prelevements social.
##' @slot tx_import une valeur \code{numeric} correspondant au taux d'impot sur le resultat.
##' @slot method_taux_cible un \code{character} correspond au nom de la methode de calcul du taux cible.
##' @docType class
##' @note Dans la version courante, la valeur de \code{method_taux_cible} doit etre parametree a "Meth1".
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(
    Class = "HypCanton",
    representation = representation(
        tx_soc = "numeric",
        tx_import = "numeric",
        method_taux_cible = "character" # PARAMETRES PAS PROPRE. IL FAUT METTRE EN COHERENCE EN
        # CAS d'UNE DEFINITION AU NIVEAU DES PASSIFS.
    )
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(
    Class = "HypCanton",
    function(object) {
        retval <- NULL

        if (!is.numeric(object@tx_soc)) retval <- c(retval, "[HypCanton] : tx_soc n'est pas numeric/n")
        if (!is.numeric(object@tx_import)) retval <- c(retval, "[HypCanton] : tx_import n'est pas numeric/n")
        if (!is.character(object@method_taux_cible)) retval <- c(retval, "[HypCanton] : method_taux_cible n'est pas un character/n")

        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)
