#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe RC
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{RC}.
##'
##' Une classe pour la gestion de la reserve de capitalisation (RC).
##'
##' @name RC
##' @slot val_debut est une valeur \code{numeric} correspondant a la valeur de la RC en debut d'annee.
##' @slot val_courante est une valeur \code{numeric} correspondant a la valeur courante de la RC.
##' @docType class
##' @author Prim'Act
##' @seealso Les methodes de calcul de la RC \code{\link{calc_RC}},
##' et de mises a jour des RC initiales et courantes \code{\link{do_update_RC_val_courante}}, \code{\link{do_update_RC_val_debut}}.
##' @keywords classes
##' @export

setClass(
    Class = "RC",
    representation = representation(
        val_debut = "numeric",
        val_courante = "numeric"
    )
)

# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(
    "RC",
    function(object) {
        retval <- NULL
        # Verification des dimensions des attributs
        if (length(object@val_debut) > 1L) retval <- c(retval, "[RC] : La longueur de l'attribut val_debut d'un objet RC est au plus de 1\n")
        if (length(object@val_courante) > 1L) retval <- c(retval, "[RC] : La longueur de l'attribut val_courante d'un objet RC est au plus de 1 \n")

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
    signature = "RC",
    definition = function(.Object, val_debut = numeric(), val_courante = numeric()) {
        if (!missing(val_debut) & !missing(val_courante)) {
            # Traitement du cas ou tous les elements sont renseignes
            .Object@val_debut <- val_debut
            .Object@val_courante <- val_courante
            validObject(.Object)
        } else if (missing(val_debut) & missing(val_courante)) {
            # Traitement du cas ou aucun element n'est renseigne
            .Object@val_debut <- numeric()
            .Object@val_courante <- numeric()
        } else {
            # Autres cas : message d'erreur
            stop("[RC] : Initialisation erronee \n")
        }
        return(.Object)
    }
)
