#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe PRE
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{PRE}.
##'
##' Une classe pour la gestion de la provision pour risque d'exigibilite (PRE).
##'
##' @name PRE
##' @slot val_debut est une valeur \code{numeric} correspondant a la valeur de la PRE en debut d'annee.
##' @slot val_courante est une valeur \code{numeric} correspondant a la valeur courante de la PRE.
##' @docType class
##' @author Prim'Act
##' @seealso Les methodes de calcul de la PRE \code{\link{calc_PRE}}, et de mises a jour des PRE initiales et courantes \code{\link{do_update_PRE_val_courante}}, \code{\link{do_update_PRE_val_debut}}.
##' @keywords classes
##' @export

setClass(
    Class = "PRE",
    representation = representation(
        val_debut = "numeric",
        val_courante = "numeric",
        ryth_dot = "integer")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur : permet ? chaque appel de l'objet de v?rifier quelques ?l?ments de base :
setValidity ("PRE",
             function (object){
                 retval <- NULL
                 #Verification des dimensions des attributs
                 if(length(object@val_debut) > 1L)
                     retval <- c(retval, "[PRE] : La longueur de l'attribut val_debut d'un objet PRE est au plus de 1\n")
                 if(length(object@val_courante) > 1L)
                     retval <- c(retval, "[PRE] : La longueur de l'attribut val_courante d'un objet PRE est au plus de 1 \n")
                 if(length(object@ryth_dot) > 1L)
                     retval <- c(retval, "[PRE] : La longueur de l'attribut ryth_dot d'un objet PRE est au plus de 1 \n")
                 if(length(object@ryth_dot) > 0L) if(object@ryth_dot < 3 | object@ryth_dot > 8)
                     retval <- c(retval, "[PRE] : L'attribut 'ryth_dot' doit etre compris entre 3 et 8 \n")

                 if (is.null(retval))
                     return (TRUE)
                 else
                     return (retval)
             })

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "PRE",
    definition = function(.Object, val_debut = numeric(), val_courante = numeric(), ryth_dot = integer()){

        if( !missing(val_debut) & !missing(val_courante) & !missing(ryth_dot)){
            # Traitement du cas ou tous les elements sont renseignes
            .Object@val_debut    <- val_debut
            .Object@val_courante <- val_courante
            .Object@ryth_dot <- ryth_dot
            validObject(.Object)
        }else if (missing(val_debut) & missing(val_courante) & missing(ryth_dot)){
            # Traitement du cas ou aucun element n'est renseigne
            .Object@val_debut    <- numeric()
            .Object@val_courante <- numeric()
            .Object@ryth_dot <- integer()
        }
        else{
            # Autres cas : message d'erreur
            stop("[PRE] : Initialisation erronee \n")
        }
        return(.Object)
    }
)
