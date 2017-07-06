#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ParamAlmEngine
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ParamAlmEngine}.
##'
##' Une classe pour le parametre ALM d'un canton.
##'
##' @name ParamAlmEngine
##' @slot ptf_reference est un objet de type \code{\link{PortFin}}, qui represente le portefeuille
##'  d'investissement de reference d'un canton.
##' @slot alloc_cible un vecteur de 4 elements rendant compte du pourcentage
##'  de l'actif composant respectivement les investissements: actions, immobiliers, obligataires et de tresorerie.
##' @slot seuil_realisation_PVL une valeur \code{numeric} correspondant au pourcentage de plus-values actions
##' qui peut etre liquidee chaque annee pour atteindre l'objectif de revalorisation cible des passifs.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
##' @include PortFin_class.R

setClass(
    Class = "ParamAlmEngine",
    representation = representation(
        ptf_reference = "PortFin",
        alloc_cible   = "numeric",
        seuil_realisation_PVL  = "numeric")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur : permet ? chaque appel de l'objet de v?rifier quelques ?l?ments de base :
setValidity ("ParamAlmEngine",
             function (object){
                 
                 retval <- NULL
                 
                 #Verification du nombre de colonnes
                 if(length(object@alloc_cible) != 4L) 
                     retval <- c(retval, "[ParamAlmEngine] : L'attribut alloc_cible doit contenir quatre elements representant respectivement la proportion d'action, d'immobilisation, d'obligation et de tresorerie du portefeuille\n")
                 if(sum(object@alloc_cible) != 1L | sum(object@alloc_cible > 1) > 0L | sum(object@alloc_cible < 0) > 0L ) 
                     retval <- c(retval, "[ParamAlmEngine] : Les elements de l'attribut alloc_cible doivent etre compris entre 0 et 1 (hors tresorerie) et leur somme doit etre egale a 1.\n")
                 if(length(object@seuil_realisation_PVL) != 1L) 
                     retval <- c(retval, "[ParamAlmEngine] : Le seuil de realisation de PVL ne peut contenir plus d'un element\n")
                 
                 if (is.null(retval)) return (TRUE)
                 else return (retval)
             })

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "ParamAlmEngine",
    definition = function(.Object, ptf_reference = new("PortFin"), alloc_cible = numeric(), seuil_realisation_PVL = numeric()){
        
        # Traitement du cas ou tout les elements sont renseignes
        if(! missing(ptf_reference) & ! missing(alloc_cible) & ! missing(seuil_realisation_PVL)) {
            .Object@ptf_reference <- ptf_reference
            .Object@alloc_cible   <- alloc_cible
            .Object@seuil_realisation_PVL <- seuil_realisation_PVL
            validObject(.Object)
            
        } else { #Traitement du cas vide
            .Object@ptf_reference <- new("PortFin")
            .Object@alloc_cible   <- numeric()
            .Object@seuil_realisation_PVL <- numeric()
        }
        
        return(.Object)
    }
)
