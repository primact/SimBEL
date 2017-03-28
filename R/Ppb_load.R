#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des donnnes et des hypotheses de la PPB
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger les valeurs des hypotheses et des donnees de PPB
##'
##' \code{ppb_load} est une methode permettant de charger les parametres associees a un
##' objet de classe \code{\link{Ppb}}.
##' @name ppb_load
##' @docType methods
##' @param file_ppb_address est un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur
##' permettant de renseigner un objet \code{\link{Ppb}}.
##' @return L'objet de la classe \code{\link{Ppb}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l’input.
##' @export
##' @include HypCanton_class.R
setGeneric(name = "ppb_load", def = function(file_ppb_address){standardGeneric("ppb_load")})
setMethod(
    f = "ppb_load",
    signature = "character",
    definition = function(file_ppb_address){
        temp <- read.csv2(file_ppb_address)
        ppb  <- new("Ppb",
                    valeur_ppb = temp[,"valeur_ppb"],
                    ppb_debut  = temp[,"ppb_debut"],
                    seuil_rep  = temp[,"seuil_rep"],
                    seuil_dot  = temp[,"seuil_dot"],
                    compte_rep = temp[,"compte_rep"],
                    compte_dot = temp[,"compte_dot"])
        return(ppb)
    }
)
