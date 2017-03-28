#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des hyp des hypotheses du canton
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger la valeur initiale des hypotheses du canton.
##'
##' \code{hyp_canton_load} est une methode permettant de charger les parametres associees a un
##' objet de classe \code{\link{HypCanton}}.
##' @name hyp_canton_load
##' @docType methods
##' @param file_hyp_canton_address est un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur
##' permettant de renseigner un objet \code{\link{HypCanton}}.
##' @return L'objet de la classe \code{\link{HypCanton}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l’input.
##' @export
##' @include HypCanton_class.R
setGeneric(name = "hyp_canton_load", def = function(file_hyp_canton_address){standardGeneric("hyp_canton_load")})
setMethod(
    f = "hyp_canton_load",
    signature = "character",
    definition = function(file_hyp_canton_address){
        temp <- read.csv2(file_hyp_canton_address)
        hyp_canton  <- new("HypCanton",
                           tx_soc            = temp[,"tx_soc"],
                           tx_import         = temp[,"tx_import"],
                           method_taux_cible = as.character(temp[,"method_taux_cible"]))
        return(hyp_canton)
    }
)
