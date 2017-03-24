#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des parametres de frais de passif
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger la valeur des frais de passif.
##'
##' \code{frais_passif_load} est une methode permettant de charger les donnees associees a un
##' objet de classe \code{\link{FraisPassif}}.
##' @name frais_passif_load
##' @docType methods
##' @param file_frais_passif_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur
##' permettant de renseigner un objet \code{\link{FraisPassif}}.
##' @return L'objet de la classe \code{\link{FraisPassif}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l’input.
##' @export
##' @aliases FraisPassif
##' @include FraisPassif-class.R
##'
setGeneric(name = "frais_passif_load", def = function(file_frais_passif_address){standardGeneric("frais_passif_load")})
setMethod(
    f = "frais_passif_load",
    signature = "character",
    definition = function(file_frais_passif_address){
        temp           <- read.csv2(file_frais_passif_address)
        frais_passifs  <- new(Class = "FraisPassif", mp = temp)
        return(frais_passifs)
    }
)
