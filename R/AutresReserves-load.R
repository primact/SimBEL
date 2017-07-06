#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des donnees des autres reserves
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger la valeur initiale de la PSAP et de la PGG.
##'
##' \code{autres_reserves_load} est une methode permettant de charger les donnees associees a un
##' objet de classe \code{\link{AutresReserves}}.
##' @name autres_reserves_load
##' @docType methods
##' @param file_autres_reserves_address est un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur
##' permettant de renseigner un objet \code{\link{AutresReserves}}.
##' @return L'objet de la classe \code{\link{AutresReserves}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l’input.
##' @export
##' @include AutresReserves-class.R
##'
setGeneric(name = "autres_reserves_load", def = function(file_autres_reserves_address){standardGeneric("autres_reserves_load")})
setMethod(
    f = "autres_reserves_load",
    signature = c(file_autres_reserves_address = "character"),
    definition = function(file_autres_reserves_address){

        # Lecture du fichier
        temp            <- read.csv2(file_autres_reserves_address)

        # Creation de l'objet
        autres_reserves <- new("AutresReserves",
                               pgg_debut      = temp[,"pgg_debut"],
                               psap_debut     = temp[,"psap_debut"],
                               pgg_valeur     = temp[,"pgg_valeur"],
                               psap_valeur    = temp[,"psap_valeur"],
                               tx_pgg_ep      = temp[,"tx_pgg_ep"],
                               tx_pgg_autres  = temp[,"tx_pgg_autres"],
                               tx_psap_ep     = temp[,"tx_psap_ep"],
                               tx_psap_autres = temp[,"tx_psap_autres"])

        # Output
        return(autres_reserves)
    }
)
