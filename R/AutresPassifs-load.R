#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des donnees des autres passifs
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger la valeur initiale des autres passifs.
##'
##' \code{autres_passif_load} est une methode permettant de charger les donnees associees a un
##' objet de classe \code{\link{AutresPassifs}}.
##' @name autres_passif_load
##' @docType methods
##' @param file_autres_passif_address est un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur permettant de renseigner un objet \code{\link{AutresPassifs}}.
##' @return L'objet de la classe \code{\link{AutresPassifs}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l'input.
##' @export
##' @include AutresPassifs-class.R

setGeneric(name = "autres_passif_load", def = function(file_autres_passif_address) {
    standardGeneric("autres_passif_load")
})
setMethod(
    f = "autres_passif_load",
    signature(file_autres_passif_address = "character"),
    definition = function(file_autres_passif_address) {
        # Lecture du fichier
        temp <- read.csv2(file_autres_passif_address, header = TRUE, colClasses = c(
            "numeric", "numeric", "numeric",
            "numeric", "numeric", "numeric",
            "numeric"
        ))

        # Tests
        if (!all(!is.na(temp))) {
            stop("[AutresPassifs - load] : Presence de NA dans le fichier d'input")
        }

        # Creation de l'objet
        if (nrow(temp) != 0L) {
            autres_passifs <- new("AutresPassifs", mp = temp)
        } else {
            autres_passifs <- new("AutresPassifs")
        }

        # Output
        return(autres_passifs)
    }
)
