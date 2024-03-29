# 10/03/2017 Guillaume de Kervenoael, Quentin Guibert

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           frais_fin_load
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger la valeur initiale des frais financiers dans un objet de type FraisFin.
##'
##' \code{frais_fin_load} est une methode permettant de charger les frais financiers.
##' @name frais_fin_load
##' @docType methods
##' @param file_frais_fin_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur permettant de renseigner les Frais
##' financier.
##' @return L'objet de la classe \code{FraisFin} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}} pour renseigner l'input.
##' @include FraisFin_class.R
setGeneric(name = "frais_fin_load", def = function(file_frais_fin_address) {
    standardGeneric("frais_fin_load")
})
setMethod(
    f = "frais_fin_load",
    signature = "character",
    definition = function(file_frais_fin_address) {
        # Lecture du fichier
        temp <- read.csv2(file_frais_fin_address, colClasses = c("numeric", "logical"))

        # Tests
        if (!all(!is.na(temp))) {
            stop("[FraisFin - load] : Presence de NA dans le fichier d'input.")
        }

        # Creation de l'objet
        frais_fin <- new("FraisFin", tx_chargement = temp[, "tx_chargement"], indicatrice_inflation = temp[, "indicatrice_inflation"])

        # Output
        return(frais_fin)
    }
)
