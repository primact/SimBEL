#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des donnnes et des hypotheses de la PPB
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger les valeurs des hypotheses et des donnees de PPB
##'
##' \code{ppb_load} est une methode permettant de charger les parametres associees a un
##' objet de classe \code{\link{Ppb}}.
##' @name ppb_load
##' @docType methods
##' @param file_ppb_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur
##' permettant de renseigner un objet \code{\link{Ppb}}.
##' @return L'objet de la classe \code{\link{Ppb}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}} pour renseigner l'input.
##' @export
##' @include HypCanton_class.R
setGeneric(name = "ppb_load", def = function(file_ppb_address) {
    standardGeneric("ppb_load")
})
setMethod(
    f = "ppb_load",
    signature = "character",
    definition = function(file_ppb_address) {
        # Lecture du fichier
        temp <- read.csv2(file_ppb_address, colClasses = rep("numeric", 3))

        # Tests
        if (!(all(!is.na(temp[, "hist_ppb"])) & all(!is.na(temp[1L, c("seuil_rep", "seuil_dot")])))) {
            stop("[PPB - load] : Presence de NA dans le fichier d'input")
        }

        # creation de l'objet
        ppb <- new("Ppb",
            hist_ppb = temp[, "hist_ppb"],
            seuil_rep = temp[1L, "seuil_rep"],
            seuil_dot = temp[1L, "seuil_dot"]
        )

        # Output
        return(ppb)
    }
)
