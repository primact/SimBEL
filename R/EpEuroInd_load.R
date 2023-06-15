#----------------------------------------------------------------------------------------------------------------------------------------------------
#           load_epeuroind
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger un portefeuille epargne initital dans un objet de type \code{\link{EpEuroInd}}.
##'
##' \code{load_epeuroind} est une methode permettant de charger un portefeuille epargne.
##' @name load_epeuroind
##' @docType methods
##' @param file_epeuroind_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur permettant de renseigner le
##' portefeuille.
##' @return L'objet de la classe \code{\link{Action}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @seealso La classe \code{\link{PortPassif}} et sa methode \code{\link{load_pp}}.
##' @include Action_class.R
setGeneric(name = "load_epeuroind", def = function(file_epeuroind_address) {
    standardGeneric("load_epeuroind")
})
setMethod(
    f = "load_epeuroind",
    signature = "character",
    definition = function(file_epeuroind_address) {
        # Lecture du fichier
        temp <- read.csv2(file_epeuroind_address, header = TRUE, colClasses = c(
            "integer",
            "integer",
            "integer",
            "integer",
            "integer",
            "factor",
            "numeric",
            "logical",
            "numeric",
            "numeric",
            "integer",
            "integer",
            "factor",
            "factor",
            "factor",
            "numeric",
            "numeric",
            "numeric",
            "integer",
            "numeric",
            "integer",
            "factor",
            "factor",
            "factor",
            "factor",
            "numeric",
            "numeric",
            "numeric",
            "numeric"
        ))

        # Tests
        if (!all(!is.na(temp))) {
            stop("[PortPassif - load] : Presence de NA dans un fichier d'input d'epargne")
        }

        # Creation de l'objet
        ptf_eei <- new(
            Class = "EpEuroInd",
            mp = temp,
            tab = new("TabEpEuroInd"),
            tab_proba = new("TabProbaEpEuroInd", temp["num_mp"])
        )

        # Output
        return(ptf_eei)
    }
)
