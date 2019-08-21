#----------------------------------------------------------------------------------------------------------------------------------------------------
#           chargement_choc
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet de charger les parametres de choc de la formule standard.
##'
##' \code{chargement_choc} est une methode permettant de charger les parametres
##' l'ensemble des parametres necessaires a la bonne application des chocs de marche et
##' de souscription au sens de la formule standard de la directive Solvabilite 2,
##'  tels que renseignes par l'utilisateur.
##' @name chargement_choc
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param folder_chocs_address est un \code{character}. Cette chaine de caractere est construite par la methode
##'  \code{\link{set_architecture}} de la classe \code{\link{Initialisation}}.
##' Elle contient l'adresse du dossier contenant les fichiers de parametres des chocs de la formule standard a appliquer.
##' Ces derniers doivent etre renseignes par l'utilisateur.
##' @return \code{x} l'objet  de la classe \\code{\link{ChocSolvabilite2}} dont les attributs \code{param_choc_mket}
##'  et \code{param_choc_sousc} ont ete mis a jour.
##' @author Prim'Act
##' @export
##' @seealso La creation de l'architecture de chargement des donnees et parametres renseignes par l'utilisateur
##'  \code{\link{set_architecture}},
##' ainsi que les classes \code{\link{ParamChocMket}} et \code{\link{ParamChocSousc}}.
##' @include ChocSolvabilite2_class.R

setGeneric(name = "chargement_choc", def = function(x, folder_chocs_address){standardGeneric("chargement_choc")})
setMethod(
    f = "chargement_choc",
    signature = c(x = "ChocSolvabilite2", folder_chocs_address = "character"),
    definition = function(x, folder_chocs_address){

        # Lecture des chocs Action, Immo, Spread
        table_choc_action_type1 <- read.csv2(paste(folder_chocs_address, "param_choc_mket_action_type1.csv", sep = "/"), colClasses = c("integer", "numeric"))
        table_choc_action_type2 <- read.csv2(paste(folder_chocs_address, "param_choc_mket_action_type2.csv", sep = "/"), colClasses = c("integer", "numeric"))
        table_choc_immo   <- read.csv2(paste(folder_chocs_address, "param_choc_mket_immo.csv", sep = "/"), colClasses = c("integer", "numeric"))
        table_choc_spread <- read.csv2(paste(folder_chocs_address, "param_choc_mket_spread.csv", sep = "/"), colClasses = c("integer", "character", "numeric", "numeric"))
        table_choc_currency <- read.csv2(paste(folder_chocs_address, "param_choc_mket_currency.csv", sep = "/"), colClasses = c("character", "logical", "numeric", "numeric"))

        # Tests
        if ((! all(! is.na(table_choc_action_type1))) | (! all(! is.na(table_choc_action_type2))) |
            (! all(! is.na(table_choc_immo))) | (! all(! is.na(table_choc_spread))) | (! all(! is.na(table_choc_currency)))
            )
            stop("[ChocSolvabilite2 - load] : Presence de NA dans un des fichiers d'input")

        # Creation de l'objet
        x@param_choc_mket <- new("ParamChocMket",
                                 table_choc_action_type1 = table_choc_action_type1,
                                 table_choc_action_type2 = table_choc_action_type2,
                                 table_choc_immo         = table_choc_immo,
                                 table_choc_spread       = table_choc_spread,
                                 table_choc_currency     = table_choc_currency
                                 )


        # Lecture fichier choc souscrption
        table_choc_sousc <- read.csv2(paste(folder_chocs_address, "param_choc_sousc.csv", sep = "/"))

        # Tests
        if (! all(! is.na(table_choc_sousc)))
            stop("[ChocSolvabilite2 - load] : Presence de NA dans un des fichiers d'input")

        # Creation de l'objet
        x@param_choc_sousc <- new("ParamChocSousc", table_choc_sousc)

        # Output
        return(x)
    }
)
