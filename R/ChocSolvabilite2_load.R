#----------------------------------------------------------------------------------------------------------------------------------------------------
#           chargement_choc
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet de charger les parametres de choc de la formule standard.
##'
##' \code{chargement_choc} est une methode permettant de charger
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
##' @return \code{x} l'objet  de la classe \\code{\link{ChocSolvabilite2}} dont les attributs \code{scenario}, \code{param_choc_mket}
##'  et \code{param_choc_sousc} ont ete mis a jour.
##' @author Prim'Act
##' @export
##' @seealso La creation de l'architecture de chargement des donnees et parametres renseignes par l'utilisateur
##'  \code{\link{set_architecture}},
##' ainsi que les classes \code{\link{ParamChocMket}} et \code{\link{ParamChocSousc}}.
##' @include ChocSolvabilite2_class.R

setGeneric(name = "chargement_choc", def = function(x, folder_chocs_address) {
  standardGeneric("chargement_choc")
})
setMethod(
  f = "chargement_choc",
  signature = c(x = "ChocSolvabilite2", folder_chocs_address = "character"),
  definition = function(x, folder_chocs_address) {
    # Lecture de la liste des chocs
    scenario <- read.csv2(paste(folder_chocs_address, "list_run_choc.csv", sep = "/"), colClasses = c("character", "logical"))
    # Scenarios de chocs a considerer
    scenario <- scenario$nom_choc[scenario$run_choc]

    # Scenario doit au moins comprendre un element
    if (length(scenario) == 0) {
      stop("[ChocSolvabilite2] : la liste de scenarios doit contenir au moins un element")
    }

    # Verifie les noms de scenarios
    names_accepted <- c(
      "central", "frais",
      "mortalite", "longevite",
      "rachat_up", "rachat_down", "rachat_mass",
      "action_type1", "action_type2", "immo",
      "taux_up", "taux_down",
      "spread",
      "currency_up", "currency_down"
    )

    if (!all(scenario %in% names_accepted)) {
      stop("[ChocSolvabilite2] : Les noms de scenarios proposes ne sont pas valides")
    }

    # Mise a jour des scenarios
    x@scenario <- scenario

    # Lecture des chocs Action, Immo, Spread, Currency
    table_choc_action_type1 <- read.csv2(
      paste(folder_chocs_address, "param_choc_mket_action_type1.csv", sep = "/"),
      colClasses = c("integer", "numeric")
    )
    table_choc_action_type2 <- read.csv2(
      paste(folder_chocs_address, "param_choc_mket_action_type2.csv", sep = "/"),
      colClasses = c("integer", "numeric")
    )
    table_choc_immo <- read.csv2(
      paste(folder_chocs_address, "param_choc_mket_immo.csv", sep = "/"),
      colClasses = c("integer", "numeric")
    )
    table_choc_spread <- read.csv2(
      paste(folder_chocs_address, "param_choc_mket_spread.csv", sep = "/"),
      colClasses = c("integer", "character", "numeric", "numeric")
    )
    table_choc_currency <- read.csv2(
      paste(folder_chocs_address, "param_choc_mket_currency.csv", sep = "/"),
      colClasses = c("character", "logical", "numeric", "numeric")
    )

    # Tests
    if ((!all(!is.na(table_choc_action_type1))) | (!all(!is.na(table_choc_action_type2))) |
      (!all(!is.na(table_choc_immo))) | (!all(!is.na(table_choc_spread))) | (!all(!is.na(table_choc_currency)))
    ) {
      stop("[ChocSolvabilite2 - load] : Presence de NA dans un des fichiers d'input")
    }

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
    if (!all(!is.na(table_choc_sousc))) {
      stop("[ChocSolvabilite2 - load] : Presence de NA dans un des fichiers d'input")
    }

    # Creation de l'objet
    x@param_choc_sousc <- new("ParamChocSousc", table_choc_sousc)



    # Finalisation de la liste de scenarios en precisant les chocs de devise

    # Liste des scenarios currency
    tab_choc_currency <- x@param_choc_mket@table_choc_currency
    nom_choc_currency <- tab_choc_currency[which(tab_choc_currency$run_choc), "currency"]

    if (length(nom_choc_currency) > 0 & any(c("currency_up", "currency_down") %in% scenario)) { # Si il y a des chocs currency
      if ("currency_up" %in% scenario) {
        rr <- which(scenario == "currency_up")
        scenario <- scenario[-rr] # Retrait du nom de scenario pour le completer de la liste des devises
        scenario <- c(scenario, paste0("currency_up_", nom_choc_currency))
      }
      if ("currency_down" %in% scenario) {
        rr <- which(scenario == "currency_down")
        scenario <- scenario[-rr] # Retrait du nom de scenario pour le completer de la liste des devises
        scenario <- c(scenario, paste0("currency_down_", nom_choc_currency))
      }
    } else { # Suppression des chocs currency appeles a tord
      rr <- which(scenario == "currency_up" | scenario == "currency_down")
      if (length(rr) > 0) {
        scenario <- scenario[-rr]
      }
    }

    # Mise a jour des scenarios
    x@scenario <- scenario

    # Lecture des matrices de correlation action, marche, souscription et bscr -- ajout VB
    matrice_choc_action <- read.csv2(
      paste(folder_chocs_address, "matrices/matrice_choc_action.csv", sep = "/"),
      colClasses = c("numeric", "numeric")
    )
    matrice_choc_mket <- read.csv2(
      paste(folder_chocs_address, "matrices/matrice_choc_mket.csv", sep = "/"),
      colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric")
    )
    matrice_choc_sousc <- read.csv2(
      paste(folder_chocs_address, "matrices/matrice_choc_sousc.csv", sep = "/"),
      colClasses = c("numeric", "numeric", "numeric", "numeric")
    )
    matrice_choc_bscr <- read.csv2(
      paste(folder_chocs_address, "matrices/matrice_choc_bscr.csv", sep = "/"),
      colClasses = c("numeric", "numeric")
    )

    # Tests
    if ((!all(!is.na(matrice_choc_action))) | (!all(!is.na(matrice_choc_mket))) |
      (!all(!is.na(matrice_choc_sousc))) | (!all(!is.na(matrice_choc_bscr)))
    ) {
      stop("[ChocSolvabilite2 - load] : Presence de NA dans un des fichiers d'input")
    }

    # Retraitement des matrices
    matrice_choc_action <- as.matrix(matrice_choc_action)
    rownames(matrice_choc_action) <- colnames(matrice_choc_action)
    matrice_choc_mket <- as.matrix(matrice_choc_mket)
    rownames(matrice_choc_mket) <- colnames(matrice_choc_mket)
    matrice_choc_sousc <- as.matrix(matrice_choc_sousc)
    rownames(matrice_choc_sousc) <- colnames(matrice_choc_sousc)
    matrice_choc_bscr <- as.matrix(matrice_choc_bscr)
    rownames(matrice_choc_bscr) <- colnames(matrice_choc_bscr)

    # Ajout des matrices dans l'objet
    x@matrice_choc_action <- matrice_choc_action
    x@matrice_choc_mket <- matrice_choc_mket
    x@matrice_choc_sousc <- matrice_choc_sousc
    x@matrice_choc_bscr <- matrice_choc_bscr

    # Output
    return(x)
  }
)
