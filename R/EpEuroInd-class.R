#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe EpEuroInd
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{EpEuroInd}.
##'
##' Une classe pour les passifs epargne en euros.
##'
##' @name EpEuroInd
##' @slot mp un objet \code{data.frame} au format fige contenant l'ensemble de model points epargne en euros.
##' @slot tab un objet de la classe \code{\link{TabEpEuroInd}} dedie au stockage de variables intermediaires.
##' @slot tab_proba un objet de la classe \code{\link{TabProbaEpEuroInd}} dedie au stockage des probabilites.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul des primes, des prestations et des PM : \code{\link{calc_primes}},
##' \code{\link{calc_prest}}, \code{\link{calc_pm}}.
##' Le calcul du taux minimum et des taux cible de revalorisation : \code{\link{calc_tx_min}}, \code{\link{calc_tx_cible}}.
##' La revalorisation des PM apres participation aux benefices : \code{\link{calc_revalo_pm}}.
##' Le vieillissement des model points sur une periode : \code{\link{vieilli_mp}}.
##' @keywords classes
##' @export
##' @include TabEpEuroInd-class.R TabProbaEpEuroInd-class.R
setClass(
    Class = "EpEuroInd",
    slots = c(mp = "data.frame", tab = "TabEpEuroInd", tab_proba = "TabProbaEpEuroInd"),
    validity = function(object) {
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_col_attentu <- 29L
        # Verification du nombre de colonnes
        if (ncol(object@mp) != nb_col_attentu) {
            retval <- c(retval, "[EpEuroInd] : Nombre d'attributs incorrect, un ptf EpEuroInd est compose d'un DF de 29 colonnes\n")
        }

        # Verification du type des colonnes
        if (!is.integer(.subset2(object@mp, 1L))) {
            retval <- c(retval, "[EpEuroInd] : num_mp n'est pas entier\n")
        }
        if (!is.integer(.subset2(object@mp, 2L))) {
            retval <- c(retval, "[EpEuroInd] : num_canton n'est pas entier\n")
        }
        if (!is.integer(.subset2(object@mp, 3L))) {
            retval <- c(retval, "[EpEuroInd] : num_prod n'est pas entier\n")
        }
        if (!is.integer(.subset2(object@mp, 4L))) {
            retval <- c(retval, "[EpEuroInd] : age n'est pas entier\n")
        }
        if (!is.integer(.subset2(object@mp, 5L))) {
            retval <- c(retval, "[EpEuroInd] : gen n'est pas entier\n")
        }
        if (!is.factor(.subset2(object@mp, 6L))) {
            retval <- c(retval, "[EpEuroInd] : num_tab_mort n'est pas factor\n")
        }
        if (!is.numeric(.subset2(object@mp, 7L))) {
            retval <- c(retval, "[EpEuroInd] : chgt_enc n'est pas numerique\n")
        }
        if (!is.logical(.subset2(object@mp, 8L))) {
            retval <- c(retval, "[EpEuroInd] : ind_chgt_enc_pos n'est pas logical\n")
        }
        if (!is.numeric(.subset2(object@mp, 9L))) {
            retval <- c(retval, "[EpEuroInd] : pm n'est pas numerique\n")
        }
        if (!is.numeric(.subset2(object@mp, 10L))) {
            retval <- c(retval, "[EpEuroInd] :  nb_contr n'est pas numeric\n")
        }
        if (!is.integer(.subset2(object@mp, 11L))) {
            retval <- c(retval, "[EpEuroInd] : anc n'est pas entier\n")
        }
        if (!is.integer(.subset2(object@mp, 12L))) {
            retval <- c(retval, "[EpEuroInd] : terme n'est pas entier\n")
        }
        if (!is.factor(.subset2(object@mp, 13L))) {
            retval <- c(retval, "[EpEuroInd] : type_cot n'est pas factor\n")
        }
        if (!is.factor(.subset2(object@mp, 14L))) {
            retval <- c(retval, "[EpEuroInd] : periode_cot n'est pas factor\n")
        }
        if (!is.factor(.subset2(object@mp, 15L))) {
            retval <- c(retval, "[EpEuroInd] : tx_cible n'est pas factor\n")
        }
        if (!is.numeric(.subset2(object@mp, 16L))) {
            retval <- c(retval, "[EpEuroInd] : chgt_prime n'est pas numeric\n")
        }
        if (!is.numeric(.subset2(object@mp, 17L))) {
            retval <- c(retval, "[EpEuroInd] : prime n'est pas numeric\n")
        }
        if (!is.numeric(.subset2(object@mp, 18L))) {
            retval <- c(retval, "[EpEuroInd] : tx_tech n'est pas numeric\n")
        }
        if (!is.integer(.subset2(object@mp, 19L))) {
            retval <- c(retval, "[EpEuroInd] : terme_tx_tech n'est pas integer\n")
        }
        if (!is.numeric(.subset2(object@mp, 20L))) {
            retval <- c(retval, "[EpEuroInd] : tmg n'est pas numeric\n")
        }
        if (!is.integer(.subset2(object@mp, 21L))) {
            retval <- c(retval, "[EpEuroInd] : terme_tmg n'est pas integer\n")
        }
        if (!is.factor(.subset2(object@mp, 22L))) {
            retval <- c(retval, "[EpEuroInd] : num_rach_tot n'est pas factor\n")
        }
        if (!is.factor(.subset2(object@mp, 23L))) {
            retval <- c(retval, "[EpEuroInd] : num_rach_part n'est pas factor\n")
        }
        if (!is.factor(.subset2(object@mp, 24L))) {
            retval <- c(retval, "[EpEuroInd] : num_rach_dyn_tot n'est pas factor\n")
        }
        if (!is.factor(.subset2(object@mp, 25L))) {
            retval <- c(retval, "[EpEuroInd] : num_rach_dyn_part n'est pas factor\n")
        }
        if (!is.numeric(.subset2(object@mp, 26L))) {
            retval <- c(retval, "[EpEuroInd] : chgt_rach n'est pas numeric\n")
        }
        if (!is.numeric(.subset2(object@mp, 27L))) {
            retval <- c(retval, "[EpEuroInd] : pm_gar n'est pas numeric\n")
        }
        if (!is.numeric(.subset2(object@mp, 28L))) {
            retval <- c(retval, "[EpEuroInd] : tx_revalo_prec n'est pas numeric\n")
        }
        if (!is.numeric(.subset2(object@mp, 29L))) {
            retval <- c(retval, "[EpEuroInd] : tx_cible_prec n'est pas numeric\n")
        }

        # Verification du nom des colonnes
        if (!all(colnames(object@mp) == c(
            "num_mp", "num_canton", "num_prod", "age", "gen", "num_tab_mort",
            "chgt_enc", "ind_chgt_enc_pos", "pm", "nb_contr", "anc", "terme", "type_cot",
            "periode_cot", "tx_cible", "chgt_prime", "prime", "tx_tech", "terme_tx_tech",
            "tmg", "terme_tmg", "num_rach_tot", "num_rach_part", "num_rach_dyn_tot", "num_rach_dyn_part",
            "chgt_rach", "pm_gar", "tx_revalo_prec", "tx_cible_prec"
        ))) {
            retval <- c(retval, "[EpEuroInd] : Noms de colonne incorrect \n")
        }

        # Verification des tables de resultats et de probas
        if (!validObject(object@tab)) {
            retval <- c(retval, "[EpEuroInd] : Objet 'tab' non valide")
        }
        if (!validObject(object@tab_proba)) {
            retval <- c(retval, "[EpEuroInd] : Objet 'tab_proba' non valide")
        }

        # Resultats du controle
        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)
