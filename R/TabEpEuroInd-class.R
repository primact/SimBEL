#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe TabEpEuroInd
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{TabEpEuroInd}.
##'
##' Une classe pour le stockage en memoire de variable de calcul au niveau du model point \code{\link{EpEuroInd}}.
##'
##' @name TabEpEuroInd
##' @slot tab un objet \code{list} au format fige contenant l'ensemble des variables stockees.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(
    Class = "TabEpEuroInd",
    slots = c(tab = "list"),
    validity = function(object) {
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_var_attentu <- 12
        # Verification du nombre de colonnes
        if (length(object@tab) != nb_var_attentu) {
            retval <- c(retval, "[TabEpEuroInd] : Nombre d'elements incorrects\n")
        }

        # Verification du type de variables de la liste
        if (!is.numeric(object@tab[[1]])) {
            retval <- c(retval, "[TabEpEuroInd] : num_mp n'est pas entier\n")
        }
        if (!is.numeric(object@tab[[2]])) {
            retval <- c(retval, "[TabEpEuroInd] : pri_net n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[3]])) {
            retval <- c(retval, "[TabEpEuroInd] : prest n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[4]])) {
            retval <- c(retval, "[TabEpEuroInd] : pm_deb n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[5]])) {
            retval <- c(retval, "[TabEpEuroInd] : pm_fin n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[6]])) {
            retval <- c(retval, "[TabEpEuroInd] : enc_charg_base_th n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[7]])) {
            retval <- c(retval, "[TabEpEuroInd] : enc_charg_rmin_th n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[8]])) {
            retval <- c(retval, "[TabEpEuroInd] : rev_stock_brut n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[9]])) {
            retval <- c(retval, "[TabEpEuroInd] : bes_tx_cible n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[10]])) {
            retval <- c(retval, "[TabEpEuroInd] : nb_contr n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[11]])) {
            retval <- c(retval, "[TabEpEuroInd] : tx_cible n'est pas numeric\n")
        }
        if (!is.numeric(object@tab[[12]])) {
            retval <- c(retval, "[TabEpEuroInd] : pm_gar n'est pas numeric\n")
        }


        # Verification du nom des colonnes
        if (sum(names(object@tab) == c(
            "num_mp", "pri_net", "prest", "pm_deb", "pm_fin", "enc_charg_base_th",
            "enc_charg_rmin_th", "rev_stock_brut", "bes_tx_cible",
            "nb_contr", "tx_cible", "pm_gar"
        )) != nb_var_attentu) {
            retval <- c(retval, "[TabEpEuroInd] : Noms de colonne incorrect \n")
        }

        # Resultats du controle
        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)
