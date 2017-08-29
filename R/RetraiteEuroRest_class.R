#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe RetraiteEuroRest
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{RetraiteEuroRest}.
##'
##' Une classe pour les passifs de retraite en euros en phase de restitution.
##'
##' @name RetraiteEuroRest
##' @slot mp un objet \code{data.frame} au format fige contenant l'ensemble de model points retraite euros en phase de restitution.
##' @slot tab un objet de la classe \code{\link{TabRetEuroRest}} dedie au stockage de variables intermediaires.
##' @slot tab_proba un objet de la classe \code{\link{TabProbaRetEuroRest}} dedie au stockage des probabilites associees aux model point.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul des prestations et des PM : \code{\link{calc_prest}}, \code{\link{calc_pm}}.
##' Le calcul du taux minimum et des taux cible de revalorisation :
##' \code{\link{calc_tx_min}}, \code{\link{calc_tx_cible}}.
##' La revalorisation des PM apres participation aux benefices : \code{\link{calc_revalo_pm}}.
##' Le vieillissement des model points sur une periode : \code{\link{vieilli_mp}}.
##' @keywords classes
##' @export
##' @include TabRetEuroRest_class.R TabProbaRetEuroRest-class.R
setClass(
    Class = "RetraiteEuroRest",
    slots = c(mp = "data.frame", tab = "TabRetEuroRest", tab_proba = "TabProbaRetEuroRest"),
    validity = function (object){
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_col_attendu <- 21L
        # Verification du nombre de colonnes
        if(ncol(object@mp) != nb_col_attendu){
            retval <- c(retval, "[RetraiteEuroRest] : Nombre d'attributs incorrect, un ptf RetraiteEuroRest est compose d'un DF de 21 colonnes\n")
        }

        # Verification du type des colonnes
        if (!is.integer(.subset2(object@mp, 1L)))  {retval <- c(retval, "[RetraiteEuroRest] : num_mp n'est pas entier\n")}
        if (!is.integer(.subset2(object@mp, 2L)))  {retval <- c(retval, "[RetraiteEuroRest] : num_canton n'est pas entier\n")}
        if (!is.integer(.subset2(object@mp, 3L)))  {retval <- c(retval, "[RetraiteEuroRest] : num_prod n'est pas entier\n")}
        if (!is.integer(.subset2(object@mp, 4L)))  {retval <- c(retval, "[RetraiteEuroRest] : age n'est pas entier\n")}
        if (!is.integer(.subset2(object@mp, 5L)))  {retval <- c(retval, "[RetraiteEuroRest] : gen n'est pas entier\n")}
        if (!is.factor(.subset2(object@mp, 6L)))   {retval <- c(retval, "[RetraiteEuroRest] : num_tab_mort n'est pas factor\n")}
        if (!is.numeric(.subset2(object@mp, 7L)))  {retval <- c(retval, "[RetraiteEuroRest] : pm n'est pas numeric\n")}
        if (!is.numeric(.subset2(object@mp, 8L)))  {retval <- c(retval, "[RetraiteEuroRest] : nb_contr n'est pas numeric\n")}
        if (!is.integer(.subset2(object@mp, 9L)))  {retval <- c(retval, "[RetraiteEuroRest] : statut_rvs n'est pas entier\n")}
        if (!is.integer(.subset2(object@mp, 10L))) {retval <- c(retval, "[RetraiteEuroRest] : age_rvs n'est pas entier\n")}
        if (!is.integer(.subset2(object@mp, 11L))) {retval <- c(retval, "[RetraiteEuroRest] : gen_rvs n'est pas entier\n")}
        if (!is.factor(.subset2(object@mp, 12L)))  {retval <- c(retval, "[RetraiteEuroRest] : num_tab_mort_rvs n'est pas factor\n")}
        if (!is.numeric(.subset2(object@mp, 13L))) {retval <- c(retval, "[RetraiteEuroRest] : tx_rvs n'est pas numeric\n")}
        if (!is.numeric(.subset2(object@mp, 14L))) {retval <- c(retval, "[RetraiteEuroRest] : tx_tech n'est pas numeric\n")}
        if (!is.factor(.subset2(object@mp, 15L)))  {retval <- c(retval, "[RetraiteEuroRest] : tx_cible n'est pas numeric\n")}
        if (!is.numeric(.subset2(object@mp, 16L))) {retval <- c(retval, "[RetraiteEuroRest] : freq_rente n'est pas factor\n")}
        if (!is.numeric(.subset2(object@mp, 17L))) {retval <- c(retval, "[RetraiteEuroRest] : rente n'est pas numeric\n")}
        if (!is.numeric(.subset2(object@mp, 18L))) {retval <- c(retval, "[RetraiteEuroRest] : rente_gar n'est pas numeric\n")}
        if (!is.numeric(.subset2(object@mp, 19L))) {retval <- c(retval, "[RetraiteEuroRest] : ch_arr n'est pas numeric\n")}
        if (!is.logical(.subset2(object@mp, 20L))) {retval <- c(retval, "[RetraiteEuroRest] : echu n'est pas logical\n")}
        if (!is.numeric(.subset2(object@mp, 21L))) {retval <- c(retval, "[RetraiteEuroRest] : tx_cible_prec n'est pas numeric\n")}


        # Verification du nom des colonnes
        if(! all(colnames(object@mp) == c("num_mp", "num_canton", "num_prod", "age", "gen", "num_tab_mort", "pm",
                                        "nb_contr", "statut_rvs", "age_rvs", "gen_rvs", "num_tab_mort_rvs", "tx_rvs",
                                        "tx_tech", "tx_cible", "freq_rente", "rente", "rente_gar", "ch_arr", "echu",
                                        "tx_cible_prec"))){
            retval <- c(retval, "[RetraiteEuroRest] : Noms de colonne incorrect \n")
        }

        # Verification des tables de resultats et de probas
        if(! validObject(object@tab)){retval <- c(retval, "[RetraiteEuroRest] : Objet 'tab' non valide")}
        if(! validObject(object@tab_proba)){retval <- c(retval, "[RetraiteEuroRest] : Objet 'tab_proba' non valide")}

        # Resultats du controle
        if (is.null(retval)){
            return (TRUE)
        }else{
            return (retval)
        }
    }
)
