#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe TabRetEuroRest
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{TabRetEuroRest}.
##'
##' Une classe pour le stockage en memoire de variable de calcul au niveau du model point de retraite euro en phase de restitution \code{\link{TabRetEuroRest}}.
##'
##' @name TabRetEuroRest
##' @slot tab un objet \code{list} au format fige contenant l'ensemble des variables stockees.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(
    Class = "TabRetEuroRest",
    slots = c(tab = "list"),
    validity = function (object){
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_var_attentu <- 8
        # Verification du nombre de colonnes
        if(length(object@tab) != nb_var_attentu){
            retval <- c(retval, "[TabRetEuroRest] : Nombre d'elements incorrects\n")
        }

        # Verification du type de variables de la liste
        if (!is.numeric(object@tab[[1]]))   {retval <- c(retval, "[TabRetEuroRest] : num_mp n'est pas entier\n")}
        if (!is.numeric(object@tab[[2]]))   {retval <- c(retval, "[TabRetEuroRest] : prest n'est pas numeric\n")}
        if (!is.numeric(object@tab[[3]]))   {retval <- c(retval, "[TabRetEuroRest] : pm_deb n'est pas numeric\n")}
        if (!is.numeric(object@tab[[4]]))   {retval <- c(retval, "[TabRetEuroRest] : pm_fin n'est pas numeric\n")}
        if (!is.numeric(object@tab[[5]]))   {retval <- c(retval, "[TabRetEuroRest] : bes_tx_cible n'est pas numeric\n")}
        if (!is.numeric(object@tab[[6]]))   {retval <- c(retval, "[TabRetEuroRest] : nb_contr n'est pas numeric\n")}
        if (!is.numeric(object@tab[[7]]))  {retval <- c(retval, "[TabRetEuroRest] : tx_cible n'est pas numeric\n")}
        if (!is.numeric(object@tab[[8]]))  {retval <- c(retval, "[TabRetEuroRest] : pm_gar n'est pas numeric\n")}


        # Verification du nom des colonnes
        if(sum(names(object@tab) == c("num_mp","prest","pm_deb","pm_fin","bes_tx_cible",
                                      "nb_contr", "tx_cible", "pm_gar")) != nb_var_attentu){
            retval <- c(retval, "[TabRetEuroRest] : Noms de colonne incorrect \n")
        }

        # Resultats du controle
        if (is.null(retval)){
            return (TRUE)
        }else{
            return (retval)
        }
    }
)
