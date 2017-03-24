#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamTableRach
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe de parametres pour les table de rachat \code{ParamTableRach}.
##'
##' Une classe de parametres pour les tables de rachat.
##' @name ParamTableMort
##' @slot age_min un entier correspondant a l'age minimal de la table.
##' @slot age_max un entier correspondant a l'age maximal de la table.
##' @slot anc_min un entier correspondant a la premiere anciennete de la table.
##' @slot anc_max un entier correspondant a la derniere anciennete de la table.
##' @slot table un \code{data frame} contenant la table de rachat.
##' @details  Une table de rachat peut etre une table de rachat partiel ou une table de rachat total.
##' Pour une table de rachat partiel, les taux de rachat sont exprimes en pourcentage de l'encours.
##' Pour une table de rachat total, les taux de rachat sont exprimes en pourcentage du nombre de contrats.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul du taux de rachat \code{\link{calc_rach}}.
##' @keywords classes
##' @export

setClass(
  Class = "ParamTableRach",
  slots = c(age_min = "integer",
            age_max = "integer",
            anc_min = "integer",
            anc_max = "integer",
            table = "data.frame"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL
    nb_col_attentu <- 3

    #Verification du nombre de colonnes
    if(dim(object@table)[2]!=nb_col_attentu) {retval <- c(retval, "[ParamTableRach] : Nombre d'attributs incorrect /n")}

    # Verification du type des colonnes
    if (!is.integer(object@table[,1]))   {retval <- c(retval, "[ParamTableRach] : anc n'est pas entier/n")}
    if (!is.integer(object@table[,2]))   {retval <- c(retval, "[ParamTableRach] : age n'est pas entier/n")}
    if (!is.double(object@table[,3]))   {retval <- c(retval, "[ParamTableRach] : taux_rachat n'est pas un double /n")}

    if (sum(object@table[,1] < 0) != 0)            {retval <- c(retval, "[ParamTableRach] : anc < 0 /n")}
    if (sum(object@table[,2] <= 0) != 0)          {retval <- c(retval, "[ParamTableRach] : age <= 0 /n")}
    if (sum(object@table[,3] < 0) != 0)          {retval <- c(retval, "[ParamTableRach] : taux_rachat < 0 /n")}

    # Verification du nom des colonnes
    if(sum(colnames(object@table) == c("anc","age","taux_rachat")) != 3)
    {retval <- c(retval, "[ParamTableRach] : Noms de colonne incorrect/n")}

    if (is.null(retval)) return (TRUE)
    else return (retval)
    })
