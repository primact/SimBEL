#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamTableMort
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe de parametres pour les tables de mortalite \code{ParamTableMort}.
##'
##' Une classe de parametres pour les tables de mortalite.
##' @name ParamTableMort
##' @slot age_min un entier correspondant a l'age minimal de la table.
##' @slot age_max un entier correspondant a l'age maximal de la table.
##' @slot gen_min un entier correspondant a la premiere generation de la table.
##' @slot gen_max un entier correspondant a la derniere generation de la table.
##' @slot table un \code{data frame} contenant la table de mortalite.
##' @note Les tables de mortalite peuvent contenir des probabilites de deces par age (qx) ou le nombre de vivants par age (lx).
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul du taux de deces \code{\link{calc_qx}}.
##' @keywords classes
##' @export

setClass(
    Class = "ParamTableMort",
    slots = c(age_min = "integer",
              age_max = "integer",
              gen_min = "integer",
              gen_max = "integer",
              table = "data.frame"),
    validity = function (object){
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_col_attentu <- 4
        
        # Gestion des noms de colonnes du data.frame
        nom_table <- names(object@table)
        lx_num   <- which(nom_table == "lx")
        qx_num   <- which(nom_table == "qx")
        
        #Verification du nombre de colonnes
        if(dim(object@table)[2]!=nb_col_attentu) {retval <- c(retval, "[ParamTableMort] : Nombre d'attributs incorrect /n")}
        
        # Verification du type des colonnes
        if (!is.integer(object@table[,1]))          {retval <- c(retval, "[ParamTableMort] : gen n'est pas entier/n")}
        if (!is.integer(object@table[,2]))          {retval <- c(retval, "[ParamTableMort] : age n'est pas entier/n")}
        if (!is.numeric(object@table[,lx_num]))     {retval <- c(retval, "[ParamTableMort] : lx n'est pas un reel /n")}
        if (!is.numeric(object@table[,qx_num]))     {retval <- c(retval, "[ParamTableMort] : qx n'est pas un reel /n")}
        
        if (sum(object@table[,1] <= 0) != 0)        {retval <- c(retval, "[ParamTableMort] : gen <= 0 /n")}
        if (sum(object@table[,2] < 0) != 0)         {retval <- c(retval, "[ParamTableMort] : age < 0 /n")}
        if (sum(object@table[,lx_num] < 0) != 0)    {retval <- c(retval, "[ParamTableMort] : lx < 0 /n")}
        if (sum(object@table[,qx_num] < 0) != 0)    {retval <- c(retval, "[ParamTableMort] : qx < 0 /n")}
        
        # Verification du nom des colonnes
        if(sum(colnames(object@table) == c("gen","age","lx", "qx")) != nb_col_attentu)
        {retval <- c(retval, "[ParamTableMort] : Noms de colonne incorrect/n")}
        
        if (is.null(retval)) return (TRUE)
        else return (retval)
    })