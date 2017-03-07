#----------------------------------------------------------
# Ce script est la definition de la classe ParamTableMort dedie aux tables de mortalite
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamTableMort
#----------------------------------------------------------------------------------------------------------------------------------------------------


##' La classe ParamTableMort
##'
##' Classe pour pour les tables de mortalite
##'
##' @name ParamTableMort
##' @slot table un data frame contenant les taux de mortalite
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
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
    nb_col_attentu <- 3

    #Verification du nombre de colonnes
    if(dim(object@table)[2]!=nb_col_attentu) {retval <- c(retval, "[ParamTableMort] : Nombre d'attributs incorrect /n")}

    # Verification du type des colonnes
    if (!is.integer(object@table[,1]))   {retval <- c(retval, "[ParamTableMort] : gen n'est pas entier/n")}
    if (!is.integer(object@table[,2]))   {retval <- c(retval, "[ParamTableMort] : age n'est pas entier/n")}
    if (!is.numeric(object@table[,3]))   {retval <- c(retval, "[ParamTableMort] : lx n'est pas un reel /n")}

    if (sum(object@table[,1] <= 0) != 0)            {retval <- c(retval, "[ParamTableMort] : gen <= 0 /n")}
    if (sum(object@table[,2] < 0) != 0)          {retval <- c(retval, "[ParamTableMort] : age < 0 /n")}
    if (sum(object@table[,3] < 0) != 0)          {retval <- c(retval, "[ParamTableMort] : lx < 0 /n")}

    # Verification du nom des colonnes
    if(sum(colnames(object@table) == c("gen","age","lx")) != 3)
    {retval <- c(retval, "[ParamTableMort] : Noms de colonne incorrect/n")}

    if (is.null(retval)) return (TRUE)
    else return (retval)
    })









