#----------------------------------------------------------
# Ce script est la definition de la classe ParamTableRach dedie aux tables de rachat
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamTableRach
#----------------------------------------------------------------------------------------------------------------------------------------------------


##' La classe ParamTableRach
##'
##' Classe pour pour les tables de rachat
##'
##' @name ParamTableRach
##' @slot table un data frame contenant les taux de rachat
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
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






	


