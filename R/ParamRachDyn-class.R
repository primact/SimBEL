#----------------------------------------------------------
# Ce script est la definition de la classe ParamRachDyn dedie aux tables de parametres pour les rachats dynamiques
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamRachDyn
#----------------------------------------------------------------------------------------------------------------------------------------------------


##' La classe ParamRachDyn
##'
##' Classe pour les tables de parametres pour les rachats dynamiques
##'
##' @name ParamRachDyn
##' @slot vec_param un data frame contenant les parametres pour les rachats dynamiques
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export

setClass(
  Class = "ParamRachDyn",
  slots = c(vec_param = "data.frame"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL
    nb_col_attentu <- 6
    
    #Verification du nombre de colonnes
    if(dim(object@vec_param)[2]!=nb_col_attentu) {retval <- c(retval, "[ParamRachDyn] : Nombre d'attributs incorrect /n")}
    
    # Verification du type des colonnes
    if (!is.double(object@vec_param[,1]))   {retval <- c(retval, "[ParamRachDyn] : alpha n'est pas entier/n")}
    if (!is.double(object@vec_param[,2]))  {retval <- c(retval, "[ParamRachDyn] : beta n'est pas entier/n")}
    if (!is.double(object@vec_param[,3]))   {retval <- c(retval, "[ParamRachDyn] : gamma n'est pas entier/n")}
    if (!is.double(object@vec_param[,4]))   {retval <- c(retval, "[ParamRachDyn] : delta n'est pas entier/n")}
    if (!is.double(object@vec_param[,5]))  {retval <- c(retval, "[ParamRachDyn] : RCMIN n'est pas entier/n")}
    if (!is.double(object@vec_param[,6]))  {retval <- c(retval, "[ParamRachDyn] : RCMAX n'est pas factor/n")}
    
    if (object@vec_param[,6]<=object@vec_param[,5]) {retval <- c(retval, "[ParamRachDyn] : RCMIN doit etre inferieure a RCMX/n")}
    
    # Verification du nom des colonnes
    if(sum(colnames(object@vec_param) == c("alpha","beta","gamma","delta","RCMIN","RCMAX")) != 6) 
    {retval <- c(retval, "[ParamRachDyn] : Noms de colonne incorrect/n")}
    
    if (is.null(retval)) return (TRUE)
    else return (retval)
    })





	


