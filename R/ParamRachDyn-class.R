#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamRachDyn
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe de parametres de rachat dynamique \code{ParamRachDyn}.
##'
##' Une classe pour les parametres de des lois de rachat dynamique.
##' @name ParamRachDyn
##' @slot vec_param un \code{data frame} contenant les parametres pour les rachats dynamiques.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul du taux de rachat dynamique \code{\link{calc_rach_dyn}}.
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








