#---------------------------------------------------------------------------------------------------------
# Ce script est la definition de la classe ParamComport qui contient les donn?es sur parametres de comportement
#---------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 10/02/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamComport
#----------------------------------------------------------------------------------------------------------------------------------------------------


##' La classe ParamComport
##'
##' Classe pour pour les parametres de comportement
##'
##' @name ParamComport
##' @slot mat_oblig
##' @slot alloc_mar
##' @slot w_n
##' @slot marge_mar
##' @slot ch_enc_mar
##' @slot ind_ref_action
##' @slot ind_ref_immo
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export

setClass(
  Class = "ParamComport",
  slots = c(mat_oblig = "numeric", alloc_mar = "numeric", w_n = "numeric",
            marge_mar = "numeric", ch_enc_mar = "numeric", ind_ref_action = "numeric", ind_ref_immo = "numeric"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL

    # Verification du type des colonnes
    if (!is.numeric(object@mat_oblig))   {retval <- c(retval, "[ParamComport] : mat_oblig n'est pas numeric\n")}
    if (!is.numeric(object@alloc_mar))  {retval <- c(retval, "[ParamComport] : alloc_mar n'est pas numeric\n")}
    if (!is.numeric(object@w_n))   {retval <- c(retval, "[ParamComport] : w_n n'est pas numeric\n")}
    if (!is.numeric(object@marge_mar))  {retval <- c(retval, "[ParamComport] : marge_mar n'est pas numeric\n")}
    if (!is.numeric(object@ch_enc_mar))   {retval <- c(retval, "[ParamComport] : ch_enc_mar n'est pas numeric\n")}
    if (!is.numeric(object@ind_ref_action))  {retval <- c(retval, "[ParamComport] : ind_ref_action n'est pas numeric\n")}
    if (!is.numeric(object@ind_ref_immo))  {retval <- c(retval, "[ParamComport] : ind_ref_immo n'est pas numeric\n")}

    if(object@mat_oblig < 0 | round(object@mat_oblig) != object@mat_oblig ) {retval <- c(retval, "[ParamComport] :  maturite doit etre un entier strictement positif. \n")}


    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (retval)
    }
  }
)
