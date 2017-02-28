#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ESG
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe ESG
##'
##' Classe pour les donnees globales d'actif
##'
##' @name ESG
##' @slot nb_simu
##' @slot ind_action
##' @slot ind_immo
##' @slot ind_oblig
##' @slot ind_treso
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export

setClass(
  Class = "ESG",
  representation = representation(
    nb_simu       = "integer",
    ind_action    = "list",
    ind_immo      = "list",
    ind_inflation = "list",
    yield_curve   = "list",
    deflateur     = "list"),
  validity = function(object){
                  retval <- NULL

                  if(!is.integer(object@nb_simu)) {retval <- "[ESG] : Objet nb_simu non valide"}
                  if(!is.list(object@ind_action))   {retval <- "[ESG] : Objet ind_action non valide"}
                  if(!is.list(object@ind_immo))   {retval <- "[ESG] : Objet ind_immo non valide"}
                  if(!is.list(object@ind_inflation))   {retval <- "[ESG] : Objet ind_inflation non valide"}
                  if(!is.list(object@yield_curve))   {retval <- "[ESG] : Objet yield_curve non valide"}
                  if(!is.list(object@deflateur))   {retval <- "[ESG] : Objet deflateur non valide"}

                  if(object@nb_simu < 1) {retval <- "[ESG] : Objet nb_sim doit etre superieur a 1."}

                  if (is.null(retval)) return (TRUE)
                  else return (retval)

  }
  )
