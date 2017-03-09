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
##' @name ChocSolvabilite2
##' @slot param_choc_mket
##' @slot param_choc_sousc : objet de la classe ParamChoc
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export

setClass(
    Class = "ChocSolvabilite2",
    representation = representation(
        param_choc_mket  = "ParamChocMket",
        param_choc_sousc = "ParamChocSousc")
)
