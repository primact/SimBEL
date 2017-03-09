#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe Initialisation
##'
##' Classe d'initialisation
##'
##' @name Initialisation
##' @slot root_address
##' @slot address
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
setClass(
    Class = "Initialisation",
    representation = representation(
        root_address  = "character",
        address       = "list",
        nb_simu       = "integer",
        nb_annee_proj = "integer")
)

