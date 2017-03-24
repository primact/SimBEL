# GK 10/03/2017

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille action.
##'
##' \code{calc_pmvl_action} est une methode permettant de calculer les valeurs de marche.
##' @name calc_pmvl_action
##' @docType methods
##' @param x objet de la classe \code{Action} (decrivant le portefeuille d'action).
##' @return Une liste composee de deux elements \code{(pvl, mvl)} correspondant respectivement 
##' aux sommes des plus values latentes actions et aux somme des moins values latentes action.
##' @author Prim'Act
##' @export
##' @aliases Action
##' @include Action_class.R

setGeneric(name = "calc_pmvl_action", def = function(x){standardGeneric("calc_pmvl_action")})
setMethod(
    f = "calc_pmvl_action",
    signature = "Action",
    definition = function(x){
        pvl <- sum((x["ptf_action"][,"val_marche"] - x["ptf_action"][,"val_nc"])*(x["ptf_action"][,"val_marche"] > x["ptf_action"][,"val_nc"]))
        mvl <- sum((x["ptf_action"][,"val_marche"] - x["ptf_action"][,"val_nc"])*(x["ptf_action"][,"val_marche"] < x["ptf_action"][,"val_nc"]))
        return(list(pvl = pvl, mvl = mvl))
    }
)
