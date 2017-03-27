#10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille immobilier.
##'
##' \code{calc_pmvl_immo} est une methode permettant de calculer les valeurs de marche.
##' @name calc_pmvl_immo
##' @docType methods
##' @param x objet de la classe \code{Immo} (decrivant le portefeuille d'immobilier).
##' @return Une liste composee de deux elements \code{(pvl, mvl)} correspondant respectivement 
##' aux sommes des plus values latentes immobilieres et aux sommes des moins values latentes immobilieres.
##' @author Prim'Act
##' @export
##' @aliases Immo
##' @include Immo_class.R

setGeneric(name = "calc_pmvl_immo", def = function(x){standardGeneric("calc_pmvl_immo")})
setMethod(
    f = "calc_pmvl_immo",
    signature = "Immo",
    definition = function(x){
        pvl <- sum((x["ptf_immo"][,"val_marche"] - x["ptf_immo"][,"val_nc"])*(x["ptf_immo"][,"val_marche"] > x["ptf_immo"][,"val_nc"]))
        mvl <- sum((x["ptf_immo"][,"val_marche"] - x["ptf_immo"][,"val_nc"])*(x["ptf_immo"][,"val_marche"] < x["ptf_immo"][,"val_nc"]))
        return(list(pvl = pvl, mvl = mvl))
    }
)