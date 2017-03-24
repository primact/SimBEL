#10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille d'obligations.
##'
##' \code{calc_pmvl_oblig} est une methode permettant de calculer les valeurs de marche.
##' @name calc_pmvl_oblig
##' @docType methods
##' @param x objet de la classe \code{Oblig} (decrivant le portefeuille d'obligations).
##' @return Une liste composee de deux elements \code{(pvl, mvl)} correspondant respectivement 
##' aux sommes des plus values latentes obligations et aux somme des moins values latentes obligations.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "calc_pmvl_oblig", def = function(x){standardGeneric("calc_pmvl_oblig")})
setMethod(
    f = "calc_pmvl_oblig",
    signature = "Oblig",
    definition = function(x){
        pvl <- sum((x["ptf_oblig"][,"val_marche"] - x["ptf_oblig"][,"val_nc"])*(x["ptf_oblig"][,"val_marche"] > x["ptf_oblig"][,"val_nc"]))
        mvl <- sum((x["ptf_oblig"][,"val_marche"] - x["ptf_oblig"][,"val_nc"])*(x["ptf_oblig"][,"val_marche"] < x["ptf_oblig"][,"val_nc"]))
        return(list(pvl = pvl, mvl = mvl))
    }
)
