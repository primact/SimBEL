#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mets a jour les sous totaux de d'actions et immobilier en plus ou moins value latente.
##'
##' \code{calc_pmvl} est une methode permettant de calculer les valeurs de marche.
##' @name calc_pmvl
##' @docType methods
##' @param x objet de la classe PortFin.
##' @return L'objet PortFin dont la somme des composantes en PVL et en MVL a ete mise a jour
##' @author Prim'Act
##' @export
##' @aliases PortFin
##' @include PortFin_class.R

setGeneric(name = "calc_pmvl", def = function(x){standardGeneric("calc_pmvl")})
setMethod(
    f = "calc_pmvl",
    signature = "PortFin",
    definition = function(x){
        x["pvl_action"] <- sum(max(x["ptf_action"]["ptf_action"][,"val_marche"] - x["ptf_action"]["ptf_action"][,"val_nc"], 0))
        x["mvl_action"] <- sum(min(x["ptf_action"]["ptf_action"][,"val_marche"] - x["ptf_action"]["ptf_action"][,"val_nc"], 0))
        x["pvl_immo"]   <- sum(max(x["ptf_immo"]["ptf_immo"][,"val_marche"] - x["ptf_immo"]["ptf_immo"][,"val_nc"], 0))
        x["mvl_immo"]   <- sum(min(x["ptf_immo"]["ptf_immo"][,"val_marche"] - x["ptf_immo"]["ptf_immo"][,"val_nc"], 0))
        return(x)
    }
)