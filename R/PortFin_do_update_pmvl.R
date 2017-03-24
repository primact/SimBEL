#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_pmvl
#----------------------------------------------------------------------------------------------------------------------------------------------------
##'  Met a jour l'ensemble des attributs pvl et pml d'un objet PortFin
##'
##' \code{do_update_pmvl} est une methode permettant de calculer le taux de rendement financier du portefeuille.
##' @name do_update_pmvl
##' @docType methods
##' @param x est un objet de la classe \code{PortFin}, 
##' @return L'objet \code{x} de la classe \code{PortFin} dont les plus values et moins values ont ete recalculees avec les elements du \code{PortFin} renseigne en input.
##' @author Prim'Act
##' @export
##' @aliases PortFin
##' @include PortFin_class.R

setGeneric(name = "do_update_pmvl", def = function(x){standardGeneric("do_update_pmvl")})
setMethod(
    f = "do_update_pmvl",
    signature = "PortFin",
    definition = function(x){
        
        # Affectation des valeurs dans l'objet PortFin
        pmvl_action <- calc_pmvl_action(x["ptf_action"])
        pmvl_immo   <- calc_pmvl_immo(x["ptf_immo"])
        pmvl_oblig  <- calc_pmvl_oblig(x["ptf_oblig"])
        
        x["pvl_action"] <- pmvl_action[["pvl"]]
        x["mvl_action"] <- pmvl_action[["mvl"]]
        x["pvl_immo"]   <- pmvl_immo[["pvl"]]
        x["mvl_immo"]   <- pmvl_immo[["mvl"]]
        x["pvl_oblig"]  <- pmvl_oblig[["pvl"]]
        x["mvl_oblig"]  <- pmvl_oblig[["mvl"]]
        return(x)
    }
)