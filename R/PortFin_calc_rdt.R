#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer la tresorerie, le compte de revalorisation, les allocations
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 26/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_rdt()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les rendements de chacune des composante des sous-portefeuilles action et immobilier du portefeuille PortFin.
##'
##' \code{calc_rdt} est une methode permettant de calculer les rendements des portfeuilles Action et Immo d'un objet PortFin.
##' @name calc_rdt
##' @docType methods
##' @param x objet de la classe PortFin.
##' @param mp_ESG objet de la classe ModelPointESG decrivant les conditions de l'annee n ( ainsi que l'annee n-1 pour les indices actions & immo).
##' @return Un data frame compose de deux colonnes et autant de lignes que le portefeuille action a de lignes.
##' @author Prim'Act
##' @export
##' @aliases PortFin
##' @include PortFin_class.R ModelPointESG_class.R

setGeneric(name = "calc_rdt", def = function(x, mp_ESG){standardGeneric("calc_rdt")})
setMethod(
  f = "calc_rdt",
  signature = c(x = "PortFin", mp_ESG = "ModelPointESG"),
  definition = function(x,mp_ESG){

    nb_action <- nrow(mp_ESG["indice_action"])
    nb_immo   <- nrow(mp_ESG["indice_immo"])
    # Creation du vecteur action/actionprev selon les indices
    index_action <- x["ptf_action"]["ptf_action"][,"num_index"]
    index_immo   <- x["ptf_immo"]["ptf_immo"][,"num_index"]

    # Afin de tenir compte des differents indices possibles de chaque ligne action, on est oblige de raisonner de la sorte pour etablir les bons cours courants et precedents
    indice_action <- data.frame()
    indice_action[1:length(index_action), "S_action"]      <- mp_ESG["indice_action"][index_action[1:length(index_action)], "S_action"]
    indice_action[1:length(index_action), "S_prev_action"] <- mp_ESG["indice_action"][index_action[1:length(index_action)], "S_prev_action"]


    # Afin de tenir compte des differents indices possibles de chaque ligne action, on est oblige de raisonner de la sorte pour etablir les bons cours courants et precedents
    indice_immo <- data.frame()
    indice_immo[1:length(index_immo), "S_immo"]      <- mp_ESG["indice_immo"][index_immo[1:length(index_immo)], "S_immo"]
    indice_immo[1:length(index_immo), "S_prev_immo"] <- mp_ESG["indice_immo"][index_immo[1:length(index_immo)], "S_prev_immo"]

    # Appel des fonctions actions et immobilier
    rdt_action <- revalo_action(x["ptf_action"], indice_action[, "S_action"], indice_action[, "S_prev_action"])
    rdt_immo   <- revalo_immo(x["ptf_immo"], indice_immo[,"S_immo"], indice_immo[, "S_prev_immo"])
    rdt_treso  <- revalo_treso(mp_ESG["yield_curve"][1], 0) # specificite de la yield curve des ModelPointESG : elle demarre a R(t,t+1), le taux precedent est donc R(t,t) = 0 


    return(list(rdt_action = rdt_action, rdt_immo = rdt_immo, rdt_treso = rdt_treso))
  }
)



