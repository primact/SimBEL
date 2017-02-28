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
##' @return Un data frame composé de deux colonnes et autant de lignes que le portefeuille action a de lignes.
##' @author Prim'Act
##' @export
##' @aliases PortFin
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

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           print_alloc()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le poids de chaque composante du portefeuille action.
##'
##' \code{pint_alloc} est une methode permettant de calculer l'allocation absolue et relative du portefeuille.
##' @name print_alloc
##' @docType methods
##' @param x objet de la classe PortFin.
##' @return Un data frame compose de deux colonnes et cinq lignes.
##' La premiere colonne decrit le montant alloue par poche et au total en valeur.
##' La seconde colonne decrit le montant alloue par poche en proportion.
##' Les lignes correspondent aux actifs : (Action / Immobilier / Obligation / Tresorerie / Actifs cumules)
##' @author Prim'Act
##' @export
##' @aliases PortFin

setGeneric(name = "print_alloc", def = function(x){standardGeneric("print_alloc")})
setMethod(
  f = "print_alloc",
  signature = "PortFin",
  definition = function(x){

    # Allocation en valeur de marche
    alloc_action <- sum(x["ptf_action"]["ptf_action"][,"val_marche"]) # Action
    alloc_immo   <- sum(x["ptf_immo"]["ptf_immo"][,"val_marche"]) # Immo
    alloc_oblig  <- sum(x["ptf_oblig"]["ptf_oblig"][,"val_marche"]) # Oblig
    alloc_treso  <- sum(x["ptf_treso"]["ptf_treso"][,"val_marche"]) # Treso
    alloc_total <- alloc_action + alloc_immo + alloc_oblig + alloc_treso # Total

    # Allocation en valeur nette comptable
    alloc_action_nc <- sum(x["ptf_action"]["ptf_action"][,"val_nc"]) # Action
    alloc_immo_nc   <- sum(x["ptf_immo"]["ptf_immo"][,"val_nc"]) # Immo
    alloc_oblig_nc  <- sum(x["ptf_oblig"]["ptf_oblig"][,"val_nc"]) # Oblig
    alloc_treso_nc  <- sum(x["ptf_treso"]["ptf_treso"][,"val_nc"]) # Treso
    alloc_total_nc <- alloc_action_nc + alloc_immo_nc + alloc_oblig_nc + alloc_treso_nc # Total

    alloc <- data.frame(alloc_valeur = c(alloc_action, alloc_immo, alloc_oblig, alloc_treso, alloc_total),
               alloc_proportion = c(alloc_action/alloc_total, alloc_immo/alloc_total,
                                    alloc_oblig/alloc_total, alloc_treso/alloc_total, alloc_total/alloc_total),
               alloc_valeur_nc = c(alloc_action_nc, alloc_immo_nc, alloc_oblig_nc, alloc_treso_nc, alloc_total_nc),
               alloc_proportion_nc = c(alloc_action_nc/alloc_total_nc, alloc_immo_nc/alloc_total_nc,
                                       alloc_oblig_nc/alloc_total_nc, alloc_treso_nc/alloc_total_nc, alloc_total_nc/alloc_total_nc),
               row.names = c("alloc_action", "alloc_immo", "alloc_oblig", "alloc_treso","alloc_total"))

    return(alloc)
  }
)
