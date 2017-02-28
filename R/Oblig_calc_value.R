#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer les valeurs de marché, et de mettre a jour les VNC et S/D d'un ptf obligataire
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Calcul de VM, VNC et Surcote Decote
#----------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante d'un portefeuille obligataire.
##'
##' \code{calc_vm_oblig} est une methode permettant de calculer les valeurs de marche.
##' @name calc_vm_oblig
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuile obligataire).
##' @param yield_curve vecteur decrivant la courbe de taux sans risque retenue.)
##' @return Un vecteur dont chaque element correspond a la valeur de marche de l'obligation du portefeuille obligataire.
##' Ce vecteur a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig
# x <- x["ptf_oblig"]
# yield_curve <- new_mp_ESG["yield_curve"]
setGeneric(name = "calc_vm_oblig", def = function(x,yield_curve){standardGeneric("calc_vm_oblig")})
setMethod(
  f = "calc_vm_oblig",
  signature = c(x = "Oblig", yield_curve = "numeric"),
  definition = function(x, yield_curve){
    if(nrow(x["ptf_oblig"]) == 0) {stop("[Oblig:calc_vm_oblig] : Portefeuille obligataire vide")}
    # Definition des vecteurs et variables utiles
    coupon   <- calc_coupon(x)
    nominal  <- calc_nominal(x)
    maturite <- x["ptf_oblig"][,"mat_res"]
    zspread  <- x["ptf_oblig"][,"zspread"]

    # Appel de la fonction echeancier : traitement separe du cas pft a une ligne et ptf a plusieurs lignes
    if(nrow(x["ptf_oblig"]) == 1){ res <- sum(echeancier(coupon, maturite, zspread, nominal, yield_curve))}   # Ptf a une ligne
    else{ res <- rowSums(echeancier(coupon, maturite, zspread, nominal, yield_curve))}                        # Ptf a plusieurs lignes

    return(res)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_sur_dec_vnc
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les surcote/decote de chaque composante d'un portefeuille obligataire.
##'
##' \code{calc_sur_dec} est une methode permettant de calculer les surcotes/decotes de chaque composante d'un portefeuille obligataire.
##' @name calc_sur_dec
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuile obligataire).
##' @return Un data.frame composé de deux colonnes : 1 ere colonne :surcotes decotes ; 2de colonne : valeurs nettes comptables.
##' @author Prim'Act
##' @export
##' @aliases Oblig
setGeneric(name = "calc_sur_dec_vnc", def = function(x){standardGeneric("calc_sur_dec_vnc")})
setMethod(
  f = "calc_sur_dec_vnc",
  signature = "Oblig",
  definition = function(x){
    if(nrow(x["ptf_oblig"]) == 0) {stop("[Oblig:calc_sur_dec_vnc] : Portefeuille obligataire vide")}
    nominal           <- calc_nominal(x)
    achat             <- x["ptf_oblig"][,"val_achat"]
    maturite_initiale <- x["ptf_oblig"][,"mat_res"] + x["ptf_oblig"][,"dur_det"]
    surcote_decote    <- (nominal - achat) / maturite_initiale
    val_nc            <- x["ptf_oblig"][,"val_nc"] + surcote_decote
    return(data.frame(surcote_decote,val_nc))
  }
)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
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
