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
##' Calcul les valeurs de marches de chaque composante du portefeuille obligation.
##'
##' \code{calc_vm_oblig} est une methode permettant de calculer les valeurs de marche.
##' @name calc_vm_oblig
##' @docType methods
##' @param x objet de la classe \code{Oblig} (decrivant le portefeuille d'obligation).
##' @param yield_curve vecteur de type \code{numeric} contenant la courbe de taux 
##' (cf. l'attribut \code{yield_curve} des objets de la classe \code{\link{ModelPointESG}}).
##' @return L'objet \code{x} dont les valeurs de marche ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

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



