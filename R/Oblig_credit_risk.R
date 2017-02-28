#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer les Yield to Maturity et Z-spreads des composantes d'un ptf obligataire
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_z_spread
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les zeros spreads de chaque composante d'un portefeuille obligataire.
##'
##' \code{calc_z_spread} est une methode permettant de calculer les zeros spread de chaque composante d'un portefeuille obligataire.
##' @name calc_z_spred
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuile obligataire).
##' @param yield_curve vecteur decrivant la courbe de taux sans risque retenue.)
##' @return Un vecteur dont chaque element correspond a la valeur du zero spread de l'obligation du portefeuille obligataire.
##' Ce vecteur a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig

setGeneric(name = "calc_z_spread", def = function(x,yield_curve){standardGeneric("calc_z_spread")})
setMethod(
  f = "calc_z_spread",
  signature = c(x = "Oblig", yield_curve = "numeric"),
  definition = function(x, yield_curve = numeric()){
    # Verification des inputs
    if(nrow(x["ptf_oblig"]) == 0) {stop("[Oblig:calc_z_spread] : Portefeuille obligataire vide")}
    if(length(yield_curve) == 0) {stop("[Oblig:calc_z_spread] : Veuillez renseigner une courbe de taux")}
    coupon   <- calc_coupon(x)
    nominal  <- calc_nominal(x)
    maturite <- x["ptf_oblig"][,"mat_res"]
    achat    <- x["ptf_oblig"][,"val_achat"]

    # Calcul des Zspreads pour chaque ligne obligataire du portefeuille
    # On resout a chaque fois la fonction VM(zsp)-VA=0
    irr <- unlist(lapply(1:length(coupon), function(i){
      multiroot(function(r){return(sum(echeancier(coupon[i], maturite[i], r, nominal[i], yield_curve)) - achat[i])},
                start = 0,
                maxiter = 300)$root}))
    return(irr)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#          yield_to_maturity
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les yield to maturity de chaque composante d'un portefeuille obligataire.
##'
##' \code{yield_to_maturity} est une methode permettant de calculer les yield to maturity de chaque composante d'un portefeuille obligataire.
##' @name yield_to_maturity
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuile obligataire).
##' @return Un vecteur dont chaque element correspond au yield to maturity de l'obligation correspondante du portefeuille obligataire.
##' Ce vecteur a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig

setGeneric(name = "yield_to_maturity", def = function(x){standardGeneric("yield_to_maturity")})
setMethod(
  f = "yield_to_maturity",
  signature = "Oblig",
  definition = function(x){
    if(nrow(x["ptf_oblig"])==0) {stop("[Oblig : yield_to_maturity] : Portefeuille vide")}
    coupon   <- calc_coupon(x)
    nominal  <- calc_nominal(x)
    maturite <- x["ptf_oblig"][,"mat_res"]
    zspread  <- x["ptf_oblig"][,"zspread"]
    vm       <- x["ptf_oblig"][,"val_marche"]
    m        <- max(maturite)
    # Calcul des YtM pour chaque ligne obligataire du portefeuille
    # On resout a chaque fois la fonction VM(YtM)-VA=0
    ytm <- unlist(lapply(1:length(coupon), function(i){
      multiroot(function(y){return(sum(echeancier(coupon[i], maturite[i], zspread[i], nominal[i], rep(y, m))) - vm[i])},
                start = 0,
                maxiter = 300)$root}))
    return(ytm)
  }
)
