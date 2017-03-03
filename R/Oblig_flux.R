#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer des flux de la classe Oblig
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Echeancier
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux d'un model point ou d'un ensemble de odels points obligataires.
##'
##' \code{echeancier} est une methode permettant de calculer les flux jusqu'a maturite residuelle.
##' @name echeancier
##' @docType methods
##' @param coupon vecteur contenant les taux de coupons
##' @param maturite vecteur d'entiers contenant les maturites residuelles
##' @param zspread vecteur contenant les zero-spreads
##' @param nominal vecteur contenant les valeurs nominales de chaque obligation
##' @param yield vecteur contenant la courbe de taux consideree (peut-etre vide)
##' @return Une matrice contenant :
##' \describe{
##' \item{\code{grid_flux} : }{la matrice d'ecoulement des flux. Cette matrice a autant de colonnes
##' que le max du vecteur de maturite residuelle, et autant de lignes que les vecteurs d'input \param{coupon,maturite,zspread,nominal}.
##' Chaque ligne decrit les flux annuels a venir pour l'actif obligataire de caracteristique renseigne en input.}
##' }
##' @author Prim'Act
##' @export
##' @aliases Oblig

setGeneric(name = "echeancier", def = function(coupon,maturite,zspread,nominal,yield){standardGeneric("echeancier")})
setMethod(
  f = "echeancier",
  signature = c(coupon = "numeric",maturite = "numeric",zspread = "numeric",nominal = "numeric", yield = "numeric"),
  definition = function(coupon,maturite,zspread,nominal,yield=numeric()){
    n  <- length(coupon)
    m  <- max(maturite)
    if(n ==1){
      indicatrice  <- rep(1,m)
      grid_flux    <- indicatrice * coupon
      grid_flux[m] <- grid_flux[m] + nominal
      # Si une courbe de taux non nulle est renseignee : on renvoi les flux actualises, sinon on renvoi les flux bruts
      if(length(yield) > 0){
        # Autre cas warning!
        if (length(yield)<m){ stop("[Oblig:echeancier] : La courbe de taux renseigne contient moins de maturite que la maturite residuelle des oblig\n")}
        grid_flux <- grid_flux /(1 + yield[1:m] + zspread)^(1:m)
      }
    }
    else {
      # Grille de versement des coupons
      grid_indicatrice_coup <- t(mapply(function(x,y){c(rep(1,x),rep(0,y-x))}, maturite, m))
      grid_coupon           <- matrix(coupon, nrow = n, ncol = m, byrow = F) * grid_indicatrice_coup
      # Grille de versement des nominaux
      grid_indicatrice_nom  <- t(mapply(function(x,y){if(x > 0) {return(c(rep(0,(x-1)),1,rep(0,(y-x))))}
                                                      if(x == 0){return(rep(0,y))}},
                                        maturite,
                                        m))
      grid_nominal          <- matrix(nominal,nrow = n, ncol = m, byrow = F) * grid_indicatrice_nom
      # Grille de flux bruts
      grid_flux             <- grid_coupon + grid_nominal

      # Si une courbe de taux non nulle est renseignee : on renvoi les flux actualises, sinon on renvoi les flux bruts
      if(length(yield)>0){
        # Autre cas warning!
        if (length(yield) < m){ stop("[Oblig:echeancier] : La courbe de taux renseigne contient moins de maturite que la maturite residuelle des oblig\n")}
        grid_actu   <- matrix((1 + yield[1:m] + rep(zspread, each = m))^-(1:m), nrow = n, ncol = m, byrow = T)
        grid_flux   <- (grid_coupon + grid_nominal) * grid_actu
      }
    }
    return(grid_flux)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Calc_coupon
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le coupon des models points constituant le portefeuille obligataire.
##'
##' \code{calc_coupon} est une methode permettant de calculer les valeurs de coupon de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_coupon
##' @docType methods
##' @param x un objet de la classe Oblig.
##' @return Un vecteur dont chaque element correspond a la valeur du coupon de l'obligation consideree : tx_coupon * parite * nominal * nb_unit.
##' Le vecteur renvoye a autant d'elements que le portefeuille obligataire en input a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig

setGeneric(name = "calc_coupon", def = function(x){standardGeneric("calc_coupon")})
setMethod(
  f = "calc_coupon",
  signature ="Oblig",
  definition = function(x){
    # Tx_coupon * Parite * Nominal
    return(x["ptf_oblig"][,"tx_coupon"] * x["ptf_oblig"][,"par"] * x["ptf_oblig"][,"nominal"] * x["ptf_oblig"][,"nb_unit"])
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_nominal
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le nominal des models points constituant le portefeuille obligataire.
##'
##' \code{calc_nominal} est une methode permettant de calculer les valeurs de nominal de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_nominal
##' @docType methods
##' @param x un objet de la classe Oblig.
##' @return Un vecteur dont chaque element correspond a la valeur du nominal de l'obligation consideree : parite * nominal * nb_unit.
##' Le vecteur renvoye a autant d'elements que le portefeuille obligataire en input a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig

setGeneric(name = "calc_nominal", def = function(x){standardGeneric("calc_nominal")})
setMethod(
  f = "calc_nominal",
  signature = c(x = "Oblig"),
  definition = function(x){
    return(x["ptf_oblig"][,"par"] * x["ptf_oblig"][,"nominal"] * x["ptf_oblig"][,"nb_unit"])
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_flux_annee
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux percus dans l'annee du fait de la detention des obligations du portefeuille obligataire.
##'
##' \code{calc_flux_annee} est une methode permettant de calculer les valeurs de nominal de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_flux_annee
##' @docType methods
##' @param x un objet de la classe Oblig.
##' @return Une liste composée de deux vecteurs:
##' \describe{
##' \item{\code{tombee_coupon} : }{Chaque element correspond aux tombees de coupon pour l'annee a venir. Ce vecteur a autant d'elements
##' que le portefeuille obligataire d'inputs a de lignes.}
##' \item{\code{tombee_echeance} : }{Chaque element correspond aux tombees d echeances pour l'annee a venir. Ce vecteur a autant d'elements
##' que le portefeuille obligataire d'inputs a de lignes.}
##' }
##' @author Prim'Act
##' @export
##' @aliases Oblig
setGeneric(name = "calc_flux_annee", def = function(x){standardGeneric("calc_flux_annee")})
setMethod(
  f = "calc_flux_annee",
  signature = "Oblig",
  definition = function(x){
    tombee_coupon   <- calc_coupon(x)
    tombee_echeance <- calc_nominal(x) * (x["ptf_oblig"][,"mat_res"] <= 1) * 1
    return(list(tombee_coupon = tombee_coupon, tombee_echeance = tombee_echeance))
  }
)
