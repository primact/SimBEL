#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Echeancier
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les flux d'un model point ou d'un ensemble de models points obligataires.
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
##' que le max du vecteur de maturite residuelle, et autant de lignes que les vecteurs d'input \code{coupon,maturite,zspread,nominal}.
##' Chaque ligne decrit les flux annuels a venir pour l'actif obligataire de caracteristique renseigne en input.}
##' }
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

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
        } else {
            if(m>1){
                # Grille de versement des coupons
                grid_indicatrice_coup <- t(mapply(function(x,y){c(rep(1,x),rep(0,y-x))}, maturite, m))
                grid_coupon           <- matrix(coupon, nrow = n, ncol = m, byrow = F) * grid_indicatrice_coup
                # Grille de versement des nominaux
                grid_indicatrice_nom  <- t(mapply(function(x,y){ if(x > 0) {return(c(rep(0,(x-1)),1,rep(0,(y-x))))}
                                                                 if(x == 0){return(rep(0,y))}
                                                                },maturite,m))
                grid_nominal          <- matrix(nominal,nrow = n, ncol = m, byrow = F) * grid_indicatrice_nom
            } else {
                # Grille de versement des coupons
                grid_indicatrice_coup <- t(mapply(function(x,y){c(rep(1,x),rep(0,y-x))}, maturite, 2))
                grid_coupon           <- matrix(coupon, nrow = n, ncol = 2, byrow = F) * grid_indicatrice_coup
                # Grille de versement des nominaux
                grid_indicatrice_nom  <- t(mapply(function(x,y){ if(x > 0) {return(c(rep(0,(x-1)),1,rep(0,(y-x))))}
                                                                 if(x == 0){return(rep(0,y))}
                                                                },maturite,2))
                grid_nominal          <- matrix(nominal,nrow = n, ncol = 2, byrow = F) * grid_indicatrice_nom
            }
            # Grille de flux bruts
            grid_flux             <- grid_coupon + grid_nominal

            # Si une courbe de taux non nulle est renseignee : on renvoi les flux actualises, sinon on renvoi les flux bruts
            if(length(yield)>0){
                # Autre cas warning!
                if (length(yield) < m){ stop("[Oblig:echeancier] : La courbe de taux renseigne contient moins de maturite que la maturite residuelle des oblig\n")}
                if (m>1){
                    grid_actu   <- matrix((1 + yield[1:m] + rep(zspread, each = m))^-(1:m), nrow = n, ncol = m, byrow = T)
                } else {
                    grid_actu   <- matrix((1 + yield[1:2] + rep(zspread, each = 2))^-(1:2), nrow = n, ncol = 2, byrow = T)
                }
                grid_flux   <- (grid_coupon + grid_nominal) * grid_actu
            }
        }
        return(grid_flux)
    }
)
