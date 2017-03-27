#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend la fonction permettant de calculer les duration et sensibilite de chaque composante d'un ptf obligataire
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           duration_sensi
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la duration de chaque composante d'un portefeuille obligataire.
##'
##' \code{duration_sensi} est une methode permettant de calculer la duration de chaque composante d'un portefeuille obligataire.
##' @name duration_sensi
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuille obligataire).
##' @return Un data frame compose de deux colonnes : la premiere est composee de la duration de chacune des obligations du portefeuille obligataire.
##' La seconde est compose de la sensibilite de chacune des obligations du portefeuille obligataire.
##' Le dataframe de sortie a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "duration_sensi", def = function(x){standardGeneric("duration_sensi")})
setMethod(
  f = "duration_sensi",
  signature = "Oblig",
  definition = function(x){
    if(nrow(x["ptf_oblig"]) == 0){stop("[Oblig : duration_sensi] : Portefeuille vide")}
    coupon   <- calc_coupon(x)
    nominal  <- calc_nominal(x)
    ytm      <- yield_to_maturity(x)
    maturite <- x["ptf_oblig"][,"mat_res"]
    zspread  <- x["ptf_oblig"][,"zspread"]
    m        <- max(maturite)
    n        <- length(coupon)

    numerateur    <- matrix(1:m, nrow = n, ncol = m, byrow = T) * echeancier(coupon,maturite,0,nominal,numeric()) * t(apply(matrix(1 / (1 + ytm + zspread), nrow = n, ncol = m, byrow = F), 1, cumprod))
    denominateur  <- echeancier(coupon,maturite,0,nominal,numeric()) * t(apply(matrix(1 / (1 + ytm + zspread), nrow = n, ncol = m, byrow = F), 1, cumprod))

    if (n == 1) {duration <- sum(numerateur) / sum(denominateur)
    } else {duration <- rowSums(numerateur) / rowSums(denominateur)}

    sensi <- duration /(1+ytm)
    return(data.frame(duration,sensi))
  }
)


