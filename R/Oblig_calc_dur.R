#----------------------------------------------------------------------------------------------------------------------------------------------------
#           duration_sensi
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la duration de chaque composante d'un portefeuille obligataire.
##'
##' \code{duration_sensi} est une methode permettant de calculer la duration de chaque composante d'un portefeuille obligataire.
##' @name duration_sensi
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire).
##' @return Un data frame compose de deux colonnes : la premiere est composee de la duration de chacune des obligations du portefeuille obligataire.
##' La seconde est compose de la sensibilite de chacune des obligations du portefeuille obligataire.
##' Le dataframe de sortie a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "duration_sensi", def = function(x) {
    standardGeneric("duration_sensi")
})
setMethod(
    f = "duration_sensi",
    signature = "Oblig",
    definition = function(x) {
        # Recuperation du PTF oblig
        ptf_oblig <- x@ptf_oblig

        if (nrow(ptf_oblig) == 0) stop("[Oblig : duration_sensi] : Portefeuille vide")

        # Calcul de donnees
        coupon <- calc_coupon(x)
        nominal <- calc_nominal(x)
        ytm <- yield_to_maturity(x)
        maturite <- ptf_oblig$mat_res
        zspread <- ptf_oblig$zspread
        m <- max(maturite)
        n <- length(coupon)


        denominateur <- echeancier(coupon, maturite, 0, nominal, numeric()) *
            t(apply(matrix(1 / (1 + ytm + zspread), nrow = n, ncol = m, byrow = FALSE), 1, cumprod))
        numerateur <- matrix(1:m, nrow = n, ncol = m, byrow = TRUE) * denominateur


        # Calcul de la duration
        if (n == 1L) {
            duration <- sum(numerateur) / sum(denominateur)
        } else {
            duration <- rowSums(numerateur) / rowSums(denominateur)
        }

        # Calcul de la sensibilite
        sensi <- duration / (1 + ytm)

        # Ouput
        return(data.frame(duration, sensi))
    }
)
