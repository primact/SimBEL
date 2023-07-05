#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_z_spread
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les zeros spreads de chaque composante d'un portefeuille obligataire.
##'
##' \code{calc_z_spread} est une methode permettant de calculer les zeros spread de chaque composante d'un portefeuille obligataire.
##' @name calc_z_spread
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuile obligataire).
##' @param yield_curve vecteur decrivant la courbe de taux sans risque retenue.
##' @return Un vecteur dont chaque element correspond a la valeur du zero spread de l'obligation du portefeuille obligataire.
##' Ce vecteur a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_z_spread", def = function(x, yield_curve) {
    standardGeneric("calc_z_spread")
})
setMethod(
    f = "calc_z_spread",
    signature = c(x = "Oblig", yield_curve = "numeric"),
    definition = function(x, yield_curve = numeric()) {
        # Recuperation du PTF oblig
        ptf_oblig <- x@ptf_oblig

        # Verification des inputs
        if (nrow(ptf_oblig) == 0L) stop("[Oblig : calc_z_spread] : Portefeuille obligataire vide")
        if (length(yield_curve) == 0L) stop("[Oblig : calc_z_spread] : Veuillez renseigner une courbe de taux")

        nom_table <- names(ptf_oblig)
        mat_res <- which(nom_table == "mat_res")
        val_mar <- which(nom_table == "val_marche")

        coupon <- calc_coupon(x) # Calcul du coupon
        nominal <- calc_nominal(x) # Calcul du nominal

        # Extraction de donnes
        maturite <- .subset2(ptf_oblig, mat_res)
        val_marche <- .subset2(ptf_oblig, val_mar)

        # Calcul des Zspreads pour chaque ligne obligataire du portefeuille
        # On resout a chaque fois la fonction VM(zsp)-VA=0
        irr <- sapply(1:length(coupon), function(i) {
            multiroot(
                function(r) {
                    return(sum(echeancier(coupon[i], maturite[i], r, nominal[i], yield_curve)) - val_marche[i])
                },
                start = 0,
                maxiter = 300
            )$root
        })

        # Output
        return(irr)
    }
)
