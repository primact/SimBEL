#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_frais
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule des frais de passif.
##'
##' \code{calc_frais} est une methode generique permettant de calculer les frais sur prestations, sur primes
##' et sur encours.
##' @name calc_frais
##' @docType methods
##' @param x objet de la classe \code{\link{FraisPassif}}.
##' @param type un \code{character} designant le type de frais applique.
##' @param nom_prod est le nom de produit de type \code{character}.
##' @param nb correspond a un nombre de contrats, utilise comme assiette de frais fixe par contrat.
##' @param mt correspond a un montant, utilise comme assiette de frais variable.
##' @param coef_inf correspond au coefficient d'inflation applique.
##' @details Le type du contrat prend pour valeur \code{prime} pour les frais sur primes, \code{prest} pour les frais
##' sur prestations et \code{enc} pour les frais sur encours.
##' @return Une liste contenant les montants de frais fixes et de frais variables.
##' @author Prim'Act
##' @export
##' @include FraisPassif-class.R

setGeneric(name = "calc_frais", def = function(x, type, nom_prod, nb, mt, coef_inf) {
    standardGeneric("calc_frais")
})
setMethod(
    f = "calc_frais",
    signature = c(x = "FraisPassif", type = "character", nom_prod = "character", nb = "numeric", mt = "numeric", coef_inf = "numeric"),
    def = function(x, type, nom_prod, nb, mt, coef_inf) {
        # Table ModelPoint
        mp <- x@mp
        names_col <- names(mp)
        frais_fixe <- which(names_col == paste("frais_fixe", type, sep = "_"))
        frais_var <- which(names_col == paste("frais_var", type, sep = "_"))
        ind_inf_frais_fixe <- which(names_col == paste("ind_inf_frais_fixe", type, sep = "_"))
        ind_inf_frais_var <- which(names_col == paste("ind_inf_frais_var", type, sep = "_"))
        num_nom_prod <- which(names_col == "nom_prod")

        # Nom de la ligne produit et des colonnes
        row <- which(.subset2(mp, num_nom_prod) == nom_prod)

        # Calcul des frais fixes
        frais_fixe <- nb * .subset2(mp, frais_fixe)[row] *
            (1 + .subset2(mp, ind_inf_frais_fixe)[row] * (coef_inf - 1))

        # Calcul des frais variables
        frais_var <- mt * .subset2(mp, frais_var)[row] *
            (1 + .subset2(mp, ind_inf_frais_var)[row] * (coef_inf - 1))

        # Output
        ret <- list(frais_fixe, frais_var)
        names(ret) <- c(paste("frais_fixe", type, sep = "_"), paste("frais_var", type, sep = "_")) # Nommage des sorties
        return(ret)
    }
)
