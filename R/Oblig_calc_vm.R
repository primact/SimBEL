#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille obligataires.
##'
##' \code{calc_vm_oblig} est une methode permettant de calculer les valeurs de marche du portefeuille obligataires.
##' @name calc_vm_oblig
##' @docType methods
##' @param x un objet de la classe \code{\link{Oblig}} (decrivant le portefeuille d'obligation).
##' @param yield_curve un vecteur de type \code{numeric} contenant la courbe de taux
##' (l'attribut \code{yield_curve} des objets de la classe \code{\link{ModelPointESG}}).
##' @return Les valeurs de marche mises a jour.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_vm_oblig", def = function(x, yield_curve){standardGeneric("calc_vm_oblig")})
setMethod(
    f = "calc_vm_oblig",
    signature = c(x = "Oblig", yield_curve = "numeric"),
    definition = function(x, yield_curve){

        # Donnees
        ptf_oblig <- x@ptf_oblig
        nom_table <- names(ptf_oblig)
        mat_res   <- which(nom_table == "mat_res")
        zsp       <- which(nom_table == "zspread")

        # Test
        if(nrow(ptf_oblig) == 0L) stop("[Oblig : calc_vm_oblig] : Portefeuille obligataire vide")

        # Definition des vecteurs et variables utiles
        coupon   <- calc_coupon(x)
        nominal  <- calc_nominal(x)
        maturite <- .subset2(ptf_oblig, mat_res)
        zspread  <- .subset2(ptf_oblig, zsp)

        # Appel de la fonction echeancier : traitement separe du cas pft a une ligne et ptf a plusieurs lignes
        if(nrow(ptf_oblig) == 1L)
            res <- sum(echeancier(coupon, maturite, zspread, nominal, yield_curve)) # Ptf a une ligne
        else
            res <- rowSums(echeancier(coupon, maturite, zspread, nominal, yield_curve)) # Ptf a plusieurs lignes

        # Output
        return(res)
    }
)



