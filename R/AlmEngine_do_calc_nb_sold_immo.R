#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_calc_nb_sold_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le nombre de titres immobilier a vendre.
##'
##' Cette methode permet de calculer pour chaque ligne d'un portefeuille
##' immobilier d'un assureur le nombre d'unites a vendre afin de realiser un certain montant de vente immo.
##' @name do_calc_nb_sold_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}}, correspondant au portefeuille immo de l'assureur.
##' @param montant_vente est un reel de type \code{numeric} correspondant a la vente totale de vm immo
##'  que l'assureur souhaite effectuer.
##' @param method_vente est un element de type \code{character} correspondant a methode de vente retenue
##'  (seule la methode proportionnelle est implementee actuellement).
##' @return \code{data.frame} contenant deux colonnes \code{(num_mp, nb_sold)} correspondant respectivement
##'  au numero de model point de chaque ligne immo du portefeuille et du nombre d'unite a vendre pour chacune
##'   d'entre elles.
##' @author Prim'Act
##' @seealso La classe \code{\link{Immo}}.
##' @export
##' @include AlmEngine_class.R Immo_class.R

setGeneric(name = "do_calc_nb_sold_immo", def = function(x, montant_vente, method_vente){standardGeneric("do_calc_nb_sold_immo")})
setMethod(
    f = "do_calc_nb_sold_immo",
    signature = c(x = "Immo", montant_vente = "numeric", method_vente = "character"),
    definition = function(x, montant_vente, method_vente){

        nom_table <- names(x@ptf_immo)
        val_marche <- which(nom_table == "val_marche")
        num_mp     <- which(nom_table == "num_mp")
        nb_unit    <- which(nom_table == "nb_unit")


        if(method_vente == "proportionnelle"){
            alloc         <- .subset2(x@ptf_immo, val_marche) / sum(.subset2(x@ptf_immo, val_marche))
            montant_vente <- montant_vente * alloc
            vm_unitaire   <- .subset2(x@ptf_immo, val_marche) / .subset2(x@ptf_immo, nb_unit)

            num_mp  <- .subset2(x@ptf_immo, num_mp)
            nb_sold <- montant_vente / vm_unitaire
        }
        return(data.frame(num_mp = num_mp, nb_sold = nb_sold))
    }
)
