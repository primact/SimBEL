#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_calc_nb_sold_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le nombre d'obligations a vendre.
##'
##' Cette methode permet de calculer pour chaque ligne d'un portefeuille
##' obligataire d'un assureur le nombre d'unites a vendre afin de realiser un certain montant de vente obligataire.
##' @name do_calc_nb_sold_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}}, correspondant au portefeuille obligataire de l'assureur.
##' @param montant_vente est un reel de type \code{numeric} correspondant a la vente totale de vm obligataire
##'  que l'assureur souhaite effectuer.
##' @param method_vente est un element de type \code{character} correspondant a methode de vente retenue
##'  (seule la methode proportionnelle est implementee actuellement).
##' @return \code{data.frame} contenant deux colonnes \code{(num_mp, nb_sold)} correspondant respectivement
##'  au numero de model point de chaque ligne obligataire du portefeuille et du nombre d'unite a vendre pour
##'   chacune d'entre elles.
##' @author Prim'Act
##' @seealso La classe \code{\link{Oblig}}.
##' @export
##' @include AlmEngine_class.R Oblig_class.R
setGeneric(name = "do_calc_nb_sold_oblig", def = function(x, montant_vente, method_vente) {
    standardGeneric("do_calc_nb_sold_oblig")
})
setMethod(
    f = "do_calc_nb_sold_oblig",
    signature = c(x = "Oblig", montant_vente = "numeric", method_vente = "character"),
    definition = function(x, montant_vente, method_vente) {
        # Donnees
        ptf_oblig <- x@ptf_oblig
        nom_table <- names(ptf_oblig)
        val_marche <- which(nom_table == "val_marche")
        num_mp <- which(nom_table == "num_mp")
        nb_unit <- which(nom_table == "nb_unit")

        if (method_vente == "proportionnelle") {
            # Extraction de la valeur de marche
            val_marche <- .subset2(ptf_oblig, val_marche)

            alloc <- val_marche / sum(val_marche) # Calul de l'allocation
            montant_vente <- montant_vente * alloc # Calcul du montant_vente
            vm_unitaire <- val_marche / .subset2(ptf_oblig, nb_unit) # Calcul de la valeur de marche unitaire
            nb_sold <- montant_vente / vm_unitaire # Calul du nombre d'unite a vendre

            # Extraction des numeros de MP
            num_mp <- .subset2(ptf_oblig, num_mp)
        }

        # Output
        return(cbind(num_mp = num_mp, nb_sold = nb_sold))
    }
)
