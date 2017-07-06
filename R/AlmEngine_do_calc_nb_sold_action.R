#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_calc_nb_sold_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le nombre d'actions a vendre.
##'
##' Cette methode permet de calculer pour chaque ligne d'un portefeuille
##'  action d'un assureur le nombre d'unites a vendre afin de realiser un certain montant de vente en actions.
##' @name do_calc_nb_sold_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}}, correspondant au portefeuille action de l'assureur.
##' @param montant_vente est un reel de type \code{numeric} correspondant a un montant de vente (en valeur de marche) totale d'action que l'assureur souhaite effectuer.
##' @param method_vente est un element de type \code{character} correspondant a methode de vente
##'  retenue (seule la methode proportionnelle est implementee actuellement).
##' @return \code{data.frame} contenant deux colonnes \code{(num_mp, nb_sold)} correspondant
##'  respectivement au numero de model point de chaque ligne action du portefeuille
##'   et du nombre d'unite a vendre pour chacune d'entre elles.
##' @author Prim'Act
##' @seealso \code{\link{Action}}.
##' @export
##' @include AlmEngine_class.R Action_class.R

setGeneric(name = "do_calc_nb_sold_action", def = function(x, montant_vente, method_vente){standardGeneric("do_calc_nb_sold_action")})
setMethod(
    f = "do_calc_nb_sold_action",
    signature = c(x = "Action", montant_vente = "numeric", method_vente = "character"),
    definition = function(x, montant_vente, method_vente){
        
        # Donnees
        ptf_action <- x@ptf_action
        nom_table  <- names(ptf_action)
        val_marche <- which(nom_table == "val_marche")
        num_mp     <- which(nom_table == "num_mp")
        nb_unit    <- which(nom_table == "nb_unit")
        
        if(method_vente == "proportionnelle"){
            # Extraction de la valeur de marche
            val_marche    <- .subset2(ptf_action, val_marche)
            
            alloc         <- val_marche / sum(val_marche) # Calul de l'allocation
            montant_vente <- montant_vente * alloc # Calcul du montant_vente
            vm_unitaire   <- val_marche / .subset2(ptf_action, nb_unit) # Calcul de la valeur de marche unitaire
            nb_sold       <- montant_vente / vm_unitaire # Calul du nombre d'unite a vendre
            
            # Extraction des numeros de MP
            num_mp      <- .subset2(ptf_action, num_mp)
        }
        
        # Output
        return(cbind(num_mp = num_mp, nb_sold = nb_sold))
    }
)
