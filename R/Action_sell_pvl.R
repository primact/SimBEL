
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_pvl_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour du portefeuille action suite a une realisation de plus-values latentes action.
##'
##' \code{sell_pvl_action} est une methode permettant de mettre a jour chaque composante d'un portefeuille action
##'  suite a la vente de tout ou partie de ce portefeuille afin de realiser un montant de plus-values latentes.
##' @name sell_pvl_action
##' @docType methods
##' @param x un objet de la classe \code{\link{Action}} (decrivant le portefeuille action en detention).
##' @param montant un montant \code{numeric} de plus-values latentes que l'on souhaite realiser.
##' @return \code{action} l'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @return \code{pmvr} le montant des plus ou moins-values realisees.
##' @note Les cessions sont realisees au prorata des plus-values latentes actions.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "sell_pvl_action", def = function(x, montant){standardGeneric("sell_pvl_action")})
setMethod(
    f = "sell_pvl_action",
    signature = c(x = "Action", montant = "numeric"),
    definition = function(x, montant){
        nom_table   <- names(x@ptf_action)
        num_mp      <- which(nom_table == "num_mp")
        val_marche  <- which(nom_table == "val_marche")
        val_achat   <- which(nom_table == "val_achat")
        val_nc      <- which(nom_table == "val_nc")

        # Verification des inputs
        if(nrow(x@ptf_action) > 0) {
            pvl_action <- calc_pmvl_action(x)[["pvl"]]
            if(pvl_action < montant) {stop("[Action : sell_pvl_action] : Tentative de realisation de plus de plus value latente que ce que le portefeuille contient. \n")}

            # Selection des lignes soumises a une operation de realisation des pvl
            temp              <- x@ptf_action[which(.subset2(x@ptf_action, val_marche) > .subset2(x@ptf_action, val_nc)),]
            num_mp_pvl        <- .subset2(temp, num_mp)
            vecteur_poids_pvl <- (.subset2(temp, val_marche) - .subset2(temp, val_nc)) / pvl_action

            # Realisation des pvl de maniere proportionnelle
            # Reviens uniquement a vendre et a racheter immediatement : seule la val_nc et la val_achat sont augmentees de montant * vecteur_poids_pvl
            identifiant <- which(.subset2(x@ptf_action, num_mp) %in% num_mp_pvl)
            x@ptf_action$val_nc[identifiant]    <- .subset2(x@ptf_action, val_nc)[identifiant] + vecteur_poids_pvl * montant
            x@ptf_action$val_achat[identifiant] <- .subset2(x@ptf_action, val_achat)[identifiant] + vecteur_poids_pvl * montant
            return(list(action = x, pmvr = montant))
        } else {
            return(list(action = x, pmvr = 0))
        }
    }
)

