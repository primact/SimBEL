#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de veillissement d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Veillissement d'un an des contrats epargne en euros.
##'
##' \code{vieilli_mp} est une methode permettant de vieillir
##'  les model points epargne en euros d'une peridoe.
##' @name vieilli_mp
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros.
##' @param pm_fin  un vecteur de type \code{numeric} contenant par model point les montants de PM revalorises apres participation aux benefices.
##' @param pm_gar  un vecteur de type \code{numeric} contenant par model point les montants de PM garanties revalorises apres participation aux
##' benefices au titre de la PPB initiale.
##' @param tx_revalo un vecteur de type \code{numeric} contenant par model point les taux de revalorisation nets appliques.
##' @param an une valeur \code{integer} correspondant a l'annee du calcul des prestations.
##' @return l'objet \code{x} vieilli d'une periode.
##' @author Prim'Act
##' @seealso Calcul de la revalorisation des PM \code{\link{calc_revalo_pm}}.
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R

#----------------------------------------------------
setGeneric(name = "vieilli_mp", def = function(x, pm_fin, pm_gar, tx_revalo, an) {
    standardGeneric("vieilli_mp")
})

#----------------------------------------------------
setMethod(
    f = "vieilli_mp",
    signature("EpEuroInd", "numeric", "numeric", "numeric", "integer"),
    def = function(x, pm_fin, pm_gar, tx_revalo, an) {
        # Tests
        nb_mp <- nrow(x@mp)
        if ((length(tx_revalo) != nb_mp) | (length(pm_fin) != nb_mp) | (length(pm_gar) != nb_mp)) {
            stop(("[PassifBase : vieilli_mp] : Les input dans la liste y ne sont pas de bonnes dimensions."))
        }

        # Donnees
        tab <- x@tab@tab
        mp <- x@mp
        nom_mp <- names(mp)
        num_age <- which(nom_mp == "age")
        num_anc <- which(nom_mp == "anc")

        # viellissement de 1 an
        x@mp$age <- .subset2(mp, num_age) + 1L
        x@mp$anc <- .subset2(mp, num_anc) + 1L

        # Ajustement du nombre de contrat, de la PM garanti (calcul FDB) et du taux cible.
        x@mp$nb_contr <- tab[["nb_contr"]]
        x@mp$pm_gar <- pm_gar
        x@tab@tab[["pm_gar"]] <- pm_gar
        x@mp$tx_cible_prec <- tab[["tx_cible"]]

        # Ajustement de PM de fin
        x@mp$pm <- pm_fin
        x@mp$tx_revalo_prec <- tx_revalo

        # output
        return(x)
    }
)

#----------------------------------------------------
setMethod(
    f = "vieilli_mp",
    signature("RetraiteEuroRest", "numeric", "numeric", "numeric", "integer"),
    def = function(x, pm_fin, pm_gar, tx_revalo, an) {
        # Test
        nb_mp <- nrow(x@mp)
        if ((length(tx_revalo) != nb_mp) | (length(pm_fin) != nb_mp)) {
            stop("[PassifBase : vieilli_mp] : L'input 'tx_rev' n'est pas de bonne dimension.")
        }

        # Duree du differe
        dur_diff <- .subset2(x@mp, which(names(x@mp) == "diff"))
        # Indicatrice differe
        ind_diff <- dur_diff < an

        # Ajustement de la rente
        x@mp$rente <- .subset2(x@mp, which(names(x@mp) == "rente")) * (1 + tx_revalo) * ind_diff +
            (1 - ind_diff) * .subset2(x@mp, which(names(x@mp) == "rente"))

        # Ajustement de la PM garantie
        x@mp$pm <- pm_fin
        x@mp$pm_gar <- pm_gar
        x@tab@tab[["pm_gar"]] <- pm_gar

        # Ajustement du taux cible
        x@mp$tx_cible_prec <- x@tab@tab[["tx_cible"]]

        # output
        return(x)
    }
)
