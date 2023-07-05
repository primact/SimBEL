#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revalo_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Revalorise les valeurs de marche du portefeuille action.
##'
##' \code{revalo_action} est une methode permettant de revaloriser et de calculer les dividendes
##' du portefeuille action sur une periode.
##' @name revalo_action
##' @docType methods
##' @param x un objet de la classe \code{\link{Action}} (decrivant le portefeuille d'action).
##' @param S un vecteur \code{numeric} correspondant a la valeur des indices actions
##' @param S_prev un vecteur \code{numeric}  correspondant a la valeur des indices actions a la periode precedente.
##' @return Un data frame compose de deux colonnes et autant de lignes que le portefeuille action a de lignes.
##' La premiere colonne decrit de le rendement annuel de chacune des actions composants le portefeuille action.
##' La seconde colonne decrit les dividendes annuelles percues au titre de chacune des actions composants le portefeuille action.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "revalo_action", def = function(x, S, S_prev) {
    standardGeneric("revalo_action")
})
setMethod(
    f = "revalo_action",
    signature = c(x = "Action", S = "numeric", S_prev = "numeric"),
    definition = function(x, S, S_prev) {
        # Recuperation du PTF actions
        ptf_action <- x@ptf_action

        # Verification des inputs
        if (length(S) != length(S_prev) | nrow(ptf_action) != length(S)) stop("[Action : revalo] : Les inputs ont des dimensions distinctes.")

        # Donnees du PTF action
        nom_table <- names(x@ptf_action)
        div <- which(nom_table == "div")
        ind_invest <- which(nom_table == "ind_invest")
        val_marche <- which(nom_table == "val_marche")
        div_ptf <- .subset2(ptf_action, div)

        # Calcul du vecteur rdt : Prise en compte du fait que les dividendes soient reinvestis ou non
        rdt <- (S / S_prev) / (1 + div_ptf * .subset2(ptf_action, ind_invest)) - 1

        # Calcul des dividendes verses en tresorerie en milieu d'annee
        div <- .subset2(ptf_action, val_marche) * sqrt(1 + rdt) * div_ptf

        # Output
        return(cbind(rdt = rdt, div = div))
    }
)
