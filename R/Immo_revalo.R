#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revalo_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Revalorise les valeurs de marche du portefeuille immobilier.
##'
##' \code{revalo_immo} est une methode permettant de revaloriser et de calculer les loyers
##' du portefeuille immobilier sur une periode.
##' @name revalo_immo
##' @docType methods
##' @param x un objet de la classe \code{\link{Immo}} (decrivant le portefeuille d'immobilier).
##' @param S un vecteur \code{numeric} correspondant a la valeur des indices immobiliers
##' @param S_prev un vecteur \code{numeric}  correspondant a la valeur des indices immobiliers a la periode precedente.
##' @return Un data frame compose de deux colonnes et autant de lignes que le portefeuille immobilier a de lignes.
##' La premiere colonne decrit de le rendement annuel de chacune des lignes d'immobilier composants le portefeuille immobilier.
##' La seconde colonne decrit les loyers annuelles percues au titre de chacune des lignes d'immobilier composants le portefeuille immobilier.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "revalo_immo", def = function(x,S,S_prev){standardGeneric("revalo_immo")})
setMethod(
  f = "revalo_immo",
  signature = c(x = "Immo", S = "numeric", S_prev = "numeric"),
  definition = function(x,S,S_prev){
    nom_table   <- names(x@ptf_immo)
    loyer       <- which(nom_table == "loyer")
    ind_invest  <- which(nom_table == "ind_invest")
    val_marche  <- which(nom_table == "val_marche")

    # Verification des inputs
    if (length(S) != length(S_prev) | nrow(x@ptf_immo) != length(S)) {stop("[Immo : revalo] : Les inputs ont des dimensions distinctes.")}
    # Calcul du vecteur rdt : Prise en compte du fait que les loyers soient reinvestis ou non
    rdt <- (S/S_prev) / (1 + .subset2(x@ptf_immo, loyer) * .subset2(x@ptf_immo, ind_invest)) - 1
    # Calcul des loyers verses en tresorerie en milieu d'annee
    loyer <- .subset2(x@ptf_immo, val_marche) * (1 + rdt)^0.5 * .subset2(x@ptf_immo, loyer)
    return(data.frame(rdt,loyer))
  }
)
