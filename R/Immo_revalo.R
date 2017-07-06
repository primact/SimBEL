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

setGeneric(name = "revalo_immo", def = function(x,S,S_prev) {standardGeneric("revalo_immo")})
setMethod(
    f = "revalo_immo",
    signature = c(x = "Immo", S = "numeric", S_prev = "numeric"),
    definition = function(x, S, S_prev){
        
        # Donnees
        ptf_immo    <- x@ptf_immo
        nb_immo     <- nrow(ptf_immo)
        nom_table   <- names(ptf_immo)
        loyer       <- which(nom_table == "loyer")
        ind_invest  <- which(nom_table == "ind_invest")
        val_marche  <- which(nom_table == "val_marche")
        
        # Verification des inputs
        if (length(S) != length(S_prev) | nb_immo != length(S)) stop("[Immo : revalo] : Les inputs ont des dimensions distinctes.")
        
        # Extraction de donnees
        loyer_immo <- .subset2(ptf_immo, loyer)
        
        # Calcul du vecteur rdt : Prise en compte du fait que les loyers soient reinvestis ou non
        rdt <- (S / S_prev) / (1 + loyer_immo * .subset2(ptf_immo, ind_invest)) - 1
        
        # Calcul des loyers verses en tresorerie en milieu d'annee
        loyer <- .subset2(ptf_immo, val_marche) * sqrt(1 + rdt) * loyer_immo
        
        # Ouput
        return(cbind(rdt = rdt, loyer = loyer))
    }
)
