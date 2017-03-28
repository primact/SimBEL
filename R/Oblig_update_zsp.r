# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_zsp_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des z-spreads d'un portefeuille obligataire.
##'
##' \code{update_zsp_oblig} est une methode permettant de mettre a jour les z-spreads des composantes d'un portefeuille obligataire.
##' @name update_zsp_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire en detention).
##' @param zspread un vecteur de \code{numeric} a assigner a l'objet \code{Obligation}.
##' @return L'objet \code{x} dont les zspreads ont ete mis a jour
##' @author Prim'Act
##' @export
##' @include Oblig_class.R


setGeneric(name = "update_zsp_oblig", def = function(x,zspread){standardGeneric("update_zsp_oblig")})
setMethod(
    f = "update_zsp_oblig",
    signature = c(x = "Oblig", zspread = "numeric"),
    definition = function(x, zspread){
        # Verification des inputs
        if (nrow(x@ptf_oblig) != length(zspread)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
        x@ptf_oblig$zspread <- zspread
        return(x)
    }
)
