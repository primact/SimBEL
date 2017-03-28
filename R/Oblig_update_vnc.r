# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_vnc_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des valeurs nettes comptables d'un portefeuille obligataire.
##'
##' \code{update_vnc_oblig} est une methode permettant de mettre a jour les valeurs nettes comptables des composantes d'un portefeuille obligataire.
##' @name update_vnc_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire en detention).
##' @param vnc un vecteur de \code{numeric} a assigner a l'objet \code{\link{Oblig}}.
##' @return L'objet \code{x} dont les valeurs nettes comptables ont ete mis a jour
##' @author Prim'Act
##' @export
##' @include Oblig_class.R


setGeneric(name = "update_vnc_oblig", def = function(x,vnc){standardGeneric("update_vnc_oblig")})
setMethod(
    f = "update_vnc_oblig",
    signature = c(x = "Oblig", vnc = "numeric"),
    definition = function(x, vnc){
        # Verification des inputs
        if (nrow(x@ptf_oblig) != length(vnc)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
        if(sum(vnc < 0) > 0) { stop("[Oblig : update_vnc_oblig] :  Le vecteur de VNC initialement entre ne peut contenir de valeurs negatives. \n")}
        x@ptf_oblig$val_nc <- vnc
        return(x)
    }
)
