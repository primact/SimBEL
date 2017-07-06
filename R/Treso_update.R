#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet d'integrer un flux (entrant ou sortant) au compte de tresorerie d'un portefeuille financier.
##'
##' \code{update_treso} est une methode permettant d'integrer un flux au compte de tresorerie.
##' @name update_treso
##' @docType methods
##' @param x objet de la classe \code{\link{Treso}}, correspondant a l'actif Tresorerie d'un assureur anterieur a integration d'un flux.
##' @param flux est un \code{numeric} correspondant a un flux.
##' S'il est positif, le flux est entrant.
##' S'il est negatif, le flux est sortant.
##' @return L'objet \code{x} mis a jour du flux precise en input.
##' @author Prim'Act
##' @export
##' @include Treso_class.R


# Fonction qui permet d'accroitre ou de decroitre un objet tresorerie d'un certain flux
setGeneric(name = "update_treso", def = function(x,flux){standardGeneric("update_treso")})
setMethod(
    f = "update_treso",
    signature = c(x = "Treso", flux = "numeric"),
    definition = function(x, flux){
        
        # Test input
        if (length(flux) > 1) stop("[Treso : update_treso] : Le flux d'input doit etre compose d'un unique element \n")
        
        # Donnees
        ptf_treso <- x@ptf_treso
        nom_ptf <- names(ptf_treso)
        
        # Mise a jour des donnees
        x@ptf_treso$val_marche <- .subset2(ptf_treso, which(nom_ptf == "val_marche")) + flux
        x@ptf_treso$val_nc     <- .subset2(ptf_treso, which(nom_ptf == "val_nc")) + flux
        
        # Output
        return(x)
    }
)
