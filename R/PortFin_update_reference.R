#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 26/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------
##' Evalue et met a jour les objets constituants un PortFin_reference.
##'
##' \code{update_PortFin_reference} est une methode permettant de calculer et mettre a jour un portefeuille financier
##' de reinvestissement suite a un vieillissement.
##' @name update_PortFin_reference
##' @docType methods
##' @param an \code{numeric} correspond a l'annee de projection du portefeuille financier de reinvestissement. 
##' @param x objet de la classe \code{PortFin}, correspondant au portefeuille financier de reinvestissement avant l'etape de vieillissement.
##' @param mp_ESG est un objet de la classe \code{ModelPointESG}, decrivant les conditions economiques permettant d'effectuer le vieillissement du portefeuille financier de reinvestissement.
##' @return L'objet de la classe \code{PortFin} renvoye correspond au portefeuille financier de reinvesitssement veilli d'une annee.
##' @author Prim'Act
##' @export
##' @seealso La fonction de mise a jour specifique au portefeuille \code{\link{update_PortFin}}.
##' @aliases PortFin
##' @include PortFin_class.R ModelPointESG_class.R


setGeneric(name = "update_PortFin_reference", def = function(an, x, mp_ESG){standardGeneric("update_PortFin_reference")})
setMethod(
    f = "update_PortFin_reference",
    signature = c(an = "numeric", x = "PortFin", mp_ESG = "ModelPointESG"),
    definition = function(an, x, mp_ESG){

        table_rdt <- calc_rdt(x, mp_ESG)

        x@annee <- as.integer(an)
        # Update Action

        table_rdt[["rdt_action"]][["rdt"]]
            # VM
            x@ptf_action@ptf_action$val_marche  <- calc_vm_action(x@ptf_action, table_rdt[["rdt_action"]]$rdt)
            # VNC
            x@ptf_action@ptf_action$val_nc      <- x@ptf_action@ptf_action$val_marche
            # VA
            x@ptf_action@ptf_action$val_achat   <- x@ptf_action@ptf_action$val_marche
        # Update Immo
            # VM
            x@ptf_immo@ptf_immo$val_marche  <-  calc_vm_immo(x@ptf_immo,  table_rdt[["rdt_immo"]]$rdt)
            # VNC
            x@ptf_immo@ptf_immo$val_nc      <- x@ptf_immo@ptf_immo$val_marche
            # VA
            x@ptf_immo@ptf_immo$val_achat   <- x@ptf_immo@ptf_immo$val_marche
        # Update Oblig
            # VM
            x@ptf_oblig@ptf_oblig$val_marche  <- calc_vm_oblig(x@ptf_oblig, mp_ESG@yield_curve)
            # VNC
            x@ptf_oblig@ptf_oblig$val_nc      <- x@ptf_oblig@ptf_oblig$val_marche
            # VA
            x@ptf_oblig@ptf_oblig$val_achat   <- x@ptf_oblig@ptf_oblig$val_marche
        return(x)
    }
)
