#--------------------------------------------------------------------------------------------------------------------
# update_PortFin_reference
#--------------------------------------------------------------------------------------------------------------------
##' Evalue et met a jour les objets constituants un PortFin_reference.
##'
##' \code{update_PortFin_reference} est une methode permettant de calculer et mettre a jour un portefeuille financier
##' de reinvestissement pour le faire vieillir d'une annee.
##' @name update_PortFin_reference
##' @docType methods
##' @param an \code{numeric} correspond a l'annee de projection du portefeuille financier de reinvestissement.
##' @param x objet de la classe \code{\link{PortFin}}, correspondant au portefeuille financier de
##' reinvestissement avant l'etape de vieillissement.
##' @param mp_ESG est un objet de la classe \code{\link{ModelPointESG}}, decrivant les conditions
##' economiques permettant d'effectuer le vieillissement du portefeuille financier de reinvestissement.
##' @return L'objet \code{x} renvoye correspond au portefeuille financier de reinvesitssement veilli d'une annee.
##' @author Prim'Act
##' @export
##' @seealso La fonction de mise a jour specifique au portefeuille \code{\link{update_PortFin}}.
##' @include PortFin_class.R ModelPointESG_class.R


setGeneric(name = "update_PortFin_reference", def = function(an, x, mp_ESG) {
    standardGeneric("update_PortFin_reference")
})
setMethod(
    f = "update_PortFin_reference",
    signature = c(an = "integer", x = "PortFin", mp_ESG = "ModelPointESG"),
    definition = function(an, x, mp_ESG) {
        table_rdt <- calc_rdt(x, mp_ESG)

        # Mise a jour de l'annnee
        x@annee <- an

        ### Update Action
        # Donnees
        ptf_action <- x@ptf_action@ptf_action
        val_marche <- calc_vm_action(x@ptf_action, table_rdt[["rdt_action"]][, "rdt"])

        # Mise a jour
        ptf_action$val_marche <- val_marche # VM
        ptf_action$val_nc <- val_marche # VNC
        ptf_action$val_achat <- val_marche # VA
        x@ptf_action@ptf_action <- ptf_action # Mise a jour PTF action


        ### Update Immo
        # Donnees
        ptf_immo <- x@ptf_immo@ptf_immo
        val_marche <- calc_vm_immo(x@ptf_immo, table_rdt[["rdt_immo"]][, "rdt"])

        # Mise a jour
        ptf_immo$val_marche <- val_marche # VM
        ptf_immo$val_nc <- val_marche # VNC
        ptf_immo$val_achat <- val_marche # VA
        x@ptf_immo@ptf_immo <- ptf_immo # Mise a jourt PTF immo


        ### Update Oblig
        # Donnees
        ptf_oblig <- x@ptf_oblig@ptf_oblig
        nom_oblig <- names(ptf_oblig)
        val_marche <- calc_vm_oblig(x@ptf_oblig, mp_ESG@yield_curve)

        # Mise a jour
        ptf_oblig$val_marche <- val_marche # VM
        ptf_oblig$val_nc <- val_marche # VNC
        ptf_oblig$val_achat <- val_marche # VA
        x@ptf_oblig@ptf_oblig <- ptf_oblig # Mise a jourt PTF oblig

        # Output
        return(x)
    }
)
