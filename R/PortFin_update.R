#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue et met a jour les objets constituants un PortFin.
##'
##' \code{update_PortFin} est une methode permettant de calculer et mettre a jour un portefeuille financier
##' pour le faire vieillir d'une annee.
##' @name update_PortFin
##' @docType methods
##' @param an \code{integer} correspond a l'annee de projection du portefeuille financier.
##' @param x objet de la classe \code{\link{PortFin}}, correspondant au portefeuille financier de l'assureur
##'  avant l'etape de vieillissement.
##' @param new_mp_ESG est un objet de la classe \code{\link{ModelPointESG}}, decrivant les conditions
##'  economiques permettant d'effectuer le vieillissement du portefeuille financier.
##' @param flux_milieu est une valeur \code{numeric} correspondant a la somme des flux percus en milieu d'annee
##' (coupons des obligations, loyers immobiliers, dividendes des actions, revenus de la tresorerie).
##' @param flux_fin est une valeur \code{numeric}  correspondant a la somme des flux percus en fin d'annee (tombee d'echeance d'obligation).
##' @return \code{ptf} un vecteur contenant les flux de sortie en echeance de l'annee
##' @return \code{revenu_fin} les revenus realises au cours de la periode (coupons, tombees d'echeance, dividendes et loyers).
##' @return \code{revenu_fin_det} les revenus detaille pour chaque actif realises au cours de la periode.
##' @return \code{var_vnc_oblig} la variation de valeur nette comptable obligataire.
##' @author Prim'Act
##' @export
##' @seealso Le vieillissement des classes d'actifs : \code{\link{vieillissement_action_PortFin}},
##' \code{\link{vieillissement_immo_PortFin}}, \code{\link{vieillissement_oblig_PortFin}},
##' \code{\link{vieillissement_treso_PortFin}}.
##' Le calcul des rendements : \code{\link{calc_rdt}}.
##' La methode de mise a jour specifique au portefeuille de reinvestissement \code{\link{update_PortFin_reference}}.
##' @include PortFin_class.R ModelPointESG_class.R

setGeneric(name = "update_PortFin", def = function(an, x, new_mp_ESG, flux_milieu, flux_fin) {
    standardGeneric("update_PortFin")
})
setMethod(
    f = "update_PortFin",
    signature = c(an = "integer", x = "PortFin", new_mp_ESG = "ModelPointESG", flux_milieu = "numeric", flux_fin = "numeric"),
    definition = function(an, x, new_mp_ESG, flux_milieu, flux_fin) {
        # Viellissement du portefeuille financier
        # Mise a jour de l'annee
        x@annee <- an

        # Vieillissement des obligations
        liste_res <- vieillissement_oblig_PortFin(x, new_mp_ESG)
        # Mise a jour du portefeuille
        x <- liste_res[["portFin"]]
        # Extraction des echeances et des coupons
        echeance <- liste_res[["echeance"]]
        coupon <- liste_res[["coupon"]]
        # Extraction de la variation de VNC obligataire
        var_vnc_oblig <- liste_res[["var_vnc_oblig"]]

        # Calcul de la table des rdt actions, tresorerie et immobiliers avec les nouveaux cours
        table_rdt <- calc_rdt(x, new_mp_ESG)

        # Vieillissement des actions
        liste_res <- vieillissement_action_PortFin(x, table_rdt)
        # Mise a jour du portefeuille
        x <- liste_res[["portFin"]]
        # Extraction des dividendes
        dividende <- liste_res[["dividende"]]

        # Vieillissement de l'immobilier
        liste_res <- vieillissement_immo_PortFin(x, table_rdt)
        # Mise a jour du portefeuille
        x <- liste_res[["portFin"]]
        # Extraction des loyers
        loyer <- liste_res[["loyer"]]

        # Calcul des revenus financiers de milieu d'annne
        revenu_fin <- coupon + dividende + loyer

        # Mise a jour des PMVL Action/Immo/Oblig
        x <- do_update_pmvl(x)
        # Revenu de la treso
        x <- vieillissement_treso_PortFin(x, revenu_fin + flux_milieu, echeance + flux_fin, table_rdt)

        return(list(
            ptf = x,
            revenu_fin = revenu_fin,
            revenu_fin_det = list(
                oblig = coupon,
                action = dividende,
                immo = loyer
            ),
            var_vnc_oblig = var_vnc_oblig
        ))
    }
)
