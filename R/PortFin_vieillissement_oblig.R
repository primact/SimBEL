#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_oblig_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Effectue le vieillissement du portefeuille obligataire d'un portefeuille financier.
##'
##' \code{vieillissement_oblig_PortFin} est une methode permettant de projeter la composante obligataire d'un portefeuille financier.
##' @name vieillissement_oblig_PortFin
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}, correspondant au portefeuille financier
##' de l'assureur avant l'etape de vieillissement.
##' @param new_mp_ESG est un objet de type \code{\link{ModelPointESG}}, correspondant aux conditions
##' economiques de l'annee du vieillissement.
##' @return \code{portFin} le portefeuille financier dont l'attribut \code{ptf_oblig} a ete vieilli d'une annee.
##' @return \code{coupon} le montant des coupons percus en milieu d'annee suite au vieillissement du portefeuille obligataire.
##' @return \code{echeance} le montant des echeance percus en fin d'annee suite au vieillissement du portefeuille obligataire.
##' @return \code{var_vnc_oblig} la variation de VNC constatee sur les titres non arrives en echeance.
##' @author Prim'Act
##' @export
##' @seealso La fonction de calcul des rendements des actifs \code{\link{calc_rdt}}.
##' @include PortFin_class.R ModelPointESG_class.R


setGeneric(name = "vieillissement_oblig_PortFin", def = function(x, new_mp_ESG) {
    standardGeneric("vieillissement_oblig_PortFin")
})
setMethod(
    f = "vieillissement_oblig_PortFin",
    signature = c(x = "PortFin", new_mp_ESG = "ModelPointESG"),
    definition = function(x, new_mp_ESG) {
        # Extraction PTF
        ptf_oblig <- x@ptf_oblig

        # Verification input :
        if (nrow(ptf_oblig@ptf_oblig) > 0) {
            # Donnees
            nom_ptf <- names(ptf_oblig@ptf_oblig)
            num_sd <- which(nom_ptf == "sd")
            num_val_nc <- which(nom_ptf == "val_nc")

            # Recalcul des VM obligs avec nouvelle courbe de taux et vieillissement du pas de temps,
            # valeur nette comptable debut
            vnc_debut <- x@vm_vnc_precedent[["vnc"]][["oblig"]]

            # Calcul des flux
            flux <- calc_flux_annee(ptf_oblig)
            coupon <- sum(flux[["tombee_coupon"]]) # Tombee de coupons
            echeance <- sum(flux[["tombee_echeance"]]) # Tombee de coupons

            # Viellissement des maturites et suppression des lignes obligataires arrivees au terme
            ptf_oblig <- update_mat_res(ptf_oblig)
            # Mise a jour des VM
            if (nrow(ptf_oblig@ptf_oblig) > 0) {
                ptf_oblig <- update_vm_oblig(ptf_oblig, calc_vm_oblig(ptf_oblig, new_mp_ESG@yield_curve))
            }
            # Mise a jour des VNC
            if (nrow(ptf_oblig@ptf_oblig) > 0) {
                ptf_oblig <- update_vnc_oblig(ptf_oblig, calc_vnc(ptf_oblig, .subset2(ptf_oblig@ptf_oblig, num_sd)))
            }

            # Calcul la VNC de fin et sa variation
            if (nrow(ptf_oblig@ptf_oblig) > 0) {
                vnc_fin <- sum(.subset2(ptf_oblig@ptf_oblig, num_val_nc))
            } else {
                vnc_fin <- 0
            }

            # Variation hors remboursements
            var_vnc_oblig <- vnc_fin - vnc_debut + echeance

            # Mise a jour PTF oblig
            x@ptf_oblig <- ptf_oblig
        } else {
            coupon <- 0
            echeance <- 0
            var_vnc_oblig <- 0
        }

        # Output
        return(list(portFin = x, coupon = coupon, echeance = echeance, var_vnc_oblig = var_vnc_oblig))
    }
)
