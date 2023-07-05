#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Methode de calcul des fins de projection : calc_fin_proj
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' calcule le flux et les resultats ajustes en fin de projection.
##'
##' \code{calc_fin_proj} est une methode permettant de calculer au niveau du canton les resultats financier, technique,
##' brut et net d'impot, ainsi que le flux de passifs soldant une projection.
##' @name calc_fin_proj
##' @docType methods
##' @param x est un objet de la classe \code{\link{Canton}}.
##' @param resultat_fin est la valeur \code{numeric} du resultat financier avant fin de projection.
##' @param result_tech est la valeur \code{numeric} du resultat technique avant fin de projection.
##' @param pm_fin_ap_pb est un vecteur \code{numeric} par produit
##' correspond au PM de fin avant application de la fin de projection.
##' @param tx_pb est un vecteur \code{numeric} par produit
##' correspond au taux de PB contractuel.
##' @param tx_enc_moy est un vecteur \code{numeric} par produit
##' correspond au taux chargement sur encours moyens.
##' @return \code{flux_fin_passif} un vecteur de flux de fin par produit.
##' @return \code{result_tech} le montant de resultat technique en fin de projection.
##' @return \code{result_fin} le montant de resultat finanacier en fin de projection.
##' @return \code{result_brut} le montant de resultat brut d'impot en fin de projection.
##' @return \code{result_net} le montant de resultat net d'impot en fin de projection.
##' @return \code{impot} le montant d'impot sur le resultat en fin de projection.
##' @export
##' @include Canton_class.R

setGeneric(name = "calc_fin_proj", def = function(x, resultat_fin, result_tech, pm_fin_ap_pb, tx_pb, tx_enc_moy) {
    standardGeneric("calc_fin_proj")
})

setMethod(
    f = "calc_fin_proj",
    signature = c(
        x = "Canton", resultat_fin = "numeric", result_tech = "numeric",
        pm_fin_ap_pb = "numeric", tx_pb = "numeric", tx_enc_moy = "numeric"
    ),
    definition = function(x, resultat_fin, result_tech, pm_fin_ap_pb, tx_pb, tx_enc_moy) {
        # Recuperation de donnees
        ptf_fin <- x@ptf_fin

        #---------------------------------------------------------------
        # Etape 1 : Ajustement sur actifs
        #---------------------------------------------------------------

        # Choix de modelisation : on force la vente de PMVL action et immo et on annule la PRE
        flux_fin_actif <- ptf_fin@pvl_action + ptf_fin@mvl_action + ptf_fin@pvl_immo + ptf_fin@mvl_immo

        # Choix de modelisation : on force la vente de PMVL oblig que lon compense avec la variation de RC
        flux_fin_actif <- flux_fin_actif + ptf_fin@pvl_oblig + ptf_fin@mvl_oblig -
            calc_RC(ptf_fin@rc, ptf_fin@pvl_oblig + ptf_fin@mvl_oblig)[["var_rc"]]

        # Ajustement du resultats financier et technique
        resultat_fin <- resultat_fin + flux_fin_actif
        result_tech <- result_tech + ptf_fin@pre@val_courante # Choix de modelisation : Reprise de PRE


        #---------------------------------------------------------------
        # Etape 2 : Revalorisation finale des passifs
        #---------------------------------------------------------------

        # Choix de modelisation : attribution du flux de fin a l'actif en PB et attribution au prorata des PM de fin
        sum_pm_fin <- sum(pm_fin_ap_pb)
        len_pm_fin <- length(pm_fin_ap_pb)
        if (sum_pm_fin != 0) {
            coef_alloc <- pm_fin_ap_pb / sum_pm_fin
        } else { # Division par 0
            coef_alloc <- rep(1, len_pm_fin) / len_pm_fin
        }

        # Coefficient de mise a l'echelle
        vnc_actif <- .subset2(print_alloc(ptf_fin), 3L * 5L)
        if (vnc_actif == 0) { # Gestion des divisions par 0
            coef_scale <- 0
        } else {
            coef_scale <- (pm_fin_ap_pb + x@ppb@valeur_ppb) / vnc_actif
        }


        revalo_fin_passif <- max(0, flux_fin_actif) * coef_alloc * coef_scale

        # Choix de modelisation : application des taux de PB contractuel et liquidation de la PPB
        revalo_fin_passif <- revalo_fin_passif * tx_pb + x@ppb@valeur_ppb * coef_alloc
        # Calcul du net de chargement sur encours
        revalo_fin_passif <- revalo_fin_passif * (1 - tx_enc_moy)
        result_tech <- result_tech - sum(revalo_fin_passif)

        # Calcul d'un flux de fin d'annee
        # Choix de modelisation : la PSAP residuelle est attribuee en totalite au passif modelise
        flux_fin_passif <- pm_fin_ap_pb + revalo_fin_passif + x@ptf_passif@autres_reserves@psap_valeur * coef_alloc

        # La reprise de PSAP n'a pas d'effet sur le resultat technique car elle est supposee
        # etre compensee par un flux immediat de prestations


        #---------------------------------------------------------------
        # Etape 3 : Recalcul du resultat
        #---------------------------------------------------------------

        # Calcul du resultats brut et net d'impot
        result_brut <- result_tech + resultat_fin
        result_net <- result_brut * (1 - x@hyp_canton@tx_import) * (result_brut > 0) + result_brut * (result_brut <= 0)
        impot <- result_brut - result_net

        # Output
        return(list(
            flux_fin_passif = flux_fin_passif,
            result_tech = result_tech,
            resultat_fin = resultat_fin,
            result_brut = result_brut,
            result_net = result_net,
            impot = impot
        ))
    }
)
