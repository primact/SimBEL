#-----------------------------------------------------------------------------------------
#         calc_res_ana
#-----------------------------------------------------------------------------------------
##' calcule le resultat analytique apres prise en compte de la participation aux benefices.
##'
##' \code{calc_res_ana} est une methode permettant de calculer le resultat analytique
##' apres attribution de participation aux benefices.
##' @name calc_res_ana
##' @docType methods
##' @param passif_av_pb est une liste produit par la methode \code{\link{viellissement_av_pb}}.
##' @param passif_ap_pb est une liste produit par la methode \code{\link{vieillissment_ap_pb}}.
##' @param resultat_fin est une valeur \code{numeric} correspondant au resultat financier.
##' @param ppb est un objet de la classe \code{\link{Ppb}} qui renvoie l'etat courant de la PPB.
##' @param result_revalo
##' @param var_pre est une valeur \code{numeric} correspondant a la variation de PRE.
##' @return Un compte de resultat analytique apres participation aux benefices.
##' @export
##' @include Canton_class.R

setGeneric(
    name = "calc_res_ana",
    def = function(passif_av_pb,
                   passif_ap_pb,
                   resultat_fin,
                   ppb,
                   result_revalo,
                   var_pre) {
        standardGeneric("calc_res_ana")
    }
)


setMethod(
    f = "calc_res_ana",
    signature = c(
        passif_av_pb = "list",
        passif_ap_pb = "list",
        resultat_fin = "numeric",
        ppb = "Ppb",
        result_revalo = "list",
        var_pre = "numeric"
    ),
    definition = function(passif_av_pb,
                          passif_ap_pb,
                          resultat_fin,
                          ppb,
                          result_revalo,
                          var_pre) {
        #### FLUX ET STOCKS ####
        flux_av_pb <- passif_av_pb[["result_av_pb"]][["flux_agg"]]
        stock_av_pb <- passif_av_pb[["result_av_pb"]][["stock_agg"]]
        flux_ap_pb <- passif_ap_pb[["flux_agg"]]
        stock_ap_pb <- passif_ap_pb[["stock_agg"]]


        #--- EXTRACTION DES STOCKS ET FLUX ABSOLUS ----
        chgt_sur_prod <- flux_av_pb[, "pri_chgt"]
        frais_sur_primes <- flux_av_pb[, "frais_var_prime"] +
            flux_av_pb[, "frais_fixe_prime"]
        primes_totales <- flux_av_pb[, "pri_brut"]
        dc <- flux_av_pb[, "dc"]
        ech <- flux_av_pb[, "ech"]
        arr_rentes <- flux_av_pb[, "arr_charg"]
        rach_struct <- flux_av_pb[, "rach_part_struct"] +
            flux_av_pb[, "rach_tot_struct"]
        rach_conj <- flux_av_pb[, "rach_mass"] +
            flux_av_pb[, "rach_part_conj"] +
            flux_av_pb[, "rach_tot_conj"]
        pb_liq <- flux_av_pb[, "rev_prest"] -
            flux_av_pb[, "it_tech_prest"]
        it_tech_prest <- flux_av_pb[, "it_tech_prest"]
        it_tech_stock <- flux_av_pb[, "it_tech_stock"]
        chgt_sur_encours <- flux_av_pb[, "enc_charg_prest"]
        pm_deb <- stock_av_pb[, "pm_deb"]
        pm_fin <- stock_ap_pb[, "pm_fin_ap_pb"]
        prov_div <- passif_av_pb[["var_psap"]] + passif_av_pb[["var_pgg"]]
        revalo_ic_pb <- flux_ap_pb[, "rev_stock_brut_ap_pb"]
        csg <- flux_ap_pb[, "soc_stock_ap_pb"]
        ic <- it_tech_prest + it_tech_stock
        pb <- revalo_ic_pb - ic
        frais_sur_encours <- flux_av_pb[, "frais_var_enc"]
        frais_sur_presta <- flux_av_pb[, "frais_var_prest"]
        autres_frais <- flux_av_pb[, "frais_fixe_enc"] +
            flux_av_pb[, "frais_fixe_prest"]

        #--- VENTILATION RESULTAT ET REP/DOT PB ----
        #### PRORATA DE LA PM MOYENNE AUGMENTEE DE LA PPB INITIALE ####
        alloc_pm_moy <- prop.table(stock_av_pb[, "pm_moy"] + ppb@ppb_debut)

        #### BESOIN ####
        bes <- flux_av_pb[, "bes_tx_cible"] -
            flux_av_pb[, "rev_stock_nette"]

        #### BESOIN POSITIF ####
        bes_pos <- pmax(bes, 0)
        if (!all(bes_pos == 0)) {
            #### AU PRORATA ####
            alloc_dot_ppb <- prop.table(bes_pos)
        } else {
            alloc_dot_ppb <- rep(1 / length(bes_pos), length(bes_pos))
        }
        #### BESOIN NEGATIF ####
        bes_neg <- pmin(bes, 0)
        if (!all(bes_neg == 0)) {
            alloc_rep_ppb <- prop.table(bes_neg)
        } else {
            alloc_rep_ppb <- rep(1 / length(bes_neg), length(bes_neg))
        }

        #--- MISE EN FORME ET EN SIGNE DES POSTES ----
        res_ana <- list(
            res_tech = list(
                sur_prod = list(
                    chgt_sur_prod = chgt_sur_prod,
                    frais_sur_primes = -(frais_sur_primes)
                ),
                sur_encours = list(
                    cotis_nettes = list(
                        primes_totales = primes_totales,
                        chgt_sur_prod = -(chgt_sur_prod)
                    ),
                    prestations = list(
                        rachats = list(
                            rach_struct = -(rach_struct),
                            rach_conj = -(rach_conj)
                        ),
                        dc = -(dc),
                        ech = -(ech),
                        arr_rentes = -(arr_rentes),
                        pb_liq = -(pb_liq),
                        it_tech_prest = -(it_tech_prest),
                        chgt_sur_encours = chgt_sur_encours
                    ),
                    charges_prov = list(
                        prov_deb = pm_deb,
                        prov_fin = -(pm_fin),
                        prov_div = -(prov_div),
                        revalo_ic_pb = revalo_ic_pb,
                        csg = -(csg)
                    ),
                    frais_sur_encours = -(frais_sur_encours),
                    frais_sur_presta = -(frais_sur_presta),
                    autres_frais = -(autres_frais)
                )
            ),
            res_fin = list(
                prod_fin = list(
                    revenu_placements = resultat_fin * alloc_pm_moy
                ),
                rep_dot_pre = -(var_pre),
                ic = -(ic),
                pb = -(pb),
                ppe_pbr = list(
                    rep = result_revalo[["pb_attrib"]][["pb_rep"]] * alloc_rep_ppb,
                    dot = -(result_revalo[["pb_attrib"]][["pb_dot"]] * alloc_dot_ppb)
                )
            )
        )
        return(res_ana)
    }
)
