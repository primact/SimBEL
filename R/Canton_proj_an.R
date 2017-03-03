#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script execute les operations a effectuer au niveau du canton sur une annee
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           proj_an
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Projette un canton sur une annee.
##'
##' \code{proj_an} est une methode permettant de projeter un canton sur une annee pour le calcul
##' des flux de best estimate.
##' @name proj_an
##' @docType methods
##' @param x est un objet de type \code{Canton}.
##' @param annee_fin est une valeur \code{numeric} correpondant a l'annee de fin de projection.
##' @return Une liste comprenant l'objet x mis a jour, les flux de best estimate, les resultats comptables.
##' @author Prim'Act
##' @export
##' @aliases Canton
##'
setGeneric(name = "proj_an", def = function(x, annee_fin){standardGeneric("proj_an")})
setMethod(
  f = "proj_an",
  signature = c(x = "Canton", annee_fin = "numeric"),
  definition = function(x, annee_fin){

    # TO DO
    # PPB sur la partie garantie

    #---------------------------------------------------------------
    # Etape 1 : Mise a jour des annees de projection
    #---------------------------------------------------------------
    x@annee <- x@annee + as.integer(1)

    #---------------------------------------------------------------
    # Etape 2 : variables economiques utilisees au passif
    # Remarque : A terme, revoir ce mode de fonctionemment qui n'est pas ideal en terme de structure.
    #---------------------------------------------------------------

    # Coefficient d'inflation
    coef_inf <- x@mp_esg@indice_inflation

    # liste des rendements
    list_rd <- calc_rdt_marche_ref(x@ptf_passif@ht@param_comport[[x@hyp_canton@method_taux_cible]],
                                   x@mp_esg)

    #---------------------------------------------------------------
    # Etape 3 : Gestion des passifs avant participation aux benefices
    #---------------------------------------------------------------
    # Evaluation du passif vieilli d'un an
    passif_av_pb <- viellissement_av_pb(x@annee, x@ptf_passif, coef_inf, list_rd, x@hyp_canton@tx_soc)
    # Mise a jour des passifs
    x@ptf_passif <- passif_av_pb[["ptf"]]

    #---------------------------------------------------------------
    # Etape 4 : Gestion des actifs avant allocation
    #---------------------------------------------------------------
    actif_vieil <- update_PortFin(x@annee, x@ptf_fin, x@mp_esg, passif_av_pb[["flux_milieu"]], passif_av_pb[["flux_fin"]])
    # Mise a jour des actifs
    x@ptf_fin <- actif_vieil[["ptf"]]
    # Extraction des revenus financiers et de la variation de VNC obligataires
    revenu_fin <- actif_vieil[["revenu_fin"]]
    var_vnc_oblig <- actif_vieil[["var_vnc_oblig"]]
    # Libere de la memoire
    rm(actif_vieil)

    # Mise a jour du portfeuille de reference
    x@param_alm@ptf_reference <- update_PortFin_reference(x@annee, x@param_alm@ptf_reference, x@mp_esg)

    #---------------------------------------------------------------
    # Etape 5 : Re-allocation des actifs et mise a jour de la PRE et de la RC
    #---------------------------------------------------------------
    # Reallocation a l'allocation cible

    actif_realloc <- reallocate(x@ptf_fin, x@param_alm@ptf_reference, x@param_alm@alloc_cible)
    x@ptf_fin <- actif_realloc[["portFin"]]

    #---------------------------------------------------------------
    # Etape 6 : Calcul des frais financiers
    #---------------------------------------------------------------
    frais_fin <- calc_frais_fin(x@ptf_fin@frais_fin, actif_realloc[["plac_moy_vm"]], coef_inf)

    #---------------------------------------------------------------
    # Etape 7 : Calcul du resultat technique
    #---------------------------------------------------------------
    # Calcul du resultats technique avec attribution de PB
    resultat_tech <- calc_result_technique(passif_av_pb, actif_realloc[["var_pre"]])

    #---------------------------------------------------------------
    # Etape 8 : Calcul du resultat financier et du TRA
    #---------------------------------------------------------------

    # Evaluation du resultat financier
    resultat_fin <- calc_resultat_fin(revenu_fin + var_vnc_oblig, actif_realloc[["pmvr"]],
                                 frais_fin, actif_realloc[["var_rc"]])

    # Calcul du TRA
    tra <- calc_tra(actif_realloc[["plac_moy_vnc"]], resultat_fin)

    #---------------------------------------------------------------
    # Etape 9 : Application de la politique de revalorisation
    #---------------------------------------------------------------

    # Calcul de la politique de revalorisation
    result_revalo <- calc_revalo(x, passif_av_pb, tra, actif_realloc[["plac_moy_vnc"]], resultat_tech)

    # Mise a jour de la PPB
    x@ppb <- result_revalo[["ppb"]]

    #---------------------------------------------------------------
    # Etape 10 : Mise a jour des passifs
    #---------------------------------------------------------------

    # Evaluation du passif vieilli d'un an apres pb
    passif_ap_pb <- vieillissment_ap_pb(x@ptf_passif, result_revalo[["add_rev_nette_stock"]], x@hyp_canton@tx_soc)
    # Mise a jour des passifs a fin d'annee
    x@ptf_passif <- passif_ap_pb[["ptf"]]

    #---------------------------------------------------------------
    # Etape 11 : Mise a jour des actifs
    #---------------------------------------------------------------

    # Realisation des eventuelles ventes de PVL actions realisees a l etape 9
    # mise a jour des actions.
    x@ptf_fin@ptf_action <- sell_pvl_action(x@ptf_fin@ptf_action, result_revalo[["pmvl_liq"]])[["action"]]

    # Mise a jour des PMVL Action/Immo/Oblig
    x@ptf_fin <- do_update_pmvl(x@ptf_fin)

    # Re-evaluation et mise a jour de la PRE
    res_pre <- calc_PRE(x@ptf_fin@pre, x@ptf_fin@pvl_action + x@ptf_fin@mvl_action +
                          x@ptf_fin@pvl_immo + x@ptf_fin@mvl_immo)
    # Mise a jour de la valeur courante de la PRE
    x@ptf_fin["pre"] <-  do_update_PRE_val_courante(x@ptf_fin@pre, res_pre[["pre_courante"]])

    #---------------------------------------------------------------
    # Etape 12 : Mise a jour des resultats financier et technique
    #---------------------------------------------------------------

    # Mise a jour du resultat financier des eventuelles PVL actions realisees a l etape 9
    resultat_fin <- resultat_fin + result_revalo[["pmvl_liq"]]

    # Mise a jour du resultat technique
    resultat_tech <- calc_result_technique_ap_pb(passif_av_pb, passif_ap_pb, x@ppb, res_pre[["var_pre"]])

    # Calcul du resultats brut et net d'impot
    result_brut <- resultat_tech + resultat_fin
    result_net <- result_brut * (1 - x@hyp_canton@tx_import) * (result_brut > 0) + result_brut * (result_brut <= 0)
    impot <- result_brut - result_net

    #---------------------------------------------------------------
    # Etape 13 : Mise a jour (des elements restants) du canton pour l'annee suivante
    #---------------------------------------------------------------

    #  Mise a jour de la tresorie : prelevement sociaux sur stock et sortie des resultats bruts
    # Si resultats brut negatif alors actionnaires compensent. Sinon sortie du resultats bruts.
    # On fait l'hypothese que le resultats net n'est pas reintegre.

    x@ptf_fin@ptf_treso <- update_treso(x@ptf_fin@ptf_treso , - result_brut -
                                             sum(passif_ap_pb[["flux_agg"]][,"soc_stock_ap_pb"]))
    # Mise a jour des montant totaux de VM et de VNC des actifs
    x@ptf_fin <- do_update_vm_vnc_precedent(x@ptf_fin)

    # PPB
    x@ppb <- init_debut_ppb(x@ppb)

    # PRE, RC
    x@ptf_fin@rc <-  do_update_RC_val_debut(x@ptf_fin@rc, x@ptf_fin@rc@val_courante)
    x@ptf_fin["pre"] <-  do_update_PRE_val_debut(x@ptf_fin@pre, x@ptf_fin@pre@val_courante) # On conserve la validation car pas fait dans l'objet
    # PGG, PSAP
    x@ptf_passif["autres_reserves"] <-  init_debut_pgg_psap(x@ptf_passif@autres_reserves) # On conserve la validation car pas fait dans l'objet


    #---------------------------------------------------------------
    # Etape 14 : Gestion des fins de projection
    #---------------------------------------------------------------
    if(x@annee == annee_fin){
      # Calcul des fins de projection
      fin_proj <- calc_fin_proj(x, resultat_fin, resultat_tech, passif_ap_pb[["stock_agg"]][,"pm_fin_ap_pb"],
                                result_revalo[["tx_pb"]], result_revalo[["tx_enc_moy"]])

      # Extraction des resultats
      flux_fin_passif <- fin_proj[["flux_fin_passif"]]
      resultat_tech <- fin_proj[["result_tech"]]
      resultat_fin <- fin_proj[["resultat_fin"]]
      result_brut <- fin_proj[["result_brut"]]
      result_net <- fin_proj[["result_net"]]
      impot <- fin_proj[["impot"]]

    } else{ # Sinon le flux de fin est nul
      flux_fin_passif = rep(0, length(passif_av_pb[["nom_produit"]]))
    }


    #---------------------------------------------------------------
    # Etape 15 : Creation d'une liste stockant les flux de BE
    #---------------------------------------------------------------

    # Reprendre les flux de resultats
    flux_produit <- cbind(passif_av_pb[["result_av_pb"]][["flux_agg"]],
                         passif_ap_pb[["flux_agg"]])

    stock_produit <- cbind(passif_av_pb[["result_av_pb"]][["stock_agg"]],
                         passif_ap_pb[["stock_agg"]])

    # Reprendre les flux de resultats hors models
    hors_model <- passif_av_pb[["result_autres_passifs"]]

    output_produit <- list(flux_produit = flux_produit,
                           stock_produit = stock_produit,
                           hors_model = hors_model
                           )


    output_be <- list(prime = c(flux_produit[, "pri_brut"], hors_model$prime),
                      prestation = c(flux_produit[, "prest"] +
                        flux_produit[, "rev_prest_nette"] -
                          flux_produit[,"rach_charg"] +
                          flux_produit[,"soc_stock_ap_pb"] +
                        flux_fin_passif, hors_model$prestation),
                      prestation_fdb = c(flux_produit[, "prest_fdb"], 0),
                      frais = c(flux_produit[,"frais_var_prime"] +
                                  flux_produit[,"frais_fixe_prime"] +
                                  flux_produit[,"frais_var_prest"] +
                                  flux_produit[,"frais_fixe_prest"], hors_model$frais))

    # validation de l'objet
    validObject(x)

    # Output
    return(list(canton = x,
                annee = x@annee,
                nom_produit = c(passif_av_pb[["nom_produit"]], "hors_model"),
                output_produit = output_produit,
                output_be = output_be,
                resultat_tech = resultat_tech,
                result_fin = resultat_fin,
                result_brut = result_brut,
                result_net = result_net
                 ))
  }
)
