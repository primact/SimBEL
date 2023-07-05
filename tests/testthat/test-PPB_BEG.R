# --------------------------------------------------------------------------------------------------------------------------
# PPB_BEG
# --------------------------------------------------------------------------------------------------------------------------
context("PPB_BEG")

# Initialisation de l'objet BE
path <- paste0(getwd(), "/donnees_tests")

# Chargement des objets necessaires
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
# init_scenario(racine)
central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))


# --------------------------------------------------------------------------------------------------------------------------
# Code prealable dans proj_an
# --------------------------------------------------------------------------------------------------------------------------
x <- central@canton
annee_fin <- 10L
pre_on <- FALSE

#---------------------------------------------------------------
# Etape 1 : Mise a jour des annees de projection
#---------------------------------------------------------------
x@annee <- x@annee + 1L

annee <- x@annee

#---------------------------------------------------------------
# Etape 2 : variables economiques utilisees au passif
# Remarque : A terme, revoir ce mode de fonctionemment qui n'est pas ideal en terme de structure.
#---------------------------------------------------------------

# Coefficient d'inflation
coef_inf <- x@mp_esg@indice_inflation

# liste des rendements
list_rd <- calc_rdt_marche_ref(x@ptf_passif@ht@param_comport[[x@hyp_canton@method_taux_cible]], x@mp_esg)

#---------------------------------------------------------------
# Etape 3 : Gestion des passifs avant participation aux benefices
#---------------------------------------------------------------
# Evaluation du passif vieilli d'un an
passif_av_pb <- viellissement_av_pb(annee, x@ptf_passif, coef_inf, list_rd, x@hyp_canton@tx_soc)
# Mise a jour des passifs
x@ptf_passif <- passif_av_pb[["ptf"]]

# QG 07/08
# Calcul de la PPB de premiere anneee attribuee au flux garanti
# pm_deb <- passif_av_pb[["result_av_pb"]][["stock_agg"]][, "pm_deb"]
# if(annee == 1L){
#     # Somme des PM
#     sum_pm_deb <- sum(pm_deb)
#
#     # Gestion des divisions par 0
#     if(sum_pm_deb != 0) # Attribution au prorata de la PM
#         ppb_init_attrib <- x@ppb@ppb_debut * pm_deb / sum_pm_deb
#     else # Division par 0
#         ppb_init_attrib <- x@ppb@ppb_debut * 1 / length(pm_deb)
#
# } else {
#     ppb_init_attrib <- rep(0, length(pm_deb))
# }

#---------------------------------------------------------------
# Etape 4 : Gestion des actifs avant allocation
#---------------------------------------------------------------
actif_vieil <- update_PortFin(annee, x@ptf_fin, x@mp_esg, passif_av_pb[["flux_milieu"]], passif_av_pb[["flux_fin"]])
# Mise a jour des actifs
x@ptf_fin <- actif_vieil[["ptf"]]

# Extraction des revenus financiers et de la variation de VNC obligataires
revenu_fin <- actif_vieil[["revenu_fin"]]
revenu_fin_det <- actif_vieil[["revenu_fin_det"]]
var_vnc_oblig <- actif_vieil[["var_vnc_oblig"]]

# Mise a jour du portfeuille de reference
x@param_alm@ptf_reference <- update_PortFin_reference(annee, x@param_alm@ptf_reference, x@mp_esg)


#---------------------------------------------------------------
# Etape 5 : Calcul des frais financiers
#---------------------------------------------------------------
# Calcul des valeurs moyennes
alloc_cour <- print_alloc(x@ptf_fin)
# Valeur moyenne des placements en valeur de marche
plac_moy_vm <- (.subset2(alloc_cour, 5L) + sum(unlist(x@ptf_fin@vm_vnc_precedent[["vm"]]))) / 2

frais_fin <- calc_frais_fin(x@ptf_fin@frais_fin, plac_moy_vm, coef_inf)

#  Mise a jour de la tresorie
x@ptf_fin@ptf_treso <- update_treso(x@ptf_fin@ptf_treso, -frais_fin)

#---------------------------------------------------------------
# Etape 6 : Re-allocation des actifs et mise a jour de la PRE et de la RC
#---------------------------------------------------------------
# Reallocation a l'allocation cible

# Gestion de l'anomalie : valeur de marche des actifs negatives
if (.subset2(print_alloc(x@ptf_fin), 5L) < 0) {
    warning(paste("Attention, la valeur de marche des actifs est negative pour
                la simulation ", x@mp_esg@num_traj, " en annee ", annee, ".", sep = ""))

    # Dans le cas d'un actif negatif, la simulation est arretee.
    return(FALSE)
}

actif_realloc <- reallocate(x@ptf_fin, x@param_alm@ptf_reference, x@param_alm@alloc_cible)
x@ptf_fin <- actif_realloc[["portFin"]]
pmvr <- list(
    oblig = actif_realloc[["pmvr_oblig"]],
    action = actif_realloc[["pmvr_action"]],
    immo = actif_realloc[["pmvr_immo"]]
)

#---------------------------------------------------------------
# Etape 7 : Calcul du resultat technique
#---------------------------------------------------------------
# Calcul du resultats technique avec attribution de PB
resultat_tech <- calc_result_technique(passif_av_pb, actif_realloc[["var_pre"]] * pre_on)

#---------------------------------------------------------------
# Etape 8 : Calcul du resultat financier et du TRA
#---------------------------------------------------------------

# Evaluation du resultat financier
resultat_fin <- calc_resultat_fin(
    revenu_fin + var_vnc_oblig, actif_realloc[["pmvr"]],
    frais_fin, actif_realloc[["var_rc"]]
)

# Calcul du TRA
tra <- calc_tra(actif_realloc[["plac_moy_vnc"]], resultat_fin)


plac_moy_vnc <- actif_realloc[["plac_moy_vnc"]]

#---------------------------------------------------------------
# Controle le calcul de la conso de PPB
#---------------------------------------------------------------
test_that("TEST_conso_PPB", {
    x1 <- x
    x1@ppb@hist_ppb <- sum(x@ppb@hist_ppb) / 8 * rep(1, 8)

    # annee 1
    temp <- calc_revalo(x1, passif_av_pb, tra, actif_realloc[["plac_moy_vnc"]], resultat_tech, annee = 1L)
    expect_equal(sum(temp$conso_ppb_init), temp$ppb@compte_rep)
    temp$ppb <- vieillissement_ppb(temp$ppb)

    # annnee 2-8
    for (i in 2:8) {
        x2 <- x1
        x2@ppb <- temp$ppb
        temp <- calc_revalo(x2, passif_av_pb, tra, actif_realloc[["plac_moy_vnc"]], resultat_tech, annee = as.integer(i))
        expect_equal(sum(temp$conso_ppb_init), temp$ppb@compte_rep)
        temp$ppb <- vieillissement_ppb(temp$ppb)
    }

    # annnee 9
    x2 <- x1
    x2@ppb <- temp$ppb
    temp <- calc_revalo(x2, passif_av_pb, tra, actif_realloc[["plac_moy_vnc"]], resultat_tech, annee = 9L)
    expect_equal(sum(temp$conso_ppb_init), 0)


    # annee 1 besoin de prelever la PPB
    passif_av_pb_temp <- passif_av_pb
    passif_av_pb_temp[["result_av_pb"]][["flux_agg"]][, "bes_tx_cible"] <- 10 * passif_av_pb_temp[["result_av_pb"]][["flux_agg"]][, "bes_tx_cible"]

    temp <- calc_revalo(x1, passif_av_pb_temp, tra = 0, actif_realloc[["plac_moy_vnc"]], resultat_tech, annee = 1L)
    expect_equal(sum(temp$conso_ppb_init), temp$ppb@compte_rep)
    temp$ppb <- vieillissement_ppb(temp$ppb)

    # annnee 2-8
    for (i in 2:8) {
        x2 <- x1
        x2@ppb <- temp$ppb
        temp <- calc_revalo(x2, passif_av_pb_temp, tra = 0, actif_realloc[["plac_moy_vnc"]], resultat_tech, annee = as.integer(i))
        expect_equal(sum(temp$conso_ppb_init), temp$ppb@compte_rep)
        temp$ppb <- vieillissement_ppb(temp$ppb)
    }
})

#---------------------------------------------------------------
# Controle le calcul de la conso de PPB
#---------------------------------------------------------------
test_that("TEST_viellissement_passif", {
    # Evaluation du passif vieilli d'un an apres pb
    passif_ap_pb <- vieillissment_ap_pb(x@ptf_passif,
        rev_nette_alloue = c(3, 3, 0, 0), rev_brute_alloue_gar =
            c(0, 0, 0, 0),
        x@hyp_canton@tx_soc
    )
    # Pas de changement avec et apres
    expect_equal(passif_ap_pb$stock_agg[, "pm_gar_ap_pb"], passif_av_pb$result_av_pb$stock_agg[, "pm_fin"])
    expect_equal(passif_ap_pb$flux_agg[, "supp_brut_gar_ap_pb"], rep(0, 4))
    expect_equal(passif_ap_pb$flux_agg[, "supp_nette_gar_ap_pb"], rep(0, 4))
    expect_equal(passif_ap_pb$ptf@eei$epeuro1@tab@tab$pm_gar, x@ptf_passif@eei$epeuro1@tab@tab$pm_gar)
    expect_equal(passif_ap_pb$ptf@eei$epeuro1@mp$pm_gar, x@ptf_passif@eei$epeuro1@tab@tab$pm_gar)
    expect_equal(passif_ap_pb$ptf@rer$reteurorest1@tab@tab$pm_gar, x@ptf_passif@rer$reteurorest1@tab@tab$pm_gar)
    expect_equal(passif_ap_pb$ptf@rer$reteurorest1@mp$pm_gar, x@ptf_passif@rer$reteurorest1@tab@tab$pm_gar)


    # Evaluation du passif vieilli d'un an apres pb
    passif_ap_pb2 <- vieillissment_ap_pb(x@ptf_passif,
        rev_nette_alloue = c(3, 3, 0, 0), rev_brute_alloue_gar =
            c(2, 2, 0, 0),
        x@hyp_canton@tx_soc
    )
    tx_enc <- x@ptf_passif@eei$epeuro1@mp$chgt_en
    alloc_mp <- x@ptf_passif@eei$epeuro1@tab@tab$bes_tx_cible / sum(x@ptf_passif@eei$epeuro1@tab@tab$bes_tx_cible)


    # Pas de changement avec et apres
    expect_equal(
        passif_ap_pb2$stock_agg[, "pm_gar_ap_pb"],
        passif_av_pb$result_av_pb$stock_agg[, "pm_fin"] + c(2, 2, 0, 0) * (1 - tx_enc) * (1 - x@hyp_canton@tx_soc)
    )
    expect_equal(passif_ap_pb2$flux_agg[, "supp_brut_gar_ap_pb"], c(2, 2, 0, 0))
    expect_equal(passif_ap_pb2$flux_agg[, "supp_nette_gar_ap_pb"], c(2, 2, 0, 0) * (1 - tx_enc))
    expect_equal(passif_ap_pb2$ptf@eei$epeuro1@tab@tab$pm_gar, x@ptf_passif@eei$epeuro1@tab@tab$pm_gar +
        2 * alloc_mp * (1 - tx_enc) * (1 - x@hyp_canton@tx_soc))
    expect_equal(passif_ap_pb2$ptf@eei$epeuro1@mp$pm_gar, passif_ap_pb2$ptf@eei$epeuro1@tab@tab$pm_gar)
    expect_equal(passif_ap_pb2$ptf@rer$reteurorest1@tab@tab$pm_gar, x@ptf_passif@rer$reteurorest1@tab@tab$pm_gar)
    expect_equal(passif_ap_pb2$ptf@rer$reteurorest1@mp$pm_gar, x@ptf_passif@rer$reteurorest1@tab@tab$pm_gar)
    expect_equal(passif_ap_pb2$ptf@eei$epeuro1@mp$pm, passif_ap_pb$ptf@eei$epeuro1@mp$pm) # Pas de changement sur la vraie PM
    expect_equal(passif_ap_pb2$ptf@rer$reteurorest1@mp$pm, passif_ap_pb$ptf@rer$reteurorest1@mp$pm) # Pas de changement sur la vraie PM
})



#---------------------------------------------------------------
# Controle d'integration
#---------------------------------------------------------------
test_that("TEST_proj_an", {
    test <- proj_an(x, 10L, pre_on = FALSE)
    expect_lte(sum(test$canton@ptf_passif@eei$epeuro1@mp$pm_gar), sum(test$canton@ptf_passif@eei$epeuro1@mp$pm))
})
