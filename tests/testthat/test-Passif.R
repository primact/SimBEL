# --------------------------------------------------------------------------------------------------------------------------
# Passif
# --------------------------------------------------------------------------------------------------------------------------
context("PortPassif")

# Initialisation de l'objet BE
racine <- new("Initialisation", root_address = "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data")
racine <- set_architecture(racine)
init_SimBEL(racine)
init_scenario(racine)
central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))

hypt <- central@canton@ptf_passif@ht


# Liste des rendements
list_rd <- calc_rdt_marche_ref(central@canton@ptf_passif@ht@param_comport[[central@canton@hyp_canton@method_taux_cible]], central@canton@mp_esg)
#--------------------------------------------------
# calc_rdt_marche_ref
#--------------------------------------------------
test_that("TEST_calc_rdt_marche_ref", {

    # Test
    expect_true(all(names(list_rd) %in% c("rdt_oblig", "rdt_action", "rdt_immo", "rdt_tre" )))
})





# --------------------------------------------------------------------------------------------------------------------------
# Retraite
# --------------------------------------------------------------------------------------------------------------------------
context("PortPassif_RetraiteEuroRest")


# Recuperation des principaux objets
retraite <- central@canton@ptf_passif@rer$reteurorest1

# Recuperation du tx cible
tx_cible <- list(tx_cible_an = retraite@mp$tx_cible_prec)


#--------------------------------------------------
# Classe TabRetEuroRest
#--------------------------------------------------
test_that("Classe_TabRetEuroRest", {

    # Test classe
    expect_is(object = retraite@tab, class = "TabRetEuroRest")

    # Tests sur les attributs de la classe
    expect_equal(object = length(retraite@tab@tab), expected = 8L)
    expect_is(object = retraite@tab@tab$num_mp, class = "numeric")
    expect_is(object = retraite@tab@tab$prest, class = "numeric")
    expect_is(object = retraite@tab@tab$pm_deb, class = "numeric")
    expect_is(object = retraite@tab@tab$pm_fin, class = "numeric")
    expect_is(object = retraite@tab@tab$bes_tx_cible, class = "numeric")
    expect_is(object = retraite@tab@tab$nb_contr, class = "numeric")
    expect_is(object = retraite@tab@tab$tx_cible, class = "numeric")
    expect_is(object = retraite@tab@tab$pm_gar, class = "numeric")
})



#--------------------------------------------------
# calc_coupon TabProbaRetEuroRest
#--------------------------------------------------
test_that("Classe_TabProbaRetEuroRest", {

    # Test classe
    expect_is(object = retraite@tab_proba, class = "TabProbaRetEuroRest")

    # Tests sur les attributs de la classe
    expect_is(object = retraite@tab_proba@ax, class = "data.frame")
    expect_is(object = retraite@tab_proba@sortie_retraite, class = "data.frame")
    expect_is(object = retraite@tab_proba@survie_un_an, class = "data.frame")
})



#--------------------------------------------------
# Classe RetraiteEuroRes
#--------------------------------------------------
test_that("Classe_RetraiteEuroRest", {

    # Test creation classe
    expect_is(object = retraite, class = "RetraiteEuroRest")

    # Fichier de data
    temp_csv <- read.csv2(paste(racine@address$data$passif, "ret_euro_rest1.csv", sep = "/"))

    # Tests
    expect_equal(object = nrow(retraite@mp), expected = nrow(temp_csv))
    expect_equal(object = ncol(retraite@mp), expected = ncol(temp_csv))

    # Tests creation & getter
    expect_identical(object = retraite@mp[["num_mp"]], expected = temp_csv[["num_mp"]])
    expect_identical(object = retraite@mp[["num_canton"]], expected = temp_csv[["num_canton"]])
    expect_identical(object = retraite@mp[["num_prod"]], expected = temp_csv[["num_prod"]])
    expect_identical(object = retraite@mp[["age"]], expected = temp_csv[["age"]])
    expect_identical(object = retraite@mp[["gen"]], expected = temp_csv[["gen"]])
    expect_identical(object = retraite@mp[["sexe"]], expected = temp_csv[["sexe"]])
    expect_identical(object = retraite@mp[["num_tab_mort"]], expected = temp_csv[["num_tab_mort"]])
    expect_identical(object = retraite@mp[["chgt_enc"]], expected = temp_csv[["chgt_enc"]])
    expect_identical(object = retraite@mp[["ind_chgt_enc_pos"]], expected = temp_csv[["ind_chgt_enc_pos"]])
    expect_identical(object = retraite@mp[["pm"]], expected = temp_csv[["pm"]])
    expect_identical(object = retraite@mp[["nb_contr"]], expected = temp_csv[["nb_contr"]])
    expect_identical(object = retraite@mp[["ind_mariage"]], expected = temp_csv[["ind_mariage"]])
    expect_identical(object = retraite@mp[["statut_rvs"]], expected = temp_csv[["statut_rvs"]])
    expect_identical(object = retraite@mp[["age_rvs"]], expected = temp_csv[["age_rvs"]])
    expect_identical(object = retraite@mp[["gen_rvs"]], expected = temp_csv[["gen_rvs"]])
    expect_identical(object = retraite@mp[["sexe_rvs"]], expected = temp_csv[["sexe_rvs"]])
    expect_identical(object = retraite@mp[["num_tab_mort_rvs"]], expected = temp_csv[["num_tab_mort_rvs"]])
    expect_identical(object = retraite@mp[["tx_rvs"]], expected = temp_csv[["tx_rvs"]])
    expect_identical(object = retraite@mp[["tx_tech"]], expected = temp_csv[["tx_tech"]])
    expect_identical(object = retraite@mp[["tx_cible"]], expected = temp_csv[["tx_cible"]])
    expect_identical(object = retraite@mp[["per_rente"]], expected = temp_csv[["per_rente"]])
    expect_identical(object = retraite@mp[["rente"]], expected = temp_csv[["rente"]])
    expect_identical(object = retraite@mp[["rente_gar"]], expected = temp_csv[["rente_gar"]])
    expect_identical(object = retraite@mp[["ch_arr"]], expected = temp_csv[["ch_arr"]])

    expect_equal(object = retraite@mp, expected = temp_csv)
})


#--------------------------------------------------
# calc_pm
#--------------------------------------------------
test_that("TEST_calc_pm",{

    # Tests sur les erreurs d'input
    expect_error(calc_pm(x = retraite))
    expect_error(calc_pm(y = list(hypt, tx_cible, "normal")))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt)))
    expect_error(calc_pm(x = retraite, y = list(tx_cible = tx_cible)))
    expect_error(calc_pm(x = retraite, y = list(method = "normal")))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt, tx_cible = tx_cible)))
    expect_error(calc_pm(x = retraite, y = list(method = "normal", tx_cible = tx_cible)))
    expect_error(calc_pm(x = retraite, y = list(method = hypt, ht = "normal", tx_cible = tx_cible)))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt, method = "normal", tx_cible = tx_cible)))
    expect_error(calc_pm(x = retraite, y = list(tx_cible = tx_cible, ht =  hypt, method = "gar")))
    expect_error(calc_pm(x = retraite, y = list(tx_cible = tx_cible, method = "gar", hypt)))
    expect_error(calc_pm(x = 0.01, y = list(hypt, tx_cible = tx_cible, method = "normal")))
    expect_error(calc_pm(x = 0.01, y = list(hypt, tx_cible = tx_cible, method = "gar")))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt, tx_cible = 0.04, method = "gar")))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt, tx_cible = c(1,2), method = "normal")))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt, tx_cible = list(1,2), method = "normal")))
    expect_error(calc_pm(x = retraite, y = list(ht = hypt, tx_cible = tx_cible, method = "test")))


    # Tests (donnees CAREL)
    res_calc_pm <- calc_pm(x = retraite, method = "normal", an = 1L, tx_cible = tx_cible)
    expect_equal(object = sum(res_calc_pm$stock$pm_fin), expected = 115499.665)

})

#--------------------------------------------------
# calc_prest
#--------------------------------------------------
test_that("TEST_calc_prest", {

    # Tests sur les erreurs d'input
    expect_error(calc_prest(x = retraite))
    expect_error(calc_prest(y = list(ht = hypt)))
    expect_error(calc_prest(x = retraite, y = list(hypt)))
    expect_error(calc_prest(y = retraite, x = list(ht = hypt)))
    expect_error(calc_prest(x = retraite, y = list(ht = 10)))


    # Tests sur les sorties de la fonction
    res_calc_prest <- calc_prest(x = retraite, method = "normal", an = 1L)

    # Tests avec les donnees CAREL
    expect_equal(object = res_calc_prest$stock$nb_debut, expected =retraite@mp$nb_contr)
    expect_equal(object = sum(res_calc_prest$flux$prest[1:3]), expected = 2490,015)
    expect_equal(object = sum(res_calc_prest$flux$prest[4:6]), expected = 6055,96)
})


#--------------------------------------------------
# vieilli_mp
#--------------------------------------------------
test_that("TEST_vieilli_mp", {

    # Appel de la fonction
    pm_fin_ap_pb <- c(100,90,5,10,20,50)
    tx_rev_net <- c(0.02,0.03,0.01,0.1,0.05,0.02)
    res <- vieilli_mp(x = retraite, pm_fin_ap_pb, tx_rev_net)

    # Tests
    expect_equal(object = res@mp$pm, expected = pm_fin_ap_pb)
})


#--------------------------------------------------
# calc_revalo_pm
#--------------------------------------------------
test_that("TEST_calc_revalo_pm", {

    # Appel de la fonction
    res <- calc_revalo_pm(x = retraite, y = list(rev_net_alloue = 50))

    # Tests
    expect_equal(object = res$stock$pm_fin_ap_pb, expected = 50 * rep(1, nrow(retraite@mp)) / nrow(retraite@mp))
    expect_equal(object = res$flux$rev_stock_brut_ap_pb, expected = 50 * rep(1, nrow(retraite@mp)) / nrow(retraite@mp))
    expect_equal(object = res$flux$rev_stock_nette_ap_pb, expected = 50 * rep(1, nrow(retraite@mp)) / nrow(retraite@mp))
    expect_equal(object = res$flux$enc_charg_stock_ap_pb, expected = rep(0, nrow(retraite@mp)))
    expect_equal(object = res$flux$soc_stock_ap_pb, expected = rep(0, nrow(retraite@mp)))

})





# --------------------------------------------------------------------------------------------------------------------------
# Epargne
# --------------------------------------------------------------------------------------------------------------------------
context("PortPassif_EpEuroInd")

# Recuperation des principaux objets
epeuro <- central@canton@ptf_passif@eei$epeuro1
proba_dyn <- calc_proba_dyn(epeuro, ht = hypt)

#--------------------------------------------------
# vieilli_mp
#--------------------------------------------------
test_that("TEST_vieilli_mp", {

    # Sauvegarde de l'ancien MP
    old_mp <- epeuro@mp

    # Donnees
    pm_fin_ap_pb <- c(100, 20)
    tx_revalo <- c(0.02,0.03)

    # Tests sur les erreurs d'input
    expect_error(vieilli_mp(x = epeuro))
    expect_error(vieilli_mp(x = epeuro, pm_fin_ap_pb))
    expect_error(vieilli_mp(x = epeuro, tx_revalo))
    expect_error(vieilli_mp(x = epeuro, pm_fin_ap_pb, c(0.01)))
    expect_error(vieilli_mp(x = epeuro, c(50), tx_revalo))

    # Appel de la fonction
    res <- vieilli_mp(x = epeuro, pm_fin_ap_pb, tx_revalo)

    # Tests
    expect_equal(object = res@mp$pm, expected = pm_fin_ap_pb)
    expect_equal(object = res@mp$tx_revalo_prec, expected = tx_revalo)
    expect_equal(object = res@mp$age, expected = old_mp$age + 1L)
    expect_equal(object = res@mp$anc, expected = old_mp$anc + 1L)
})


#--------------------------------------------------
# calc_primes
#--------------------------------------------------
test_that("TEST_calc_primes", {

    # Appel de la fonction
    res <- calc_primes(epeuro)

    # Tests
    expect_equal(object = res$stock$nb_vers, expected = c(0,0))
    expect_equal(object = res$flux$pri_brut, expected = c(0,0))
    expect_equal(object = res$flux$pri_net, expected = c(0,0))
    expect_equal(object = res$flux$pri_chgt, expected = c(0,0))


    # Prime non nulle
    epeuro_bis <- epeuro
    epeuro_bis@mp$prime <- c(1,1)
    res <- calc_primes(epeuro_bis)

    # Tests
    expect_equal(object = res$stock$nb_vers, expected = c(1,1))
    expect_equal(object = res$flux$pri_brut, expected = c(1,1))
    expect_equal(object = res$flux$pri_net, expected = (1 - epeuro_bis@mp$chgt_prime))
    expect_equal(object = res$flux$pri_chgt, expected = epeuro_bis@mp$chgt_prime)

})


#--------------------------------------------------
# calc_prest
#--------------------------------------------------
test_that("TEST_calc_prest", {

    # Donnees
    tx_an <- c(0.05,0.05)
    tx_min <- list(tx_an = c(0.05,0.05), tx_se = sqrt(1 + tx_an) - 1)

    # Erreurs d'input
    expect_error(calc_prest(x = epeuro))
    expect_error(calc_prest(y = list(proba_dyn = proba_dyn)))
    expect_error(calc_prest(x = epeuro, an = 1L,  y = list(proba_dyn)))
    expect_error(calc_prest(x = epeuro, method = "test", an = 1L, y = list(proba_dyn = 10, tx_min = tx_min, tx_soc = 0.155)))

    # Appel de la fonction
    res <- calc_prest(x = epeuro, method = "normal", an = 1L, y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = 0.155))
})



#--------------------------------------------------
# calc_tx_cible
#--------------------------------------------------
test_that("TEST_calc_tx_cible", {

    # Appel de la fonction
    res <- calc_tx_cible(x = epeuro, y = list(ht = hypt, list_rd = list_rd))

    # Tests
    expect_equal(res$tx_cible_se, expected = sqrt(1+res$tx_cible_an)-1)

})

#--------------------------------------------------
# calc_pm
#--------------------------------------------------
test_that("TEST_calc_pm", {

    # Donnees
    prime <- calc_primes(epeuro)
    tx_min <-  calc_tx_min(epeuro, an = 1L)
    prest <- calc_prest(x = epeuro, method = "normal", an = 1L, y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = 0.155))
    tx_cible <- calc_tx_cible(x = epeuro, y = list(ht = hypt, list_rd = list_rd))

    # Appel de la fonction
    res <- calc_pm(epeuro, method = "normal", an = 1L, tx_cible = tx_cible, list(tab_prime = prime[["flux"]], tab_prest = prest[["flux"]],
                                                                                 tx_min = tx_min, tx_soc = 0.155))

})
