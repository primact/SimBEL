# ##################
# Tests ALMEngine
# ##################

context("ALMEngine")


# Dossier de DATA
folder_ESG_address <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/parametres/esg/ESG"
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/actif"



#----------------------------------------------------------------------------------------------------------------------------------------------------
# Elements necessaires aux calculs ulterieurs
alloc_cible <- c(.25,.25,.48,.02)
table_ESG <- chargement_ESG(folder_ESG_address, 2L, 5L)
mp_ESG <- extract_ESG(table_ESG, 1L, 0L)


PtfFin     <- chargement_PortFin(path, mp_ESG)
PtfFin_ref <- chargement_PortFin_reference(paste(path, "Portefeuille_reference", sep = "/"), mp_ESG)

param_alm_engine <- new("ParamAlmEngine", ptf_reference = PtfFin_ref, alloc_cible = alloc_cible, seuil_realisation_PVL = 0)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Test fonctions : Reallocate
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_classe", {

    # Test classe
    expect_s4_class(param_alm_engine, "ParamAlmEngine")

    # Tests attributs
    expect_equal(param_alm_engine@ptf_reference, PtfFin_ref)
    expect_equal(param_alm_engine@alloc_cible, alloc_cible)
    expect_equal(param_alm_engine@seuil_realisation_PVL, 0)
})

#----------------------------------------------------------------------------------------------------------------------------------------------------
# do_calc_nb_sold
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_do_calc_nb_sold", {

    # Donnees necessaires
    montant_vente = 10000
    method_vente = "proportionnelle"


    ## 1 - Action
    # Appel de la fonction
    res <- do_calc_nb_sold_action(PtfFin@ptf_action, montant_vente, method_vente)

    # Resultats attendus
    vm <- PtfFin@ptf_action@ptf_action$val_marche
    nb_unit <- PtfFin@ptf_action@ptf_action$nb_unit
    res_att <- montant_vente * (nb_unit / sum(vm))

    # Test
    expect_equal(res[, "nb_sold"], res_att, tolerance = 0.01)


    ## 2 - Immo
    # Appel de la fonction
    res <- do_calc_nb_sold_immo(PtfFin@ptf_immo, montant_vente, method_vente)

    # Resultats attendus
    vm <- PtfFin@ptf_immo@ptf_immo$val_marche
    nb_unit <- PtfFin@ptf_immo@ptf_immo$nb_unit
    res_att <- montant_vente * (nb_unit / sum(vm))

    # Test
    expect_equal(res[, "nb_sold"], res_att, tolerance = 0.01)


    ## 3 - Action
    # Appel de la fonction
    res <- do_calc_nb_sold_oblig(PtfFin@ptf_oblig, montant_vente, method_vente)

    # Resultats attendus
    vm <- PtfFin@ptf_oblig@ptf_oblig$val_marche
    nb_unit <- PtfFin@ptf_oblig@ptf_oblig$nb_unit
    res_att <- montant_vente * (nb_unit / sum(vm))

    # Test
    expect_equal(res[, "nb_sold"], res_att, tolerance = 0.01)

})


#----------------------------------------------------------------------------------------------------------------------------------------------------
# nb_sold
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_create_ptf_bought", {

    ## 1 - Action
    # Appel de la fonction
    ptf_action <- PtfFin_ref@ptf_action
    res     <- create_ptf_bought_action(ptf_action, coefficient = rep(2, nrow(ptf_action@ptf_action)))

    # Tests
    expect_equal(res@ptf_action$val_marche, 2 * ptf_action@ptf_action$val_marche)
    expect_equal(res@ptf_action$val_nc, 2 * ptf_action@ptf_action$val_nc)
    expect_equal(res@ptf_action$val_achat, 2 * ptf_action@ptf_action$val_achat)
    expect_equal(res@ptf_action$nb_unit, 2 * ptf_action@ptf_action$nb_unit)
    expect_equal(res@ptf_action$num_mp, ptf_action@ptf_action$num_mp)
    expect_equal(res@ptf_action$presence, ptf_action@ptf_action$presence)
    expect_equal(res@ptf_action$cessible, ptf_action@ptf_action$cessible)
    expect_equal(res@ptf_action$dur_det, ptf_action@ptf_action$dur_det)
    expect_equal(res@ptf_action$pdd, ptf_action@ptf_action$pdd)
    expect_equal(res@ptf_action$num_index, ptf_action@ptf_action$num_index)
    expect_equal(res@ptf_action$div, ptf_action@ptf_action$div)
    expect_equal(res@ptf_action$ind_invest, ptf_action@ptf_action$ind_invest)


    ## 2 - Immo
    # Appel de la fonction
    ptf_immo <- PtfFin_ref@ptf_immo
    res     <- create_ptf_bought_immo(ptf_immo, coefficient = rep(2, nrow(ptf_immo@ptf_immo)))

    # Tests
    expect_equal(res@ptf_immo$val_marche, 2 * ptf_immo@ptf_immo$val_marche)
    expect_equal(res@ptf_immo$val_nc, 2 * ptf_immo@ptf_immo$val_nc)
    expect_equal(res@ptf_immo$val_achat, 2 * ptf_immo@ptf_immo$val_achat)
    expect_equal(res@ptf_immo$nb_unit, 2 * ptf_immo@ptf_immo$nb_unit)
    expect_equal(res@ptf_immo$num_mp, ptf_immo@ptf_immo$num_mp)
    expect_equal(res@ptf_immo$presence, ptf_immo@ptf_immo$presence)
    expect_equal(res@ptf_immo$cessible, ptf_immo@ptf_immo$cessible)
    expect_equal(res@ptf_immo$dur_det, ptf_immo@ptf_immo$dur_det)
    expect_equal(res@ptf_immo$pdd, ptf_immo@ptf_immo$pdd)
    expect_equal(res@ptf_immo$num_index, ptf_immo@ptf_immo$num_index)
    expect_equal(res@ptf_immo$loyer, ptf_immo@ptf_immo$loyer)
    expect_equal(res@ptf_immo$ind_invest, ptf_immo@ptf_immo$ind_invest)


    ## 3 - Oblig
    # Appel de la fonction
    ptf_oblig <- PtfFin_ref@ptf_oblig
    res     <- create_ptf_bought_oblig(ptf_oblig, coefficient = rep(2, nrow(ptf_oblig@ptf_oblig)))

    # Tests
    expect_equal(res@ptf_oblig$val_marche, 2 * ptf_oblig@ptf_oblig$val_marche)
    expect_equal(res@ptf_oblig$val_nc, 2 * ptf_oblig@ptf_oblig$val_nc)
    expect_equal(res@ptf_oblig$val_achat, 2 * ptf_oblig@ptf_oblig$val_achat)
    expect_equal(res@ptf_oblig$nb_unit, 2 * ptf_oblig@ptf_oblig$nb_unit)
    expect_equal(res@ptf_oblig$num_mp, ptf_oblig@ptf_oblig$num_mp)
    expect_equal(res@ptf_oblig$presence, ptf_oblig@ptf_oblig$presence)
    expect_equal(res@ptf_oblig$cessible, ptf_oblig@ptf_oblig$cessible)
    expect_equal(res@ptf_oblig$dur_det, ptf_oblig@ptf_oblig$dur_det)
    expect_equal(res@ptf_oblig$nominal, ptf_oblig@ptf_oblig$nominal)
    expect_equal(res@ptf_oblig$tx_coupon, ptf_oblig@ptf_oblig$tx_coupon)
    expect_equal(res@ptf_oblig$par, ptf_oblig@ptf_oblig$par)
    expect_equal(res@ptf_oblig$mat_res, ptf_oblig@ptf_oblig$mat_res)
    expect_equal(res@ptf_oblig$type, ptf_oblig@ptf_oblig$type)
    expect_equal(res@ptf_oblig$rating, ptf_oblig@ptf_oblig$rating)
    expect_equal(res@ptf_oblig$duration, ptf_oblig@ptf_oblig$duration)
    expect_equal(res@ptf_oblig$zspread, ptf_oblig@ptf_oblig$zspread)
    expect_equal(res@ptf_oblig$cc, 2 * ptf_oblig@ptf_oblig$cc)
    expect_equal(res@ptf_oblig$sd, ptf_oblig@ptf_oblig$sd)
})


#--------------------------------------------------------------------------------------------------------------------------
# reallocate
#--------------------------------------------------------------------------------------------------------------------------
test_that("TEST_reallocate", {

    # Appel de la fonction
    res <- reallocate(PtfFin, param_alm_engine@ptf_reference, param_alm_engine@alloc_cible)

    # Test
    expect_equal(as.numeric(print_alloc(res[[1]])[,"alloc_proportion"]), c(alloc_cible, 1))

    # Test avec erreur
    expect_error(reallocate(PtfFin, param_alm_engine@ptf_reference, c(0.3,0.5,0.48,0.2)))
    expect_error(reallocate(PtfFin, param_alm_engine@ptf_reference, c(0.5,0.5,-0.2,0.2)))
})


#--------------------------------------------------------------------------------------------------------------------------
# create_ptf_bought_action
#--------------------------------------------------------------------------------------------------------------------------
test_that("TEST_create_ptf_bough", {

    ## 1 - Action
    # Appel de la fonction
    coefficient = c(1/3, 1/3, 1/3)
    res <- create_ptf_bought_action(PtfFin@ptf_action, coefficient)

    # Tests
    expect_equal(res@ptf_action$val_marche, 1/3 * PtfFin@ptf_action@ptf_action$val_marche)
    expect_equal(res@ptf_action$val_nc, 1/3 * PtfFin@ptf_action@ptf_action$val_nc)
    expect_equal(res@ptf_action$val_achat, 1/3 * PtfFin@ptf_action@ptf_action$val_achat)


    ## 2 - Immo
    # Appel de la fonction
    coefficient = c(1/3, 1/3)
    res <- create_ptf_bought_immo(PtfFin@ptf_immo, coefficient)

    # Tests
    expect_equal(res@ptf_immo$val_marche, 1/3 * PtfFin@ptf_immo@ptf_immo$val_marche)
    expect_equal(res@ptf_immo$val_nc, 1/3 * PtfFin@ptf_immo@ptf_immo$val_nc)
    expect_equal(res@ptf_immo$val_achat, 1/3 * PtfFin@ptf_immo@ptf_immo$val_achat)


    ## 3 - Oblig
    # Appel de la fonction
    coefficient = c(1/4,1/4,1/4,1/4)
    res <- create_ptf_bought_oblig(PtfFin@ptf_oblig, coefficient)

    # Tests
    expect_equal(res@ptf_oblig$val_marche, 1/4 * PtfFin@ptf_oblig@ptf_oblig$val_marche)
    expect_equal(res@ptf_oblig$val_nc, 1/4 * PtfFin@ptf_oblig@ptf_oblig$val_nc)
    expect_equal(res@ptf_oblig$val_achat, 1/4 * PtfFin@ptf_oblig@ptf_oblig$val_achat)


    ## Autre tets
    expect_equal(create_ptf_bought_action(PtfFin@ptf_action, c(1,1,1)), PtfFin@ptf_action)
    expect_equal(create_ptf_bought_immo(PtfFin@ptf_immo, c(1,1)), PtfFin@ptf_immo)
    expect_equal(create_ptf_bought_oblig(PtfFin@ptf_oblig, c(1,1,1,1)), PtfFin@ptf_oblig)
})
