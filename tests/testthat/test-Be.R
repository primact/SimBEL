# --------------------------------------------------------------------------------------------------------------------------
# Passif
# --------------------------------------------------------------------------------------------------------------------------
context("Be")

# Initialisation de l'objet BE
path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
init_scenario(racine)
central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))

# --------------------------------------------------------------------------------------------------------------------------
# Class Be
# --------------------------------------------------------------------------------------------------------------------------
context("Class Be")

test_that("Classe_Be", {

    # Test classe
    expect_is(object = central, class = "Be")

    # Tests sur les attributs de la classe
    expect_is(object = central@param_be, class = "ParamBe")
    expect_is(object = central@canton, class = "Canton")
    expect_is(object = central@esg, class = "ESG")
    expect_is(object = central@base, class = "DataBase")
    expect_is(object = central@tab_flux, class = "list")
    expect_is(object = central@tab_be, class = "list")
    expect_is(object = central@tab_result, class = "list")
})


#--------------------------------------------------
# run_be_simu
#--------------------------------------------------
test_that("TEST_run_be_simu",{

  expect_error(test <- run_be_simu(x = central, i = 1L, pre_on = T), NA)

    # Tests sur les erreurs d'input
    expect_error(run_be_simu(x = central))
    expect_error(run_be_simu(x = central, i = 1, pre_on = T))

    # Test sur le ouput
    expect_is(object = test$resultats, class = "list")
    expect_is(object = test$canton, class = "Canton")

    # Tests des flux de resultats
    expect_equal(object = sum(test$resultats$prime), expected = 0)
    expect_equal(object = sum(test$resultats$prestation), expected = 362049.4)
    expect_equal(object = sum(test$resultats$flux_be), expected = 387718.6279)
    expect_equal(object = sum(test$resultats$be), expected = 366145.9263)
    expect_equal(object = sum(test$resultats$result_tech_actu), expected = -223138.343)
    expect_equal(object = sum(test$resultats$result_fin_actu), expected = 1104014.95)
    expect_equal(object = sum(test$resultats$result_brut_actu), expected = 880876.6)
    expect_equal(object = sum(test$resultats$result_net_actu), expected = 559656.3471)
})

#--------------------------------------------------
# run_be
#--------------------------------------------------
test_that("TEST_run_be",{

  central@base <- new("DataBase", file_adress = paste(racine@root_address, "internal_ws/data/database", sep = "/"), ecriture_base = T, choc_name = "central")
  central@base@ecriture_base <- T
  expect_error(test <- run_be(x = central, pre_on = T, parallel = F), NA)

  # Tab flux
  expect_equal(object = sum(test$be@tab_flux$prime), expected = 0)
  expect_equal(object = sum(test$be@tab_flux$prestation), expected = 629732.7355)
  expect_equal(object = sum(test$be@tab_flux$prestation_fdb), expected = 40331.13317)
  expect_equal(object = sum(test$be@tab_flux$frais), expected = 32207.58731)
  expect_equal(object = sum(test$be@tab_flux$flux_be), expected =
                 - sum(test$be@tab_flux$prime) + sum(test$be@tab_flux$prestation) + sum(test$be@tab_flux$frais))

  # Tab Be
  expect_equal(object = sum(test$be@tab_be$prime_actu), expected = 0)
  expect_equal(object = sum(test$be@tab_be$prestation_actu), expected = 598264.202)
  expect_equal(object = sum(test$be@tab_be$prestation_fdb_actu), expected = 38510.53482)
  expect_equal(object = sum(test$be@tab_be$frais_actu), expected = 31323.384)
  expect_equal(object = sum(test$be@tab_be$be), expected = 629587.586)

  # Tab result
  expect_equal(object = sum(test$be@tab_result$result_tech_actu), expected = -505458.419)
  expect_equal(object = sum(test$be@tab_result$result_fin_actu), expected = 1526894.222)
  expect_equal(object = sum(test$be@tab_result$result_brut_actu), expected = 1021435.802)
  expect_equal(object = sum(test$be@tab_result$result_net_actu), expected = 327237.638)
})
