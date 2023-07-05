# --------------------------------------------------------------------------------------------------------------------------
# Currrency risk
# --------------------------------------------------------------------------------------------------------------------------
context("Currency risk")

# Initialisation de l'objet BE
path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
init_scenario(racine)

central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
currency_up_usd <- get(load(paste(racine@address$save_folder$currency_up, "USD_best_estimate.RData", sep = "/")))
currency_down_usd <- get(load(paste(racine@address$save_folder$currency_down, "USD_best_estimate.RData", sep = "/")))


# Test sur les actifs
#--------------------------------------------------
# calc_rdt_marche_ref
#--------------------------------------------------
test_that("TEST_choc_currency_portfolio", {
    actif_central <- print_alloc(central@canton@ptf_fin)
    actif_cup <- print_alloc(currency_up_usd@canton@ptf_fin)
    actif_cdown <- print_alloc(currency_down_usd@canton@ptf_fin)

    # Calcul manuel
    ptf_action <- central@canton@ptf_fin@ptf_action@ptf_action
    ptf_action_cup <- currency_up_usd@canton@ptf_fin@ptf_action@ptf_action
    ptf_action_cdown <- currency_down_usd@canton@ptf_fin@ptf_action@ptf_action

    ptf_immo <- central@canton@ptf_fin@ptf_immo@ptf_immo
    ptf_immo_cup <- currency_up_usd@canton@ptf_fin@ptf_immo@ptf_immo
    ptf_immo_cdown <- currency_down_usd@canton@ptf_fin@ptf_immo@ptf_immo

    ptf_oblig <- central@canton@ptf_fin@ptf_oblig@ptf_oblig
    ptf_oblig_cup <- currency_up_usd@canton@ptf_fin@ptf_oblig@ptf_oblig
    ptf_oblig_cdown <- currency_down_usd@canton@ptf_fin@ptf_oblig@ptf_oblig

    # Test portefeuille action
    expect_identical(ptf_action[1:2, ], ptf_action_cup[1:2, ])
    expect_identical(ptf_action[1:2, ], ptf_action_cdown[1:2, ])
    expect_identical(ptf_action[3, -2], ptf_action_cup[3, -2])
    expect_identical(ptf_action[3, -2], ptf_action_cdown[3, -2])
    expect_equal(ptf_action[3, "val_marche"] * (1 - 0.25), ptf_action_cup[3, "val_marche"])
    expect_equal(ptf_action[3, "val_marche"] * (1 + 0.25), ptf_action_cdown[3, "val_marche"])

    # Test portefeuille immo
    expect_identical(ptf_immo[1, ], ptf_immo_cup[1, ])
    expect_identical(ptf_immo[1, ], ptf_immo_cdown[1, ])
    expect_identical(ptf_immo[2, -2], ptf_immo_cup[2, -2])
    expect_identical(ptf_immo[2, -2], ptf_immo_cdown[2, -2])
    expect_equal(ptf_immo[2, "val_marche"] * (1 - 0.25), ptf_immo_cup[2, "val_marche"])
    expect_equal(ptf_immo[2, "val_marche"] * (1 + 0.25), ptf_immo_cdown[2, "val_marche"])

    # Test portefeuille oblig
    expect_identical(ptf_oblig[1:3, ], ptf_oblig_cup[1:3, ])
    expect_identical(ptf_oblig[1:3, ], ptf_oblig_cdown[1:3, ])
    expect_identical(ptf_oblig[4, -c(2, 16)], ptf_oblig_cup[4, -c(2, 16)])
    expect_identical(ptf_oblig[4, -c(2, 16)], ptf_oblig_cdown[4, -c(2, 16)])
    expect_equal(ptf_oblig[4, "val_marche"] * (1 - 0.25), ptf_oblig_cup[4, "val_marche"])
    expect_equal(ptf_oblig[4, "val_marche"] * (1 + 0.25), ptf_oblig_cdown[4, "val_marche"])
})
