# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc immo")

path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
table_choc <- init_scenario(racine)

central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
actif_central <- print_alloc(central@canton@ptf_fin)
immo <- get(load(paste(racine@address$save_folder$immo, "best_estimate.RData", sep = "/")))
actif_immo <- print_alloc(immo@canton@ptf_fin)

choc_immo <- 0.25

# Test sur les actifs
#--------------------------------------------------

test_that("TEST_choc_immo", {
    # Verifications des classes
    expect_s4_class(central, "Be")
    expect_s4_class(immo, "Be")

    # Verifications de la maj du portefeuille
    # alloc_valeur
    expect_equal(actif_immo[1, 1], actif_central[1, 1])
    expect_equal(actif_immo[2, 1], actif_central[2, 1] * (1 - choc_immo))
    expect_equal(actif_immo[3, 1], actif_central[3, 1])
    expect_equal(actif_immo[4, 1], actif_central[4, 1])
    expect_equal(actif_immo[1, 1] + actif_immo[2, 1] / (1 - choc_immo) + actif_immo[3, 1] + actif_immo[4, 1], actif_central[5, 1])

    # alloc_valeur_nc
    expect_equal(actif_immo[1, 3], actif_central[1, 3])
    expect_equal(actif_immo[2, 3], actif_central[2, 3])
    expect_equal(actif_immo[3, 3], actif_central[3, 3])
    expect_equal(actif_immo[4, 3], actif_central[4, 3])
    expect_equal(actif_immo[5, 3], actif_central[5, 3])
})
