# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc action")

path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
table_choc <- init_scenario(racine)

central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
actif_central <- print_alloc(central@canton@ptf_fin)
action_type1 <- get(load(paste(racine@address$save_folder$action_type1, "best_estimate.RData", sep = "/")))
actif_action_type1 <- print_alloc(action_type1@canton@ptf_fin)

# Valeur de marche action de type 1 et 2
vm_action_type1_central <- central@canton@ptf_fin@ptf_action@ptf_action$val_marche[central@canton@ptf_fin@ptf_action@ptf_action$num_index == 1]
vm_action_type1_choc <- action_type1@canton@ptf_fin@ptf_action@ptf_action$val_marche[action_type1@canton@ptf_fin@ptf_action@ptf_action$num_index == 1]
vm_action_type2_central <- central@canton@ptf_fin@ptf_action@ptf_action$val_marche[central@canton@ptf_fin@ptf_action@ptf_action$num_index == 2]
vm_action_type2_choc <- action_type1@canton@ptf_fin@ptf_action@ptf_action$val_marche[action_type1@canton@ptf_fin@ptf_action@ptf_action$num_index == 2]

# Parametres de chocs
choc_action_type1 <- 0.39
choc_action_type2 <- 0.49


# Test sur les actifs
#--------------------------------------------------

test_that("TEST_choc_action_portfolio", {
    # Verifications classe
    expect_s4_class(central, "Be")
    expect_s4_class(action_type1, "Be")

    # Verifications des chocs par type d'action
    expect_equal(vm_action_type1_choc, vm_action_type1_central * (1 - choc_action_type1))
    expect_equal(vm_action_type2_choc, vm_action_type2_central * (1 - choc_action_type2))

    # Verifications de la maj du portefeuille
    expect_equal(actif_action_type1[2, 1], actif_central[2, 1])
    expect_equal(actif_action_type1[3, 1], actif_central[3, 1])
    expect_equal(actif_action_type1[4, 1], actif_central[4, 1])
    expect_equal(actif_action_type1[1, 3], actif_central[1, 3])
    expect_equal(actif_action_type1[2, 3], actif_central[2, 3])
    expect_equal(actif_action_type1[3, 3], actif_central[3, 3])
    expect_equal(actif_action_type1[4, 3], actif_central[4, 3])
    expect_equal(actif_action_type1[5, 3], actif_central[5, 3])
})
