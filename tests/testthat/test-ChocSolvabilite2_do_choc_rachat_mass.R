# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc rachat_mass")

path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
table_choc <- init_scenario(racine)

central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
central@base <- new("DataBase",
    file_adress = paste(racine@root_address, "internal_ws/data/database", sep = "/"), ecriture_base = FALSE, choc_name = "central"
)
BE_central_result <- run_be(central, FALSE, FALSE)

rachat_mass <- get(load(paste(racine@address$save_folder$rachat_mass, "best_estimate.RData", sep = "/")))
rachat_mass@base <- new("DataBase",
    file_adress = paste(racine@root_address, "internal_ws/data/database", sep = "/"), ecriture_base = FALSE, choc_name = "rachat_mass"
)
BE_rachat_mass_result <- run_be(rachat_mass, FALSE, FALSE)


# Parametres de test
choc_rachat_mass <- 0.4

# Test sur le chargement

test_that("TEST_choc_rachat_mass", {
    # Verification du chargement du parametre
    expect_equal(rachat_mass@canton@ptf_passif@choc_lapse_mass, choc_rachat_mass)
})
