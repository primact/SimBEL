# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc frais")

path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
table_choc <- init_scenario(racine)

# Chargement scenarios SimBEL
central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
frais <- get(load(paste(racine@address$save_folder$frais, "best_estimate.RData", sep = "/")))

# Chargement choc SimBEL
# choc_frais_inflation <- table_choc@param_choc_sousc@mp$choc_frais_inflation
# choc_frais_assiette <- table_choc@param_choc_sousc@mp$choc_frais_assiette

# Chargement choc manuel
choc_frais_inflation <- 0.01
choc_frais_assiette <- 0.1

# Chargement table inflation
ind_infl_csv <- read.csv2(paste(path, "input/parametres/esg/ESG/Simulation_Indices/Inflation.csv", sep = "/"))
ind_infl_down_csv <- read.csv2(paste(path, "input/parametres/esg/ESG_down/Simulation_Indices/Inflation.csv", sep = "/"))
ind_infl_up_csv <- read.csv2(paste(path, "input/parametres/esg/ESG_up/Simulation_Indices/Inflation.csv", sep = "/"))

spread <- get(load(paste(racine@address$save_folder$spread, "best_estimate.RData", sep = "/")))


test_that("TEST_choc_frais", {
    # Verifications du chargement des parametres
    expect_equal(choc_frais_inflation, choc_frais_inflation)
    expect_equal(choc_frais_assiette, choc_frais_assiette)

    # Verifications de la modification des frais
    expect_equal(frais@canton@ptf_passif@fp@mp$frais_fixe_prime, (1 + choc_frais_assiette) * central@canton@ptf_passif@fp@mp$frais_fixe_prime)
    expect_equal(frais@canton@ptf_passif@fp@mp$frais_var_prime, (1 + choc_frais_assiette) * central@canton@ptf_passif@fp@mp$frais_var_prime)
    expect_equal(frais@canton@ptf_passif@fp@mp$frais_fixe_prest, (1 + choc_frais_assiette) * central@canton@ptf_passif@fp@mp$frais_fixe_prest)
    expect_equal(frais@canton@ptf_passif@fp@mp$frais_var_prest, (1 + choc_frais_assiette) * central@canton@ptf_passif@fp@mp$frais_var_prest)
    expect_equal(frais@canton@ptf_passif@fp@mp$frais_var_enc, (1 + choc_frais_assiette) * central@canton@ptf_passif@fp@mp$frais_var_enc)
    expect_equal(frais@canton@ptf_passif@fp@mp$frais_var_enc, (1 + choc_frais_assiette) * central@canton@ptf_passif@fp@mp$frais_var_enc)

    # Verification du chargement de l'indice inflation
    for (i in 0:50) {
        expect_equal(central@esg@ind_inflation[, i], ind_infl_csv[, i])
        expect_equal(central@esg@ind_inflation[, i], ind_infl_up_csv[, i])
        expect_equal(central@esg@ind_inflation[, i], ind_infl_down_csv[, i])
    }
})
