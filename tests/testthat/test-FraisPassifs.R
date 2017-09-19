# ##################
# Tests FraisPassifs
# ##################

context("FraisPassifs")

# Lecture du fichier csv
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/passif/frais_passif.csv"
frais_passifs_csv <- read.csv2(path, header = TRUE)

# Creation de l'objet
frais_passif <- new("FraisPassif", frais_passifs_csv)

#----------------------------------------------------------------------------------
# Classe et attributs
#----------------------------------------------------------------------------------
test_that("TEST_frais_passif", {

    # Test classe
    expect_s4_class(frais_passif, "FraisPassif")

    # Tests attributs
    expect_equal(frais_passif@mp$frais_fixe_prime, frais_passifs_csv$frais_fixe_prime)
    expect_equal(frais_passif@mp$frais_var_prime, frais_passifs_csv$frais_var_prime)
    expect_equal(frais_passif@mp$frais_var_prime, frais_passifs_csv$frais_var_prime)
    expect_equal(frais_passif@mp$ind_inf_frais_fixe_prime, frais_passifs_csv$ind_inf_frais_fixe_prime)
    expect_equal(frais_passif@mp$ind_inf_frais_var_prime, frais_passifs_csv$ind_inf_frais_var_prime)
    expect_equal(frais_passif@mp$frais_fixe_prest, frais_passifs_csv$frais_fixe_prest)
    expect_equal(frais_passif@mp$frais_var_prest, frais_passifs_csv$frais_var_prest)
    expect_equal(frais_passif@mp$ind_inf_frais_fixe_prest, frais_passifs_csv$ind_inf_frais_fixe_prest)
    expect_equal(frais_passif@mp$ind_inf_frais_var_prest, frais_passifs_csv$ind_inf_frais_var_prest)
    expect_equal(frais_passif@mp$frais_fixe_enc, frais_passifs_csv$frais_fixe_enc)
    expect_equal(frais_passif@mp$frais_var_enc, frais_passifs_csv$frais_var_enc)
    expect_equal(frais_passif@mp$ind_inf_frais_fixe_enc, frais_passifs_csv$ind_inf_frais_fixe_enc)
    expect_equal(frais_passif@mp$ind_inf_frais_var_enc, frais_passifs_csv$ind_inf_frais_var_enc)
})



#----------------------------------------------------------------------------------
# Load
#----------------------------------------------------------------------------------
test_that("TEST_load", {

    # Appel de la fonction
    res <- frais_passif_load(path)

    # Test
    expect_identical(res, frais_passif)
})



#----------------------------------------------------------------------------------
# calc_frais
#----------------------------------------------------------------------------------
test_that("TEST_calc_frais", {

    # Appel de la fonction
    res <- calc_frais(frais_passif, "prime", "epeuro1", 3, 5, 1.02)

    # Tests
    expect_equal(res$frais_fixe_prime, 0, tolerance = 0.01)
    expect_equal(res$frais_var_prime, 0, tolerance = 0.01)
})
