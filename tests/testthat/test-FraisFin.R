# ##################
# Tests FraisFin
# ##################

context("FraisFin")


folder_ESG_address <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/parametres/esg/ESG"
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/actif"

# Creation objets
frais <- new("FraisFin", tx_chargement = 0.15, indicatrice_inflation = TRUE)
frais_bis <- new("FraisFin", tx_chargement = 0.15, indicatrice_inflation = FALSE)

# Chargement des donnees necessaires
table_ESG <- chargement_ESG(folder_ESG_address, 2L, 50L)
mp_ESG <-extract_ESG(table_ESG, 1L, 0L)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Test de la classe de frais :
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_FraisFin", {

    # Test classe
    expect_s4_class(frais, "FraisFin")

    # Tests attributs
    expect_equal(frais@tx_chargement, 0.15)
    expect_equal(frais@indicatrice_inflation, TRUE)
})

#----------------------------------------------------------------------------------
# calc_frais_fin
#----------------------------------------------------------------------------------
test_that("TEST_calc_frais_fin", {

    # Appel de la fonction
    res <- calc_frais_fin(frais, vm_moy = 100, coef_inflation = 0.04)
    res_bis <- calc_frais_fin(frais_bis, vm_moy = 100, coef_inflation = 0.04)

    # Test
    expect_equal(res, 0.15 * 100 * 0.04)
    expect_equal(res_bis, 0.15 * 100)
})


#----------------------------------------------------------------------------------
# frais_fin_load
#----------------------------------------------------------------------------------
test_that("TEST_load", {

    # Appel de la fonction
    file_frais_fin_address = paste(path,"Frais_financier.csv", sep = "/")
    csv <- read.csv2(file_frais_fin_address, header = TRUE)
    res <- frais_fin_load(file_frais_fin_address)

    # Test
    expect_equal(res@tx_chargement, csv[1,"tx_chargement"])
    expect_equal(res@indicatrice_inflation, csv[1,"indicatrice_inflation"])

})
