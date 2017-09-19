# ##################
# Tests ESG
# ##################

context("ESG")


folder_ESG_address <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/parametres/esg/ESG"
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/actif"


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe ESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_ESG", {

    # Chargement de l'ensemble des donnees d'inputs (indices + courbes de taux) : doit fonctionner
    x <- chargement_ESG(folder_ESG_address, 2L, 5L)


    # Extraction des conditions d'une annee sur une trajectoire
    res <- extract_ESG(x, 2L, 1L)
    res <- extract_ESG(x, 2L, 0L)
    res <- extract_ESG(x, 2L, 5L)

    # Test classe
    expect_s4_class(res, "ModelPointESG")
})
