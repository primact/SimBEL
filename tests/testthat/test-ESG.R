# ##################
# Tests ESG
# ##################

context("ESG")

path <- paste0(getwd(), "/donnees_tests/input")
folder_ESG_address <- paste0(path, "/parametres/esg/ESG")
path <- paste0(path, "/donnees/actif")

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
