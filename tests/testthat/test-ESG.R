# ##################
# Tests ESG
# ##################

context("ESG")


folder_ESG_address <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/parametres/esg/ESG"
path <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/donnees/actif"


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
