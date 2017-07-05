# ##################
# Tests ParamAlmEngine
# ##################

context("ParamAlmEngine")


path <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/donnees/actif"
folder_ESG_address <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/parametres/esg/ESG"


#----------------------------------------------------------------------------------------------------------------------------------------------------
# Elements necessaires aux calculs ulterieurs
alloc_cible <- c(.25,.25,.48,.02)
table_ESG <- chargement_ESG(folder_ESG_address, 2L, 5L)
mp_ESG <- extract_ESG(table_ESG, 1L, 0L)

PtfFin <- chargement_PortFin(path, mp_ESG)
PtfFin_ref <- chargement_PortFin_reference(paste(path, "Portefeuille_reference", sep = "/"), mp_ESG)

param_alm_engine <- new("ParamAlmEngine", ptf_reference = PtfFin_ref, alloc_cible = alloc_cible, seuil_realisation_PVL = 0)


#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_ParamAlmEngine", {
    
    # Test classe
    expect_s4_class(param_alm_engine, "ParamAlmEngine")
    
    # Tests attributs
    expect_equal(param_alm_engine@ptf_reference, PtfFin_ref)
    expect_equal(param_alm_engine@alloc_cible, alloc_cible)
    expect_equal(param_alm_engine@seuil_realisation_PVL, 0)
})
