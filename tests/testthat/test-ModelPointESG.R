# ##################
# Tests ModelPointESG
# ##################

context("ModelPointESG")


folder_ESG_address <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/parametres/esg/ESG"

# Donnees
ind_inf <- (1:100)/100
yield_curve <- (1:100)/100
deflateur <- 1/(1+(1:100)/100)^(1:100)

# Creation d'unn objet Model Point
mpesg <- new("ModelPointESG",
             annee = 1L,
             num_traj = 1L,
             indice_action = data.frame(S_action = 1,
                                        S_prev_action = 2),
             indice_immo   = data.frame(S_immo = 5,
                                        S_prev_immo = 6),
             indice_inflation = ind_inf,
             yield_curve = yield_curve,
             deflateur = deflateur)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe ModelPointESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Test_Classe", {
    
    # Test classe
    expect_s4_class(mpesg, "ModelPointESG")
    
    # Attributs
    expect_equal(mpesg@annee, 1L)
    expect_equal(mpesg@num_traj, 1L)
    expect_equal(mpesg@indice_action$S_action, 1)
    expect_equal(mpesg@indice_action$S_prev_action, 2)
    expect_equal(mpesg@indice_immo$S_immo, 5)
    expect_equal(mpesg@indice_immo$S_prev_immo, 6)
    expect_equal(mpesg@indice_inflation, ind_inf)
    expect_equal(mpesg@yield_curve, yield_curve)
    expect_equal(mpesg@deflateur, deflateur)
})

