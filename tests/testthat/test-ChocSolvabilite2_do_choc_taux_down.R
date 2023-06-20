# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc taux_down")

path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
table_choc <- init_scenario(racine)


central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
taux_down <- get(load(paste(racine@address$save_folder$taux_down, "best_estimate.RData", sep = "/")))

actif_central <- print_alloc(central@canton@ptf_fin)
actif_taux_down <- print_alloc(taux_down@canton@ptf_fin)


# Chargement des courbes de taux
courbe_taux0 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_0_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux1 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_1_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux2 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_2_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux3 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_3_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux4 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_4_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux5 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_5_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux6 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_6_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux7 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_7_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux8 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_8_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux9 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_9_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux10 <- read.csv2(
    paste(path, "input/parametres/esg/ESG/Simulation_CourbeDesTaux/Courbe_Taux_dans_10_an_G2++_20160510_14h26m26s_QZCsto _.csv", sep = "/")
)
courbe_taux_down0 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_0_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down1 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_1_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down2 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_2_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down3 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_3_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down4 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_4_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down5 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_5_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down6 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_6_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down7 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_7_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down8 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_8_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down9 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_9_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)
courbe_taux_down10 <- read.csv2(
    paste(path, "input/parametres/esg/ESG_down/Simulation_CourbeDesTaux/Courbe_Taux_dans_10_an_G2++_20160510_14h32m00s_QZCsto _.csv", sep = "/")
)

# Test sur les actifs
#--------------------------------------------------

test_that("TEST_choc_taux_down_portfolio", {
    # Verifications classe
    expect_s4_class(central, "Be")
    expect_s4_class(taux_down, "Be")
    expect_s4_class(taux_down, "Be")

    # Verification evolution val marche oblig
    expect_true(actif_taux_down[3, 1] != actif_central[3, 1])

    # Verifications maj portefeuille
    # alloc_valeur
    expect_equal(actif_taux_down[1, 1], actif_central[1, 1])
    expect_equal(actif_taux_down[2, 1], actif_central[2, 1])
    expect_equal(actif_taux_down[4, 1], actif_central[4, 1])
    expect_equal(actif_taux_down[1, 3], actif_central[1, 3])
    expect_equal(actif_taux_down[2, 3], actif_central[2, 3])
    expect_equal(actif_taux_down[3, 3], actif_central[3, 3])
    expect_equal(actif_taux_down[4, 3], actif_central[4, 3])

    # Verification chargement courbes de taux
    expect_equal(central@esg@yield_curve$annee0, courbe_taux0)
    expect_equal(central@esg@yield_curve$annee1, courbe_taux1)
    expect_equal(central@esg@yield_curve$annee2, courbe_taux2)
    expect_equal(central@esg@yield_curve$annee3, courbe_taux3)
    expect_equal(central@esg@yield_curve$annee4, courbe_taux4)
    expect_equal(central@esg@yield_curve$annee5, courbe_taux5)
    expect_equal(central@esg@yield_curve$annee6, courbe_taux6)
    expect_equal(central@esg@yield_curve$annee7, courbe_taux7)
    expect_equal(central@esg@yield_curve$annee8, courbe_taux8)
    expect_equal(central@esg@yield_curve$annee9, courbe_taux9)
    expect_equal(central@esg@yield_curve$annee10, courbe_taux10)

    expect_equal(taux_down@esg@yield_curve$annee0, courbe_taux_down0)
    expect_equal(taux_down@esg@yield_curve$annee1, courbe_taux_down1)
    expect_equal(taux_down@esg@yield_curve$annee2, courbe_taux_down2)
    expect_equal(taux_down@esg@yield_curve$annee3, courbe_taux_down3)
    expect_equal(taux_down@esg@yield_curve$annee4, courbe_taux_down4)
    expect_equal(taux_down@esg@yield_curve$annee5, courbe_taux_down5)
    expect_equal(taux_down@esg@yield_curve$annee6, courbe_taux_down6)
    expect_equal(taux_down@esg@yield_curve$annee7, courbe_taux_down7)
    expect_equal(taux_down@esg@yield_curve$annee8, courbe_taux_down8)
    expect_equal(taux_down@esg@yield_curve$annee9, courbe_taux_down9)
    expect_equal(taux_down@esg@yield_curve$annee10, courbe_taux_down10)
})
