# ##################
# Tests AutresReserves
# ##################

context("AutresReserves")

# Dossier de DATA
path <- paste0(getwd(), "/donnees_tests/input")
folder_tab <- paste(path, "/donnees/passif/autres_reserves.csv", sep = "/")


#----------------------------------------------------------------------------------------------------------------------------------------------------
# Elements necessaires aux calculs

reserves_csv <- read.csv2(folder_tab, header = TRUE, colClasses = c("numeric", "numeric", "numeric",
                                                                    "numeric", "numeric", "numeric",
                                                                    "numeric", "numeric"))
reserves <- new("AutresReserves", reserves_csv$pgg_debut, reserves_csv$psap_debut, reserves_csv$pgg_valeur, reserves_csv$psap_valeur,
                reserves_csv$tx_pgg_ep, reserves_csv$tx_pgg_autres, reserves_csv$tx_psap_ep, reserves_csv$tx_psap_autres)


#----------------------------------------------------------------------------------
# Classe
#----------------------------------------------------------------------------------
test_that("TEST_classe", {

    # Classe
    expect_s4_class(reserves, "AutresReserves")

    # Attributs
    expect_equal(reserves@pgg_debut, reserves_csv$pgg_debut)
    expect_equal(reserves@psap_debut, reserves_csv$psap_debut)
    expect_equal(reserves@pgg_valeur, reserves_csv$pgg_valeur)
    expect_equal(reserves@psap_valeur, reserves_csv$psap_valeur)
    expect_equal(reserves@tx_pgg_ep, reserves_csv$tx_pgg_ep)
    expect_equal(reserves@tx_pgg_autres, reserves_csv$tx_pgg_autres)
    expect_equal(reserves@tx_psap_ep, reserves_csv$tx_psap_ep)
    expect_equal(reserves@tx_psap_autres, reserves_csv$tx_psap_autres)
})


#----------------------------------------------------------------------------------
# load
#----------------------------------------------------------------------------------
test_that("TEST_load", {

    # Appel de la fonction
    res <- autres_reserves_load(folder_tab)

    # Test
    expect_identical(res, reserves)
})


#----------------------------------------------------------------------------------
# update_reserves
#----------------------------------------------------------------------------------
test_that("TEST_update_reserves", {

    # Donnees
    prest_ep <- 10
    prest_autres <- 20
    pm_ep <- 30
    pm_autres <- 40

    # Appel de la fonction
    res <- update_reserves(reserves, prest_ep, prest_autres, pm_ep, pm_autres)
})


#----------------------------------------------------------------------------------
# init_debut_pgg_psap
#----------------------------------------------------------------------------------
test_that("TEST_init_debut_pgg_psap", {

    # Appel de la fonction
    res <- init_debut_pgg_psap(reserves)

    # Test
    expect_equal(res@pgg_debut, reserves@psap_valeur)
    expect_equal(res@psap_debut, reserves@pgg_valeur)
    expect_equal(res@pgg_valeur, reserves@pgg_valeur)
    expect_equal(res@psap_valeur, reserves@psap_valeur)
    expect_equal(res@tx_pgg_ep, reserves@tx_pgg_ep)
    expect_equal(res@tx_pgg_autres, reserves@tx_pgg_autres)
    expect_equal(res@tx_psap_ep, reserves@tx_psap_ep)
    expect_equal(res@tx_psap_autres, reserves@tx_psap_autres)
})
