# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc spread")

path <- paste0(getwd(), "/donnees_tests")

table_choc_spread_csv <- read.csv2(
    paste(path, "input/parametres/chocs/param_choc_mket_spread.csv", sep = "/"),
    colClasses = c("integer", "character", "numeric", "numeric")
)
ptf_obligation_csv <- read.csv2(
    paste(path, "input/donnees/actif/Portefeuille_obligation.csv", sep = "/"),
    colClasses = c(
        "integer", "numeric", "numeric", "numeric", "logical", "logical", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric", "factor", "integer",
        "numeric", "numeric", "numeric", "numeric", "character", "numeric"
    )
)

ligne1 <- ptf_obligation_csv[1, ] # rating = 3 ; duration = 5
ligne2 <- ptf_obligation_csv[2, ] # rating = 1 ; duration = 5
ligne3 <- ptf_obligation_csv[3, ] # rating = 2 ; duration = 12
ligne4 <- ptf_obligation_csv[4, ] # rating = 4 ; duration = 16



racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
table_choc <- init_scenario(racine)
central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
actif_central <- print_alloc(central@canton@ptf_fin)
spread <- get(load(paste(racine@address$save_folder$spread, "best_estimate.RData", sep = "/")))
actif_spread <- print_alloc(spread@canton@ptf_fin)


# Test manuels
val_marche_oblig <- ligne1$val_marche + ligne2$val_marche + ligne3$val_marche + ligne4$val_marche

choc_spread_manuel <- do_choc_spread_unitaire(table_choc_spread_csv, ligne1) +
    ligne2$val_marche + # ligne 2 non choquees car souveraine
    do_choc_spread_unitaire(table_choc_spread_csv, ligne3) +
    do_choc_spread_unitaire(table_choc_spread_csv, ligne4)



# Test du choc du portefeuille obligation ligne par ligne
#--------------------------------------------------

test_that("TEST_choc_taux_portfolio", {
    # Verification du central
    expect_equal(actif_central[3, 1], val_marche_oblig)

    # Verifications choc_spread_unitaire : oblig souveraines choquees
    expect_equal(do_choc_spread_unitaire(table_choc_spread_csv, ligne1), ligne1$val_marche * (1 - 0.025 * 5))
    expect_equal(do_choc_spread_unitaire(table_choc_spread_csv, ligne2), ligne2$val_marche * (1 - 0.011 * 5))
    expect_equal(do_choc_spread_unitaire(table_choc_spread_csv, ligne3), ligne3$val_marche * (1 - (0.105 + 0.005 * (12 - 10))))
    expect_equal(do_choc_spread_unitaire(table_choc_spread_csv, ligne4), ligne4$val_marche * (1 - (0.44 + 0.005 * (16 - 15))))

    # Verification choc_spread : oblig souveraines non choquees
    expect_equal(actif_spread[3, 1], choc_spread_manuel)

    # Verifications de la maj du portefeuille
    expect_equal(actif_spread[1, 1], actif_central[1, 1])
    expect_equal(actif_spread[2, 1], actif_central[2, 1])
    expect_equal(actif_spread[4, 1], actif_central[4, 1])
    expect_equal(actif_spread[1, 3], actif_central[1, 3])
    expect_equal(actif_spread[2, 3], actif_central[2, 3])
    expect_equal(actif_spread[3, 3], actif_central[3, 3])
    expect_equal(actif_spread[4, 3], actif_central[4, 3])
    expect_equal(actif_spread[5, 3], actif_central[5, 3])
})
