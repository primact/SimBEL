# ##################
# Tests Treso
# ##################

context("Treso")

# Lecture du fichier csv
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/actif"
ptf_treso_csv <- read.csv2(paste(path, "Tresorerie.csv", sep = "/"), header = TRUE,
                          colClasses = c("integer",  "double", "double"))

# Creation de l'objet
ptf_treso <- new("Treso", ptf_treso_csv)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe Tresorerie
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_treso_classe", {

    # Test classe
    expect_s4_class(ptf_treso, "Treso")

    # Tests attributs
    expect_equal(ptf_treso@ptf_treso, ptf_treso_csv)
})


#--------------------------------------------------
# revalo_treso
#--------------------------------------------------
test_that("TEST_revalo_treso", {

    # Appel de la fonction
    Rt      <- .102
    Rt_prev <- .100
    res <- revalo_treso(Rt, Rt_prev)

    # Tests
    expect_equal(res, (1 + Rt) / (1 + Rt_prev) - 1)

    # Test avec erreur
    expect_error(revalo_treso(c(0.102,0.115), c(0.100,0.120)))
})


#--------------------------------------------------
# revenu_treso
#--------------------------------------------------
test_that("TEST_revenu_treso", {

    # Appel de la fonction
    flux_milieu <- 500
    rdt <- 0.02
    revenu_treso(ptf_treso, rdt, flux_milieu)

})


#--------------------------------------------------
# Test fonction calc_vm :
#--------------------------------------------------
test_that("TEST_calc_vm_treso", {

    # Appel de la fonction
    flux_milieu <- 1000
    flux_fin <- 500
    rdt <- 0.02
    res <- calc_vm_treso(ptf_treso, rdt, flux_milieu, flux_fin)

    # Resultat attendu
    vm <- ptf_treso@ptf_treso$val_marche * (1 + rdt) + flux_milieu * sqrt(1 + rdt) + flux_fin

    # Test
    expect_equal(res, vm)

    # Tests avec erreurs
    expect_error(calc_vm_treso(ptf_treso, c(rdt, 0.1), c(1000, 1000), c(500, 500)))
})



#--------------------------------------------------
# Test fonction update_treso:
#--------------------------------------------------
test_that("TEST_update_treso", {

    ## 1 - Montant positif
    # Appel de la fonction
    flux <- 5000
    res <- update_treso(ptf_treso, flux)

    # Test
    expect_equal(res@ptf_treso$val_marche, ptf_treso@ptf_treso$val_marche + flux)
    expect_equal(res@ptf_treso$val_nc, ptf_treso@ptf_treso$val_nc + flux)


    ## 2 - Montant negatif
    # Appel de la fonction
    flux <- -5000
    res <- update_treso(ptf_treso, flux)

    # Test
    expect_equal(res@ptf_treso$val_marche, ptf_treso@ptf_treso$val_marche + flux)
    expect_equal(res@ptf_treso$val_nc, ptf_treso@ptf_treso$val_nc + flux)


    ## 3 - Montant negatif
    # Appel de la fonction
    flux <- -50000
    res <- update_treso(ptf_treso, flux)

    # Test
    expect_equal(res@ptf_treso$val_marche, ptf_treso@ptf_treso$val_marche + flux)
    expect_equal(res@ptf_treso$val_nc, ptf_treso@ptf_treso$val_nc + flux)
})

