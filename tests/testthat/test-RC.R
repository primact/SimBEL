# Fichier test
# GK 21/02/2017

context("RC")

rc <- new("RC", val_debut = 100, val_courante = 150)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe RC
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_Classe", {

    # Test classe
    expect_s4_class(rc, "RC")

    # Test attributs
    expect_equal(rc@val_debut, 100)
    expect_equal(rc@val_courante, 150)
})

#----------------------------------------------------------------------------------
# Calcul de la valeur courante de RC
#----------------------------------------------------------------------------------
test_that("TEST_calc_rc", {

    ## 1 - Moins value
    # Appel de la fonction
    res <- calc_RC(rc, -200)

    # Tests
    expect_equal(res$rc_courante, 0)
    expect_equal(res$var_rc, -100)


    ## 2 - Plus value
    # Appel de la fonction
    res <- calc_RC(rc, 200)

    # Tests
    expect_equal(res$rc_courante, 350)
    expect_equal(res$var_rc, 250)
})


#----------------------------------------------------------------------------------
# Fonctions d'update de RC
#----------------------------------------------------------------------------------
test_that("TEST_update", {

    # Donnees
    pmvr <- calc_RC(rc, -200)[[1]]


    ## 1 - Update val_courante
    # Appel de la fonction
    res <- do_update_RC_val_courante(rc, pmvr)

    # Test
    expect_equal(res@val_debut, 100)
    expect_equal(res@val_courante, 0)


    # 2 - Update val_debut
    # Appel de la fonction
    res <- do_update_RC_val_debut(rc, pmvr)

    # Test
    expect_equal(res@val_debut, 0)
    expect_equal(res@val_courante, 150)
})


#----------------------------------------------------------------------------------
# rc_load
#----------------------------------------------------------------------------------
test_that("TEST_load", {

  # Donnees
  path <- paste0(getwd(), "/donnees_tests/input/donnees/actif")
  file_rc_address <- paste(path, "RC.csv", sep = "/")
  csv_file <- read.csv2(file_rc_address, header = T)

  # Appel de la fonction
  res <- rc_load(file_rc_address)

  # Tests
  expect_equal(res@val_debut, csv_file[1,"rc_init"])
  expect_equal(res@val_courante, csv_file[1,"rc_init"])
})
