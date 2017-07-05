# ##################
# Tests PRE
# ##################

context("PRE")


# Creation de l'objet
pre <- new("PRE", val_debut = 100, val_courante = 150, ryth_dot = 5L)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe PRE
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_PRE_classe", {
    
    # Test classe
    expect_s4_class(pre, "PRE")
    
    # Tests attributs
    expect_equal(pre@val_debut, 100)
    expect_equal(pre@val_courante, 150)
    expect_equal(pre@ryth_dot, 5L)
})



#----------------------------------------------------------------------------------
# calc_PRE
#----------------------------------------------------------------------------------
test_that("TEST_calc_PRE", {
    
    ## 1 - Moins value
    # Appel de la fonction
    res <- calc_PRE(pre, -200)
    
    # Test
    expect_equal(res$pre_courante, 140)
    expect_equal(res$var_pre, 40)
    
    
    ## 2 - Plus value
    # Appel de la fonction
    res <- calc_PRE(pre, 200)
    
    # Test
    expect_equal(res$pre_courante, 0)
    expect_equal(res$var_pre, -100)
    
    
    # Tests avec erreur
    expect_error(calc_PRE(pre, c(200,-200)))
})


#----------------------------------------------------------------------------------
# do_update_PRE_val_courante
#----------------------------------------------------------------------------------
test_that("TEST_update_val_courante", {
    
    # Appel de la fonction
    val_courante <- calc_PRE(pre, pmvl_action_immo = -200)[[1]]
    res <- do_update_PRE_val_courante(pre, val_courante)
    
    # Tests
    expect_equal(res@val_debut, 100)
    expect_equal(res@val_courante, 140)
    expect_equal(res@ryth_dot, 5)
})


#----------------------------------------------------------------------------------
# do_update_PRE_val_debut
#----------------------------------------------------------------------------------
test_that("TEST_update_val_debut", {
    
    # Appel de la fonction
    val_courante <- calc_PRE(pre, pmvl_action_immo = -200)[[1]]
    res <- do_update_PRE_val_debut(pre, val_courante)
    
    # Tests
    expect_equal(res@val_debut, 140)
    expect_equal(res@val_courante, 150)
    expect_equal(res@ryth_dot, 5)
})


#----------------------------------------------------------------------------------
# pre_load
#----------------------------------------------------------------------------------
test_that("TEST_load", {
    
    # Donnees
    path <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/donnees/actif"
    file_PRE_address <- paste(path, "PRE.csv", sep = "/")
    csv_file <- read.csv2(file_PRE_address, header = T)
    
    # Appel de la fonction
    res <- pre_load(file_PRE_address)
    
    # Tests
    expect_equal(res@val_debut, csv_file[1,"pre_init"])
    expect_equal(res@val_courante, csv_file[1,"pre_init"])
    expect_equal(res@ryth_dot, csv_file[1,"ryth_dot"])
    
})
