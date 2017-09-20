# ##################
# Tests Immo
# ##################

context("Immo")

# Lecture du fichier csv
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/input/donnees/actif"
ptf_immo_csv <- read.csv2(paste(path, "Portefeuille_immobilier.csv", sep = "/"), header = TRUE,
                            colClasses = c("integer","double","double", "double",
                                           "logical","logical","double", "double",
                                           "double","integer","double","logical"))

# Creation de l'objet
ptf_immo <- new("Immo", ptf_immo_csv)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe Immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_immo_classe", {

    # Test classe
    expect_s4_class(ptf_immo, "Immo")

    # Tests attributs
    expect_equal(ptf_immo@ptf_immo, ptf_immo_csv)
})


#----------------------------------------------------------------------------------
# revalo_immo
#----------------------------------------------------------------------------------
test_that("TEST_revalo_immo", {

    # Appel de la fonction
    S      <- c(102,115)
    S_prev <- c(100,120)
    res <- revalo_immo(ptf_immo, S, S_prev)

    # Tests
    expect_equal(res[,"rdt"], c(0.0079, -0.0416), tolerance = 0.01)
    expect_equal(res[,"loyer"], c(60438.299, 2447.4), tolerance = 0.01)

    # Test avec erreur
    expect_error(revalo_immo(ptf_immo, c(102,115,120), c(100,120,120)))
    expect_error(revalo_immo(ptf_immo, 102, 100))
})


#----------------------------------------------------------------------------------
# calc_vm_immo
#----------------------------------------------------------------------------------
test_that("TEST_calc_vm_immo",{

    # Appel de la fonction
    S      <- c(102,115)
    S_prev <- c(100,120)
    rdt <- revalo_immo(ptf_immo, S, S_prev)[,"rdt"]
    res <- calc_vm_immo(ptf_immo, rdt)

    # Test
    expect_equal(res, c(5056392.93, 47916.67), tolerance = 0.01)
})



#----------------------------------------------------------------------------------
# calc_pmvl_immo
#----------------------------------------------------------------------------------
test_that("TEST_calc_pmvl_immo", {

    # Appel de la fonction
    res <- calc_pmvl_immo(ptf_immo)

    # Resultats attendus
    val_marche <- ptf_immo@ptf_immo$val_marche
    val_nc <- ptf_immo@ptf_immo$val_nc
    pvl <- sum((val_marche>val_nc) * (val_marche- val_nc ))
    mvl <- sum((val_marche<val_nc) * (val_marche - val_nc ))

    # Test
    expect_equal(res$pvl, pvl)
    expect_equal(res$mvl, mvl)
})



#----------------------------------------------------------------------------------
# sell_immo
#----------------------------------------------------------------------------------
test_that("TEST_sell_immo", {

    ## 1 : Vente partielle uniquement
    # Appel de la fonction
    num_sold = 1L
    nb_sold  = ptf_immo@ptf_immo$nb_unit[which(ptf_immo@ptf_immo$num_mp == num_sold)] / 2
    res <- sell_immo(ptf_immo, num_sold, nb_sold)

    # Resultats attendus
    pct_sold <- c(nb_sold, 0)

    # Test
    expect_equal(res$immo@ptf_immo$val_marche, ptf_immo@ptf_immo$val_marche * ( 1 - pct_sold))
    expect_equal(res$immo@ptf_immo$val_nc, ptf_immo@ptf_immo$val_nc * ( 1 - pct_sold))
    expect_equal(res$immo@ptf_immo$val_achat, ptf_immo@ptf_immo$val_achat * ( 1 - pct_sold))
    expect_equal(res$immo@ptf_immo$presence, ptf_immo@ptf_immo$presence)
    expect_equal(res$immo@ptf_immo$cessible, ptf_immo@ptf_immo$cessible)
    expect_equal(res$immo@ptf_immo$nb_unit, ptf_immo@ptf_immo$nb_unit * ( 1 - pct_sold))
    expect_equal(res$immo@ptf_immo$dur_det, ptf_immo@ptf_immo$dur_det)
    expect_equal(res$immo@ptf_immo$pdd, ptf_immo@ptf_immo$pdd)
    expect_equal(res$immo@ptf_immo$num_index, ptf_immo@ptf_immo$num_index)
    expect_equal(res$immo@ptf_immo$div, ptf_immo@ptf_immo$div)
    expect_equal(res$immo@ptf_immo$ind_invest, ptf_immo@ptf_immo$ind_invest)



    ## 2 : Vente totale uniquement
    # Appel de la fonction
    num_sold = 2L
    nb_sold = ptf_immo@ptf_immo$nb_unit[which(ptf_immo@ptf_immo$num_mp == num_sold)]
    res <- sell_immo(ptf_immo, num_sold, nb_sold)

    # Test
    expect_equal(nrow(res$immo@ptf_immo), 1L)
    expect_equal(res$immo@ptf_immo, ptf_immo_csv[1,])


    ## 3 : Vente partielle et totale simultanee
    # Appel de la fonction
    num_sold <- c(1L, 2L)
    nb_sold_part <- ptf_immo@ptf_immo$nb_unit[which(ptf_immo@ptf_immo$num_mp == 1L)] / 2
    nb_sold_tot <- ptf_immo@ptf_immo$nb_unit[which(ptf_immo@ptf_immo$num_mp == 2L)]
    nb_sold <- c(nb_sold_part, nb_sold_tot)
    res <- sell_immo(ptf_immo, num_sold, nb_sold)

    # Calcul des resultats attendus
    pct_sell <- nb_sold / ptf_immo@ptf_immo$nb_unit

    # Test
    expect_equal(nrow(res$immo@ptf_immo), 1L)
    expect_equal(res$immo@ptf_immo$val_marche, ptf_immo@ptf_immo$val_marche[1] * ( 1 - pct_sell[1]))
    expect_equal(res$immo@ptf_immo$val_nc, ptf_immo@ptf_immo$val_nc[1] * ( 1 - pct_sell[1]))
    expect_equal(res$immo@ptf_immo$val_achat, ptf_immo@ptf_immo$val_achat[1] * ( 1 - pct_sell[1]))
    expect_equal(res$immo@ptf_immo$nb_unit, ptf_immo@ptf_immo$nb_unit[1] * ( 1 - pct_sell[1]))


    ## 4.1 : Problemes de vente
    # Vente d'une immo nom presente
    expect_error(sell_immo(ptf_immo, 17, 0.1))
    # Vente en trop grande quantite
    expect_error(sell_immo(ptf_immo, 1, 1.5))
    # Idem en vente multiple
    expect_error(sell_immo(ptf_immo, c(1,17), c(0.2,0.3)))
    expect_error(sell_immo(ptf_immo, c(1,2), c(0.1,1.1)))
})


#----------------------------------------------------------------------------------
# buy_immo
#----------------------------------------------------------------------------------
test_that("TEST_buy_immo", {

    ## 1 : Achat d'un PTF non vide
    # Appel de la fonction
    res <- buy_immo(ptf_immo, new("Immo", ptf_immo_csv))

    # Test
    expect_equal(nrow(res@ptf_immo), 4L)


    ## 2 : Acht a partir d'un PTF vide
    # Appel de la focntion
    res <- buy_immo(new("Immo"), ptf_immo)

    # Test
    expect_equal(res, ptf_immo)


    # Achat d'un PTF vide
    expect_error(buy_immo(ptf_immo, new("Immo")))
})


#----------------------------------------------------------------------------------
# update_vm_immo
#----------------------------------------------------------------------------------
test_that("TEST_update_vm_immo", {

    # Appel de la fonction
    vm <- c(5000,10000)
    res <- update_vm_immo(ptf_immo, vm)

    # Test
    expect_equal(res@ptf_immo$num_mp, ptf_immo@ptf_immo$num_mp)
    expect_equal(res@ptf_immo$val_marche, vm)
    expect_equal(res@ptf_immo$val_nc, ptf_immo@ptf_immo$val_nc)
    expect_equal(res@ptf_immo$val_achat, ptf_immo@ptf_immo$val_achat)
    expect_equal(res@ptf_immo$presence, ptf_immo@ptf_immo$presence)
    expect_equal(res@ptf_immo$cessible, ptf_immo@ptf_immo$cessible)
    expect_equal(res@ptf_immo$nb_unit, ptf_immo@ptf_immo$nb_unit)
    expect_equal(res@ptf_immo$dur_det, ptf_immo@ptf_immo$dur_det)
    expect_equal(res@ptf_immo$pdd, ptf_immo@ptf_immo$pdd)
    expect_equal(res@ptf_immo$num_index, ptf_immo@ptf_immo$num_index)
    expect_equal(res@ptf_immo$loyer, ptf_immo@ptf_immo$loyer)
    expect_equal(res@ptf_immo$ind_invest, ptf_immo@ptf_immo$ind_invest)


    # Tests avec erreur
    expect_error(update_vm_immo(ptf_immo, c(10000,-10000)))
    expect_error(update_vm_immo(ptf_immo, c(10000)))
})



#----------------------------------------------------------------------------------
# update_dur_det_immo
#----------------------------------------------------------------------------------
test_that("TEST_update_dur_det_immo", {

    # Appel de la fonction
    res <- update_dur_det_immo(ptf_immo)

    # Test
    expect_equal(res@ptf_immo$num_mp, ptf_immo@ptf_immo$num_mp)
    expect_equal(res@ptf_immo$val_marche, ptf_immo@ptf_immo$val_marche)
    expect_equal(res@ptf_immo$val_nc, ptf_immo@ptf_immo$val_nc)
    expect_equal(res@ptf_immo$val_achat, ptf_immo@ptf_immo$val_achat)
    expect_equal(res@ptf_immo$presence, ptf_immo@ptf_immo$presence)
    expect_equal(res@ptf_immo$cessible, ptf_immo@ptf_immo$cessible)
    expect_equal(res@ptf_immo$nb_unit, ptf_immo@ptf_immo$nb_unit)
    expect_equal(res@ptf_immo$dur_det, ptf_immo@ptf_immo$dur_det + 1L)
    expect_equal(res@ptf_immo$pdd, ptf_immo@ptf_immo$pdd)
    expect_equal(res@ptf_immo$num_index, ptf_immo@ptf_immo$num_index)
    expect_equal(res@ptf_immo$loyer, ptf_immo@ptf_immo$loyer)
    expect_equal(res@ptf_immo$ind_invest, ptf_immo@ptf_immo$ind_invest)

    # Test avec warning : PTF vide
    expect_warning(update_dur_det_immo(new("Immo")))
})

