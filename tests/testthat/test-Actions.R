# ##################
# Tests Actions
# ##################

context("Actions")

library(rootSolve)

# Lecture du fichier csv
path <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/donnees/actif"
ptf_action_csv <- read.csv2(paste(path, "Portefeuille_action.csv", sep = "/"), header = TRUE,
                               colClasses = c("integer","double","double", "double",
                                              "logical","logical","double",  "double",
                                              "double","integer","double","logical"))

# Creation de l'objet
ptf_action <- new("Action", ptf_action_csv)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe Action
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_action_classe", {

    # Test classe
    expect_s4_class(ptf_action, "Action")

    # Tests attributs
    expect_equal(ptf_action@ptf_action$num_mp, ptf_action_csv$num_mp)
    expect_equal(ptf_action@ptf_action$val_marche, ptf_action_csv$val_marche)
    expect_equal(ptf_action@ptf_action$val_nc, ptf_action_csv$val_nc)
    expect_equal(ptf_action@ptf_action$val_achat, ptf_action_csv$val_achat)
    expect_equal(ptf_action@ptf_action$presence, ptf_action_csv$presence)
    expect_equal(ptf_action@ptf_action$cessible, ptf_action_csv$cessible)
    expect_equal(ptf_action@ptf_action$nb_unit, ptf_action_csv$nb_unit)
    expect_equal(ptf_action@ptf_action$dur_det, ptf_action_csv$dur_det)
    expect_equal(ptf_action@ptf_action$pdd, ptf_action_csv$pdd)
    expect_equal(ptf_action@ptf_action$num_index, ptf_action_csv$num_index)
    expect_equal(ptf_action@ptf_action$div, ptf_action_csv$div)
    expect_equal(ptf_action@ptf_action$ind_invest, ptf_action_csv$ind_invest)
})


#----------------------------------------------------------------------------------
# revalo_action
#----------------------------------------------------------------------------------
test_that("TEST_revalo_action", {

    # Appel de la fonction
    S <- c(102,115,130)
    S_prev <- c(100,120,125)
    res <- revalo_action(ptf_action, S, S_prev)

    # Tests
    expect_equal(res[, "rdt"], c(.02, -.042, -.028), tolerance = 0.02)
    expect_equal(res[, "div"], c(0, 0, 6901.172), tolerance = 0.02)
})

#----------------------------------------------------------------------------------
# calc_vm_action
#----------------------------------------------------------------------------------
test_that("TEST_calc_vm_action", {

    # Appel de la fonction
    S <- c(102,115,130)
    S_prev <- c(100,120,125)
    rdt <- revalo_action(ptf_action, S, S_prev)[, "rdt"]
    res <- calc_vm_action(ptf_action, rdt)

    # Test
    expect_equal(res, c(6925173.07, 2168831.43, 97196.26), tolerance = 0.01)
})

#----------------------------------------------------------------------------------
# calc_pmvl_action
#----------------------------------------------------------------------------------
test_that("TEST_calc_pmvl_action", {

    # Appel de la fonction
    res <- calc_pmvl_action(ptf_action)

    # Calcul des resultats
    val_marche <- ptf_action@ptf_action$val_marche
    val_nc <- ptf_action@ptf_action$val_nc
    pvl <- sum((val_marche > val_nc) * (val_marche - val_nc ))
    mvl <- sum((val_marche < val_nc) * (val_marche - val_nc ))

    # Test
    expect_equal(res$pvl, pvl)
    expect_equal(res$mvl, mvl)

})



#----------------------------------------------------------------------------------
# sell_action
#----------------------------------------------------------------------------------
test_that("TEST_sell_action", {

    ## 1 : Vente partielle uniquement
    # Appel de la fonction
    num_sold = 1:2
    nb_sold  = ptf_action@ptf_action$nb_unit[which(ptf_action@ptf_action$num_mp %in% num_sold)] / 2
    res <- sell_action(ptf_action, num_sold, nb_sold)

    # Calcul des resultats attendus
    pct_sell <- c(nb_sold, 0) / ptf_action@ptf_action$nb_unit

    # Test
    expect_equal(res$action@ptf_action$val_marche, ptf_action@ptf_action$val_marche * ( 1 - pct_sell))
    expect_equal(res$action@ptf_action$val_nc, ptf_action@ptf_action$val_nc * ( 1 - pct_sell))
    expect_equal(res$action@ptf_action$val_achat, ptf_action@ptf_action$val_achat * ( 1 - pct_sell))
    expect_equal(res$action@ptf_action$presence, ptf_action@ptf_action$presence)
    expect_equal(res$action@ptf_action$cessible, ptf_action@ptf_action$cessible)
    expect_equal(res$action@ptf_action$nb_unit, ptf_action@ptf_action$nb_unit * ( 1 - pct_sell))
    expect_equal(res$action@ptf_action$dur_det, ptf_action@ptf_action$dur_det)
    expect_equal(res$action@ptf_action$pdd, ptf_action@ptf_action$pdd)
    expect_equal(res$action@ptf_action$num_index, ptf_action@ptf_action$num_index)
    expect_equal(res$action@ptf_action$div, ptf_action@ptf_action$div)
    expect_equal(res$action@ptf_action$ind_invest, ptf_action@ptf_action$ind_invest)



    ## 2 : Vente totale uniquement
    # Appel de la fonction
    num_sold = 3
    nb_sold = ptf_action@ptf_action$nb_unit[which(ptf_action@ptf_action$num_mp == num_sold)]
    res <- sell_action(ptf_action, num_sold, nb_sold)

    # Test
    expect_equal(nrow(res$action@ptf_action), 2L)
    expect_equal(res$action@ptf_action$nb_unit, ptf_action@ptf_action$nb_unit[1:2])
    expect_equal(res$action@ptf_action$val_marche, ptf_action@ptf_action$val_marche[1:2])
    expect_equal(res$action@ptf_action$val_nc, ptf_action@ptf_action$val_nc[1:2])
    expect_equal(res$action@ptf_action$val_achat, ptf_action@ptf_action$val_achat[1:2])


    ## 3 : Vente partielle et totale simultanee
    # Appel de la fonction
    num_sold <- 1:3
    nb_sold_part  = ptf_action@ptf_action$nb_unit[which(ptf_action@ptf_action$num_mp %in% 1:2)] / 2
    nb_sold_tot = ptf_action@ptf_action$nb_unit[which(ptf_action@ptf_action$num_mp == 3)]
    nb_sold <- c(nb_sold_part, nb_sold_tot)
    res <- sell_action(ptf_action, num_sold, nb_sold)

    # Calcul des resultats attendus
    pct_sell <- nb_sold / ptf_action@ptf_action$nb_unit

    # Test
    expect_equal(nrow(res$action@ptf_action), 2L)
    expect_equal(res$action@ptf_action$val_marche, ptf_action@ptf_action$val_marche[1:2] * ( 1 - pct_sell[1:2]))
    expect_equal(res$action@ptf_action$val_nc, ptf_action@ptf_action$val_nc[1:2] * ( 1 - pct_sell[1:2]))
    expect_equal(res$action@ptf_action$val_achat, ptf_action@ptf_action$val_achat[1:2] * ( 1 - pct_sell[1:2]))
    expect_equal(res$action@ptf_action$nb_unit, ptf_action@ptf_action$nb_unit[1:2] * ( 1 - pct_sell[1:2]))


    ## 4.1 : Problemes de vente
    # Vente d'une action nom presente
    expect_error(sell_action(ptf_action, 17, 100))
    # Vente en trop grande quantite
    expect_error(sell_action(ptf_action, 1, 10000))
    # Idem en vente multiple
    expect_error(sell_action(ptf_action, c(1,17), c(100,100)))
    expect_error(sell_action(ptf_action, c(1,2), c(10,10000)))

})


#----------------------------------------------------------------------------------
# buy_action
#----------------------------------------------------------------------------------
test_that("TEST_buy_action", {

    ## 1 : Achat d'un PTF non vide
    # Appel de la fonction
    res <- buy_action(ptf_action, new("Action", ptf_action_csv))

    # Test
    expect_equal(nrow(res@ptf_action), 6L)


    ## 2 : Acht a partir d'un PTF vide
    # Appel de la focntion
    res <- buy_action(new("Action"), ptf_action)

    # Test
    expect_equal(res, ptf_action)


    # Achat d'un PTF vide
    expect_error(buy_action(ptf_action, new("Action")))

})



#----------------------------------------------------------------------------------
# sell_pvl_action
#----------------------------------------------------------------------------------
test_that("TEST_sell_pvl_action", {

    # Appel de la fonction
    montant <- 50000
    res <- sell_pvl_action(ptf_action, montant)

    # Test
    expect_equal(res$pmvr, montant)
    #### TESTS A FINALISER

    # Test avec erreur
    expect_error(sell_pvl_action(ptf_action, 1000000))
})


#----------------------------------------------------------------------------------
# update_vm_action
#----------------------------------------------------------------------------------
test_that("TEST_update_vm_action", {

    # Appel de la fonction
    vm <- c(10000,10000,10000)
    res <- update_vm_action(ptf_action, vm)

    # Test
    expect_equal(res@ptf_action$num_mp, ptf_action@ptf_action$num_mp)
    expect_equal(res@ptf_action$val_marche, vm)
    expect_equal(res@ptf_action$val_nc, ptf_action@ptf_action$val_nc)
    expect_equal(res@ptf_action$val_achat, ptf_action@ptf_action$val_achat)
    expect_equal(res@ptf_action$presence, ptf_action@ptf_action$presence)
    expect_equal(res@ptf_action$cessible, ptf_action@ptf_action$cessible)
    expect_equal(res@ptf_action$nb_unit, ptf_action@ptf_action$nb_unit)
    expect_equal(res@ptf_action$dur_det, ptf_action@ptf_action$dur_det)
    expect_equal(res@ptf_action$pdd, ptf_action@ptf_action$pdd)
    expect_equal(res@ptf_action$num_index, ptf_action@ptf_action$num_index)
    expect_equal(res@ptf_action$div, ptf_action@ptf_action$div)
    expect_equal(res@ptf_action$ind_invest, ptf_action@ptf_action$ind_invest)


    # Tests avec erreur
    expect_error(update_vm_action(ptf_action, c(10000,-10000,10000)))
    expect_error(update_vm_action(ptf_action, c(10000,10000)))
})



#----------------------------------------------------------------------------------
# update_dur_det_action
#----------------------------------------------------------------------------------
test_that("TEST_update_dur_det_action", {

    # Appel de la fonction
    res <- update_dur_det_action(ptf_action)

    # Test
    expect_equal(res@ptf_action$num_mp, ptf_action@ptf_action$num_mp)
    expect_equal(res@ptf_action$val_marche, ptf_action@ptf_action$val_marche)
    expect_equal(res@ptf_action$val_nc, ptf_action@ptf_action$val_nc)
    expect_equal(res@ptf_action$val_achat, ptf_action@ptf_action$val_achat)
    expect_equal(res@ptf_action$presence, ptf_action@ptf_action$presence)
    expect_equal(res@ptf_action$cessible, ptf_action@ptf_action$cessible)
    expect_equal(res@ptf_action$nb_unit, ptf_action@ptf_action$nb_unit)
    expect_equal(res@ptf_action$dur_det, ptf_action@ptf_action$dur_det + 1L)
    expect_equal(res@ptf_action$pdd, ptf_action@ptf_action$pdd)
    expect_equal(res@ptf_action$num_index, ptf_action@ptf_action$num_index)
    expect_equal(res@ptf_action$div, ptf_action@ptf_action$div)
    expect_equal(res@ptf_action$ind_invest, ptf_action@ptf_action$ind_invest)

    # Test avec warning : PTF vide
    expect_warning(update_dur_det_action(new("Action")))
})
