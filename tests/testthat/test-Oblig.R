# ##################
# Tests Oblig
# ##################

context("Oblig")

library(rootSolve)

# Lecture du fichier csv
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/actif"
ptf_oblig_csv <- read.csv2(paste(path, "Portefeuille_obligation.csv", sep = "/"), header = TRUE,
                            colClasses = c("integer", "double",  "double", "double", "logical", "logical",
                                           "double",  "double",  "double", "double", "double", "double",
                                           "factor",  "integer", "double", "double", "double", "double"))

# Creation de l'objet
ptf_oblig <- new("Oblig", ptf_oblig_csv)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Classe Oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("TEST_class", {

    # Teste la classe
    expect_s4_class(ptf_oblig, "Oblig")

    # Teste les attributs
    expect_equal(ptf_oblig@ptf_oblig, ptf_oblig_csv)
})

#--------------------------------------------------
# calc_coupon
#--------------------------------------------------
test_that("TEST_calc_coupon", {

    # Appel de la fonction
    res <- calc_coupon(ptf_oblig)

    # Resultats attendus
    cc <- ptf_oblig@ptf_oblig$cc

    # Test
    expect_equal(res, cc, tolerance = 0.01)
})


#--------------------------------------------------
# calc_nominal
#--------------------------------------------------
test_that("TEST_calc_nominal", {

    # Appel de la fonction
    res <- calc_nominal(ptf_oblig)

    # Resultat attendu
    nom <- ptf_oblig@ptf_oblig$nominal * ptf_oblig@ptf_oblig$nb_unit * ptf_oblig@ptf_oblig$par

    # Test
    expect_equal(res, nom, tolerance = .01)
})


#--------------------------------------------------
# calc_flux_annee
#--------------------------------------------------
test_that("TEST_calc_flux_annee", {

    # Appel de la fonction
    res <- calc_flux_annee(ptf_oblig)

    # Test
    expect_equal(res$tombee_coupon, calc_coupon(ptf_oblig))
    expect_equal(res$tombee_echeance, calc_nominal(ptf_oblig) * (ptf_oblig@ptf_oblig$mat_res<=1))
})


#--------------------------------------------------
# echeancier
#--------------------------------------------------
test_that("TEST_echeancier", {

    # Appel de la fonction
    coupon <- calc_coupon(ptf_oblig)
    maturite <- ptf_oblig@ptf_oblig$mat_res
    zspread <- ptf_oblig@ptf_oblig$zspread
    yield_curve <- c(1:150)/150
    nominal <- calc_nominal(ptf_oblig)
    res <- echeancier(coupon,maturite,zspread,nominal,yield_curve)

    # Test
    expect_equal(ncol(res), max(maturite))
    expect_equal(nrow(res), nrow(ptf_oblig@ptf_oblig))
})


#--------------------------------------------------
# calc_vm
#--------------------------------------------------
test_that("TEST_calc_vm_oblig", {

    # Donnees
    coupon <- calc_coupon(ptf_oblig)
    maturite <- ptf_oblig@ptf_oblig$mat_res
    zspread <- ptf_oblig@ptf_oblig$zspread
    nominal <- calc_nominal(ptf_oblig)


    ## 1 - Sans courbe de taux
    # Appel de la fonction
    yield_curve <- numeric()
    res <- calc_vm_oblig(ptf_oblig,yield_curve)

    # Test
    expect_equal(res, rowSums(echeancier(coupon, maturite, zspread, nominal, yield_curve)))


    ## 2 - Avec courbe de taux
    # Appel de la fonction
    yield_curve <- (1:150)/150
    res <- calc_vm_oblig(ptf_oblig,yield_curve)

    # Test
    expect_equal(res, rowSums(echeancier(coupon, maturite, zspread, nominal, yield_curve)))


    # Test avec erreur
    expect_error(calc_vm_oblig(new("Oblig"), yield_curve))
})


#--------------------------------------------------
# calc_pmvl_oblig
#--------------------------------------------------
test_that("TEST_calc_pmvl_oblig", {

    # Appel de la fonction
    res <- calc_pmvl_oblig(ptf_oblig)

    # Resultats attendus
    val_marche <- ptf_oblig@ptf_oblig$val_marche
    val_nc <- ptf_oblig@ptf_oblig$val_nc
    pvl <- sum((val_marche > val_nc) * (val_marche - val_nc))
    mvl <- sum((val_marche < val_nc) * (val_marche - val_nc))

    # Test
    expect_equal(res$pvl, pvl)
    expect_equal(res$mvl, mvl)
})


#--------------------------------------------------
# calc_z_spread
#--------------------------------------------------
test_that("TEST_calc_zspread", {

    # Appel de la fonction
    yield_curve <- (1:150)/150
    res <-  calc_z_spread(ptf_oblig, yield_curve)

    # Test
    ### TEST A FAIRE


    # Test a faire
    expect_error(calc_z_spread(ptf_oblig, numeric()))
})


#--------------------------------------------------
# yield_to_maturity
#--------------------------------------------------
test_that("TEST_yield_to_maturity", {

    # Appel de la fonction
    res <- yield_to_maturity(ptf_oblig)

    # Test
    expect_length(res, nrow(ptf_oblig@ptf_oblig))
    # TEST A FINALISER

    # Test avec erreur (PTF vide)
    expect_error(yield_to_maturity(new("Oblig")))
})




#--------------------------------------------------
# update_mat_res
#--------------------------------------------------
test_that("TEST_update_mat_res", {

    # Appel de la fonction
    res <- update_mat_res(ptf_oblig)

    # Test
    expect_equal(res@ptf_oblig$mat_res, ptf_oblig@ptf_oblig$mat_res[3:4] - 1)

    # Vieillissement de 2 ans
    res <- update_mat_res(res)

    # Test
    expect_equal(nrow(res@ptf_oblig), length(which(ptf_oblig@ptf_oblig$mat_res > 2)))
})


#--------------------------------------------------
# 10 : buy
#--------------------------------------------------
test_that("TEST_buy_oblig", {

    ## 1 - Achat d'un PTF non vide
    # Appel de la fonction
    res <- buy_oblig(ptf_oblig, ptf_oblig)

    # Test
    expect_equal(nrow(res@ptf_oblig), 2 * nrow(ptf_oblig@ptf_oblig))


    ## 2 - Achat d'un PTF vide
    # Appel de la fonction
    res <- buy_oblig(new("Oblig"), ptf_oblig)

    # Test
    expect_equal(res@ptf_oblig[1:17], ptf_oblig@ptf_oblig[1:17])
    expect_equal(res@ptf_oblig[,"sd"], calc_sur_dec(ptf_oblig))


    ## Test avec erreur
    expect_error(buy_oblig(ptf_oblig, new("Oblig")))
})

#--------------------------------------------------
# sell_oblig
#--------------------------------------------------
test_that("TEST_sell_oblig", {

    ## 1 - Vente totale d'une ligne
    # Appel de la fonction
    num_sold = 1
    nb_sold = ptf_oblig@ptf_oblig$nb_unit[which(ptf_oblig@ptf_oblig$num_mp == num_sold)]
    res <- sell_oblig(ptf_oblig, num_sold, nb_sold)

    # Test
    expect_equal(res$oblig@ptf_oblig, ptf_oblig@ptf_oblig[2:4,])


    ## 2 - Vente partielle d'une ligne
    # Appel de la fonction
    num_sold = 1
    nb_sold = 500
    res <- sell_oblig(ptf_oblig,num_sold, nb_sold)

    # Resultats attendus
    pct_sold <- c(500,0,0,0) / ptf_oblig@ptf_oblig$nb_unit

    # Tests
    expect_equal(res$oblig@ptf_oblig$num_mp, ptf_oblig@ptf_oblig$num_mp)
    expect_equal(res$oblig@ptf_oblig$val_marche, ptf_oblig@ptf_oblig$val_marche * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$val_nc, ptf_oblig@ptf_oblig$val_nc * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$val_achat, ptf_oblig@ptf_oblig$val_achat * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$presence, ptf_oblig@ptf_oblig$presence)
    expect_equal(res$oblig@ptf_oblig$cessible, ptf_oblig@ptf_oblig$cessible)
    expect_equal(res$oblig@ptf_oblig$nb_unit, ptf_oblig@ptf_oblig$nb_unit * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$dur_det, ptf_oblig@ptf_oblig$dur_det)
    expect_equal(res$oblig@ptf_oblig$nominal, ptf_oblig@ptf_oblig$nominal)
    expect_equal(res$oblig@ptf_oblig$tx_coupon, ptf_oblig@ptf_oblig$tx_coupon)
    expect_equal(res$oblig@ptf_oblig$par, ptf_oblig@ptf_oblig$par)
    expect_equal(res$oblig@ptf_oblig$mat_res, ptf_oblig@ptf_oblig$mat_res)
    expect_equal(res$oblig@ptf_oblig$type, ptf_oblig@ptf_oblig$type)
    expect_equal(res$oblig@ptf_oblig$rating, ptf_oblig@ptf_oblig$rating)
    expect_equal(res$oblig@ptf_oblig$duration, ptf_oblig@ptf_oblig$duration)
    expect_equal(res$oblig@ptf_oblig$zspread, ptf_oblig@ptf_oblig$zspread)
    expect_equal(res$oblig@ptf_oblig$cc, ptf_oblig@ptf_oblig$cc)
    expect_equal(res$oblig@ptf_oblig$sd, ptf_oblig@ptf_oblig$sd)


    ## 3 - Vente partielle et totale
    num_sold = c(1,2)
    nb_sold_1 = ptf_oblig@ptf_oblig$nb_unit[which(ptf_oblig@ptf_oblig$num_mp == 1)]
    nb_sold = c(nb_sold_1, 100)
    res <- sell_oblig(ptf_oblig,num_sold, nb_sold)

    # Resultats attendus
    pct_sold <- c(100,0,0) / ptf_oblig@ptf_oblig$nb_unit[2:4]

    # Tests
    expect_equal(nrow(res$oblig@ptf_oblig), 3L)
    expect_equal(res$oblig@ptf_oblig$val_marche, ptf_oblig@ptf_oblig$val_marche[2:4] * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$val_nc, ptf_oblig@ptf_oblig$val_nc[2:4] * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$val_achat, ptf_oblig@ptf_oblig$val_achat[2:4] * (1 - pct_sold))
    expect_equal(res$oblig@ptf_oblig$nb_unit, ptf_oblig@ptf_oblig$nb_unit[2:4] * (1 - pct_sold))


})



#----------------------------------------------------------------------------------
# update_vm_oblig
#----------------------------------------------------------------------------------
test_that("TEST_update_vm_oblig", {

    # Appel de la fonction
    vm <- c(10,10,10,10)
    res <- update_vm_oblig(ptf_oblig, vm)

    # Test
    expect_equal(nrow(res@ptf_oblig), 4L)
    expect_equal(res@ptf_oblig$val_marche, vm)

    # Tests avec erreur
    expect_error(update_vm_oblig(ptf_oblig, c(10,10,10,10,10)))
    expect_error(update_vm_oblig(ptf_oblig, c(10,10,10,-10)))
})



#----------------------------------------------------------------------------------
# update_vnc_oblig
#----------------------------------------------------------------------------------
test_that("TEST_update_vnc_oblig", {

    # Appel de la fonction
    vnc <- c(10,10,10,10)
    res <- update_vnc_oblig(ptf_oblig, vnc)

    # Test
    expect_equal(nrow(res@ptf_oblig), 4L)
    expect_equal(res@ptf_oblig$val_nc, vnc)

    # Tests avec erreur
    expect_error(update_vnc_oblig(ptf_oblig, c(10,10,10,10,10)))
    expect_error(update_vnc_oblig(ptf_oblig, c(10,10,10,-10)))
})




#----------------------------------------------------------------------------------
# update_sd_oblig
#----------------------------------------------------------------------------------
test_that("TEST_update_sd_oblig", {

    # Appel de la fonction
    sd <- c(10,10,10,10)
    res <- update_sd_oblig(ptf_oblig, sd)

    # Test
    expect_equal(nrow(res@ptf_oblig), 4L)
    expect_equal(res@ptf_oblig$sd, sd)

    # Tests avec erreur
    expect_error(update_sd_oblig(ptf_oblig, c(10,10,10,10,10)))
})



#----------------------------------------------------------------------------------
# update_dur_oblig
#----------------------------------------------------------------------------------
test_that("TEST_update_dur_oblig", {

    # Appel de la fonction
    dur <- c(10,10,10,10)
    res <- update_dur_oblig(ptf_oblig, dur)

    # Test
    expect_equal(nrow(res@ptf_oblig), 4L)
    expect_equal(res@ptf_oblig$duration, dur)

    # Tests avec erreur
    expect_error(update_dur_oblig(ptf_oblig, c(10,10,10,10,10)))
    expect_error(update_dur_oblig(ptf_oblig, c(10,10,10,-10)))
})



#----------------------------------------------------------------------------------
# update_zsp_oblig
#----------------------------------------------------------------------------------
test_that("TEST_update_zsp_oblig", {

    # Appel de la fonction
    zsp <- c(10,10,10,10)
    res <- update_zsp_oblig(ptf_oblig, zsp)

    # Test
    expect_equal(nrow(res@ptf_oblig), 4L)
    expect_equal(res@ptf_oblig$zspread, zsp)

    # Tests avec erreur
    expect_error(update_zsp_oblig(ptf_oblig, c(10,10,10,10,10)))
})



#----------------------------------------------------------------------------------
# update_cc_oblig
#----------------------------------------------------------------------------------
test_that("TEST_update_cc_oblig", {

    # Appel de la fonction
    cc <- c(10,10,10,10)
    res <- update_cc_oblig(ptf_oblig, cc)

    # Test
    expect_equal(nrow(res@ptf_oblig), 4L)
    expect_equal(res@ptf_oblig$cc, cc)

    # Tests avec erreur
    expect_error(update_cc_oblig(ptf_oblig, c(10,10,10,10,10)))
})



#----------------------------------------------------------------------------------
# calc_sur_dec
#----------------------------------------------------------------------------------
test_that("TEST_calc_sur_dec", {

    # Appel de la fonction
    res <- calc_sur_dec(ptf_oblig)

    # Resultats attendus
    nb_unit <- ptf_oblig@ptf_oblig$nb_unit
    nominal <- calc_nominal(ptf_oblig) / nb_unit
    vnc     <- ptf_oblig@ptf_oblig$val_nc / nb_unit
    res_att <- (nominal - vnc) / ptf_oblig@ptf_oblig$mat_res

    # Test
    expect_equal(res, res_att, tolerance = 0.01)
})
