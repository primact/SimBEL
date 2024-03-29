# ##################
# Tests PPB
# ##################

context("PPB_et_revalorisation")

# Doosier de donnees
path <- paste0(getwd(), "/donnees_tests/input")

folder <- paste0(path, "/parametres/ppb/param_ppb.csv")

# Creation de l'objet
ppb_csv <- read.csv2(folder, header = TRUE)
ppb <- ppb_load(folder)


#--------------------------------------------------------------------------
# Classe
#--------------------------------------------------------------------------
test_that("TEST_Classe_PPB", {
    liste <- c(10, 5, 0, 1, 2, 5, 10, 2)
    ppb_test <- new("Ppb", liste, 1, 0.5, 0, 0) # Creation objet PPB
    expect_equal(object = ppb_test["hist_ppb"], expected = liste)
    expect_equal(object = ppb_test["valeur_ppb"], expected = sum(liste))
    expect_equal(object = ppb_test["ppb_debut"], expected = sum(liste))
    expect_equal(object = ppb_test["seuil_rep"], expected = 1)
    expect_equal(object = ppb_test["seuil_dot"], expected = 0.5)
    expect_equal(object = ppb_test["compte_rep"], expected = 0)
    expect_equal(object = ppb_test["compte_dot"], expected = 0)

    # Setters
    liste <- c(10, 5, 0, 1, 2, 5, 10, 20)
    ppb_test["hist_ppb"] <- liste
    expect_equal(object = ppb_test["hist_ppb"], expected = liste)
    expect_equal(object = ppb_test["valeur_ppb"], expected = sum(liste))
})



#--------------------------------------------------------------------------
# ppb_load
#--------------------------------------------------------------------------
test_that("TEST_ppb_load", {
    # Tests
    expect_equal(object = ppb["hist_ppb"], expected = ppb_csv[, "hist_ppb"])
    expect_equal(object = ppb["valeur_ppb"], expected = sum(ppb_csv[, "hist_ppb"]))
    expect_equal(object = ppb["ppb_debut"], expected = sum(ppb_csv[, "hist_ppb"]))
    expect_equal(object = ppb["seuil_rep"], expected = ppb_csv[1, "seuil_rep"])
    expect_equal(object = ppb["seuil_dot"], expected = ppb_csv[1, "seuil_dot"])
    expect_equal(object = ppb["compte_rep"], expected = 0)
    expect_equal(object = ppb["compte_dot"], expected = 0)
})


#--------------------------------------------------------------------------
# calc_dotation_ppb
#--------------------------------------------------------------------------
test_that("calc_dotation_ppb", {
    liste <- c(5, 0, 0, 0, 0, 0, 0, 0)
    ppb_test <- new("Ppb", liste, 0.2, 0.5, 0, 0) # Creation objet PPB

    # Tests de dotations
    expect_equal(object = calc_dotation_ppb(ppb_test, 0.5)$dotation, expected = 0.5)
    expect_equal(object = calc_dotation_ppb(ppb_test, 0.5)$ppb@valeur_ppb, expected = ppb_test@valeur_ppb + 0.5)
    expect_equal(object = calc_dotation_ppb(ppb_test, 1.5)$dotation, expected = 1.5)
    expect_equal(object = calc_dotation_ppb(ppb_test, 1.5)$ppb@valeur_ppb, expected = ppb_test@valeur_ppb + 1.5)
    expect_equal(object = calc_dotation_ppb(ppb_test, 3)$dotation, expected = 2.5)
    expect_equal(object = calc_dotation_ppb(ppb_test, 3)$ppb@valeur_ppb, expected = ppb_test@valeur_ppb + 2.5)
})



#--------------------------------------------------------------------------
# calc_reprise_ppb
#--------------------------------------------------------------------------
test_that("TEST_calc_reprise_ppb", {
    # Modification de l'objet
    test <- c(200, 100, 50, 40, 40, 30, 20, 10)
    ppb@hist_ppb <- test
    ppb@valeur_ppb <- sum(test)
    ppb@ppb_debut <- sum(test)
    ppb@seuil_rep <- 1

    # Test pour somme des ppb > reprise
    res <- calc_reprise_ppb(ppb, 100) # Appel de la fonction

    # Tests
    expect_equal(object = res$ppb@hist_ppb, expected = c(200, 100, 50, 40, 0, 0, 0, 0))
    expect_equal(object = res$ppb@valeur_ppb, expected = sum(test) - 100)
    expect_equal(object = res$ppb@ppb_debut, expected = sum(test))
    expect_equal(object = res$ppb@compte_rep, expected = 100)
    expect_equal(object = res$reprise, expected = 100)

    ## Test pour somme des ppb < reprise

    # Modifications pour tester la fonction
    test <- c(20, 10, 30, 0, 0, 5, 1, 3) # Vecteur valeur ppb
    # Modification de l'objet pour tester
    ppb@hist_ppb <- test
    ppb@valeur_ppb <- sum(test)
    ppb@ppb_debut <- sum(test)

    res <- calc_reprise_ppb(ppb, 100) # Appel de la fonction

    # Tests
    expect_equal(object = res$ppb@hist_ppb, expected = rep(0, 8))
    expect_equal(object = res$ppb@valeur_ppb, expected = 0)
    expect_equal(object = res$ppb@ppb_debut, expected = sum(test))
    expect_equal(object = res$ppb@compte_rep, expected = sum(test))
    expect_equal(object = res$reprise, expected = sum(test))

    ppb_test2 <- ppb
    ppb_test2@seuil_rep <- 0.5
    res <- calc_reprise_ppb(ppb_test2, 100) # Appel de la fonction

    # Tests
    expect_equal(object = res$ppb@hist_ppb, expected = c(20, 10, 4.5, 0, 0, 0, 0, 0))
    expect_equal(object = res$ppb@valeur_ppb, expected = res$ppb@ppb_debut * 0.5)
    expect_equal(object = res$ppb@ppb_debut, expected = sum(test))
    expect_equal(object = res$ppb@compte_rep, expected = res$ppb@ppb_debut * 0.5)
    expect_equal(object = res$reprise, expected = res$ppb@ppb_debut * 0.5)
})


#--------------------------------------------------------------------------
# ppb_8ans
#--------------------------------------------------------------------------
test_that("TEST_ppb_8ans", {
    # Modification de l'objet
    # Modifications pour tester la fonction
    test <- c(20, 10, 30, 0, 0, 5, 1, 3) # Vecteur valeur ppb
    # Modification de l'objet pour tester
    ppb@hist_ppb <- test
    ppb@valeur_ppb <- sum(test)
    ppb@ppb_debut <- sum(test)
    ppb@compte_rep <- ppb@compte_rep + 30

    # Appel de la fonction
    res <- ppb_8ans(x = ppb)

    # Tests
    expect_equal(object = res$ppb_8, expected = test[8])
    expect_equal(object = res$ppb@ppb_debut, expected = sum(test))
    expect_equal(object = res$ppb@hist_ppb, expected = c(test[1:7], 0))
    expect_equal(object = res$ppb@valeur_ppb, expected = sum(test[1:7]))
    expect_equal(object = res$ppb@compte_rep, expected = test[8] + 30)
})


#--------------------------------------------------------------------------
# vieillissement_ppb
#--------------------------------------------------------------------------
test_that("TEST_vieillissement_ppb", {
    # Modification de l'objet
    test <- ppb
    test@compte_rep <- 100
    test@compte_dot <- 50
    test@hist_ppb[8] <- 15
    test@valeur_ppb <- sum(test@hist_ppb)
    test@ppb_debut <- sum(test@hist_ppb)

    # Appel de la fonction
    res <- vieillissement_ppb(x = test) # Appel de la fonction

    # Tests
    new <- c(50, ppb_csv[1:7, "hist_ppb"])
    expect_equal(object = res@hist_ppb, expected = new)
    expect_equal(object = res@valeur_ppb, expected = sum(new))
    expect_equal(object = res@ppb_debut, expected = sum(new))
    expect_equal(object = res@compte_rep, expected = 0)
    expect_equal(object = res@compte_dot, expected = 0)
    expect_equal(object = res@seuil_rep, expected = ppb_csv[1, "seuil_rep"])
    expect_equal(object = res@seuil_dot, expected = ppb_csv[1, "seuil_dot"])
})



#--------------------------------------------------------------------------
# RevaloEngine - base_fin
#--------------------------------------------------------------------------
tra_1 <- 0.05
tra_2 <- 0.02
tra_3 <- 0.01
tra_4 <- -0.01

pm_moy <- rep(100, 4)
tx_pb <- c(0.90, 0.95, 0.97, 1)

liste <- c(5, 0, 0, 0, 0, 0, 0, 0)
ppb_test <- new("Ppb", liste, 0.2, 0.5, 0, 0) # Creation objet PPB

tx_enc_moy <- c(0, 0.005, 0.01, 0.015)

test_that("TEST_base_fin", {
    base_fin <- tra_1 * (sum(pm_moy) + ppb["ppb_debut"]) * pm_moy / sum(pm_moy)

    # Test 1
    expect_equal(object = base_prod_fin(tra_1, pm_moy, ppb)[["base_prod_fin"]], expected = base_fin)
    expect_equal(object = base_prod_fin(tra_1, pm_moy, ppb)[["base_prod_fin_port"]], expected = sum(base_fin))

    # Test 2 avec un produit
    expect_equal(object = base_prod_fin(tra_1, pm_moy[1], ppb)[["base_prod_fin"]], expected = tra_1 * (pm_moy[1] + ppb["ppb_debut"]))
    expect_equal(object = base_prod_fin(tra_1, pm_moy[1], ppb)[["base_prod_fin_port"]], expected = tra_1 * (pm_moy[1] + ppb["ppb_debut"]))
})





#--------------------------------------------------------------------------
# finance_cible_ppb
#--------------------------------------------------------------------------
test_that("TEST_finance_cible_ppb", {
    # Donnees necessaires aux tests
    ppb@compte_rep <- 100
    ppb@compte_dot <- 50

    res_ppb_8 <- ppb_8ans(ppb)
    ppb <- res_ppb_8[["ppb"]]


    # PPB8 > TxCible & rev = 0
    ppb_8 <- c(80, 40)
    res <- finance_cible_ppb(c(50, 30), c(0, 0), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = sum(res$rev_stock_nette), expected = sum(ppb_8))
    expect_equal(object = res$rev_stock_nette, expected = ppb_8)
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 0)

    # PPB8 > TxCible & rev !=0
    ppb_8 <- c(80, 40)
    res <- finance_cible_ppb(c(50, 30), c(20, 10), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 30)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 30)
    expect_equal(object = res$rev_stock_nette, expected = ppb_8)
    expect_equal(object = res$dotation, expected = 30)
    expect_equal(object = res$reprise, expected = 0)

    # Taux cible faible
    ppb_8 <- c(20, 40)
    res <- finance_cible_ppb(c(10, 10), c(50, 50), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 100)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 100)
    expect_equal(object = res$rev_stock_nette, expected = ppb_8)
    expect_equal(object = res$dotation, expected = 100)
    expect_equal(object = res$reprise, expected = 0)

    # PPB8 nulle
    ppb_8 <- c(0, 0)
    res <- finance_cible_ppb(c(20, 60), c(80, 100), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 100)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 100)
    expect_equal(object = res$rev_stock_nette, expected = c(20, 60))
    expect_equal(object = res$dotation, expected = 100)
    expect_equal(object = res$reprise, expected = 0)

    # Test 5
    ppb_8 <- c(10, 40)
    res <- finance_cible_ppb(c(20, 60), c(0, 50), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 20)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 20)
    expect_equal(object = res$rev_stock_nette, expected = c(20, 60))
    expect_equal(object = res$dotation, expected = 20)
    expect_equal(object = res$reprise, expected = 0)

    # Test 6
    ppb_8 <- c(10, 40)
    res <- finance_cible_ppb(c(20, 60), c(0, 20), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = calc_reprise_ppb(ppb, 10)$ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb - 10)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep + 10)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = res$rev_stock_nette, expected = c(20, 60))
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 10)


    # Test 7
    ppb_8 <- c(20, 40)
    res <- finance_cible_ppb(c(30, 50), c(50, 20), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 50)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 50)
    expect_equal(object = res$rev_stock_nette, expected = c(30, 50))
    expect_equal(object = res$dotation, expected = 50)
    expect_equal(object = res$reprise, expected = 0)


    # PPB8 < et > TxCible
    ppb_8 <- c(80, 20)
    res <- finance_cible_ppb(c(50, 30), c(20, 10), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 20)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 20)
    expect_equal(object = res$rev_stock_nette, expected = c(80, 30))
    expect_equal(object = res$dotation, expected = 20)
    expect_equal(object = res$reprise, expected = 0)

    # PPB8 < et > TxCible
    ppb_8 <- c(80, 20)
    res <- finance_cible_ppb(c(50, 30), c(20, 10), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 20)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 20)
    expect_equal(object = res$rev_stock_nette, expected = c(80, 30))
    expect_equal(object = res$dotation, expected = 20)
    expect_equal(object = res$reprise, expected = 0)

    # PPB8 nulle & revenu < TxCible
    ppb_8 <- c(0, 0)
    res <- finance_cible_ppb(c(20, 60), c(10, 50), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = calc_reprise_ppb(ppb, 20)$ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb - 20)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep + 20)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = res$rev_stock_nette, expected = c(20, 60))
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 20)

    # Tests
    ppb_8 <- c(50, 50)
    res <- finance_cible_ppb(c(40, 40), c(10, 10), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 20)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 20)
    expect_equal(object = res$rev_stock_nette, expected = c(50, 50))
    expect_equal(object = res$dotation, expected = 20)
    expect_equal(object = res$reprise, expected = 0)


    # PPB8 + rev < TxCible
    ppb_8 <- c(10, 40)
    res <- finance_cible_ppb(c(30, 70), c(10, 20), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = calc_reprise_ppb(ppb, 20)$ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb - 20)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep + 20)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = res$rev_stock_nette, expected = c(30, 70))
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 20)


    # Test
    ppb_8 <- c(0, 10)
    res <- finance_cible_ppb(c(30, 70), c(10, 20), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = calc_reprise_ppb(ppb, 60)$ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb - 60)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep + 60)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = res$rev_stock_nette, expected = c(30, 70))
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 60)


    # Test : cas problematique
    ppb_8 <- c(10, 80)
    res <- finance_cible_ppb(c(30, 70), c(10, 20), ppb, ppb_8) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb + 10)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot + 10)
    expect_equal(object = res$rev_stock_nette, expected = c(30, 80))
    expect_equal(object = res$dotation, expected = 10)
    expect_equal(object = res$reprise, expected = 0)

    ## Dotation limitee
    ppb@seuil_dot <- 0

    # Test
    ppb8_ind <- c(80, 40)
    res <- finance_cible_ppb(bes_cible = c(50, 30), rev_stock_nette = c(100, 50), ppb, ppb8_ind) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = res$rev_stock_nette, expected = c(180, 90))
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 0)

    # Test
    ppb8_ind <- c(80, 0)
    res <- finance_cible_ppb(bes_cible = c(50, 30), rev_stock_nette = c(100, 10), ppb, ppb8_ind) # Appel de la fonction
    expect_equal(object = res$ppb@hist_ppb, expected = ppb@hist_ppb)
    expect_equal(object = res$ppb@valeur_ppb, expected = ppb@valeur_ppb)
    expect_equal(object = res$ppb@ppb_debut, expected = ppb@ppb_debut)
    expect_equal(object = res$ppb@compte_rep, expected = ppb@compte_rep)
    expect_equal(object = res$ppb@compte_dot, expected = ppb@compte_dot)
    expect_equal(object = res$rev_stock_nette, expected = c(160, 30))
    expect_equal(object = res$dotation, expected = 0)
    expect_equal(object = res$reprise, expected = 0)
})
