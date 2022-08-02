# --------------------------------------------------------------------------------------------------------------------------
# lapse mass
# --------------------------------------------------------------------------------------------------------------------------
context("Lapse mass")

# Initialisation de l'objet BE
path <- paste0(getwd(), "/donnees_tests")
racine <- new("Initialisation", root_address = path)
racine <- set_architecture(racine)
init_SimBEL(racine)
init_scenario(racine)

central <- get(load(paste(racine@address$save_folder$central, "best_estimate.RData", sep = "/")))
lapse_mass <- get(load(paste(racine@address$save_folder$rachat_mass, "best_estimate.RData", sep = "/")))

#---------------------------------------------------------------
# Controle de l'import
#---------------------------------------------------------------
test_that("TEST_import_choc_rachat_mass", {
  expect_equal(lapse_mass@canton@ptf_passif@choc_lapse_mass, 0.4)
  expect_equal(central@canton@ptf_passif@choc_lapse_mass, 0)
})
#---------------------------------------------------------------
# Controle de classe Portpassif
#---------------------------------------------------------------
test_that("TEST_portpassif", {
ptf_passif <- new("PortPassif")

expect_s4_class(ptf_passif, "PortPassif")
expect_equal(ptf_passif@choc_lapse_mass, 0)
expect_equal(ptf_passif@choc_mort_cat, 0)
})


#---------------------------------------------------------------
# Controle de calcul du choc de rachat passifs sur une annee
#---------------------------------------------------------------
test_that("TEST_calc_prest_rach_massif", {

produit <- central@canton@ptf_passif@eei$epeuro1
an <- 1L
ht <- central@canton@ptf_passif@ht

  # Calcul du taux minimum
  tx_min <-  calc_tx_min(produit, an)

  # Calcul des probabilites dynamiques
  proba_dyn <- calc_proba_dyn(produit, ht = ht)

  prest_ref1 <- calc_prest(produit, method = "normal", an = 1L,
                           y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = 0.15, choc_lapse_mass = 0))
  prest_ref2 <- calc_prest(produit, method = "normal", an = 2L,
                           y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = 0.15, choc_lapse_mass = 0))
  prest_1 <- calc_prest(produit, method = "normal", an = 1L,
                        y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = 0.15, choc_lapse_mass = 0.4))

  prest_2 <- calc_prest(produit, method = "normal", an = 2L,
                        y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = 0.15, choc_lapse_mass = 0.4))

  # Test des rachats massifs
  expect_identical(prest_ref2, prest_2) # Pas de rachat massif au dela de t =1
  expect_equal(prest_ref1$flux$rach_mass, rep(0,2)) # Pas de rachat massif si choc lapse = 0
  expect_equal(prest_1$flux$rach_mass, produit@mp$pm * 0.4) # Choc de 0.40
  expect_equal(prest_1$flux$rach_tot, prest_ref1$flux$rach_tot * 0.6) # Choc de 0.40 donc 0.6 restat
  expect_equal(prest_1$flux$dc, prest_ref1$flux$dc * 0.6) # Choc de 0.40 donc 0.6 restat
  expect_equal(prest_1$flux$rach_part, prest_ref1$flux$rach_part * 0.6) # Choc de 0.40 donc 0.6 restat
  expect_equal(prest_1$flux$prest, (prest_ref1$flux$prest)*0.6 + produit@mp$pm * 0.4) # Choc de 0.40
  expect_equal(prest_1$flux$rev_prest, (prest_ref1$flux$rev_prest)*0.6) # pas de revalo des rachats massifs
  expect_equal(prest_1$flux$it_tech_prest, (prest_ref1$flux$it_tech_prest)*0.6) # pas de revalo des rachats massifs
  expect_equal(prest_1$stock$nb_rach_mass, rep(0.4,2))
  expect_equal(prest_1$stock$nb_dc, (prest_ref1$stock$nb_dc)*0.6)
  expect_equal(prest_1$stock$nb_contr_fin, (prest_ref1$stock$nb_contr_fin)*0.6)
})


#---------------------------------------------------------------
# Controle de recueuil du choc de rachat passifs sur une annee
#---------------------------------------------------------------
test_that("TEST_vieillissement_av_pb_rach_massif", {


  # Pas de choc
  ptf1 <- central@canton@ptf_passif
  passif_viell1 <- viellissement_av_pb(an = 1L, ptf1, 0, c(0,0,0,0), 0.15)

  expect_equal(passif_viell1$result_av_pb$flux_agg[, "rach_mass"], rep(0,4))
  expect_equal(passif_viell1$result_av_pb$flux_agg[, "rach_charg_mass"], rep(0,4))
  expect_equal(passif_viell1$result_av_pb$stock_agg[, "nb_rach_mass"], rep(0,4))
  expect_equal(passif_viell1$flux_debut, 0)

  # tentative de choc en annee 2
  ptf2 <- ptf1
  ptf2@choc_lapse_mass <- 0.4
  passif_viell2 <- viellissement_av_pb(an = 2L, ptf2, 0, c(0,0,0,0), 0.15)

  expect_equal(passif_viell2$result_av_pb$flux_agg[, "rach_mass"], rep(0,4))
  expect_equal(passif_viell2$result_av_pb$flux_agg[, "rach_charg_mass"], rep(0,4))
  expect_equal(passif_viell2$result_av_pb$stock_agg[, "nb_rach_mass"], rep(0,4))
  expect_equal(passif_viell2$flux_debut, 0)

  # Choc en t=1
  ptf3 <- ptf1
  ptf3@choc_lapse_mass <- 0.4
  passif_viell3 <- viellissement_av_pb(an = 1L, ptf3, 0, c(0,0,0,0), 0.15)

  expect_equivalent(passif_viell3$result_av_pb$flux_agg[1, "rach_mass"], sum(ptf1@eei$epeuro1@mp$pm * 0.4))
  expect_equivalent(passif_viell3$result_av_pb$flux_agg[3, "rach_mass"], 0) # Retraite
  expect_equal(passif_viell3$result_av_pb$flux_agg[1, "rach_tot"], passif_viell1$result_av_pb$flux_agg[1, "rach_tot"] * 0.6)
  expect_equal(passif_viell3$result_av_pb$flux_agg[1, "dc"], passif_viell1$result_av_pb$flux_agg[1, "dc"] * 0.6)
  expect_equivalent(passif_viell3$result_av_pb$stock[1, "nb_rach_mass"], 0.4 * 2)
  expect_equivalent(passif_viell3$result_av_pb$stock[1, "pm_fin"], passif_viell1$result_av_pb$stock[1, "pm_fin"] +
                      passif_viell1$result_av_pb$flux_agg[1, "prest"] * 0.4 -
                      passif_viell1$result_av_pb$stock[1, "pm_deb"] * 0.4)

  expect_equal(passif_viell3$flux_debut, - sum(ptf1@eei$epeuro1@mp$pm * 0.4) * 2)
  expect_equivalent(passif_viell3$flux_milieu, passif_viell1$flux_milieu + passif_viell1$result_av_pb$flux_agg[1, "prest"] * 0.4 *2)

})

#---------------------------------------------------------------
# Controle du proj_an du choc de rachat passifs sur une annee
#---------------------------------------------------------------
test_that("TEST_proj_an", {

  canton1 <- central@canton
  test <- proj_an(canton1 , 10L, pre_on = F)

  # Pas de choc apres la premiere annee
  canton2 <- central@canton
  canton2@annee <- 1L
  test2 <- proj_an(canton2 , 10L, pre_on = F)
  canton2@ptf_passif@choc_lapse_mass <- 0.4
  test3 <- proj_an(canton2 , 10L, pre_on = F)
  # Retrait du choc pour pouvoir utiliser une comparaison globale
  test3$canton@ptf_passif@choc_lapse_mass <- 0

  expect_equal(test2, test3)

  # Application du choc
  #---------------------------------------------------------------
  # Etape 1 : Mise a jour des annees de projection
  #---------------------------------------------------------------
  x <- canton1
  x@annee <- x@annee + 1L

  annee <-  x@annee

  #---------------------------------------------------------------
  # Etape 2 : variables economiques utilisees au passif
  # Remarque : A terme, revoir ce mode de fonctionemment qui n'est pas ideal en terme de structure.
  #---------------------------------------------------------------

  # Coefficient d'inflation
  coef_inf <- x@mp_esg@indice_inflation

  # liste des rendements
  list_rd <- calc_rdt_marche_ref(x@ptf_passif@ht@param_comport[[x@hyp_canton@method_taux_cible]], x@mp_esg)

  #---------------------------------------------------------------
  # Etape 3 : Gestion des passifs avant participation aux benefices
  #---------------------------------------------------------------
  # Evaluation du passif vieilli d'un an
  x1 <- x
  x1@ptf_passif@choc_lapse_mass <- 0.4

  # Cas avec choc et sans choc
  passif_av_pb_ref <- viellissement_av_pb(annee, x@ptf_passif, coef_inf, list_rd, x@hyp_canton@tx_soc)
  passif_av_pb_choc <- viellissement_av_pb(annee, x1@ptf_passif, coef_inf, list_rd, x1@hyp_canton@tx_soc)
  x1@ptf_passif <- passif_av_pb_choc[["ptf"]]

  # Actif
  x1@ptf_fin@ptf_treso <- update_treso(x1@ptf_fin@ptf_treso , passif_av_pb_choc[["flux_debut"]])
  actif_realloc_mass <- reallocate(x1@ptf_fin, x1@param_alm@ptf_reference, x1@param_alm@alloc_cible)
  x1@ptf_fin <- actif_realloc_mass[["portFin"]]
  pmvr_mass <- list(oblig = actif_realloc_mass[["pmvr_oblig"]],
                    action = actif_realloc_mass[["pmvr_action"]],
                    immo = actif_realloc_mass[["pmvr_immo"]])

  # Verification de levolution de la valeur de marche a cause de la treso
  expect_equal(print_alloc(x1@ptf_fin)[5,1], print_alloc(x@ptf_fin)[5,1] + passif_av_pb_choc[["flux_debut"]])
})



