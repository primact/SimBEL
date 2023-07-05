# ##################
# Tests PortFin
# ##################

context("PortFin")


# Dossier de DATA
path <- paste0(getwd(), "/donnees_tests/input")
folder_ESG_address <- paste0(path, "/parametres/esg/ESG")
path <- paste0(path, "/donnees/actif")


#--------------------------------------------------------------------------
# Chargement des tables ESG et extractions de ModelPointsESG specifiques
#--------------------------------------------------------------------------
table_ESG <- chargement_ESG(folder_ESG_address, nb_simu = 2L, 5L)
mp_ESG <- extract_ESG(table_ESG, 2L, 0L)
new_mp_ESG <- extract_ESG(table_ESG, 2L, 1L)


#--------------------------------------------------------------------------
# Chargement des PTF
#--------------------------------------------------------------------------

# Chargement des portefeuilles inputs : doit tourner
ptf_action_csv <- read.csv2(paste(path, "Portefeuille_action.csv", sep = "/"),
    header = TRUE,
    colClasses = c(
        "integer", "double", "double", "double",
        "logical", "logical", "double", "double",
        "double", "integer", "double", "logical", "character", "numeric"
    )
)
ptf_immo_csv <- read.csv2(paste(path, "Portefeuille_immobilier.csv", sep = "/"),
    header = TRUE,
    colClasses = c(
        "integer", "double", "double", "double",
        "logical", "logical", "double", "double",
        "double", "integer", "double", "logical", "character", "numeric"
    )
)
ptf_oblig_csv <- read.csv2(paste(path, "Portefeuille_obligation.csv", sep = "/"),
    header = TRUE,
    colClasses = c(
        "integer", "double", "double", "double", "logical", "logical",
        "double", "double", "double", "double", "double", "double",
        "factor", "integer", "double", "double", "double", "double", "character", "numeric"
    )
)
ptf_treso_csv <- read.csv2(paste(path, "Tresorerie.csv", sep = "/"),
    header = TRUE,
    colClasses = c("integer", "double", "double")
)


pre <- pre_load(paste(path, "PRE.csv", sep = "/"))
rc <- rc_load(paste(path, "RC.csv", sep = "/"))
frais <- frais_fin_load(paste(path, "Frais_financier.csv", sep = "/"))


# Initialisation du PTF fin
ptffin <- chargement_PortFin(path, mp_ESG)

test_that("TEST_classe", {
    # Test classe
    expect_s4_class(ptffin, "PortFin")

    # Resultats attendus
    pmvl <- calc_pmvl(ptffin)

    # Tests attributs
    expect_equal(ptffin@annee, 0L)
    expect_equal(ptffin@ptf_action@ptf_action, ptf_action_csv)
    expect_equal(ptffin@ptf_immo@ptf_immo, ptf_immo_csv)
    expect_equal(ptffin@ptf_oblig@ptf_oblig[1:17], ptf_oblig_csv[1:17])
    expect_equal(ptffin@ptf_treso@ptf_treso, ptf_treso_csv)
    expect_s4_class(ptffin@pre, "PRE")
    expect_s4_class(ptffin@rc, "RC")
    expect_s4_class(ptffin@frais_fin, "FraisFin")
    expect_equal(ptffin@pvl_action, pmvl@pvl_action)
    expect_equal(ptffin@pvl_immo, pmvl@pvl_immo)
    expect_equal(ptffin@pvl_oblig, pmvl@pvl_oblig)
    expect_equal(ptffin@mvl_action, pmvl@mvl_action)
    expect_equal(ptffin@mvl_immo, pmvl@mvl_immo)
    expect_equal(ptffin@mvl_oblig, pmvl@mvl_oblig)
})


#--------------------------------------------------------------------------
# chargement_PortFin_reference
#--------------------------------------------------------------------------
test_that("TEST_chgt_ptffin_ref", {
    # Appel de la fonction
    path_ptf_ref <- paste(path, "Portefeuille_reference", sep = "/")
    PtfFin_ref <- chargement_PortFin_reference(path_ptf_ref, mp_ESG)

    # Tests
    expect_s4_class(PtfFin_ref, "PortFin")
})

#--------------------------------------------------------------------------
# chargement_PortFin
#--------------------------------------------------------------------------
test_that("TEST_chgt_ptffin", {
    # Appel de la fonction
    PtfFin <- chargement_PortFin(path, mp_ESG)

    # Tests
    expect_s4_class(PtfFin, "PortFin")
})





#--------------------------------------------------------------------------
# calc_rdt
#--------------------------------------------------------------------------
test_that("TEST_calc_rdt", {
    # Appel de la fonction
    res <- calc_rdt(ptffin, mp_ESG)

    # Tests
    expect_equal(res$rdt_action[, "rdt"], c(0, 0, -0.0654), tolerance = 0.01)
    expect_equal(res$rdt_action[, "div"], c(0, 0, 6767.155), tolerance = 0.01)
    expect_equal(res$rdt_immo[, "rdt"], c(-0.0118, 0), tolerance = 0.01)
    expect_equal(res$rdt_immo[, "loyer"], c(59842.83, 2500), tolerance = 0.01)
    expect_equal(res$rdt_treso, -0.00157, tolerance = 0.01)
})

#--------------------------------------------------------------------------
# calc_pmvl
#--------------------------------------------------------------------------
test_that("TEST_calc_pmvl", {
    # Appel de la fonction
    res <- calc_pmvl(ptffin)

    # Tests
    expect_s4_class(res, "PortFin")
})



#--------------------------------------------------------------------------
# print_alloc
#--------------------------------------------------------------------------
test_that("TEST_print_alloc", {
    # Appel de la fonction
    res <- print_alloc(ptffin)

    # Tests
    expect_equal(res[5, 1], sum(res[1:4, 1]))
    expect_equal(res[5, 3], sum(res[1:4, 3]))
    expect_equal(sum(res[1:4, 2]), 1)
    expect_equal(sum(res[1:4, 4]), 1)
})

#--------------------------------------------------------------------------
# resultat_fin
#--------------------------------------------------------------------------
test_that("TEST_resultat_fin", {
    # Donnees necessaires
    revenu <- 1
    produit <- 10
    frais_fin <- 5
    var_rc <- 2

    # Appel de la fonction
    res <- calc_resultat_fin(revenu, produit, frais_fin, var_rc)

    # Resultat attendu
    res_fin <- revenu + produit - frais_fin - var_rc

    # Test
    expect_equal(res, res_fin)
})


#--------------------------------------------------------------------------
# tra
#--------------------------------------------------------------------------
test_that("TEST_calc_tra", {
    # Donnees necessaires
    plac_moy <- 10
    res_fin <- 2

    # Appel de la fonction
    res <- calc_tra(plac_moy, res_fin)

    # Resultats attendus
    tra <- res_fin / plac_moy

    # Test
    expect_equal(res, tra)
    expect_equal(calc_tra(0, res_fin), 0)
})
