# ##################
# Tests AutresPassifs
# ##################

context("AutresPassifs")

# Dossier de DATA
path <- paste0(getwd(), "/donnees_tests/input")
folder_tab <- paste(path, "/donnees/passif/autres_passifs.csv", sep = "/")


#----------------------------------------------------------------------------------------------------------------------------------------------------
# Elements necessaires aux calculs

ptf_csv <- read.csv2(folder_tab, header = TRUE, colClasses = c(
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric"
))
ptf <- new("AutresPassifs", ptf_csv)


#----------------------------------------------------------------------------------
# Classe
#----------------------------------------------------------------------------------
test_that("TEST_classe", {
    # Classe
    expect_s4_class(ptf, "AutresPassifs")

    # Tests attributs
    expect_equal(ptf@mp, ptf_csv)
})


#----------------------------------------------------------------------------------
# load
#----------------------------------------------------------------------------------
test_that("TEST_load", {
    # Appel de la fonction
    res <- autres_passif_load(folder_tab)

    # Test
    expect_identical(res, ptf)
})



#----------------------------------------------------------------------------------
# proj_annee
#----------------------------------------------------------------------------------
test_that("TEST_proj_annee", {
    # Donnees
    an <- 1L
    inf <- 1.02

    # Appel de la fonction
    res <- proj_annee_autres_passifs(an, ptf, inf)

    # Resultats attendus
    res_att <- res[res$annee == an]
    res_att$frais <- res_att$frais * inf

    # Test
    expect_equal(res, res_att, tolerance = 0.03)
})
