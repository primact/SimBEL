# #############
# Tests TauxPB
# #############

context("TauxPB")

# Lecture du fichier csv
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests/donnees/passif/taux_pb.csv"
taux_pb_csv <- read.csv2(path, header = TRUE)

# Creation de l'objet
taux_pb <- new("TauxPB", taux_pb_csv)

# Tests sur la creation de la classe
test_that("TEST_taux_pb", {

    # Test classe
    expect_s4_class(taux_pb, "TauxPB")

    # Tests attributs
    expect_equal(taux_pb@mp$nom_prod, taux_pb_csv$nom_prod)
    expect_equal(taux_pb@mp$taux_pb, taux_pb_csv$taux_pb)
})
