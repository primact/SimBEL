# #############
# Tests TauxPB
# #############

context("TauxPB")

# Lecture du fichier csv
path <- "P:/Dossiers publics/02 - Missions/OUTIL BE PRIMACT/11_Travaux_Damien/02_Codes/03_TestsUnitaires/00_Data/input/donnees/passif/taux_pb.csv"
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
