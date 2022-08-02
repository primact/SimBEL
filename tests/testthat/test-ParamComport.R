# ##################
# Tests ParamComport
# ##################

context("ParamComport")

# Lecture du fichier csv
path <- paste0(getwd(), "/donnees_tests/input/parametres/tables")
param_comport_csv <- read.csv2(paste(path, "param_comport.csv", sep = "/"), header = TRUE)

# Recuperation des donnees
mat_oblig <- param_comport_csv[,"mat_oblig"]
alloc_mar <- c(param_comport_csv[,"alloc_mar_action"], param_comport_csv[,"alloc_mar_immo"],
                           param_comport_csv[,"alloc_mar_tres"], param_comport_csv[,"alloc_mar_oblig"])
w_n <- param_comport_csv[,"w_n"]
marge_mar <- param_comport_csv[,"marge_mar"]
ch_enc_mar <- param_comport_csv[,"ch_enc_mar"]
ind_ref_action <- param_comport_csv[,"ind_ref_action"]
ind_ref_immo <- param_comport_csv[,"ind_ref_immo"]


# Creation de l'objet
param_comport <- new(Class = "ParamComport",mat_oblig , alloc_mar , w_n , marge_mar, ch_enc_mar, ind_ref_action , ind_ref_immo)

# Tests sur l'objet
test_that("TEST_ParamComport", {

    # Test classe
    expect_s4_class(param_comport, "ParamComport")

    # Tests attributs
    expect_equal(param_comport@mat_oblig, expected = mat_oblig)
    expect_equal(param_comport@alloc_mar, expected = alloc_mar)
    expect_equal(param_comport@w_n, expected = w_n)
    expect_equal(param_comport@marge_mar, expected = marge_mar)
    expect_equal(param_comport@ch_enc_mar, expected = ch_enc_mar)
    expect_equal(param_comport@ind_ref_action, expected = ind_ref_action)
    expect_equal(param_comport@ind_ref_immo, expected = ind_ref_immo)
})
