# ##################
# Tests HypTech
# ##################

context("HypTech")

# Dossier de DATA
path <- "C:/Users/quentin.guibert.PRIMACT/Documents/Dropbox/Code_BE_Env_Dev/01_Dev/SimBEL/tests/donnees_tests"
folder_tab <- paste(path, "input/parametres/tables", sep = "/")


#----------------------------------------------------------------------------------------------------------------------------------------------------
# Elements necessaires aux calculs

init <- new("Initialisation", root_address = path)
init <- set_architecture(init)

hypt <- load_ht(init)



#----------------------------------------------------------------------------------
# Classe
#----------------------------------------------------------------------------------
test_that("Test_classe", {

    # Classe
    expect_s4_class(hypt, "HypTech")

    # Attributs
    expect_s4_class(hypt@tables_mort[[1]], "ParamTableMort")
    expect_s4_class(hypt@tables_rach[[1]], "ParamTableRach")
    expect_s4_class(hypt@param_rach_dyn[[1]], "ParamRachDyn")
    expect_s4_class(hypt@param_comport[[1]], "ParamComport")
})



#----------------------------------------------------------------------------------
# Tables de mortalite
#----------------------------------------------------------------------------------
test_that("TEST_tables_morta", {

    # Recuperation de l'objet
    tab_mort <- hypt@tables_mort[[1]]

    # Tests constantes
    expect_equal(tab_mort@age_min, min(tab_mort@table$age))
    expect_equal(tab_mort@age_max, max(tab_mort@table$age))
    expect_equal(tab_mort@gen_min, min(tab_mort@table$gen))
    expect_equal(tab_mort@gen_max, max(tab_mort@table$gen))

    # Tests table
    tab_mort <- tab_mort@table
    expect_true(all(colnames(tab_mort) %in% c("age", "gen", "qx", "lx")))
    expect_equal(prod(tab_mort$qx<=1), 1)
    expect_equal(prod(0<=tab_mort$qx), 1)
    expect_equal(prod(0<=tab_mort$lx), 1)

    # Tests qx
    i <- 70
    lx <- tab_mort$lx[i]
    lx1 <- tab_mort$lx[i+1]
    qx <- 1 - lx1/lx
    expect_equal(tab_mort$qx[70], qx)
})




#----------------------------------------------------------------------------------
# Tables de rachats
#----------------------------------------------------------------------------------
test_that("TEST_tables_rachats", {

    # Recuperation de l'objet
    tab_rach <- hypt@tables_rach[[1]]

    # Tests constantes
    expect_equal(tab_rach@age_min, min(tab_rach@table$age))
    expect_equal(tab_rach@age_max, max(tab_rach@table$age))
    expect_equal(tab_rach@anc_min, min(tab_rach@table$anc))
    expect_equal(tab_rach@anc_max, max(tab_rach@table$anc))

    # Lecture fichier csv
    nom_tab <- as.character(read.csv2(paste(folder_tab, "empl_tables_rachats.csv", sep = "/"))[1,"nom_table_csv"])
    tab_csv <- read.csv2(paste(folder_tab, nom_tab, sep = "/"))

    # Tests table
    tab_rach <- tab_rach@table
    expect_equal(tab_rach, tab_csv)
    expect_true(all(colnames(tab_rach) %in% c("age", "anc", "taux_rachat")))
    expect_equal(prod(tab_rach$taux_rachat<=1), 1)
    expect_equal(prod(0<=tab_rach$taux_rachat), 1)
})




#----------------------------------------------------------------------------------
# Tables de param comport
#----------------------------------------------------------------------------------
test_that("TEST_param_comport", {

    # Recuperation de l'objet
    param_comport <- hypt@param_comport[[1]]

    # Lecture fichier csv
    tab_csv <- read.csv2(paste(folder_tab, "param_comport.csv", sep = "/"))[1,]

    # Test attributs
    expect_equal(param_comport@mat_oblig, tab_csv$mat_oblig)
    expect_equal(param_comport@alloc_mar, c(tab_csv$alloc_mar_action, tab_csv$alloc_mar_immo, tab_csv$alloc_mar_tres, tab_csv$alloc_mar_oblig))
    expect_equal(param_comport@w_n, tab_csv$w_n)
    expect_equal(param_comport@marge_mar, tab_csv$marge_mar)
    expect_equal(param_comport@ch_enc_mar, tab_csv$ch_enc_mar)
    expect_equal(param_comport@ind_ref_action, tab_csv$ind_ref_action)
    expect_equal(param_comport@ind_ref_immo, tab_csv$ind_ref_immo)
})



#----------------------------------------------------------------------------------
# ParamRachDyn
#----------------------------------------------------------------------------------
test_that("TEST_ParamRachDyn", {

    # Recuperation de l'objet
    param <- hypt@param_rach_dyn[[1]]

    # Lecture du fichier csv
    nom_tab <- as.character(read.csv2(paste(folder_tab, "empl_param_rachats_conj.csv", sep = "/"))[1,"nom_table_csv"])
    tab_csv <- read.csv2(paste(folder_tab, nom_tab, sep = "/"))

    # Tests attributs
    expect_equal(param@vec_param, tab_csv)
})



#----------------------------------------------------------------------------------
# convert_table
#----------------------------------------------------------------------------------
test_that("TEST_convert_table", {

    # Recuperation d'une table
    tab_mort <- hypt@tables_mort[[1]]@table

    # Appels de la fonction
    res <- convert_table(tab_mort[1:3], "lx")
    res2 <- convert_table(res[c(1,2,4)], "qx")

    # Test
    expect_equal(res2, tab_mort[c(1,2,4,3)])
})



#----------------------------------------------------------------------------------
# calc_proba_deces
#----------------------------------------------------------------------------------
test_that("TEST_calc_proba_deces", {

    # Donnees necessaires
    tab_mort <- hypt@tables_mort[[1]]

    # Appels de la fonction
    res <- calc_proba_deces(tab_mort, 23L, 1900L, 1L)
    res1 <- calc_proba_deces(tab_mort, 23L, 0L, 1L)
    res2 <- calc_proba_deces(tab_mort, 1206L, 1900L, 1L)
    res3 <- calc_proba_deces(tab_mort, 23L, 3000L, 1L)

    # Resultats attendus
    qx <- tab_mort@table$qx[which(tab_mort@table$age == 23L & tab_mort@table$gen == 1900L)]
    qx2 <- tab_mort@table$qx[which(tab_mort@table$age == max(tab_mort@table$age) & tab_mort@table$gen == 1900L)]
    qx3 <- tab_mort@table$qx[which(tab_mort@table$age == 23L & tab_mort@table$gen == max(tab_mort@table$gen))]

    # Test
    expect_equal(res, qx)
    expect_equal(res1, qx)
    expect_equal(res2, qx2)
    expect_equal(res3, qx3)

    # Tests avec erreurs
    expect_error(calc_proba_deces(tab_mort, -5L, 1900L, 1L))
    expect_error(calc_proba_deces(tab_mort, -5L, 1900L, 0L))
})
