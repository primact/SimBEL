# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc mortalite")

path <- paste0(getwd(), "/donnees_tests")

# Parametres de test
choc_mortalite <- 0.15
rang_test <- 36

# Chargement SimBEL
init <- new("Initialisation", root_address = path)
init <- set_architecture(init)
hypt <- load_ht(init)
tables_choc <- get_choc_table(hypt, choc_mortalite)

# Chargement manuel des tables
table_mort_tgf05 <- read.csv2(
    paste(path, "input/parametres/tables/TGF05_lx.csv", sep = "/"),
    header = TRUE, colClasses = c("integer", "integer", "numeric")
)
table_mort_expF <- read.csv2(
    paste(path, "input/parametres/tables/Table_Exp_F.csv", sep = "/"),
    header = TRUE, colClasses = c("integer", "integer", "numeric")
)
table_mort_expH <- read.csv2(
    paste(path, "input/parametres/tables/Table_Exp_H.csv", sep = "/"),
    header = TRUE, colClasses = c("integer", "integer", "numeric")
)


# Test sur les modifications des tables de mortalite

test_that("TEST_choc_mortalite", {
    # Verifications du chargement des tables
    expect_equal(hypt@tables_mort$TM1@table$gen, table_mort_expH$gen)
    expect_equal(hypt@tables_mort$TM1@table$age, table_mort_expH$age)
    expect_equal(hypt@tables_mort$TM1@table$lx, table_mort_expH$valeur)
    expect_equal(hypt@tables_mort$TM2@table$gen, table_mort_expF$gen)
    expect_equal(hypt@tables_mort$TM2@table$age, table_mort_expF$age)
    expect_equal(hypt@tables_mort$TM2@table$lx, table_mort_expF$valeur)
    expect_equal(hypt@tables_mort$test@table$lx, table_mort_tgf05$valeur)
    expect_equal(hypt@tables_mort$test@table$lx, table_mort_tgf05$valeur)
    expect_equal(hypt@tables_mort$test@table$lx, table_mort_tgf05$valeur)

    # Verifications du calcul de qx dans le cas general
    expect_equal(
        hypt@tables_mort$TM2@table$qx[rang_test],
        (table_mort_expF$valeur[rang_test] - table_mort_expF$valeur[rang_test + 1]) / table_mort_expF$valeur[rang_test]
    )
    expect_equal(
        hypt@tables_mort$TM1@table$qx[rang_test],
        (table_mort_expH$valeur[rang_test] - table_mort_expH$valeur[rang_test + 1]) / table_mort_expH$valeur[rang_test]
    )
    expect_equal(
        hypt@tables_mort$TM2@table$qx[120],
        (table_mort_expF$valeur[120] - table_mort_expF$valeur[121]) / table_mort_expF$valeur[120]
    )
    expect_equal(
        hypt@tables_mort$TM1@table$qx[120],
        (table_mort_expH$valeur[120] - table_mort_expH$valeur[121]) / table_mort_expH$valeur[120]
    )

    # Verifications de lx dans le cas general
    expect_equal(hypt@tables_mort$TM1@table[1, "lx"], 100000)
    expect_equal(hypt@tables_mort$TM2@table[1, "lx"], 100000)
    expect_equal(hypt@tables_mort$test@table[1, "lx"], 100000)
    expect_equal(hypt@tables_mort$TM1@table[122, "lx"], 0)
    expect_equal(hypt@tables_mort$TM2@table[122, "lx"], 0)

    # Verifivations du calcul de qx dans le cas choc mortalite
    expect_equal(tables_choc@tables_mort$TM1@table$qx, pmin(hypt@tables_mort$TM1@table$qx * (1 + choc_mortalite), 1))
    expect_equal(tables_choc@tables_mort$TM2@table$qx, pmin(hypt@tables_mort$TM2@table$qx * (1 + choc_mortalite), 1))
    expect_equal(tables_choc@tables_mort$test@table$qx, pmin(hypt@tables_mort$test@table$qx * (1 + choc_mortalite), 1))

    # Verifications du recalcul des lx dans le cas choc mortalite
    expect_equal(tables_choc@tables_mort$TM1@table[1, "lx"], 100000)
    expect_equal(tables_choc@tables_mort$TM2@table[1, "lx"], 100000)
    expect_equal(tables_choc@tables_mort$test@table[1, "lx"], 100000)
    expect_equal(
        tables_choc@tables_mort$TM1@table[2, "lx"],
        tables_choc@tables_mort$TM1@table[1, "lx"] * (1 - tables_choc@tables_mort$TM1@table[1, "qx"])
    )
    expect_equal(
        tables_choc@tables_mort$TM2@table[2, "lx"],
        tables_choc@tables_mort$TM2@table[1, "lx"] * (1 - tables_choc@tables_mort$TM2@table[1, "qx"])
    )
    expect_equal(
        tables_choc@tables_mort$test@table[2, "lx"],
        tables_choc@tables_mort$test@table[1, "lx"] * (1 - tables_choc@tables_mort$test@table[1, "qx"])
    )
    expect_equal(
        tables_choc@tables_mort$TM1@table[rang_test, "lx"],
        tables_choc@tables_mort$TM1@table[rang_test - 1, "lx"] * (1 - tables_choc@tables_mort$TM1@table[rang_test - 1, "qx"])
    )
    expect_equal(
        tables_choc@tables_mort$TM2@table[rang_test, "lx"],
        tables_choc@tables_mort$TM2@table[rang_test - 1, "lx"] * (1 - tables_choc@tables_mort$TM2@table[rang_test - 1, "qx"])
    )
    expect_equal(
        tables_choc@tables_mort$test@table[rang_test, "lx"],
        tables_choc@tables_mort$test@table[rang_test - 1, "lx"] * (1 - tables_choc@tables_mort$test@table[rang_test - 1, "qx"])
    )
    expect_equal(tables_choc@tables_mort$TM1@table[122, "lx"], 0)
    expect_equal(tables_choc@tables_mort$TM2@table[122, "lx"], 0)
})
