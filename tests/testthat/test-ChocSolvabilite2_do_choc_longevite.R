# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc longevite")

path <- paste0(getwd(), "/donnees_tests")

# Parametres de test
choc_longevite <- -0.20
rang_test <- 36

# Chargement SimBEL
init <- new("Initialisation", root_address = path)
init <- set_architecture(init)
hypt <- load_ht(init)
tables_choc <- get_choc_table(hypt, choc_longevite)

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


# Test sur les modifications des tables de longevite

test_that("TEST_choc_longevite", {
    # Verification du chargement des tables
    expect_equal(hypt@tables_mort$TM1@table$gen, table_mort_expH$gen)
    expect_equal(hypt@tables_mort$TM1@table$age, table_mort_expH$age)
    expect_equal(hypt@tables_mort$TM1@table$lx, table_mort_expH$valeur)
    expect_equal(hypt@tables_mort$TM2@table$gen, table_mort_expF$gen)
    expect_equal(hypt@tables_mort$TM2@table$age, table_mort_expF$age)
    expect_equal(hypt@tables_mort$TM2@table$lx, table_mort_expF$valeur)
    expect_equal(hypt@tables_mort$test@table$lx, table_mort_tgf05$valeur)
    expect_equal(hypt@tables_mort$test@table$lx, table_mort_tgf05$valeur)
    expect_equal(hypt@tables_mort$test@table$lx, table_mort_tgf05$valeur)

    # Verification du calcul de qx dans le cas general
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

    # Verification de lx dans le cas general
    expect_equal(hypt@tables_mort$TM1@table[1, "lx"], 100000)
    expect_equal(hypt@tables_mort$TM2@table[1, "lx"], 100000)
    expect_equal(hypt@tables_mort$test@table[1, "lx"], 100000)
    expect_equal(hypt@tables_mort$TM1@table[122, "lx"], 0)
    expect_equal(hypt@tables_mort$TM2@table[122, "lx"], 0)

    # Verifivation du calcul de qx dans le cas choc longevite
    expect_equal(tables_choc@tables_mort$TM1@table$qx, pmin(hypt@tables_mort$TM1@table$qx * (1 + choc_longevite), 1))
    expect_equal(tables_choc@tables_mort$TM2@table$qx, pmin(hypt@tables_mort$TM2@table$qx * (1 + choc_longevite), 1))
    expect_equal(tables_choc@tables_mort$test@table$qx, pmin(hypt@tables_mort$test@table$qx * (1 + choc_longevite), 1))

    # Verification du recalcul des lx dans le cas choc longevite
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
