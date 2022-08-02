# ##################
# Tests ChocSolvabilite2
# ##################
context("Choc rachat_up")

path <- paste0(getwd(), "/donnees_tests")

# Parametres de test
choc_rachat_up <- 0.5
choc_rachat_up_lim <- 1
rang_test <- 36

# Chargement SimBEL
init <- new("Initialisation", root_address = path)
init <- set_architecture(init)
hypt <- load_ht(init)
tables_choc_up <- get_choc_rach(hypt, "up", choc_rachat_up, choc_rachat_up_lim)

# Chargement manuel des tables
table_rachat_trp1 <- read.csv2(paste(path, "input/parametres/tables/trp1.csv", sep = "/"), header = TRUE, colClasses = c("integer", "integer", "numeric"))
table_rachat_trt1 <- read.csv2(paste(path, "input/parametres/tables/trt1.csv", sep = "/"), header = TRUE, colClasses = c("integer", "integer", "numeric"))


# Test sur les modifications des tables de rachat_up

test_that("TEST_choc_rachat_up", {
    # Verifications du chargement des tables
    expect_equal(hypt@tables_rach$TRP1@table$anc, table_rachat_trp1$anc)
    expect_equal(hypt@tables_rach$TRP1@table$age, table_rachat_trp1$age)
    expect_equal(hypt@tables_rach$TRP1@table$taux_rachat, table_rachat_trp1$taux_rachat)
    expect_equal(hypt@tables_rach$TRT1@table$anc, table_rachat_trt1$anc)
    expect_equal(hypt@tables_rach$TRT1@table$age, table_rachat_trt1$age)
    expect_equal(hypt@tables_rach$TRT1@table$taux_rachat, table_rachat_trt1$taux_rachat)

    # Verifications des ages et anciennetes min et max
    expect_true(hypt@tables_rach$TRP1@age_min>=0)
    expect_true(hypt@tables_rach$TRP1@age_max<=122)
    expect_true(hypt@tables_rach$TRP1@anc_min>=0)
    expect_true(hypt@tables_rach$TRP1@anc_max<=80)
    expect_true(hypt@tables_rach$TRT1@age_min>=0)
    expect_true(hypt@tables_rach$TRT1@age_max<=122)
    expect_true(hypt@tables_rach$TRT1@anc_min>=0)
    expect_true(hypt@tables_rach$TRT1@anc_max<=80)

    # Verifivation du calcul de qx dans le cas choc rachat_up
    expect_equal(tables_choc_up@tables_rach$TRP1@table$taux_rachat, pmin(hypt@tables_rach$TRP1@table$taux_rachat*(1+choc_rachat_up),choc_rachat_up_lim))
  })
