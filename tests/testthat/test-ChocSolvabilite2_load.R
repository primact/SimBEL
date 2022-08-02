# ##################
# Tests ChocSolvabilite2
# ##################
context("ChocSolvabilite2 load")


# Lecture des fichiers csv
path <- paste0(getwd(), "/donnees_tests/input/parametres/chocs")

scenario_csv <- read.csv2(paste(path, "list_run_choc.csv", sep = "/"), colClasses = c("character", "logical"))
table_choc_action_type1_csv <- read.csv2(paste(path, "param_choc_mket_action_type1.csv", sep = "/"), colClasses = c("integer", "numeric"))
table_choc_action_type2_csv <- read.csv2(paste(path, "param_choc_mket_action_type2.csv", sep = "/"), colClasses = c("integer", "numeric"))
table_choc_immo_csv   <- read.csv2(paste(path, "param_choc_mket_immo.csv", sep = "/"), colClasses = c("integer", "numeric"))
table_choc_spread_csv <- read.csv2(paste(path, "param_choc_mket_spread.csv", sep = "/"), colClasses = c("integer", "character", "numeric", "numeric"))
table_choc_currency_csv <- read.csv2(paste(path, "param_choc_mket_currency.csv", sep = "/"), colClasses = c("character", "logical", "numeric", "numeric"))
table_choc_sousc_csv <- read.csv2(paste(path, "param_choc_sousc.csv", sep = "/"))
matrice_choc_action_csv <- read.csv2(paste(path, "matrices/matrice_choc_action.csv", sep = "/"), colClasses = c("numeric", "numeric"))
matrice_choc_mket_csv <- read.csv2(paste(path, "matrices/matrice_choc_mket.csv", sep = "/"), colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric"))
matrice_choc_sousc_csv <- read.csv2(paste(path, "matrices/matrice_choc_sousc.csv", sep = "/"), colClasses = c("numeric", "numeric", "numeric", "numeric"))
matrice_choc_bscr_csv <- read.csv2(paste(path, "matrices/matrice_choc_bscr.csv", sep = "/"), colClasses = c("numeric", "numeric"))

# Retraitement des scenarios pour choc currency
scenario_csv <- scenario_csv$nom_choc[scenario_csv$run_choc==T]
nom_choc_currency <- table_choc_currency_csv$currency[table_choc_currency_csv$run_choc == TRUE]
if(length(nom_choc_currency) > 0 & any(c("currency_up", "currency_down") %in% scenario_csv)){ # Si il y a des chocs currency
  if("currency_up" %in% scenario_csv){
    rr <- which(scenario_csv == "currency_up")
    scenario_csv <- scenario_csv[-rr] # Retrait du nom de scenario pour le completer de la liste des devises
    scenario_csv <- c(scenario_csv, paste0("currency_up_", nom_choc_currency))
  }
  if("currency_down" %in% scenario_csv){
    rr <- which(scenario_csv == "currency_down")
    scenario_csv <- scenario_csv[-rr] # Retrait du nom de scenario pour le completer de la liste des devises
    scenario_csv <- c(scenario_csv, paste0("currency_down_", nom_choc_currency))
  }

} else{ # Suppression des chocs currency appeles a tord
  rr <- which(scenario_csv == "currency_up" | scenario_csv == "currency_down")
  if(length(rr) > 0){
    scenario_csv <- scenario_csv[-rr]
  }
}

# Creation de l'objet
table_choc <- chargement_choc(new("ChocSolvabilite2"), path)



test_that("TEST_ChocSolvabilite2_classe", {

    # Verification classe
    expect_s4_class(table_choc, "ChocSolvabilite2")

    # Verification scenarios
    expect_equal(table_choc@scenario, scenario_csv)

    # Verifications chargement parametres
    # param_choc_mket
    expect_equal(table_choc@param_choc_mket@table_choc_action_type1$num_index, table_choc_action_type1_csv$num_index)
    expect_equal(table_choc@param_choc_mket@table_choc_action_type1$choc_action, table_choc_action_type1_csv$choc_action)
    expect_equal(table_choc@param_choc_mket@table_choc_action_type2$num_index, table_choc_action_type2_csv$num_index)
    expect_equal(table_choc@param_choc_mket@table_choc_action_type2$choc_action, table_choc_action_type2_csv$choc_action)
    expect_equal(table_choc@param_choc_mket@table_choc_immo$num_index, table_choc_immo_csv$num_index)
    expect_equal(table_choc@param_choc_mket@table_choc_immo$choc_immo, table_choc_immo_csv$choc_immo)
    expect_equal(table_choc@param_choc_mket@table_choc_spread$rating, table_choc_spread_csv$rating)
    expect_equal(table_choc@param_choc_mket@table_choc_spread$duration, table_choc_spread_csv$duration)
    expect_equal(table_choc@param_choc_mket@table_choc_spread$param_A, table_choc_spread_csv$param_A)
    expect_equal(table_choc@param_choc_mket@table_choc_spread$param_B, table_choc_spread_csv$param_B)
    expect_equal(table_choc@param_choc_mket@table_choc_currency$currency, table_choc_currency_csv$currency)
    expect_equal(table_choc@param_choc_mket@table_choc_currency$run_choc, table_choc_currency_csv$run_choc)
    expect_equal(table_choc@param_choc_mket@table_choc_currency$choc_currency_up, table_choc_currency_csv$choc_currency_up)
    expect_equal(table_choc@param_choc_mket@table_choc_currency$choc_currency_down, table_choc_currency_csv$choc_currency_down)

    # param_choc_sousc
    expect_equal(table_choc@param_choc_sousc@mp$choc_frais_inflation, table_choc_sousc_csv$choc_frais_inflation)
    expect_equal(table_choc@param_choc_sousc@mp$choc_frais_assiette, table_choc_sousc_csv$choc_frais_assiette)
    expect_equal(table_choc@param_choc_sousc@mp$choc_mortalite, table_choc_sousc_csv$choc_mortalite)
    expect_equal(table_choc@param_choc_sousc@mp$choc_longevite, table_choc_sousc_csv$choc_longevite)
    expect_equal(table_choc@param_choc_sousc@mp$choc_rachat_up, table_choc_sousc_csv$choc_rachat_up)
    expect_equal(table_choc@param_choc_sousc@mp$choc_rachat_up_lim, table_choc_sousc_csv$choc_rachat_up_lim)
    expect_equal(table_choc@param_choc_sousc@mp$choc_rachat_down, table_choc_sousc_csv$choc_rachat_down)
    expect_equal(table_choc@param_choc_sousc@mp$choc_rachat_down_lim, table_choc_sousc_csv$choc_rachat_down_lim)
    expect_equal(table_choc@param_choc_sousc@mp$choc_rachat_massif, table_choc_sousc_csv$choc_rachat_massif)

    # matrice_choc_action
    expect_equal(as.vector(table_choc@matrice_choc_action[,1]), matrice_choc_action_csv$action_type1)
    expect_equal(as.vector(table_choc@matrice_choc_action[,2]), matrice_choc_action_csv$action_type2)

    # matrice_choc_mket
    expect_equal(as.vector(table_choc@matrice_choc_mket[,1]), matrice_choc_mket_csv$taux)
    expect_equal(as.vector(table_choc@matrice_choc_mket[,2]), matrice_choc_mket_csv$action)
    expect_equal(as.vector(table_choc@matrice_choc_mket[,3]), matrice_choc_mket_csv$immo)
    expect_equal(as.vector(table_choc@matrice_choc_mket[,4]), matrice_choc_mket_csv$spread)
    expect_equal(as.vector(table_choc@matrice_choc_mket[,5]), matrice_choc_mket_csv$currency)

    # matrice_choc_sousc
    expect_equal(as.vector(table_choc@matrice_choc_sousc[,1]), matrice_choc_sousc_csv$mortalite)
    expect_equal(as.vector(table_choc@matrice_choc_sousc[,2]), matrice_choc_sousc_csv$longevite)
    expect_equal(as.vector(table_choc@matrice_choc_sousc[,3]), matrice_choc_sousc_csv$rachat)
    expect_equal(as.vector(table_choc@matrice_choc_sousc[,4]), matrice_choc_sousc_csv$frais)

    # matrice_choc_bscr
    expect_equal(as.vector(table_choc@matrice_choc_bscr[,1]), matrice_choc_bscr_csv$mket)
    expect_equal(as.vector(table_choc@matrice_choc_bscr[,2]), matrice_choc_bscr_csv$sousc)

})

