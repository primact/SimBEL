library(readxl)

tolerance <- 1e-6
sheets <- 1:3
years <- 2024:2073
input_range <- "E8:E31"
analyse_pm_range <- "F37:BC54"
analyse_pm_nrow <- 18
res_range <- "F60:F96"
res_sublevel_indices <- c(4:5, 8:9, 12:19, 21:28, 31:34, 36:37)
excel_file_path <- file.path(getwd(), "donnees_tests/others/Maquette compte de résultat SimBEL.xlsx")

#--- Excel helper functions ----
get_data <- function(sheet_number, range, excel_file = excel_file_path) {
    sheet_name <- sprintf("Compte de résultat %s", as.character(sheet_number))
    excel_data <- suppressMessages(read_excel(excel_file, sheet = sheet_name, range = range, col_names = FALSE))
    return(unname(as.matrix(excel_data)))
}

get_input_params <- function(sheet_number, excel_file = excel_file_path) {
    excel_data <- get_data(sheet_number = sheet_number, range = input_range, excel_file = excel_file)

    # Convert the input data to a named list
    input_params <- setNames(as.list(unlist(na.omit(excel_data))), c(
        "pm_deb",
        "ppb_deb",
        "frac_ppb_dot",
        "sortie_ppb",
        "cotis_brut",
        "chgt_cotis",
        "frais_cotis",
        "tx_prod_fin",
        "it_tech",
        "tx_pb_prest",
        "tx_pb",
        "tx_chgt_enc",
        "tx_frais_enc",
        "tx_dc",
        "tx_rach_struct",
        "tx_rach_conj",
        "tx_ech",
        "tx_chgt_prest",
        "tx_frais_prest",
        "tx_soc"
    ))
    return(input_params)
}

#--- Analysis helper functions ----
analyse_pm <- function(
    #--- PARAMETRES PAR DEFAUT ----
    pm_deb = 1000000,
    ppb_deb = 5000,
    frac_ppb_dot = .20,
    sortie_ppb = 1 / 8,
    cotis_brut = 1000,
    chgt_cotis = .05,
    frais_cotis = .04,
    tx_prod_fin = .04,
    it_tech = .01,
    tx_pb_prest = .012,
    tx_pb = .90,
    tx_chgt_enc = .005,
    tx_frais_enc = .002,
    tx_dc = .015,
    tx_rach_struct = .05,
    tx_rach_conj = .02,
    tx_ech = .005,
    tx_chgt_prest = .02,
    tx_frais_prest = .03,
    tx_soc = .128,
    n = 1L
    #-------
    ) {
    stopifnot(is.integer(n) && n >= 1) # CONTROLE RECURSION
    #### DEROULE PM ####
    #--- COTISATIONS NETTES ----
    cotis_net <- cotis_brut * (1 - chgt_cotis)

    #--- SORTIES ----
    sorties <- list(
        dc = -pm_deb * tx_dc,
        ech = -pm_deb * tx_ech,
        rach_struct = -pm_deb * tx_rach_struct,
        rach_conj = -pm_deb * tx_rach_conj,
        ic_brut_prest = -pm_deb *
            (tx_dc + tx_ech + tx_rach_struct + tx_rach_conj) *
            it_tech,
        pb_liq = pmin(
            pm_deb *
                (tx_dc + tx_ech + tx_rach_struct + tx_rach_conj) *
                (it_tech - tx_pb_prest),
            0
        ),
        chgt_enc = pm_deb *
            (tx_dc + tx_ech + tx_rach_struct + tx_rach_conj) *
            tx_chgt_enc
    )

    total_sorties <- sorties$dc + sorties$ech + sorties$rach_struct +
        sorties$rach_conj + sorties$ic_brut_prest + sorties$pb_liq +
        sorties$chgt_enc

    #--- REVALORISATION ----
    revalorisation <- list(
        ic = (pm_deb + cotis_net - pm_deb *
            (tx_dc + tx_ech + tx_rach_struct + tx_rach_conj)) *
            it_tech,
        pb = (pm_deb + ppb_deb) * tx_prod_fin * tx_pb * (1 - frac_ppb_dot) +
            sorties$ic_brut_prest + sorties$pb_liq,
        ic_pb_liq = -(sorties$ic_brut_prest + sorties$pb_liq)
    )

    total_revalorisation <- revalorisation$ic + revalorisation$pb +
        revalorisation$ic_pb_liq

    #--- CHARGEMENTS SUR ENCOURS ----
    chargements_encours <- -pmin(
        (pm_deb + cotis_net - pm_deb *
            (tx_dc + tx_ech + tx_rach_struct + tx_rach_conj)) *
            tx_chgt_enc,
        total_revalorisation
    )

    #--- CHARGEMENTS SUR PRESTATIONS ----
    chargements_prestations <- -pm_deb *
        (tx_dc + tx_ech + tx_rach_struct + tx_rach_conj) *
        tx_chgt_prest

    #--- CSG ----
    csg <- -(chargements_encours + total_revalorisation) * tx_soc
    #-------
    #### PM FIN ####
    pm_fin <- pm_deb + cotis_net + total_sorties + total_revalorisation +
        chargements_encours + chargements_prestations + csg

    #### RECURSION ####
    if (n == 1) {
        return(data.frame(
            "PM_début" = pm_deb,
            "Cotisations_nettes" = cotis_net,
            "Sorties" = total_sorties,
            #--- DETAIL SORTIES ----
            "dont_IC_bruts_sur_prestations" = sorties$ic_brut_prest,
            "dont_Décès" = sorties$dc,
            "dont_Echéances" = sorties$ech,
            "dont_Rachats_structurels" = sorties$rach_struct,
            "dont_Rachats_conjoncturels" = sorties$rach_conj,
            "dont_PB_liquidée" = sorties$pb_liq,
            "dont_Chargements_sur_encours" = sorties$chgt_enc,
            #-------
            "Revalorisation" = total_revalorisation,
            #--- DETAIL REVALORISATION ----
            "dont_IC" = revalorisation$ic,
            "dont_PB" = revalorisation$pb,
            "dont_IC_et_PB_liquidée" = revalorisation$ic_pb_liq,
            #-------
            "Chargements_sur_encours" = chargements_encours,
            "Chargements_sur_prestations" = chargements_prestations,
            "CSG" = csg,
            "PM_fin" = pm_fin
        ))
    } else {
        analyse_pm(
            #### MISE A JOUR ####
            pm_deb = pm_fin,
            ppb_deb = ppb_deb -
                ppb_deb * sortie_ppb +
                (pm_deb + ppb_deb) * tx_prod_fin * tx_pb * frac_ppb_dot,
            #--- AUTRES ARGUMENTS ----
            frac_ppb_dot = frac_ppb_dot,
            sortie_ppb = sortie_ppb,
            cotis_brut = cotis_brut,
            chgt_cotis = chgt_cotis,
            frais_cotis = frais_cotis,
            tx_prod_fin = tx_prod_fin,
            it_tech = it_tech,
            tx_pb_prest = tx_pb_prest,
            tx_pb = tx_pb,
            tx_chgt_enc = tx_chgt_enc,
            tx_frais_enc = tx_frais_enc,
            tx_dc = tx_dc,
            tx_rach_struct = tx_rach_struct,
            tx_rach_conj = tx_rach_conj,
            tx_ech = tx_ech,
            tx_chgt_prest = tx_chgt_prest,
            tx_frais_prest = tx_frais_prest,
            tx_soc = tx_soc,
            #--- RECURSION ----
            n = n - 1L
        )
    }
}

#### Analyse PM ####
context("Analyse PM")

#--- Load the input data from the Excel spreadsheet ----
input_data <- lapply(sheets, get_input_params)

#--- Test the function by comparing the output to the expected results ----
test_that("analyse_pm function output matches expected results", {
    #--- Load the expected output data from the Excel spreadsheet ----
    expected_output <- lapply(
        sheets,
        \(sheet) get_data(sheet, range = analyse_pm_range)
    )

    #--- Apply the function to the input data ----
    actual_output <- lapply(
        input_data,
        \(params) matrix(unlist(sapply(
            seq_along(years),
            \(n) do.call(analyse_pm, c(params, n = n))
        )), nrow = analyse_pm_nrow)
    )

    #--- Compare the actual output to the expected output ----

    expect_equal(length(actual_output), length(expected_output),
        info = "Number of matrices",
        tolerance = tolerance
    )

    for (i in seq_along(actual_output)) {
        actual <- actual_output[[i]]
        expected <- expected_output[[i]]

        expect_equal(nrow(actual), nrow(expected),
            info = paste("Number of rows (Matrix", i, ")"),
            tolerance = tolerance
        )
        expect_equal(ncol(actual), ncol(expected),
            info = paste("Number of columns (Matrix", i, ")"),
            tolerance = tolerance
        )

        actual_elements <- as.vector(actual)
        expected_elements <- as.vector(expected)

        expect_equal(actual_elements, expected_elements,
                             info = paste("Element of (Matrix", i, ")"),
                             tolerance = tolerance
                         )

        # for (j in seq_along(actual_elements)) {
        #     expect_equal(actual_elements[j], expected_elements[j],
        #         info = paste("Element ", j, " (Matrix", i, ")"),
        #         tolerance = tolerance
        #     )
        # }
    }
})

#### RESULTAT ANALYTIQUE ####
context("Resultat Analytique")

#--- Helper function to generate SimBEL objects ----
generate_objects <- function(params, pm_df) {
    p <- params

    passif_av_pb <- list(
        result_av_pb = list(
            flux_agg = data.frame(
                pri_chgt = p$chgt_cotis * p$cotis_brut,
                ###
                frais_var_prime = p$frais_cotis * p$cotis_brut / 2,
                frais_fixe_prime = p$frais_cotis * p$cotis_brut / 2,
                ###
                pri_brut = p$cotis_brut,
                dc = p$pm_deb * p$tx_dc,
                ech = p$pm_deb * p$tx_ech,
                arr_charg = 0,
                ###
                rach_part_struct = p$pm_deb * p$tx_rach_struct / 2,
                rach_tot_struct = p$pm_deb * p$tx_rach_struct / 2,
                ###
                rach_mass = p$pm_deb * p$tx_rach_conj / 3,
                rach_part_conj = p$pm_deb * p$tx_rach_conj / 3,
                rach_tot_conj = p$pm_deb * p$tx_rach_conj / 3,
                ###
                it_tech_prest = p$pm_deb * (p$tx_dc +
                    p$tx_ech + p$tx_rach_struct + p$tx_rach_conj) *
                    p$it_tech,
                rev_prest = pmax(p$pm_deb * (p$tx_dc +
                    p$tx_ech + p$tx_rach_struct + p$tx_rach_conj) *
                    p$tx_pb_prest, 0),
                it_tech_stock = (p$pm_deb *
                    (1 - (p$tx_dc + p$tx_ech + p$tx_rach_struct + p$tx_rach_conj)) +
                    p$cotis_brut * (1 - p$chgt_cotis)) * p$it_tech,
                enc_charg_prest = p$pm_deb *
                    (p$tx_dc + p$tx_ech + p$tx_rach_struct + p$tx_rach_conj) *
                    p$tx_chgt_enc,
                frais_var_enc = (p$pm_deb *
                    (1 - (p$tx_dc + p$tx_ech + p$tx_rach_struct + p$tx_rach_conj)) +
                    p$cotis_brut * (1 - p$chgt_cotis)) *
                    p$tx_frais_enc,
                frais_var_prest = p$pm_deb *
                    (p$tx_dc + p$tx_ech + p$tx_rach_struct + p$tx_rach_conj) *
                    p$tx_frais_prest,
                frais_fixe_enc = 0,
                frais_fixe_prest = 0,
                bes_tx_cible = 1,
                rev_stock_nette = 0
            ),
            stock_agg = data.frame(
                pm_deb = p$pm_deb,
                pm_moy = (pm_df$PM_début + pm_df$PM_fin) / 2
            )
        ),
        var_psap = 0,
        var_pgg = 0
    )

    passif_ap_pb <- list(
        flux_agg = data.frame(
            rev_stock_brut_ap_pb = pm_df$Revalorisation,
            soc_stock_ap_pb = (pm_df$Revalorisation - pmin((p$pm_deb *
                (1 - (p$tx_dc + p$tx_ech + p$tx_rach_struct + p$tx_rach_conj)) +
                p$cotis_brut * (1 - p$chgt_cotis)) *
                p$tx_chgt_enc, pm_df$Revalorisation)) * p$tx_soc
        ),
        stock_agg = data.frame(
            pm_fin_ap_pb = pm_df$PM_fin
        )
    )


    resultat_fin <- (p$pm_deb + p$ppb_deb) * p$tx_prod_fin

    ppb <- new("Ppb")
    ppb@ppb_debut <- p$ppb_deb

    result_revalo <- list(
        pb_attrib = list(
            pb_rep = p$ppb_deb * p$sortie_ppb,
            pb_dot = (p$pm_deb + p$ppb_deb) * p$tx_prod_fin * p$tx_pb * p$frac_ppb_dot
        )
    )

    var_pre <- 0

    return(list(
        passif_av_pb = passif_av_pb,
        passif_ap_pb = passif_ap_pb,
        resultat_fin = resultat_fin,
        ppb = ppb,
        result_revalo = result_revalo,
        var_pre = var_pre
    ))
}

#--- Creating objects for test for only one year ----
objects <- lapply(input_data, function(params) {
    generate_objects(params, do.call(analyse_pm, params))
})

#--- Load the expected output data from the Excel spreadsheet ----
expected_output <- lapply(sheets, function(sheet) {
    get_data(sheet, range = res_range)[res_sublevel_indices, ]
})

#--- Testing function calc_res_ana ----
test_that("calc_res_ana outputs match expected values", {
    for (i in seq_along(expected_output)) {
        actual <- unname(unlist(do.call(calc_res_ana, objects[[i]])))
        expected <- unname(unlist(expected_output[[i]]))

        expect_equal(length(actual), length(expected),
            info = paste("Length (Sheet", i, ")")
        )

        for (j in seq_along(actual)) {
            expect_equal(actual[j], expected[j],
                info = paste("Element", j, "(Sheet", i, ")"),
                tolerance = tolerance
            )
        }
    }
})
