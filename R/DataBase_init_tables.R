#----------------------------------------------------------------------------------------------------------------------------------------------------
#           init_tables
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Intialise les tables de la base de donnees. Cette fonction initialise les differentes tables de la base.
##' Chacune des tables possede 4 colonnes : num_sim, num_an, prod et flux.
##'
##' @name init_tables
##' @docType methods
##' @param x est un objet de type \code{\link{DataBase}}.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "init_tables", def = function(x) {standardGeneric("init_tables")})
setMethod(
  f = "init_tables",
  signature = c(x = "DataBase"),
  definition = function(x){

    ## Suppression des tables existantes
    sapply(dbListTables(x@database), function(t) dbRemoveTable(x@database, t))

    ## Creation des differentes tables
    # 1 - Table OUTPUT_BE
    dbSendQuery(conn = x@database,
                "CREATE TABLE OUTPUT_BE
                    (num_sim INTEGER,
                    annee INTEGER,
                    prod VARCHAR(20),
                    prime REAL,
                    frais REAL,
                    prestation REAL,
                    prestation_fdb REAL)")

    # 2 - Table OUTPUT_VAN_AGG
    dbSendQuery(conn = x@database,
                "CREATE TABLE OUTPUT_VAN_AGG
                    (num_sim INTEGER,
                    annee INTEGER,
                    prime REAL,
                    frais REAL,
                    prestation REAL,
                    prestation_fdb REAL,
                    result_tech REAL,
                    result_fin REAL,
                    result_brut REAL,
                    result_net REAL)")

    # 3 - Table OUTPUT_PRODUIT
    dbSendQuery(conn = x@database,
                "CREATE TABLE OUTPUT_PRODUIT
                    (num_sim INTEGER,
                    annee INTEGER,
                    prod VARCHAR(20),
                    pri_brut REAL,
                    pri_net REAL,
                    pri_chgt REAL,
                    ech REAL,
                    rach_mass REAL,
                    rach_tot REAL,
                    dc REAL,
                    rach_part REAL,
                    rente REAL,
                    prest REAL,
                    rev_ech REAL,
                    rev_rach_tot REAL,
                    rev_dc REAL,
                    rev_rach_part REAL,
                    rev_prest REAL,
                    rev_prest_nette REAL,
                    enc_charg_prest REAL,
                    rach_charg REAL,
                    rach_charg_mass REAL,
                    soc_prest REAL,
                    it_tech_prest REAL,
                    arr_charg REAL,
                    rev_stock_brut REAL,
                    rev_stock_nette REAL,
                    enc_charg_stock REAL,
                    enc_charg_base_th REAL,
                    enc_charg_rmin_th REAL,
                    base_enc_th REAL,
                    soc_stock REAL,
                    it_tech_stock REAL,
                    it_tech REAL,
                    bes_tx_cible REAL,
                    prest_fdb REAL,
                    frais_fixe_prime REAL,
                    frais_var_prime REAL,
                    frais_fixe_prest REAL,
                    frais_var_prest REAL,
                    frais_fixe_enc REAL,
                    frais_var_enc REAL,
                    bes_tmg_prest REAL,
                    bes_tmg_stock REAL,
                    rev_stock_brut_ap_pb REAL,
                    rev_stock_nette_ap_pb REAL,
                    enc_charg_stock_ap_pb REAL,
                    soc_stock_ap_pb REAL,
                    frais_fin REAL,
                    nb_vers REAL,
                    nb_ech REAL,
                    nb_rach_mass REAL,
                    nb_rach_tot REAL,
                    nb_dc REAL,
                    nb_debut REAL,
                    nb_sortie REAL,
                    nb_contr_fin REAL,
                    nb_contr_moy REAL,
                    pm_deb REAL,
                    pm_fin REAL,
                    pm_moy REAL,
                    pm_fin_ap_pb REAL,
                    flux_fin_passif REAL,
                    ppb8 REAL,
                    chgt REAL,
                    delta_pm REAL,
                    frais REAL,
                    credit REAL,
                    debit REAL)")

    # 4 - Table OUTPUT_PRODUIT_AGG
    dbSendQuery(conn = x@database,
                "CREATE TABLE OUTPUT_PRODUIT_AGG
                    (num_sim INTEGER,
                    annee INTEGER,
                    pri_brut REAL,
                    pri_net REAL,
                    pri_chgt REAL,
                    ech REAL,
                    rach_mass REAL,
                    rach_tot REAL,
                    dc REAL,
                    rach_part REAL,
                    rente REAL,
                    prest REAL,
                    rev_ech REAL,
                    rev_rach_tot REAL,
                    rev_dc REAL,
                    rev_rach_part REAL,
                    rev_prest REAL,
                    rev_prest_nette REAL,
                    enc_charg_prest REAL,
                    rach_charg REAL,
                    rach_charg_mass REAL,
                    soc_prest REAL,
                    it_tech_prest REAL,
                    arr_charg REAL,
                    rev_stock_brut REAL,
                    rev_stock_nette REAL,
                    enc_charg_stock REAL,
                    enc_charg_base_th REAL,
                    enc_charg_rmin_th REAL,
                    base_enc_th REAL,
                    soc_stock REAL,
                    it_tech_stock REAL,
                    it_tech REAL,
                    bes_tx_cible REAL,
                    prest_fdb REAL,
                    frais_fixe_prime REAL,
                    frais_var_prime REAL,
                    frais_fixe_prest REAL,
                    frais_var_prest REAL,
                    frais_fixe_enc REAL,
                    frais_var_enc REAL,
                    bes_tmg_prest REAL,
                    bes_tmg_stock REAL,
                    rev_stock_brut_ap_pb REAL,
                    rev_stock_nette_ap_pb REAL,
                    enc_charg_stock_ap_pb REAL,
                    soc_stock_ap_pb REAL,
                    frais_fin REAL,
                    nb_vers REAL,
                    nb_ech REAL,
                    nb_rach_mass REAL,
                    nb_rach_tot REAL,
                    nb_dc REAL,
                    nb_debut REAL,
                    nb_sortie REAL,
                    nb_contr_fin REAL,
                    nb_contr_moy REAL,
                    pm_deb REAL,
                    pm_fin REAL,
                    pm_moy REAL,
                    pm_fin_ap_pb REAL,
                    flux_fin_passif REAL,
                    ppb8 REAL,
                    chgt REAL,
                    delta_pm REAL,
                    frais REAL,
                    credit REAL,
                    debit REAL,
                    resultat REAL,
                    result_tech REAL,
                    result_fin REAL,
                    result_brut REAL,
                    result_net REAL)")

    # 5 - Table des produits non modelisees
    dbSendQuery(conn = x@database,
                "CREATE TABLE HORS_MODEL
                    (num_sim INTEGER,
                    prod VARCHAR(20),
                    annee INTEGER,
                    prime REAL,
                    prestation REAL,
                    frais REAL,
                    pm_deb REAL,
                    pm_fin REAL,
                    it REAL)")

    # 6 - Table des BE
    dbSendQuery(conn = x@database,
                "CREATE TABLE BE
                    (num_sim INTEGER,
                    prod VARCHAR(20),
                    be REAL)")

    # 7 - Table ACTIF
    dbSendQuery(conn = x@database,
                "CREATE TABLE ACTIF
                    (num_sim INTEGER,
                    annee INTEGER,
                    actif VARCHAR(20),
                    num_mp INTEGER,
                    val_marche REAL,
                    val_nc REAL,
                    val_achat REAL,
                    presence REAL,
                    cessible REAL,
                    nb_unit REAL,
                    dur_det REAL,
                    pdd REAL,
                    num_index INTEGER,
                    div REAL,
                    ind_invest REAL,
                    currency VARCHAR(20),
                    fx_rate REAL,
                    loyer REAL,
                    nominal REAL,
                    tx_coupon REAL,
                    par REAL,
                    mat_res REAL,
                    type REAL,
                    rating REAL,
                    duration REAL,
                    zspread REAL,
                    cc REAL,
                    sd REAL)")

    # 8 - Table FLUX_FIN
    dbSendQuery(conn = x@database,
                "CREATE TABLE FLUX_FIN
                    (num_sim INTEGER,
                    annee INTEGER,
                    revenu_oblig REAL,
                    revenu_action REAL,
                    revenu_immo REAL,
                    var_vnc_oblig REAL,
                    pmvr_oblig REAL,
                    pmvr_action REAL,
                    pmvr_immo REAL,
                    frais_fin REAL,
                    var_rc REAL)")

    # 9 - Table PB
    dbSendQuery(conn = x@database,
                "CREATE TABLE PB
                    (num_sim INTEGER,
                    annee INTEGER,
                    ppb8 REAL,
                    stock_ppb REAL,
                    tot_pb_rep REAL,
                    tot_pb_dot REAL,
                    diff_pb REAL)")

    # Output
    # return(x)
  }
)
