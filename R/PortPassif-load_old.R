# #----------------------------------------------------------
# # Main
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 06/02/2017. Fait par MT
# #----------------------------------------------------------
# # Tout supprimer
# rm(list = ls())
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Definition des chemins
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# scripts <- paste("Z:/02 - Missions/OUTIL BE PRIMACT/06_Env_Dev/passif/R",sep="")
# inputs <- paste("Z:/02 - Missions/OUTIL BE PRIMACT/06_Env_Dev/passif/inputs",sep="")
# sortie <- paste("Z:/02 - Missions/OUTIL BE PRIMACT/06_Env_Dev/passif/sorties",sep="")
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #          Chargement des differentes classes
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #chargement des inputs HyptTech
# # Chargement de la classe HYPTEch
# source(paste(scripts,"HypTech-class.R",sep="/"))
# source(paste(scripts,"HypTech-internal.R",sep="/"))
# source(paste(scripts,"HypTech-get_qx_rach.R",sep="/"))
# source(paste(scripts,"HypTech-get_qx_mort.R",sep="/"))
# source(paste(scripts,"HypTech-get_rach_dyn.R",sep="/"))
# # Chargement de la classe ParamTableMort
# source(paste(scripts,"ParamTableMort-class.R",sep="/"))
# source(paste(scripts,"ParamTableMort-internal.R",sep="/"))
# source(paste(scripts,"ParamTableMort-calc_qx.R",sep="/"))
# # Chargement de la classe ParamTableRach
# source(paste(scripts,"ParamTableRach-class.R",sep="/"))
# source(paste(scripts,"ParamTableRach-internal.R",sep="/"))
# source(paste(scripts,"ParamTableRach-calc_rach.R",sep="/"))
# # Chargement  de la classe ParamRachDyn
# source(paste(scripts,"ParamRachDyn-class.R",sep="/"))
# source(paste(scripts,"ParamRachDyn-internal.R",sep="/"))
# source(paste(scripts,"ParamRachDyn-calc_rach_dyn.R",sep="/"))
# # Chargement de la classe EpEuroInd
# source(paste(scripts,"EpEuroInd-class.R",sep="/"))
# source(paste(scripts,"EpEuroInd-internal.R",sep="/"))
# source(paste(scripts,"EpEuroInd-vieilli_mp.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_tx_min.R",sep="/"))
# source(paste(scripts,"taux_period-function.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_primes.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_tx_cible.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_tx_sortie.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_prest.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_pm.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_bes_tx_cible.R",sep="/"))
# source(paste(scripts,"EpEuroInd-calc_revalo_pm.R",sep="/"))
# source(paste(scripts,"EpEuroInd-maj_mp.R",sep="/"))
# # Chargement de la classe FraisPassif
# source(paste(scripts,"FraisPassif-class.R",sep="/"))
# source(paste(scripts,"FraisPassif-internal.R",sep="/"))
# # Chargement du script de test de la classe PortPassif
# source(paste(scripts,"PortPassif-class.R",sep="/"))
# source(paste(scripts,"PortPassif-internal.R",sep="/"))
# source(paste(scripts,"PortPassif-calc_primes_pp.R",sep="/"))
# source(paste(scripts,"PortPassif-calc_prest_pp.R",sep="/"))
# source(paste(scripts,"PortPassif-calc_pm_pp.R",sep="/"))
# source(paste(scripts,"PortPassif-calc_bes_tx_cible_pp.R",sep="/"))
# source(paste(scripts,"PortPassif-calc_revalo_pm_pp.R",sep="/"))
# source(paste(scripts,"PortPassif-maj_mp_pp.R",sep="/"))
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #          Chargement des fichiers d'input
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# # Tables de mortalite
# tm1 <- read.csv2(paste(inputs,"tf_02_dc.csv",sep="/"),header = TRUE)
# # Tables de rachat
# trp1 <- read.csv2(paste(inputs, "trp1.csv", sep = "/"), header = TRUE)
# trt1 <- read.csv2(paste(inputs, "trt1.csv", sep = "/"), header = TRUE)
# # Tables de parametres de rachats dynamique
# prcp1=read.csv2(paste(inputs,"prcp1.csv",sep="/"),header = TRUE)
# prct1=read.csv2(paste(inputs,"prcp1.csv",sep="/"),header = TRUE)
# # Fichier Epargne
# ep_euro_csv <- read.csv2(paste(inputs,"ep_euro.csv",sep="/"),header = TRUE)
# # Ficher frais passif
# frais_passif_csv=read.csv2(paste(inputs,"frais_passif.csv",sep="/"),header = TRUE)
# #valeur en dur
# tx_soc=0.155
# an = 0
# list_rd = list(rdt_action = 0.1, rdt_immo = 0.1, rdt_tre = 0.1, rdt_oblig = 0.1)
# list_param_mar = list(mat_oblig = 5, alloc_mar = c(0.2,0.2,0.3,0.3), w_n = 0.5, marge_mar = 0, ch_enc_mar = 0, ind_ref_action = 1, ind_ref_immo = 1)
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #          Instanciation des differentes classes
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# TM1 <- new(Class ="ParamTableMort",tm1)
# TRT1 <- new(Class ="ParamTableRach", trt1)
# TRP1 <- new(Class ="ParamTableRach", trp1)
# PRCP1=new(Class="ParamRachDyn",data.frame(prcp1))
# PRCT1=new(Class="ParamRachDyn",data.frame(prct1))
# HT1=new(Class="HypTech",list(tm1=TM1),list(trt1=TRT1,trp1=TRP1),list(prcp1=PRCP1,prct1=PRCT1))
# frais_passif <- new(Class = "FraisPassif", frais_passif_csv)
# ep_euro <- new(Class = "EpEuroInd", ep_euro_csv)
#
# pp <- new(Class = "PortPassif", list(ep_euro1 = ep_euro,ep_euro2 = ep_euro), HT1, frais_passif)
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #          Calcul
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# # Calcul des primes
# tab_prime_pp <- calc_primes_pp(pp, mt_ap = 100)
# # Calcul des prestations
# tab_prest_pp <- calc_prest_pp(pp, an , method = "normal", tx_soc, mt_ap = 100)
# # Calcul des pm
# tab_pm_pp <- calc_pm_pp(pp,tab_prime_pp, tab_prest_pp, an , method = "normal", tx_soc, mt_ap = 100)
# # Calcul du besoin en taux cible
# bes_tx_cible_pp=calc_bes_tx_cible_pp(pp,tab_prime_pp, tab_prest_pp, tab_pm_pp)
# # Calcul de la pm apres revalo
# revalo_pm_ap_pb_pp=calc_revalo_pm_pp(pp, rev_net_alloue_pp = c(100,100), bes_tx_cible_pp, tab_pm_pp, tab_prime_pp, tab_prest_pp, an , tx_soc)
# # Mise a jour du passif
# pp_1an <- maj_mp_pp(pp, tab_pm_pp, tab_prime_pp,  tab_prest_pp, revalo_pm_ap_pb_pp)
