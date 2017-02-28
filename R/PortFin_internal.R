#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "PortFin",
  definition = function(x,i){
    switch(EXPR = i,
           # Data frame Financier
           "annee"      = {return(x@annee)},
           "ptf_action" = {return(x@ptf_action)},
           "ptf_immo"   = {return(x@ptf_immo)},
           "ptf_oblig"  = {return(x@ptf_oblig)},
           "ptf_treso"  = {return(x@ptf_treso)},
           "pre"  = {return(x@pre)},
           "rc"  = {return(x@rc)},
           "frais_fin"  = {return(x@frais_fin)},
           "pvl_action" = {return(x@pvl_action)},
           "pvl_immo"   = {return(x@pvl_immo)},
           "pvl_oblig"  = {return(x@pvl_oblig)},
           "mvl_action" = {return(x@mvl_action)},
           "mvl_immo"   = {return(x@mvl_immo)},
           "mvl_oblig"  = {return(x@mvl_oblig)},
           "vm_vnc_precedent" = {return(x@vm_vnc_precedent)},
           stop("[PortFin] : Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "PortFin",
  definition = function(x,i,j,value){
    switch(EXPR = i,
           "annee"      = {x@annee <- value},
           "ptf_action" = {x@ptf_action <- value},
           "ptf_immo"   = {x@ptf_immo   <- value},
           "ptf_oblig"  = {x@ptf_oblig  <- value},
           "ptf_treso"  = {x@ptf_treso  <- value},
           "pre"  = {x@pre  <- value},
           "rc"  = {x@rc  <- value},
           "frais_fin"  = {x@frais_fin  <- value},
           "pvl_action" = {x@pvl_action<- value},
           "pvl_immo"   = {x@pvl_immo  <- value},
           "pvl_oblig"  = {x@pvl_oblig  <- value},
           "mvl_action" = {x@mvl_action<- value},
           "mvl_immo"   = {x@mvl_immo  <- value},
           "mvl_oblig"  = {x@mvl_oblig  <- value},
           "vm_vnc_precedent" = {x@vm_vnc_precedent <- value},
           stop("[PortFin] : Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Constructeur general
# #----------------------------------------------------------------------------------------------------------------------------------------------------
#
# portFin <- function(ptf=list(annee      = integer(),
#                              action     = data.frame(),
#                              immo       = data.frame(),
#                              oblig      = data.frame(),
#                              treso      = data.frame(),
#                              pvl_action = numeric(),
#                              pvl_immo   = numeric(),
#                              pvl_oblig  = numeric(),
#                              mvl_action = numeric(),
#                              mvl_immo   = numeric(),
#                              mvl_oblig  = numeric(),
#                              vm_vnc_precedent = list())){
#   ptf_action <- new("Action", ptf[["action"]])
#   ptf_immo   <- new("Immo"  , ptf[["immo"]])
#   ptf_oblig  <- new("Oblig" , ptf[["oblig"]])
#   ptf_treso  <- new("Treso" , ptf[["treso"]])
#
#   return(new( Class = "PortFin",
#               annee      = ptf[["annee"]],
#               ptf_action = ptf_action,
#               ptf_immo   = ptf_immo,
#               ptf_oblig  = ptf_oblig,
#               ptf_treso  = ptf_treso,
#               pvl_action = ptf[["pvl_action"]],
#               pvl_immo   = ptf[["pvl_immo"]],
#               pvl_oblig  = ptf[["pvl_oblig"]],
#               mvl_action = ptf[["mvl_action"]],
#               mvl_immo   = ptf[["mvl_immo"]],
#               mvl_oblig  = ptf[["mvl_oblig"]],
#               vm_vnc_precedent =  ptf[["vm_vnc_precedent"]]))}
#

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur du portefeuille de reference
#----------------------------------------------------------------------------------------------------------------------------------------------------
# # folder_PortFin_reference_address <- "Z:/02 - Missions/OUTIL BE PRIMACT/06_Env_Dev/actif/inputs/Portefeuille_reference"
# # Fonction de chargement general des elements de l'ESG
# setGeneric(name = "chargement_PortFin_reference",function(folder_PortFin_reference_address, mp_ESG){standardGeneric("chargement_PortFin_reference")})
# setMethod(
#   f = "chargement_PortFin_reference",
#   signature = c(folder_PortFin_reference_address = "character", mp_ESG = "ModelPointESG"),
#   definition = function(folder_PortFin_reference_address, mp_ESG){
# 
#     # Nom des fichiers
#     # La colonne 1 contient le type,
#     # La colonne 2 contient le nom du fichier,
#     file_name           <- read.csv2(paste(folder_PortFin_reference_address, "/noms_fichiers.csv", sep = ""), header = T)[1:3,1:2]
#     Data_ref_Action     <- read.csv2(paste(folder_PortFin_reference_address, file_name[1,2], sep = "/"))
#     Data_ref_Immo       <- read.csv2(paste(folder_PortFin_reference_address, file_name[2,2], sep = "/"))
#     Data_ref_Oblig      <- read.csv2(paste(folder_PortFin_reference_address, file_name[3,2], sep = "/"))
# 
#     # Construction du portefeuille de reference action
#     ptf_action <- new("Action", ptf = data.frame(num_mp     = as.integer(Data_ref_Action[,"num_mp"]),
#                                                  val_marche = as.double(Data_ref_Action[,"val_unitaire_achat"]),
#                                                  val_nc     = as.double(Data_ref_Action[,"val_unitaire_achat"]),
#                                                  val_achat  = as.double(Data_ref_Action[,"val_unitaire_achat"]),
#                                                  presence   = as.logical(rep(TRUE, nrow(Data_ref_Action))),
#                                                  cessible   = as.logical(rep(TRUE, nrow(Data_ref_Action))),
#                                                  nb_unit    = as.double(Data_ref_Action[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
#                                                  dur_det    = as.double(rep(0, nrow(Data_ref_Action))),
#                                                  pdd        = as.double(rep(0, nrow(Data_ref_Action))),
#                                                  num_index  = as.integer(Data_ref_Action[,"num_index"]),
#                                                  div        = as.double(Data_ref_Action[,"div"]),
#                                                  ind_invest = as.logical(Data_ref_Action[,"ind_invest"])))
# 
#     # Construction du portefeuille de reference Immo
#     ptf_immo <- new("Immo", ptf = data.frame(num_mp     = as.integer(Data_ref_Immo[,"num_mp"]),
#                                              val_marche = as.double(Data_ref_Immo[,"val_unitaire_achat"]),
#                                              val_nc     = as.double(Data_ref_Immo[,"val_unitaire_achat"]),
#                                              val_achat  = as.double(Data_ref_Immo[,"val_unitaire_achat"]),
#                                              presence   = as.logical(rep(TRUE, nrow(Data_ref_Immo))),
#                                              cessible   = as.logical(rep(TRUE, nrow(Data_ref_Immo))),
#                                              nb_unit    = as.double(Data_ref_Immo[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
#                                              dur_det    = as.double(rep(0, nrow(Data_ref_Immo))),
#                                              pdd        = as.double(rep(0, nrow(Data_ref_Immo))),
#                                              num_index  = as.integer(Data_ref_Immo[,"num_index"]),
#                                              loyer      = as.double(Data_ref_Immo[,"loyer"]),
#                                              ind_invest = as.logical(Data_ref_Immo[,"ind_invest"])))
# 
#     # Construction du portefeuille de reference Immo
#     ptf_oblig <- new("Oblig", ptf = data.frame(num_mp     = as.integer(Data_ref_Oblig[,"num_mp"]),
#                                               val_marche = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
#                                               val_nc     = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
#                                               val_achat  = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
#                                               presence   = as.logical(rep(TRUE, nrow(Data_ref_Oblig))),
#                                               cessible   = as.logical(rep(TRUE, nrow(Data_ref_Oblig))),
#                                               nb_unit    = as.double(Data_ref_Oblig[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
#                                               dur_det    = as.double(rep(0, nrow(Data_ref_Oblig))),
#                                               nominal    = as.double(Data_ref_Oblig[,"nominal_unitaire"]),
#                                               tx_coupon  = as.double(Data_ref_Oblig[,"tx_coupon"]),
#                                               par        = as.double(Data_ref_Oblig[,"parite"]),
#                                               mat_res    = as.double(Data_ref_Oblig[,"mat_res"]),
#                                               type       = as.factor(Data_ref_Oblig[,"type"]),
#                                               rating     = as.integer(Data_ref_Oblig[,"rating"]),
#                                               duration   = as.double(rep(0, nrow(Data_ref_Oblig))),
#                                               zspread    = as.double(rep(0, nrow(Data_ref_Oblig))),
#                                               cc         = as.double(rep(0, nrow(Data_ref_Oblig))),
#                                               sd         = as.double(rep(0, nrow(Data_ref_Oblig)))))
# 
#     # Recalcul Dur/Zsp/CC/SD du ptf_oblig
#     ptf_oblig["ptf_oblig"][,"cc"]       <- calc_coupon(ptf_oblig)
#     ptf_oblig["ptf_oblig"][,"zspread"]  <- calc_z_spread(ptf_oblig, mp_ESG["yield_curve"])
#     ptf_oblig["ptf_oblig"][,"sd"]       <- calc_sur_dec_vnc(ptf_oblig)[,"surcote_decote"]
#     ptf_oblig["ptf_oblig"][,"duration"] <- duration_sensi(ptf_oblig)[, "duration"]
# 
# 
#     x <- new("PortFin", annee = as.integer(0),
#                         ptf_action = ptf_action,
#                         ptf_immo   = ptf_immo,
#                         ptf_oblig  = ptf_oblig,
#                         ptf_treso  = new("Treso"),
#                         pvl_action = 0,
#                         pvl_immo   = 0,
#                         pvl_oblig  = 0,
#                         mvl_action = 0,
#                         mvl_immo   = 0,
#                         mvl_oblig  = 0,
#                         vm_vnc_precedent = list(vm = list(action = sum(ptf_action$val_marche),
#                                                           immo   = sum(ptf_immo$val_marche),
#                                                           oblig  = sum(ptf_oblig$val_marche)),
#                                                 vnc = list(action = sum(ptf_action$val_nc),
#                                                            immo   = sum(ptf_immo$val_nc),
#                                                            oblig  = sum(ptf_oblig$val_nc))))
#     return(x)
#   }
# )
