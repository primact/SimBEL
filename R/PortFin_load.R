# Cette fonction permettra a l'instar de la fonctionalite load de charger les inputs portefeuille de reference
# et de reconstruire le portefeuille de reference a partir de ces dernieres

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur du portefeuille d'actif
#----------------------------------------------------------------------------------------------------------------------------------------------------
# folder_PortFin_address <- "Z:/02 - Missions/OUTIL BE PRIMACT/06_Env_Dev/actif/inputs"
# Fonction de chargement general des elements de l'ESG
setGeneric(name = "chargement_PortFin",function(folder_PortFin_address, mp_ESG){standardGeneric("chargement_PortFin")})
setMethod(
    f = "chargement_PortFin",
    signature = c(folder_PortFin_address = "character", mp_ESG = "ModelPointESG"),
    definition = function(folder_PortFin_address, mp_ESG){

        # Nom des fichiers
        # La colonne 1 contient le type,
        # La colonne 2 contient le nom du fichier,
        file_name       <- read.csv2(paste(folder_PortFin_address, "/noms_fichiers.csv", sep = ""), header = T)
        Data_Action     <- read.csv2(paste(folder_PortFin_address, file_name[1,2], sep = "/"))
        Data_Immo       <- read.csv2(paste(folder_PortFin_address, file_name[2,2], sep = "/"))
        Data_Oblig      <- read.csv2(paste(folder_PortFin_address, file_name[3,2], sep = "/"))
        Data_Treso      <- read.csv2(paste(folder_PortFin_address, file_name[4,2], sep = "/"))

        # Chargement du portefeuille action
        ptf_action <- new("Action", ptf = data.frame(num_mp     = as.integer(Data_Action[,"num_mp"]),
                                                     val_marche = as.double(Data_Action[,"val_marche"]),
                                                     val_nc     = as.double(Data_Action[,"val_nc"]),
                                                     val_achat  = as.double(Data_Action[,"val_achat"]),
                                                     presence   = as.logical(Data_Action[,"presence"]),
                                                     cessible   = as.logical(Data_Action[,"cessible"]),
                                                     nb_unit    = as.double(Data_Action[,"nb_unit"]),
                                                     dur_det    = as.double(Data_Action[,"dur_det"]),
                                                     pdd        = as.double(Data_Action[,"pdd"]),
                                                     num_index  = as.integer(Data_Action[,"num_index"]),
                                                     div        = as.double(Data_Action[,"div"]),
                                                     ind_invest = as.logical(Data_Action[,"ind_invest"])))
        # Verification input action
        if(sum(ptf_action@ptf_action$val_marche <= 0) > 0 | sum(ptf_action@ptf_action$nb_unit <= 0) > 0)
          stop("[chargement_PortFin] : Le portefeuille Action ne peut contenir de ligne dont
               la valeur de marche ou le nombre d'unite est negatif ou nul. \n")

        # Chargement du portefeuille Immo
        ptf_immo <- new("Immo", ptf = data.frame(num_mp     = as.integer(Data_Immo[,"num_mp"]),
                                                 val_marche = as.double(Data_Immo[,"val_marche"]),
                                                 val_nc     = as.double(Data_Immo[,"val_nc"]),
                                                 val_achat  = as.double(Data_Immo[,"val_achat"]),
                                                 presence   = as.logical(Data_Immo[,"presence"]),
                                                 cessible   = as.logical(Data_Immo[,"cessible"]),
                                                 nb_unit    = as.double(Data_Immo[,"nb_unit"]),
                                                 dur_det    = as.double(Data_Immo[,"dur_det"]),
                                                 pdd        = as.double(Data_Immo[,"pdd"]),
                                                 num_index  = as.integer(Data_Immo[,"num_index"]),
                                                 loyer      = as.double(Data_Immo[,"loyer"]),
                                                 ind_invest = as.logical(Data_Immo[,"ind_invest"])))

        # Verification input immo
        if(sum(ptf_immo@ptf_immo$val_marche <= 0) > 0 | sum(ptf_immo@ptf_immo$nb_unit <= 0) > 0)
          stop("[chargement_PortFin] : Le portefeuille Immo ne peut contenir
               de ligne dont la valeur de marche ou le nombre d'unite est negatif ou nul. \n")

        # Chargement du portefeuille Oblig
        ptf_oblig <- new("Oblig", ptf = data.frame(num_mp     = as.integer(Data_Oblig[,"num_mp"]),
                                                   val_marche = as.double(Data_Oblig[,"val_marche"]),
                                                   val_nc     = as.double(Data_Oblig[,"val_nc"]),
                                                   val_achat  = as.double(Data_Oblig[,"val_achat"]),
                                                   presence   = as.logical(Data_Oblig[,"presence"]),
                                                   cessible   = as.logical(Data_Oblig[,"cessible"]),
                                                   nb_unit    = as.double(Data_Oblig[,"nb_unit"]),
                                                   dur_det    = as.double(Data_Oblig[,"dur_det"]),
                                                   nominal    = as.double(Data_Oblig[,"nominal"]),
                                                   tx_coupon  = as.double(Data_Oblig[,"tx_coupon"]),
                                                   par        = as.double(Data_Oblig[,"par"]),
                                                   mat_res    = as.double(Data_Oblig[,"mat_res"]),
                                                   type       = as.factor(Data_Oblig[,"type"]),
                                                   rating     = as.integer(Data_Oblig[,"rating"]),
                                                   duration   = as.double(Data_Oblig[,"duration"]),
                                                   zspread    = as.double(Data_Oblig[,"zspread"]),
                                                   cc         = as.double(Data_Oblig[,"cc"]),
                                                   sd         = as.double(Data_Oblig[,"sd"])))
        # Verification input oblig
        if(sum(ptf_oblig@ptf_oblig$val_marche <= 0) > 0 | sum(ptf_oblig@ptf_oblig$nb_unit <= 0) > 0)
          stop("[chargement_PortFin] : Le portefeuille Oblig ne peut contenir de ligne
               dont la valeur de marche ou le nombre d'unite est negatif ou nul. \n")


        # Chargement du portefeuille Treso
        ptf_treso <- new("Treso", ptf = data.frame(num_mp     = as.integer(Data_Treso[,"num_mp"]),
                                                   val_marche = as.double(Data_Treso[,"val_marche"]),
                                                   val_nc     = as.double(Data_Treso[,"val_nc"])))
        # Chargement des PRE/RC et Frais fin
        file_PRE_address    <- paste(folder_PortFin_address, file_name[5,2], sep = "/")
        file_RC_address     <- paste(folder_PortFin_address, file_name[6,2], sep = "/")
        file_frais_fin_address  <- paste(folder_PortFin_address, file_name[7,2], sep = "/")

        pre       <- pre_load(file_PRE_address)
        rc        <- rc_load(file_RC_address)
        frais_fin <- frais_fin_load(file_frais_fin_address)

        x <- new("PortFin", annee = as.integer(0),
                 ptf_action = ptf_action,
                 ptf_immo   = ptf_immo,
                 ptf_oblig  = ptf_oblig,
                 ptf_treso  = ptf_treso,
                 pre        = pre,
                 rc         = rc,
                 frais_fin  = frais_fin,
                 pvl_action = 0,
                 pvl_immo   = 0,
                 pvl_oblig  = 0,
                 mvl_action = 0,
                 mvl_immo   = 0,
                 mvl_oblig  = 0,
                 vm_vnc_precedent = list(vm = list(action = sum(ptf_action@ptf_action$val_marche),
                                                   immo   = sum(ptf_immo@ptf_immo$val_marche),
                                                   oblig  = sum(ptf_oblig@ptf_oblig$val_marche)),
                                         vnc = list(action = sum(ptf_action@ptf_action$val_nc),
                                                    immo   = sum(ptf_immo@ptf_immo$val_nc),
                                                    oblig  = sum(ptf_oblig@ptf_oblig$val_nc))))
        # Recalcul des pmvl
        x <- calc_pmvl(x)
        # Recalcul des surcote decote
        x@ptf_oblig <- update_sd_oblig(x@ptf_oblig, calc_sur_dec(x@ptf_oblig))
        return(x)
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur du portefeuille de reference
#----------------------------------------------------------------------------------------------------------------------------------------------------
# folder_PortFin_reference_address <- "Z:/02 - Missions/OUTIL BE PRIMACT/06_Env_Dev/actif/inputs/Portefeuille_reference"
# Fonction de chargement general des elements de l'ESG
setGeneric(name = "chargement_PortFin_reference",function(folder_PortFin_reference_address, mp_ESG){standardGeneric("chargement_PortFin_reference")})
setMethod(
    f = "chargement_PortFin_reference",
    signature = c(folder_PortFin_reference_address = "character", mp_ESG = "ModelPointESG"),
    definition = function(folder_PortFin_reference_address, mp_ESG){

        # Nom des fichiers
        # La colonne 1 contient le type,
        # La colonne 2 contient le nom du fichier,
        file_name           <- read.csv2(paste(folder_PortFin_reference_address, "/noms_fichiers_reference.csv", sep = ""), header = T)[1:3,1:2]
        Data_ref_Action     <- read.csv2(paste(folder_PortFin_reference_address, file_name[1,2], sep = "/"))
        Data_ref_Immo       <- read.csv2(paste(folder_PortFin_reference_address, file_name[2,2], sep = "/"))
        Data_ref_Oblig      <- read.csv2(paste(folder_PortFin_reference_address, file_name[3,2], sep = "/"))

        # Construction du portefeuille de reference action
        ptf_action <- new("Action", ptf = data.frame(num_mp     = as.integer(Data_ref_Action[,"num_mp"]),
                                                     val_marche = as.double(Data_ref_Action[,"val_unitaire_achat"]),
                                                     val_nc     = as.double(Data_ref_Action[,"val_unitaire_achat"]),
                                                     val_achat  = as.double(Data_ref_Action[,"val_unitaire_achat"]),
                                                     presence   = as.logical(rep(TRUE, nrow(Data_ref_Action))),
                                                     cessible   = as.logical(rep(TRUE, nrow(Data_ref_Action))),
                                                     nb_unit    = as.double(Data_ref_Action[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
                                                     dur_det    = as.double(rep(0, nrow(Data_ref_Action))),
                                                     pdd        = as.double(rep(0, nrow(Data_ref_Action))),
                                                     num_index  = as.integer(Data_ref_Action[,"num_index"]),
                                                     div        = as.double(Data_ref_Action[,"div"]),
                                                     ind_invest = as.logical(Data_ref_Action[,"ind_invest"])))
        # Verification input action
        if(sum(ptf_action@ptf_action$val_marche <= 0) > 0 | sum(ptf_action@ptf_action$nb_unit <= 0) > 0) stop("[chargement_PortFin_reference] : Le portefeuille Action de reference ne peut contenir de ligne dont la valeur nominale unitaire est nulle. \n")

        # Construction du portefeuille de reference Immo
        ptf_immo <- new("Immo", ptf = data.frame(num_mp     = as.integer(Data_ref_Immo[,"num_mp"]),
                                                 val_marche = as.double(Data_ref_Immo[,"val_unitaire_achat"]),
                                                 val_nc     = as.double(Data_ref_Immo[,"val_unitaire_achat"]),
                                                 val_achat  = as.double(Data_ref_Immo[,"val_unitaire_achat"]),
                                                 presence   = as.logical(rep(TRUE, nrow(Data_ref_Immo))),
                                                 cessible   = as.logical(rep(TRUE, nrow(Data_ref_Immo))),
                                                 nb_unit    = as.double(Data_ref_Immo[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
                                                 dur_det    = as.double(rep(0, nrow(Data_ref_Immo))),
                                                 pdd        = as.double(rep(0, nrow(Data_ref_Immo))),
                                                 num_index  = as.integer(Data_ref_Immo[,"num_index"]),
                                                 loyer      = as.double(Data_ref_Immo[,"loyer"]),
                                                 ind_invest = as.logical(Data_ref_Immo[,"ind_invest"])))

        # Verification input immo
        if(sum(ptf_immo@ptf_immo$val_marche <= 0) > 0 | sum(ptf_immo@ptf_immo$nb_unit <= 0) > 0) stop("[chargement_PortFin_reference] : Le portefeuille Immo de reference ne peut contenir de ligne dont la valeur nominale unitaire est nulle. \n")


        # Construction du portefeuille de reference Immo
        ptf_oblig <- new("Oblig", ptf = data.frame(num_mp     = as.integer(Data_ref_Oblig[,"num_mp"]),
                                                   val_marche = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
                                                   val_nc     = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
                                                   val_achat  = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
                                                   presence   = as.logical(rep(TRUE, nrow(Data_ref_Oblig))),
                                                   cessible   = as.logical(rep(TRUE, nrow(Data_ref_Oblig))),
                                                   nb_unit    = as.double(Data_ref_Oblig[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
                                                   dur_det    = as.double(rep(0, nrow(Data_ref_Oblig))),
                                                   nominal    = as.double(Data_ref_Oblig[,"nominal_unitaire"]),
                                                   tx_coupon  = as.double(Data_ref_Oblig[,"tx_coupon"]),
                                                   par        = as.double(Data_ref_Oblig[,"parite"]),
                                                   mat_res    = as.double(Data_ref_Oblig[,"mat_res"]),
                                                   type       = as.factor(Data_ref_Oblig[,"type"]),
                                                   rating     = as.integer(Data_ref_Oblig[,"rating"]),
                                                   duration   = as.double(rep(0, nrow(Data_ref_Oblig))),
                                                   zspread    = as.double(rep(0, nrow(Data_ref_Oblig))),
                                                   cc         = as.double(rep(0, nrow(Data_ref_Oblig))),
                                                   sd         = as.double(rep(0, nrow(Data_ref_Oblig)))))

        # Verification input oblig
        if(sum(ptf_oblig@ptf_oblig$val_marche <= 0) > 0 | sum(ptf_oblig@ptf_oblig$nb_unit <= 0) > 0) stop("[chargement_PortFin_reference] : Le portefeuille Oblig de reference ne peut contenir de ligne dont la valeur nominale unitaire est nulle. \n")


        # Recalcul Dur/Zsp/CC/SD du ptf_oblig
        ptf_oblig["ptf_oblig"][,"cc"]       <- calc_coupon(ptf_oblig)
        ptf_oblig["ptf_oblig"][,"zspread"]  <- calc_z_spread(ptf_oblig, mp_ESG["yield_curve"])
        ptf_oblig["ptf_oblig"][,"sd"]       <- calc_sur_dec(ptf_oblig)
        ptf_oblig["ptf_oblig"][,"duration"] <- duration_sensi(ptf_oblig)[, "duration"]

        x <- new("PortFin", annee = as.integer(0),
                 ptf_action = ptf_action,
                 ptf_immo   = ptf_immo,
                 ptf_oblig  = ptf_oblig,
                 ptf_treso  = new("Treso"),
                 pre        = new("PRE"),
                 rc         = new("RC"),
                 frais_fin  = new("FraisFin"),
                 pvl_action = 0,
                 pvl_immo   = 0,
                 pvl_oblig  = 0,
                 mvl_action = 0,
                 mvl_immo   = 0,
                 mvl_oblig  = 0,
                 vm_vnc_precedent = list(vm = list(action = sum(ptf_action@ptf_action$val_marche),
                                                   immo   = sum(ptf_immo@ptf_immo$val_marche),
                                                   oblig  = sum(ptf_oblig@ptf_oblig$val_marche)),
                                         vnc = list(action = sum(ptf_action@ptf_action$val_nc),
                                                    immo   = sum(ptf_immo@ptf_immo$val_nc),
                                                    oblig  = sum(ptf_oblig@ptf_oblig$val_nc))))
        return(x)
    }
)
