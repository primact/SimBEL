# Cette fonction permettra a l'instar de la fonctionalite load de charger les inputs portefeuille de reference
# et de reconstruire le portefeuille de reference a partir de ces dernieres

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur du portefeuille d'actif
#----------------------------------------------------------------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------------------------------------------
#           chargement_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Charge le PortFin a partir des donnees renseignees par l'utilisateur.
##'
##' \code{chargement_PortFin} est une methode permettant de creer un objet \code{PortFin} a partir des donnees renseignees par l'utilisateur.
##' @name chargement_PortFin
##' @docType methods
##' @param folder_PortFin_address est un chemin de type \code{character}, cf la methode \code{\link{set_architecture}}
##' @param mp_ESG est un objet de la classe \code{ModelPointESG}, qui fournit le resultat financier du porfeuille.
##' @return L'objet \code{PortFin} tel que precise par les donnees initiales et les parametres renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @aliases PortFin
##' @include PortFin_class.R ModelPointESG_class.R

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
        ptf_action <- new("Action", ptf = data.frame(num_mp     = (Data_Action[,"num_mp"]),
                                                     val_marche = (Data_Action[,"val_marche"]),
                                                     val_nc     = (Data_Action[,"val_nc"]),
                                                     val_achat  = (Data_Action[,"val_achat"]),
                                                     presence   = (Data_Action[,"presence"]),
                                                     cessible   = (Data_Action[,"cessible"]),
                                                     nb_unit    = (Data_Action[,"nb_unit"]),
                                                     dur_det    = (Data_Action[,"dur_det"]),
                                                     pdd        = (Data_Action[,"pdd"]),
                                                     num_index  = (Data_Action[,"num_index"]),
                                                     div        = (Data_Action[,"div"]),
                                                     ind_invest = (Data_Action[,"ind_invest"])))
        # Verification input action
        if(sum(ptf_action@ptf_action$val_marche <= 0) > 0 | sum(ptf_action@ptf_action$nb_unit <= 0) > 0)
          stop("[chargement_PortFin] : Le portefeuille Action ne peut contenir de ligne dont
               la valeur de marche ou le nombre d'unite est negatif ou nul. \n")

        # Chargement du portefeuille Immo
        ptf_immo <- new("Immo", ptf = data.frame(num_mp     = (Data_Immo[,"num_mp"]),
                                                 val_marche = (Data_Immo[,"val_marche"]),
                                                 val_nc     = (Data_Immo[,"val_nc"]),
                                                 val_achat  = (Data_Immo[,"val_achat"]),
                                                 presence   = (Data_Immo[,"presence"]),
                                                 cessible   = (Data_Immo[,"cessible"]),
                                                 nb_unit    = (Data_Immo[,"nb_unit"]),
                                                 dur_det    = (Data_Immo[,"dur_det"]),
                                                 pdd        = (Data_Immo[,"pdd"]),
                                                 num_index  = (Data_Immo[,"num_index"]),
                                                 loyer      = (Data_Immo[,"loyer"]),
                                                 ind_invest = (Data_Immo[,"ind_invest"])))

        # Verification input immo
        if(sum(ptf_immo@ptf_immo$val_marche <= 0) > 0 | sum(ptf_immo@ptf_immo$nb_unit <= 0) > 0)
          stop("[chargement_PortFin] : Le portefeuille Immo ne peut contenir
               de ligne dont la valeur de marche ou le nombre d'unite est negatif ou nul. \n")

        # Chargement du portefeuille Oblig
        ptf_oblig <- new("Oblig", ptf = data.frame(num_mp     = (Data_Oblig[,"num_mp"]),
                                                   val_marche = (Data_Oblig[,"val_marche"]),
                                                   val_nc     = (Data_Oblig[,"val_nc"]),
                                                   val_achat  = (Data_Oblig[,"val_achat"]),
                                                   presence   = (Data_Oblig[,"presence"]),
                                                   cessible   = (Data_Oblig[,"cessible"]),
                                                   nb_unit    = (Data_Oblig[,"nb_unit"]),
                                                   dur_det    = (Data_Oblig[,"dur_det"]),
                                                   nominal    = (Data_Oblig[,"nominal"]),
                                                   tx_coupon  = (Data_Oblig[,"tx_coupon"]),
                                                   par        = (Data_Oblig[,"par"]),
                                                   mat_res    = (Data_Oblig[,"mat_res"]),
                                                   type       = (Data_Oblig[,"type"]),
                                                   rating     = (Data_Oblig[,"rating"]),
                                                   duration   = (Data_Oblig[,"duration"]),
                                                   zspread    = (Data_Oblig[,"zspread"]),
                                                   cc         = (Data_Oblig[,"cc"]),
                                                   sd         = (Data_Oblig[,"sd"])))
        # Verification input oblig
        if(sum(ptf_oblig@ptf_oblig$val_marche <= 0) > 0 | sum(ptf_oblig@ptf_oblig$nb_unit <= 0) > 0)
          stop("[chargement_PortFin] : Le portefeuille Oblig ne peut contenir de ligne
               dont la valeur de marche ou le nombre d'unite est negatif ou nul. \n")


        # Chargement du portefeuille Treso
        ptf_treso <- new("Treso", ptf = data.frame(num_mp     = (Data_Treso[,"num_mp"]),
                                                   val_marche = (Data_Treso[,"val_marche"]),
                                                   val_nc     = (Data_Treso[,"val_nc"])))
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

