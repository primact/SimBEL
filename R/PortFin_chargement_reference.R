#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur du portefeuille de reference
#----------------------------------------------------------------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------------------------------------------
#           chargement_PortFin_reference
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Charge le PortFin de reinvestissement a partir des donnees renseignees par l'utilisateur.
##'
##' \code{chargement_PortFin_reference} est une methode permettant de creer un objet \code{PortFin} correspondant au portefeuille finanicer de reinvestissement
##' a partir des donnees renseignees par l'utilisateur.
##' @name chargement_PortFin_reference
##' @docType methods
##' @param folder_PortFin_reference_address est un chemin de type \code{character}, cf la methode \code{\link{set_architecture}}
##' @param mp_ESG est un objet de la classe \code{\link{ModelPointESG}}, qui fournit le resultat financier du porfeuille.
##' @return L'objet \code{\link{PortFin}} correspondant au portefeuille financier de reinvestissement
##' tel que precise par les donnees initiales et les parametres renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @include PortFin_class.R ModelPointESG_class.R

setGeneric(name = "chargement_PortFin_reference",function(folder_PortFin_reference_address, mp_ESG){standardGeneric("chargement_PortFin_reference")})
setMethod(
    f = "chargement_PortFin_reference",
    signature = c(folder_PortFin_reference_address = "character", mp_ESG = "ModelPointESG"),
    definition = function(folder_PortFin_reference_address, mp_ESG){

        # Nom des fichiers
        # La colonne 1 contient le type,
        # La colonne 2 contient le nom du fichier,
        file_name           <- read.csv2(paste(folder_PortFin_reference_address, "/noms_fichiers_reference.csv", sep = ""), header = T,
                                         colClasses = rep("character", 2))[1:3,1:2]
        Data_ref_Action     <- read.csv2(paste(folder_PortFin_reference_address, file_name[1,2], sep = "/"),
                                         colClasses = c("integer", "numeric", "integer", "numeric",
                                                        "logical", "numeric",
                                                        "character", "numeric"))
        Data_ref_Immo       <- read.csv2(paste(folder_PortFin_reference_address, file_name[2,2], sep = "/"),
                                         colClasses = c("integer", "numeric", "integer", "numeric",
                                                        "logical", "numeric",
                                                        "character", "numeric"))
        Data_ref_Oblig      <- read.csv2(paste(folder_PortFin_reference_address, file_name[3,2], sep = "/"),
                                         colClasses = c("integer", rep("numeric", 4), "factor", "integer",
                                                        "numeric", "character", "numeric"))

        # Construction du portefeuille de reference action
        ptf <- data.frame(num_mp     = (Data_ref_Action[,"num_mp"]),
                         val_marche = (Data_ref_Action[,"val_unitaire_achat"]),
                         val_nc     = (Data_ref_Action[,"val_unitaire_achat"]),
                         val_achat  = (Data_ref_Action[,"val_unitaire_achat"]),
                         presence   = (rep(TRUE, nrow(Data_ref_Action))),
                         cessible   = (rep(TRUE, nrow(Data_ref_Action))),
                         nb_unit    = (Data_ref_Action[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
                         dur_det    = (rep(0, nrow(Data_ref_Action))),
                         pdd        = (rep(0, nrow(Data_ref_Action))),
                         num_index  = (Data_ref_Action[,"num_index"]),
                         div        = (Data_ref_Action[,"div"]),
                         ind_invest = (Data_ref_Action[,"ind_invest"]),
                         currency   = (Data_ref_Action[,"currency"]),
                         fx_rate   = (Data_ref_Action[,"fx_rate"])
        )
        # currency en character
        ptf$currency <- as.character(ptf$currency)
        # Creation object
        ptf_action <- new("Action", ptf = ptf)
        # Verification input action
        if(sum(ptf_action@ptf_action$val_marche <= 0) > 0 | sum(ptf_action@ptf_action$nb_unit <= 0) > 0) stop("[chargement_PortFin_reference] : Le portefeuille Action de reference ne peut contenir de ligne dont la valeur nominale unitaire est nulle. \n")

        # Construction du portefeuille de reference Immo
        ptf <- data.frame(num_mp     = (Data_ref_Immo[,"num_mp"]),
                         val_marche = (Data_ref_Immo[,"val_unitaire_achat"]),
                         val_nc     = (Data_ref_Immo[,"val_unitaire_achat"]),
                         val_achat  = (Data_ref_Immo[,"val_unitaire_achat"]),
                         presence   = (rep(TRUE, nrow(Data_ref_Immo))),
                         cessible   = (rep(TRUE, nrow(Data_ref_Immo))),
                         nb_unit    = (Data_ref_Immo[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
                         dur_det    = (rep(0, nrow(Data_ref_Immo))),
                         pdd        = (rep(0, nrow(Data_ref_Immo))),
                         num_index  = (Data_ref_Immo[,"num_index"]),
                         loyer      = (Data_ref_Immo[,"loyer"]),
                         ind_invest = (Data_ref_Immo[,"ind_invest"]),
                         currency   = (Data_ref_Immo[,"currency"]),
                         fx_rate   = (Data_ref_Immo[,"fx_rate"])
        )
        # currency en character
        ptf$currency <- as.character(ptf$currency)
        # Creation object
        ptf_immo <- new("Immo", ptf = ptf)
        # Verification input immo
        if(sum(ptf_immo@ptf_immo$val_marche <= 0) > 0 | sum(ptf_immo@ptf_immo$nb_unit <= 0) > 0) stop("[chargement_PortFin_reference] : Le portefeuille Immo de reference ne peut contenir de ligne dont la valeur nominale unitaire est nulle. \n")


        # Construction du portefeuille de reference Oblig
        ptf <- data.frame(num_mp     = (Data_ref_Oblig[,"num_mp"]),
                         val_marche = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
                         val_nc     = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
                         val_achat  = Data_ref_Oblig[,"nominal_unitaire"] * Data_ref_Oblig[,"parite"] * Data_ref_Oblig[,"allocation"],
                         presence   = (rep(TRUE, nrow(Data_ref_Oblig))),
                         cessible   = (rep(TRUE, nrow(Data_ref_Oblig))),
                         nb_unit    = (Data_ref_Oblig[,"allocation"]), # IMPORTANT : ici on affecte l'allocation, histoire de savoir quelle ventilation d'achat dans les nouvelles lignes est effectuee
                         dur_det    = (rep(0, nrow(Data_ref_Oblig))),
                         nominal    = (Data_ref_Oblig[,"nominal_unitaire"]),
                         tx_coupon  = (Data_ref_Oblig[,"tx_coupon"]),
                         par        = (Data_ref_Oblig[,"parite"]),
                         mat_res    = (Data_ref_Oblig[,"mat_res"]),
                         type       = (Data_ref_Oblig[,"type"]),
                         rating     = (Data_ref_Oblig[,"rating"]),
                         duration   = (rep(0, nrow(Data_ref_Oblig))),
                         zspread    = (rep(0, nrow(Data_ref_Oblig))),
                         cc         = (rep(0, nrow(Data_ref_Oblig))),
                         sd         = (rep(0, nrow(Data_ref_Oblig))),
                         currency   = (Data_ref_Oblig[,"currency"]),
                         fx_rate    = (Data_ref_Oblig[,"fx_rate"])
        )
        # currency en character
        ptf$currency <- as.character(ptf$currency)
        # Creation object
        ptf_oblig <- new("Oblig", ptf = ptf)
        # Verification input oblig
        if(sum(ptf_oblig@ptf_oblig$val_marche <= 0) > 0 | sum(ptf_oblig@ptf_oblig$nb_unit <= 0) > 0) stop("[chargement_PortFin_reference] : Le portefeuille Oblig de reference ne peut contenir de ligne dont la valeur nominale unitaire est nulle. \n")


        # Recalcul Dur/Zsp/CC/SD du ptf_oblig
        ptf_oblig["ptf_oblig"][,"cc"]       <- calc_coupon(ptf_oblig)
        ptf_oblig["ptf_oblig"][,"zspread"]  <- calc_z_spread(ptf_oblig, mp_ESG["yield_curve"])
        ptf_oblig["ptf_oblig"][,"sd"]       <- calc_sur_dec(ptf_oblig)
        ptf_oblig["ptf_oblig"][,"duration"] <- duration_sensi(ptf_oblig)[, "duration"]

        x <- new("PortFin", annee = 0L,
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
                                                   oblig  = sum(ptf_oblig@ptf_oblig$val_marche),
                                                   treso  = 0),
                                         vnc = list(action = sum(ptf_action@ptf_action$val_nc),
                                                    immo   = sum(ptf_immo@ptf_immo$val_nc),
                                                    oblig  = sum(ptf_oblig@ptf_oblig$val_nc),
                                                    treso = 0)))
        return(x)
    }
)
