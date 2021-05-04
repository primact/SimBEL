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
##' \code{chargement_PortFin} est une methode permettant de creer un objet \code{\link{PortFin}} a partir des donnees renseignees par l'utilisateur.
##' @name chargement_PortFin
##' @docType methods
##' @param folder_PortFin_address est un chemin de type \code{character}, cf la methode \code{\link{set_architecture}}
##' @param mp_ESG est un objet de la classe \code{\link{ModelPointESG}}, qui fournit le resultat financier du porfeuille.
##' @return L'objet \code{\link{PortFin}} tel que precise par les donnees initiales et les parametres renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @include PortFin_class.R ModelPointESG_class.R

setGeneric(name = "chargement_PortFin",function(folder_PortFin_address, mp_ESG){standardGeneric("chargement_PortFin")})
setMethod(
    f = "chargement_PortFin",
    signature = c(folder_PortFin_address = "character", mp_ESG = "ModelPointESG"),
    definition = function(folder_PortFin_address, mp_ESG){

        # Fichier contenant les noms des differnts PTF
        file_name       <- read.csv2(paste(folder_PortFin_address, "/noms_fichiers.csv", sep = ""), header = T,
                                     colClasses = rep("character", 2))

        # Chargement des differents PTF
        ptf_action <- load_action(paste(folder_PortFin_address, file_name[1L,2L], sep = "/"))
        ptf_oblig  <- load_oblig(paste(folder_PortFin_address, file_name[3L,2L], sep = "/"))
        ptf_immo   <- load_immo(paste(folder_PortFin_address, file_name[2L,2L], sep = "/"))
        ptf_treso   <- load_treso(paste(folder_PortFin_address, file_name[4L,2L], sep = "/"))


        # Chargement des PRE/RC et Frais fin
        pre       <- pre_load(paste(folder_PortFin_address, file_name[5L,2L], sep = "/"))
        rc        <- rc_load(paste(folder_PortFin_address, file_name[6L,2L], sep = "/"))
        frais_fin <- frais_fin_load(paste(folder_PortFin_address, file_name[7L,2L], sep = "/"))

        # Creation de l'objet
        x <- new("PortFin", annee = 0L,
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
                                                   oblig  = sum(ptf_oblig@ptf_oblig$val_marche),
                                                   treso  = sum(ptf_treso@ptf_treso$val_marche)),
                                         vnc = list(action = sum(ptf_action@ptf_action$val_nc),
                                                    immo   = sum(ptf_immo@ptf_immo$val_nc),
                                                    oblig  = sum(ptf_oblig@ptf_oblig$val_nc),
                                                    treso  = sum(ptf_treso@ptf_treso$val_nc))))
        # Recalcul des pmvl
        x <- calc_pmvl(x)

        # Recalcul des surcote decote
        x@ptf_oblig <- update_sd_oblig(x@ptf_oblig, calc_sur_dec(x@ptf_oblig))

        # Output
        return(x)
    }
)

