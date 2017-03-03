##' Initialisation d'un workspace.
##'
##' \code{init_SimBEL} est la methode d'initialisation d'un workspace.
##' @name init_SimBEL
##' @docType methods
##' @param x un objet de la classe \code{Initialisation}.
##' @param nb_simu nombre de simulation.
##' @param nb_annee_proj nombre d'annee de projection.  
##' @return Pas de sortie.
##' @author Prim'Act
##' @export
##' @aliases Initialisation
setGeneric(name = "init_SimBEL", def = function(x, nb_simu, nb_annee_proj){standardGeneric("init_SimBEL")})
setMethod(
    f = "init_SimBEL",
    signature = c("Initialisation", "integer", "integer"),
    definition = function(x, nb_simu, nb_annee_proj){
        # Verification des inputs
        if(length(x@address) == 0) {stop("[Initialisation : init_SimBEL] : Veuillez faire tourner la fonction set_architecture sur l'objet Initialisation avant de lancer le processus d'initialisation. \n")}        
#         folder_ESG          <- paste(x@root_address, "input/donnees/actif/ESG", sep = "/")
#         folder_PortFin      <- paste(x@root_address, "input/donnees/actif", sep = "/")
#         folder_PortFin_ref  <- paste(x@root_address, "input/donnees/actif/Portefeuille_reference", sep = "/")
#         folder_Ptf_Passif   <- paste(x@root_address,"input/donnees/location", sep = "/")
#         
        # Actif
            # ESG et ModelPoint_ESG
            table_ESG   <- chargement_ESG(x@address$data_ESG, nb_simu, nb_annee_proj)
            mp_ESG_init <- extract_ESG(table_ESG, nb_simu, annee = as.integer(0))
            # Portfeuille financier de l'assureur
            Ptf_Fin     <- chargement_PortFin(x@address$data_actif, mp_ESG_init)
        # Passif
        Ptf_Passif <- load_pp(x)
        
        # Chargement d'un canton initial
        canton_init <- new("Canton")
        canton_init@annee        <- as.integer(0)
        canton_init@ptf_fin      <- Ptf_Fin
        canton_init@ptf_passif   <- Ptf_Passif
        canton_init@mp_esg       <- mp_ESG_init
        canton_init@ppb          <- new("Ppb") # Tout a 0 car MGP pas de PPB
        canton_init@hyp_canton   <- new("HypCanton", tx_soc = .15, tx_import = .33, method_taux_cible = "Meth1") # Pas propre
        canton_init@param_alm    <- new("ParamAlmEngine") # Canton initial port fin de ref vide
        canton_init@param_revalo <- new("ParamRevaloEngine", taux_pb_fi = .85, taux_pb_tech = .9, tx_marge_min = 0)
        
        # Sauvegarde au format .RData du canton initial
        save(canton_init, file = paste(x@address$init_save_folder, "canton_init.RData", sep = "/"))
    }
)
