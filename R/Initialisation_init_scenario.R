# alloc_cible = c(.25,.25,.48,.02)
# seuil_realisation_PVL = 0


setGeneric(name = "init_scenario", def = function(x, alloc_cible, seuil_realisation_PVL, nb_simu, nb_annee_proj){standardGeneric("init_scenario")})
setMethod(
    f = "init_scenario",
    signature = c("Initialisation", "numeric", "numeric", "numeric", "numeric"),
    definition = function(x, alloc_cible, seuil_realisation_PVL, nb_simu, nb_annee_proj){
        # Traitement du central uniquement pour l'instant
        # A faire ensuite
        # Creation repertoire scenario choc concerne
        # Chargement de l'ESG correspondant au choc depuis le dossier input
        # chargement du mp_ESG_init
        # Chgement du canton initial sauvegardé lors de l'appel de la fonction init()
        # Ajustement du portefeuille de reinvesissement, cree selon le scenario de choc
        # Application du scenario de choc au PtfFin assureur
        # Sauvegarde de l'objet BE cree
        # Ecrasement des zspreads renseignes par l'utilisateur dans le canton@ptf_fin@ptf_oblig
        
        # Cas central simplifie
        canton_init <- get(load(paste(x@address$init_save_folder, "canton_init.RData", sep = "/")))
        Ptf_Fin_ref <- chargement_PortFin_reference(x@address$data_Ptf_reference, canton_init@mp_esg)
        canton_init@param_alm <- new("ParamAlmEngine", Ptf_Fin_ref, alloc_cible, seuil_realisation_PVL) 
        
        best_estimate           <- new("Be")
        best_estimate@param_be  <- new("ParamBe", nb_annee = as.integer(nb_annee_proj))
        best_estimate@canton    <- canton_init
        best_estimate@esg       <- chargement_ESG(x@address$data_ESG, nb_simu, nb_annee_proj)
        
        #save()
    }
)
