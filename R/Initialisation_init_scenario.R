##' Initialisation des scenarios de chocs d'un workspace.
##'
##' \code{init_scenario} est la methode d'initialisation.
##' @name init_scenario
##' @docType methods
##' @param x un objet de la classe \code{Initialisation}.
##' @param alloc_cible
##' @param seuil_realisation_PVL
##' @param nb_simu
##' @param nb_annee_proj
##' @return Pas de sortie.
##' @author Prim'Act
##' @export
##' @aliases Initialisation


setGeneric(name = "init_scenario", def = function(x){standardGeneric("init_scenario")})
setMethod(
    f = "init_scenario",
    signature = "Initialisation",
    definition = function(x){
        # Creation des dossiers initiaux
        init_create_folder(x)

        # a mettre sous forme de lecture d'un fichier csv
        table_choc <- chargement_choc(new("ChocSolvabilite2"), racine@address[["param"]][["chocs"]])
        scenario = c("central", "frais",
                     "mortalite", "longevite",
                     "rachat_up", "rachat_down",
                     "action", "immo",
                     "taux_up", "taux_down",
                     "spread")
        
        lapply(scenario, function(name_scenario){
            if (name_scenario != "taux_up" & name_scenario != "taux_down"){
                # Chargement de la photo initiale
                canton_init <- get(load(paste(x@address[["save_folder"]][["init"]], "canton_init.RData", sep = "/")))

                # Chargement de l'ESG concerne :
                table_ESG   <- chargement_ESG(x@address[["param"]][["ESG"]], x@nb_simu, x@nb_annee_proj)
                # Mise a jour du Model Point d'ESG du canton init en accord avec le scenario :
                canton_init@mp_esg <- extract_ESG(table_ESG, x@nb_simu, as.integer(0))

                Ptf_Fin_ref <- chargement_PortFin_reference(x@address[["data"]][["ptf_reference"]], canton_init@mp_esg)
                canton_init@param_alm <- param_alm_engine_load(paste(x@address[["param"]][["alm"]], "param_alm.csv", sep = "/"), Ptf_Fin_ref)
                #canton_init@param_alm <- new("ParamAlmEngine", Ptf_Fin_ref, alloc_cible, seuil_realisation_PVL)

                canton_init <- switch(EXPR = name_scenario,
                                         "action"      = do_choc_action(table_choc, canton_init),
                                         "immo"        = do_choc_immo(table_choc, canton_init),
                                         "spread"      = do_choc_spread(table_choc, canton_init),
                                         "frais"       = do_choc_frais(table_choc, canton_init),
                                         "mortalite"   = do_choc_mortalite(table_choc, canton_init), # MARCHE PAS VOIR AVEC MT
                                         "longevite"   = do_choc_longevite(table_choc, canton_init), # MARCHE PAS VOIR AVEC MT
                                         "rachat_up"   = do_choc_rachat_up(table_choc, canton_init),
                                         "rachat_down" = do_choc_rachat_down(table_choc, canton_init),
                                         "central"     = canton_init)

                # Ecraser les zpsreads renseignes par l'utilisateur dans le PortFin et le PortFin ref
                # Port fin
                zspread                       <- calc_z_spread(canton_init@ptf_fin@ptf_oblig, canton_init@mp_esg@yield_curve)
                canton_init@ptf_fin@ptf_oblig <- update_zsp_oblig(canton_init@ptf_fin@ptf_oblig, zspread)
                # PortFin reference
                zspread                                       <- calc_z_spread(canton_init@param_alm@ptf_reference@ptf_oblig, canton_init@mp_esg@yield_curve)
                canton_init@param_alm@ptf_reference@ptf_oblig <- update_zsp_oblig(canton_init@param_alm@ptf_reference@ptf_oblig, zspread)

                best_estimate           <- new("Be")
                best_estimate@param_be  <- new("ParamBe", x@nb_annee_proj)
                best_estimate@canton    <- canton_init
                best_estimate@esg       <- table_ESG

                # Sauvegarde
                save(best_estimate, file = paste(x@address[["save_folder"]][[name_scenario]], "best_estimate.RData", sep = "/"))
            } else {
                # Chargement de la photo initiale
                canton_init <- get(load(paste(x@address[["save_folder"]][["init"]], "canton_init.RData", sep = "/")))

                # Chargement de l'ESG concerne :
                table_ESG   <- chargement_ESG(x@address[["param"]][["ESG"]], x@nb_simu, x@nb_annee_proj)
                # Mise a jour du Model Point d'ESG du canton init en accord avec le scenario : necessaire que pour le taux
                canton_init@mp_esg <- extract_ESG(table_ESG, x@nb_simu, as.integer(0))

                Ptf_Fin_ref <- chargement_PortFin_reference(x@address[["data"]][["ptf_reference"]], canton_init@mp_esg)
                canton_init@param_alm <- param_alm_engine_load(paste(x@address[["param"]][["alm"]], "param_alm.csv", sep = "/"), Ptf_Fin_ref)
                #canton_init@param_alm <- new("ParamAlmEngine", Ptf_Fin_ref, alloc_cible, seuil_realisation_PVL)

                # Ecraser les zpsreads renseignes par l'utilisateur dans le PortFin et le PortFin ref
                # IMPORTANT : LES COURBES DE TAUX POUR RECALCULER LES ZSPREADS SONT LES COURBES NON CHOQUEES
                # Port fin
                zspread                       <- calc_z_spread(canton_init@ptf_fin@ptf_oblig, canton_init@mp_esg@yield_curve)
                canton_init@ptf_fin@ptf_oblig <- update_zsp_oblig(canton_init@ptf_fin@ptf_oblig, zspread)
                # PortFin reference
                zspread                                       <- calc_z_spread(canton_init@param_alm@ptf_reference@ptf_oblig, canton_init@mp_esg@yield_curve)
                canton_init@param_alm@ptf_reference@ptf_oblig <- update_zsp_oblig(canton_init@param_alm@ptf_reference@ptf_oblig, zspread)

                # Chargement de l'ESG concerne : selon le scenario de taux
                table_ESG   <- switch(EXPR = name_scenario,
                                      "taux_up"   = chargement_ESG(x@address[["param"]][["ESG_up"]], x@nb_simu, x@nb_annee_proj),
                                      "taux_down" = chargement_ESG(x@address[["param"]][["ESG_down"]], x@nb_simu, x@nb_annee_proj))
                # Mise a jour du Model Point d'ESG du canton init en accord avec le scenario :
                canton_init@mp_esg <- extract_ESG(table_ESG, x@nb_simu, as.integer(0))


                best_estimate           <- new("Be")
                best_estimate@param_be  <- new("ParamBe", x@nb_annee_proj)
                best_estimate@canton    <- do_choc_spread(table_choc, canton_init)
                best_estimate@esg       <- table_ESG

                # Sauvegarde
                save(best_estimate, file = paste(x@address[["save_folder"]][[name_scenario]], "best_estimate.RData", sep = "/"))
            }
        })
    }
)
