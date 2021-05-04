##' Initialisation des scenarios : central et de chocs d'un workspace.
##'
##' \code{init_scenario} est la methode d'initialisation.
##' @name init_scenario
##' @docType methods
##' @param x un objet de la classe \code{\link{Initialisation}}.
##' @return Pas de sortie.
##' @note Cette methode cree l'architecture, puis les objets \code{\link{Be}} correspondant a chacun des scenarios : central et de chocs de la formule standard.
##' @author Prim'Act
##' @export
##' @include Initialisation_class.R


setGeneric(name = "init_scenario", def = function(x){standardGeneric("init_scenario")})
setMethod(
    f = "init_scenario",
    signature = c(x = "Initialisation"),
    definition = function(x){

        # Message utilisateur
        message("Initialisation des donnees et parametres pour chaque choc")

        # Creation des dossiers initiaux
        init_create_folder(x)

        # a mettre sous forme de lecture d'un fichier csv
        table_choc <- chargement_choc(new("ChocSolvabilite2"), x@address[["param"]][["chocs"]])


        for(name_scenario in table_choc@scenario){
          # Message
          message(paste("Chargement du choc : ", name_scenario, sep = ""))

            if (name_scenario != "taux_up" & name_scenario != "taux_down"){
                # Chargement de la photo initiale
                canton_init <- get(load(paste(x@address[["save_folder"]][["init"]], "canton_init.RData", sep = "/")))

                # Chargement de l'ESG concerne :
                table_ESG   <- chargement_ESG(x@address[["param"]][["ESG"]], x@nb_simu, x@nb_annee_proj)

                # Application d'un choc sur l'inflation pour le choc frais
                if( name_scenario == "frais"){
                  table_ESG <- get_choc_inflation_frais(table_ESG,
                                                        table_choc@param_choc_sousc@mp$choc_frais_inflation)
                }

                # Mise a jour du Model Point d'ESG du canton init en accord avec le scenario :
                canton_init@mp_esg <- extract_ESG(table_ESG, x@nb_simu, 0L)

                Ptf_Fin_ref <- chargement_PortFin_reference(x@address[["data"]][["ptf_reference"]], canton_init@mp_esg)
                canton_init@param_alm <- param_alm_engine_load(paste(x@address[["param"]][["alm"]], "param_alm.csv", sep = "/"), Ptf_Fin_ref)
                #canton_init@param_alm <- new("ParamAlmEngine", Ptf_Fin_ref, alloc_cible, seuil_realisation_PVL)


                # Autres passifs choques
                input_autres_passifs_choc <- read.csv2(paste(x@address[["data"]][["autres_passifs_choc"]],"noms_liens_autres_passifs_choc.csv",sep="/"),
                                                       header = TRUE, colClasses = rep("character", 2))
                # Chargement des passifs choques
                autres_passifs_choc <- switch(EXPR = name_scenario,
                                              "action_type1" = canton_init@ptf_passif@autres_passifs,
                                              "action_type2" = canton_init@ptf_passif@autres_passifs,
                                              "immo"         = canton_init@ptf_passif@autres_passifs,
                                              "spread"       = canton_init@ptf_passif@autres_passifs,
                                              "currency_up"  = canton_init@ptf_passif@autres_passifs,
                                              "currency_down"= canton_init@ptf_passif@autres_passifs,
                                              "frais"        = autres_passif_load(
                                                                    paste(x@address[["data"]][["autres_passifs_choc"]],
                                                                    input_autres_passifs_choc[which(input_autres_passifs_choc$choc_sousc == "frais"), "nom_table_csv"],
                                                                    sep = "/")),
                                              "mortalite"   = autres_passif_load(
                                                                    paste(x@address[["data"]][["autres_passifs_choc"]],
                                                                    input_autres_passifs_choc[which(input_autres_passifs_choc$choc_sousc == "mortalite"), "nom_table_csv"],
                                                                    sep = "/")),
                                              "longevite"   = autres_passif_load(
                                                                    paste(x@address[["data"]][["autres_passifs_choc"]],
                                                                        input_autres_passifs_choc[which(input_autres_passifs_choc$choc_sousc == "longevite"), "nom_table_csv"],
                                                                        sep = "/")),
                                              "rachat_up"   = autres_passif_load(
                                                                    paste(x@address[["data"]][["autres_passifs_choc"]],
                                                                        input_autres_passifs_choc[which(input_autres_passifs_choc$choc_sousc == "rachat_up"), "nom_table_csv"],
                                                                        sep = "/")),
                                              "rachat_down" = autres_passif_load(
                                                                    paste(x@address[["data"]][["autres_passifs_choc"]],
                                                                        input_autres_passifs_choc[which(input_autres_passifs_choc$choc_sousc == "rachat_down"), "nom_table_csv"],
                                                                        sep = "/")),
                                              "rachat_mass" = autres_passif_load(
                                                paste(x@address[["data"]][["autres_passifs_choc"]],
                                                      input_autres_passifs_choc[which(input_autres_passifs_choc$choc_sousc == "rachat_mass"), "nom_table_csv"],
                                                      sep = "/")),
                                              "central"     = canton_init@ptf_passif@autres_passifs)


                # Transformation du canton apres choc
                canton_init <- switch(EXPR = name_scenario,
                                      "action_type1" = do_choc_action_type1(table_choc, canton_init),
                                      "action_type2" = do_choc_action_type2(table_choc, canton_init),
                                      "immo"         = do_choc_immo(table_choc, canton_init),
                                      "spread"       = do_choc_spread(table_choc, canton_init),
                                      "frais"        = do_choc_frais(table_choc, canton_init, autres_passifs_choc),
                                      "mortalite"    = do_choc_mortalite(table_choc, canton_init, autres_passifs_choc),
                                      "longevite"    = do_choc_longevite(table_choc, canton_init, autres_passifs_choc),
                                      "rachat_up"    = do_choc_rachat_up(table_choc, canton_init, autres_passifs_choc),
                                      "rachat_down"  = do_choc_rachat_down(table_choc, canton_init, autres_passifs_choc),
                                      "rachat_mass"  = do_choc_rachat_mass(table_choc, canton_init, autres_passifs_choc),
                                      canton_init     # Alternative
                )

                # Choc de devise
                nlong <- nchar(name_scenario)
                if(length(grep("currency_up", name_scenario)) > 0){
                  direction <- "currency_up"
                  nom_currency <- substr(name_scenario, nlong - 2, nlong)
                  canton_init <- do_choc_currency(table_choc, nom_currency, "up", canton_init)
                }
                if(length(grep("currency_down", name_scenario)) > 0){
                  direction <- "currency_down"
                  nom_currency <- substr(name_scenario, nlong - 2, nlong)
                  canton_init <- do_choc_currency(table_choc, nom_currency, "down", canton_init)
                }

                # Ecraser les zpsreads renseignes par l'utilisateur dans le PortFin et le PortFin ref
                # Port fin
                zspread                       <- calc_z_spread(canton_init@ptf_fin@ptf_oblig, canton_init@mp_esg@yield_curve)
                canton_init@ptf_fin@ptf_oblig <- update_zsp_oblig(canton_init@ptf_fin@ptf_oblig, zspread)
                # PortFin reference
                zspread                                       <- calc_z_spread(canton_init@param_alm@ptf_reference@ptf_oblig, canton_init@mp_esg@yield_curve)
                canton_init@param_alm@ptf_reference@ptf_oblig <- update_zsp_oblig(canton_init@param_alm@ptf_reference@ptf_oblig, zspread)

                # creation du nouvel objet best estimate en situation de choc
                best_estimate           <- new("Be")
                best_estimate@param_be  <- new("ParamBe", x@nb_annee_proj)
                best_estimate@canton    <- canton_init
                best_estimate@esg       <- table_ESG

            } else {
                # Chargement de la situation initiale
                canton_init <- get(load(paste(x@address[["save_folder"]][["init"]], "canton_init.RData", sep = "/")))

                # Chargement de l'ESG concerne :
                table_ESG   <- chargement_ESG(x@address[["param"]][["ESG"]], x@nb_simu, x@nb_annee_proj)
                # Mise a jour du Model Point d'ESG du canton init en accord avec le scenario : necessaire que pour le taux
                canton_init@mp_esg <- extract_ESG(table_ESG, x@nb_simu, 0L)

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
                canton_init@mp_esg <- extract_ESG(table_ESG, x@nb_simu, 0L)

                # creation du nouvel objet best estimate en situation de choc
                best_estimate           <- new("Be")
                best_estimate@param_be  <- new("ParamBe", x@nb_annee_proj)
                best_estimate@esg       <- table_ESG
                best_estimate@canton    <- do_choc_taux(canton_init)
            }


            # Calcul du BE sur 1 simulation pour initialiser le tableau des probas
            res_be_simu <- NULL ; i <- 1L
            while(is.null(res_be_simu)) {
                res_be_simu <- run_be_simu(best_estimate, i, FALSE)[["canton"]]
                i <- i + 1L
            }

            # Mise a jour des tableau de probabilites
            for(name_tab in best_estimate@canton@ptf_passif@names_class_prod) {

                for(name_product in names(best_estimate@canton@ptf_passif[name_tab]))
                    best_estimate@canton@ptf_passif[name_tab][[name_product]]@tab_proba <- res_be_simu@ptf_passif[name_tab][[name_product]]@tab_proba

            }

            # Mise a jour du booleen de calcul des probas
            best_estimate@canton@ptf_passif@calc_proba <- FALSE


            # Sauvegarde
            # Choc de devise
            if(length(grep("currency_up", name_scenario)) > 0){
              direction <- "currency_up"
              nom_currency <- substr(name_scenario, nlong - 2, nlong)
              save(best_estimate, file = paste(x@address[["save_folder"]][[direction]], paste0(nom_currency,"_best_estimate.RData"), sep = "/"))
            } else if(length(grep("currency_down", name_scenario)) > 0){
              direction <- "currency_down"
              nom_currency <- substr(name_scenario, nlong - 2, nlong)
              save(best_estimate, file = paste(x@address[["save_folder"]][[direction]], paste0(nom_currency,"_best_estimate.RData"), sep = "/"))
            } else{
              save(best_estimate, file = paste(x@address[["save_folder"]][[name_scenario]], "best_estimate.RData", sep = "/"))
            }

        }
        # Output
        return(message("Fin du chargement des chocs"))

    }
)
