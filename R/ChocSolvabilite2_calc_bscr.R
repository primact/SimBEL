#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_bscr
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet de calculer le BSCR selon la formule standard.
##'
##' \code{calc_bscr} est une methode permettant de lancer
##' l'ensemble des projections necessaires au calcul du BSCR.
##' au sens de la formule standard de la directive Solvabilite 2.
##' @name calc_bscr
##' @docType methods
##' @param table_choc un objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param racine un objet de la classe \code{\link{Initialisation}}.
##' @param pre_on  une valeur \code{logical} qui lorsqu'elle vaut \code{TRUE} prend en compte la variation
##' de PRE dans le resultat technique utilisee pour le calcul de la participation aux benefices reglementaires.
##' @param parallel une valeur \code{logical} qui indique si les calculs seront parallelises.
##' @param nb_coeur une valeur \code{integer} qui indique le nombre de coeurs utilises dans le cas ou les calculs sont
##' parallelises. Par defaut cette valeur est egale a 0.
##' @param ecriture_base un \code{logical} qui indique si les output doivent etre ecrits dans la base \code{SQLite}.
##' @return une liste de \code{matrix} contenant les valeurs des differents sous-SCR ainsi que le BSCR.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Initialisation_class.R

setGeneric(name = "calc_bscr", def = function(table_choc, racine, pre_on, parallel, nb_coeur = 0L, ecriture_base){standardGeneric("calc_bscr")})
setMethod(
  f = "calc_bscr",
  signature = c(table_choc = "ChocSolvabilite2", racine = "Initialisation", pre_on = "logical", parallel = "logical", nb_coeur = "integer", ecriture_base = "logical"),
  definition = function(table_choc, racine, pre_on, parallel, nb_coeur, ecriture_base){

      # Sauvegarde des scenarios selectionnes
      table_scenario <- table_choc@scenario

      # Cas particulier des scenarios currency : isolement de ces scenarios
      if (length(grep( "currency_up", table_scenario))>0){
          # Sauvegarde des scenarios de change a la hausse
          table_scenario_currency_up <- grep( "currency_up", table_scenario, value=TRUE)
          # Sauvegarde des devises utilisees pour le choc de change
          name_currency <- unique(sub("currency_down_", "", sub("currency_up_", "", table_scenario_currency_up)))
          # Sauvegarde des scenarios selectionnes sans les scenarios de change
          table_scenario_wo_currency <- table_scenario[-c(grep( "currency_up", table_scenario), grep( "currency_down", table_scenario))]
      }
      if (length(grep( "currency_down", table_scenario))>0){
          # Sauvegarde des scenarios de change a la baisse
          table_scenario_currency_down <- grep( "currency_down", table_scenario, value=TRUE)
          # Sauvegarde des devises utilisees pour le choc de change
          name_currency <- unique(sub("currency_down_","", sub("currency_up_","", table_scenario_currency_down)))
          # Sauvegarde des scenarios selectionnes sans
          table_scenario_wo_currency <- table_scenario[-c(grep( "currency_up", table_scenario), grep( "currency_down", table_scenario))]
      }
      # Initialisation pour la boucle de calcul
      Be <- list()
      file_adress = paste(racine@root_address, "internal_ws/data/database", sep = "/")

      # Chargement des 'Be' pour le scenario central et chaque situation de choc de la formule standard
      Be_scenarios <- lapply(table_scenario_wo_currency, function(s){
          get(load(paste(racine@address$save_folder[[s]], "best_estimate.RData", sep = "/")))
      })
      names(Be_scenarios) <- table_scenario_wo_currency

      # Cas particulier des scenarios currency
      if (length(grep( "currency_up", table_scenario))>0){
          Be_scenarios_currency_up <- lapply(name_currency, function(s){
                get(load(paste(racine@address$save_folder[["currency_up"]],"/", s, "_best_estimate.RData", sep="")))
          })
          names(Be_scenarios_currency_up) <- table_scenario_currency_up
          Be_scenarios <- append(Be_scenarios, Be_scenarios_currency_up)
      }
      if (length(grep( "currency_down", table_scenario))>0){
            Be_scenarios_currency_down <- lapply(name_currency, function(s){
                get(load(paste(racine@address$save_folder[["currency_down"]],"/", s, "_best_estimate.RData", sep="")))
            })
            names(Be_scenarios_currency_down) <- table_scenario_currency_down
            Be_scenarios <- append(Be_scenarios, Be_scenarios_currency_down)
      }

      # Lancement de la boucle de projection
      for (name_scenario in table_scenario) {

          # Creation d'un nouvel objet DataBase
          Be_scenarios[[name_scenario]]@base <- new("DataBase", file_adress, ecriture_base = ecriture_base, choc_name = name_scenario)

          # Projection du Be
          Be_res <- run_be(x = Be_scenarios[[name_scenario]], pre_on = pre_on, parallel = parallel, nb_coeur = nb_coeur)

          # Sauvegarde dans une liste du Be projete
          Be_res_list <- list(Be_res)
          names(Be_res_list) <- name_scenario
          Be <- append(Be, Be_res_list)
      }

      # Extraction des best estimates calcules pour chaque choc
      Be_valeurs = lapply(table_scenario, function(s){
          Be[[s]]$be@tab_be$be[1, 1]
      }
      )
      names(Be_valeurs) = table_scenario

      # Fonction permettant de calculer l'?volution de la NAV
      delta_nav <- function( Actif_central, BE_central, Actif_choque, BE_choque){
          pmax(0, (Actif_central-BE_central) - ( Actif_choque - BE_choque))
      }

      # Initialisation des SCR
      vectSCR_action <- matrix(0, nrow=1, ncol=2, dimnames = list( "valeur", c("action_type1", "action_type2")))
      vectSCR_mket   <- matrix(0, nrow=1, ncol=5, dimnames = list( "valeur", c("taux", "action", "immo", "spread", "currency")))
      vectSCR_sousc  <- matrix(0, nrow=1, ncol=4, dimnames = list( "valeur", c("mortalite", "longevite", "rachat", "frais")))
      vectBSCR		   <- matrix(0, nrow=1, ncol=2, dimnames = list( "valeur", c("SCR_mket", "SCR_sousc")))

      # Extraction des actifs et calcul du sous-SCR pour chaque choc
      # Central
      ACTIF_central <- print_alloc(Be[["central"]]$be@canton@ptf_fin)[5, 1]

      # Action type 1
      if ("action_type1" %in% table_scenario){
          ACTIF_action_type_1 <- print_alloc(Be[["action_type1"]]$be@canton@ptf_fin)[5,1]
          vectSCR_action[1,"action_type1"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_action_type_1, Be_valeurs[["action_type1"]])
      }

      # Action type 2
      if ("action_type2" %in% table_scenario){
          ACTIF_action_type_2 <- print_alloc(Be[["action_type2"]]$be@canton@ptf_fin)[5,1]
          vectSCR_action[1,"action_type2"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_action_type_2, Be_valeurs[["action_type2"]])
      }

      # Immobilier
      if ("immo" %in% table_scenario){
          ACTIF_immo <- print_alloc(Be[["immo"]]$be@canton@ptf_fin)[5,1]
          vectSCR_mket[1, "immo"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_immo, Be_valeurs[["immo"]])
      }

      # Spread
      if ("spread" %in% table_scenario){
          ACTIF_spread <- print_alloc(Be[["spread"]]$be@canton@ptf_fin)[5,1]
          vectSCR_mket[1, "spread"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_spread, Be_valeurs[["spread"]])
      }

      # Taux
      SCR_taux_up <- 0
      SCR_taux_down <- 0
      if ("taux_up" %in% table_scenario){
          ACTIF_taux_up <- print_alloc(Be[["taux_up"]]$be@canton@ptf_fin)[5,1]
          SCR_taux_up <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_taux_up, Be_valeurs[["taux_up"]])
      }
      if ("taux_down" %in% table_scenario){
          ACTIF_taux_down <- print_alloc(Be[["taux_down"]]$be@canton@ptf_fin)[5,1]
          SCR_taux_down <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_taux_down, Be_valeurs[["taux_down"]])
      }
      vectSCR_mket[1, "taux"] <- max(SCR_taux_up, SCR_taux_down)

      # Change
      if (length(grep( "currency_up", table_scenario))>0){
          ACTIF_currency_up <- lapply(table_scenario_currency_up, function(s){
              print_alloc(Be[[s]]$be@canton@ptf_fin)[5,1]
          })
          names(ACTIF_currency_up) <- table_scenario_currency_up
          SCR_currency_up <- lapply(table_scenario_currency_up, function(s){
              pmax(0, (ACTIF_central-Be_valeurs[["central"]]) - ( ACTIF_currency_up[[s]] - Be_valeurs[[s]]))
          })
          names(SCR_currency_up) <- table_scenario_currency_up
          vectSCR_mket[1, "currency"] <- sum(as.numeric(SCR_currency_up))
      }
      if (length(grep( "currency_down", table_scenario))>0){
          ACTIF_currency_down <- lapply(table_scenario_currency_down, function(s){
              print_alloc(Be[[s]]$be@canton@ptf_fin)[5,1]
          })
          names(ACTIF_currency_down) <- table_scenario_currency_down
          SCR_currency_down <- lapply(table_scenario_currency_down, function(s){
              pmax(0, (ACTIF_central-Be_valeurs[["central"]]) - ( ACTIF_currency_down[[s]] - Be_valeurs[[s]]))
          })
          names(SCR_currency_down) <- table_scenario_currency_down
          vectSCR_mket[1, "currency"] <- sum(as.numeric(SCR_currency_down))
      }
      if (length(grep( "currency_up", table_scenario))>0 && length(grep( "currency_down", table_scenario))>0){
          SCR_currency_devise = lapply(table_scenario_currency_up, function(s){
              max(SCR_currency_up[[s]], ACTIF_currency_down[[s]])
          })
          vectSCR_mket[1, "currency"] <- sum(as.numeric(SCR_currency_devise))
      }

      # Mortalite
      if ("mortalite" %in% table_scenario){
          vectSCR_sousc[1, "mortalite"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_central, Be_valeurs[["mortalite"]])
      }

      # Longevite
      if ("longevite" %in% table_scenario){
          vectSCR_sousc[1, "longevite"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_central, Be_valeurs[["longevite"]])
      }

      # Rachat
      SCR_rachat_up <- 0
      SCR_rachat_down <- 0
      SCR_rachat_mass <- 0
      if ("rachat_up" %in% table_scenario){
          SCR_rachat_up <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_central, Be_valeurs[["rachat_up"]])
      }
      if ("rachat_down" %in% table_scenario){
          SCR_rachat_down <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_central, Be_valeurs[["rachat_down"]])
      }
      if ("rachat_mass" %in% table_scenario){
        SCR_rachat_mass <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_central, Be_valeurs[["rachat_mass"]])
      }
      vectSCR_sousc[1, "rachat"] <- max(SCR_rachat_up, SCR_rachat_down, SCR_rachat_mass)

      # Frais
      if ("frais" %in% table_scenario){
          vectSCR_sousc[1, "frais"] <- delta_nav(ACTIF_central, Be_valeurs[["central"]], ACTIF_central, Be_valeurs[["frais"]])
      }

      # Stockage des matrices
      matrice_choc_action <- table_choc@matrice_choc_action
      matrice_choc_mket <- table_choc@matrice_choc_mket
      matrice_choc_sousc <- table_choc@matrice_choc_sousc
      matrice_choc_bscr <- table_choc@matrice_choc_bscr

      # Fonction d'agregation des sous SCR
      agregation_SCR <- function(vect_SCR, matrice_correlation){
          sqrt(vect_SCR %*% matrice_correlation %*% t(vect_SCR))
      }

      #Calcul du SCR Action
      SCR_action <- agregation_SCR(vectSCR_action, matrice_choc_action)

      # Calcul du SCR Marche
      vectSCR_mket[1, "action"] <- SCR_action
      SCR_mket <- agregation_SCR(vectSCR_mket, matrice_choc_mket)

      # Calcul du SCR Souscription Vie
      SCR_sousc <- agregation_SCR(vectSCR_sousc, matrice_choc_sousc)

      # Calcul du BSCR
      vectBSCR[1, "SCR_mket"] <- SCR_mket
      vectBSCR[1, "SCR_sousc"] <- SCR_sousc
      BSCR <- agregation_SCR(vectBSCR, matrice_choc_bscr)
      colnames(BSCR) <- "BSCR"

      # Output
      resultat_bscr <- list(vectSCR_action, vectSCR_mket, vectSCR_sousc, BSCR)
      names(resultat_bscr) <- c("vectSCR_action", "vectSCR_mket", "vectSCR_sousc", "BSCR")
      return(resultat_bscr)
      }
)

