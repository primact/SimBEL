#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script execute les operations a effectuer au niveau du canton sur une annee
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           proj_an
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Projette un canton sur une periode.
##'
##' \code{proj_an} est une methode permettant de projeter un canton sur une annee. Cette methode calcule
##' les flux de best estimate des passifs et fait vieillir d'une annee les elements du canton.
##' @name proj_an
##' @docType methods
##' @param x est un objet de type \code{\link{Canton}}.
##' @param annee_fin est une valeur \code{integer} correpondant a l'annee de fin de projection.
##' @param pre_on est une valeur \code{logical} qui lorsqu'elle vaut \code{TRUE} prend en compte la variation
##' de PRE dans le resultat technique, utilisee pour le calcul de la participation aux benefices reglementaires.
##' @details Cette methode est la procedure central du package \code{SimBEL} puisqu'elle cohorde les interactions entre
##' les actifs et les passifs, declenche l'algorithme de revalorisation, calcule le resultat comptable et evalue les
##' flux de best estimate.
##' @return \code{canton} l'objet  \code{x} vieilli d'une annee.
##' @return \code{annee} l'annee de projection.
##' @return \code{nom_produit} le nom des produits de passifs consideres.
##' @return \code{output_produit} une liste comprenant les variables de flux, les variables de stocks et les resultats
##' des passifs non-modelises.
##' @return \code{output_be} une liste comprenant les flux utilises pour le calcul du best estimate par produit.
##' @return \code{result_tech} la valeur du resultat technique.
##' @return \code{result_fin} la valeur du resultat financier.
##' @return \code{tra} la valeur du taux de rendement de l'actif.
##' @return \code{result_brut} la valeur du resultat brut d'impot.
##' @return \code{result_net} la valeur du resultat net d'impot.
##' @author Prim'Act
##' @seealso Le viellissement du portefeuille de passif avant PB : \code{\link{viellissement_av_pb}}.
##' Le viellissement du portefeuille financier : \code{\link{update_PortFin}}, \code{\link{update_PortFin_reference}}.
##' L'affiche de l'etat courant du portefeuille financier : \code{\link{print_alloc}}.
##' Le calcul des frais financier : \code{\link{calc_frais_fin}}.
##' La reallocation du portefeuille financier : \code{\link{reallocate}}.
##' Le calcul de la PRE : \code{\link{calc_PRE}}.
##' Le calcul du resultat technique : \code{\link{calc_result_technique}}, \code{\link{calc_result_technique_ap_pb}}.
##' Le calcul du resultat financier et du TRA : \code{\link{calc_resultat_fin}}, \code{\link{calc_tra}}.
##' L'application de l'algorithme d'attribution de la participation aux benefices : \code{\link{calc_revalo}}.
##' Le viellissement du portefeuille de passif apres PB : \code{\link{vieillissment_ap_pb}}.
##' Les autres methodes de vieillissement des actifs et de passifs: \code{\link{sell_pvl_action}},
##' \code{\link{do_update_pmvl}}, \code{\link{do_update_PRE_val_courante}},
##' \code{\link{do_update_vm_vnc_precedent}}, \code{\link{vieillissement_ppb}}, \code{\link{do_update_RC_val_debut}},
##' \code{\link{do_update_PRE_val_debut}}, \code{\link{init_debut_pgg_psap}}.
##' Le calcul des fins de projection : \code{\link{calc_fin_proj}}.
##' @export
##' @include Canton_class.R
##'
setGeneric(name = "proj_an", def = function(x, annee_fin, pre_on){standardGeneric("proj_an")})
setMethod(
    f = "proj_an",
    signature = c(x = "Canton", annee_fin = "integer", pre_on = "logical"),
    definition = function(x, annee_fin, pre_on){
        
        #---------------------------------------------------------------
        # Etape 1 : Mise a jour des annees de projection
        #---------------------------------------------------------------
        x@annee <- x@annee + 1L
        
        annee <-  x@annee
        
        #---------------------------------------------------------------
        # Etape 2 : variables economiques utilisees au passif
        # Remarque : A terme, revoir ce mode de fonctionemment qui n'est pas ideal en terme de structure.
        #---------------------------------------------------------------
        
        # Coefficient d'inflation
        coef_inf <- x@mp_esg@indice_inflation
        
        # liste des rendements
        list_rd <- calc_rdt_marche_ref(x@ptf_passif@ht@param_comport[[x@hyp_canton@method_taux_cible]], x@mp_esg)
        
        #---------------------------------------------------------------
        # Etape 3 : Gestion des passifs avant participation aux benefices
        #---------------------------------------------------------------
        # Evaluation du passif vieilli d'un an
        passif_av_pb <- viellissement_av_pb(annee, x@ptf_passif, coef_inf, list_rd, x@hyp_canton@tx_soc)
        # Mise a jour des passifs
        x@ptf_passif <- passif_av_pb[["ptf"]]
        
        # Calcul de la PPB de premiere anneee attribuee au flux garanti
        pm_deb <- passif_av_pb[["result_av_pb"]][["stock_agg"]][, "pm_deb"]
        if(annee == 1L){
            # Somme des PM
            sum_pm_deb <- sum(pm_deb)
            
            # Gestion des divisions par 0
            if(sum_pm_deb != 0) # Attribution au prorata de la PM
                ppb_init_attrib <- x@ppb@ppb_debut * pm_deb / sum_pm_deb
            else # Division par 0
                ppb_init_attrib <- x@ppb@ppb_debut * 1 / length(pm_deb)
            
        } else {
            ppb_init_attrib <- rep(0, length(pm_deb))
        }
        
        #---------------------------------------------------------------
        # Etape 4 : Gestion des actifs avant allocation
        #---------------------------------------------------------------
        actif_vieil <- update_PortFin(annee, x@ptf_fin, x@mp_esg, passif_av_pb[["flux_milieu"]], passif_av_pb[["flux_fin"]])
        # Mise a jour des actifs
        x@ptf_fin <- actif_vieil[["ptf"]]
        
        # Extraction des revenus financiers et de la variation de VNC obligataires
        revenu_fin <- actif_vieil[["revenu_fin"]]
        revenu_fin_det <- actif_vieil[["revenu_fin_det"]]
        var_vnc_oblig <- actif_vieil[["var_vnc_oblig"]]
        
        # Mise a jour du portfeuille de reference
        x@param_alm@ptf_reference <- update_PortFin_reference(annee, x@param_alm@ptf_reference, x@mp_esg)
        
        
        #---------------------------------------------------------------
        # Etape 5 : Calcul des frais financiers
        #---------------------------------------------------------------
        # Calcul des valeurs moyennes
        alloc_cour <- print_alloc(x@ptf_fin)
        # Valeur moyenne des placements en valeur de marche
        plac_moy_vm <- (.subset2(alloc_cour, 5L) + sum(unlist(x@ptf_fin@vm_vnc_precedent[["vm"]]))) /2
        
        frais_fin <- calc_frais_fin(x@ptf_fin@frais_fin, plac_moy_vm, coef_inf)
        
        #  Mise a jour de la tresorie
        x@ptf_fin@ptf_treso <- update_treso(x@ptf_fin@ptf_treso , - frais_fin)
        
        #---------------------------------------------------------------
        # Etape 6 : Re-allocation des actifs et mise a jour de la PRE et de la RC
        #---------------------------------------------------------------
        # Reallocation a l'allocation cible
        
        # Gestion de l'anomalie : valeur de marche des actifs negatives
        if(.subset2(print_alloc(x@ptf_fin), 5L) < 0){
            warning(paste("Attention, la valeur de marche des actifs est negative pour
                    la simulation ", x@mp_esg@num_traj, " en annee ", annee,".", sep = ""))
            
            # Dans le cas d'un actif negatif, la simulation est arretee.
            return(FALSE)
        }
        
        actif_realloc <- reallocate(x@ptf_fin, x@param_alm@ptf_reference, x@param_alm@alloc_cible)
        x@ptf_fin <- actif_realloc[["portFin"]]
        pmvr <- list(oblig = actif_realloc[["pmvr_oblig"]], 
                     action = actif_realloc[["pmvr_action"]], 
                     immo = actif_realloc[["pmvr_immo"]])
        
        #---------------------------------------------------------------
        # Etape 7 : Calcul du resultat technique
        #---------------------------------------------------------------
        # Calcul du resultats technique avec attribution de PB
        resultat_tech <- calc_result_technique(passif_av_pb, actif_realloc[["var_pre"]] * pre_on)
        
        #---------------------------------------------------------------
        # Etape 8 : Calcul du resultat financier et du TRA
        #---------------------------------------------------------------
        
        # Evaluation du resultat financier
        resultat_fin <- calc_resultat_fin(revenu_fin + var_vnc_oblig, actif_realloc[["pmvr"]],
                                          frais_fin, actif_realloc[["var_rc"]])
        
        # Calcul du TRA
        tra <- calc_tra(actif_realloc[["plac_moy_vnc"]], resultat_fin)
        
        #---------------------------------------------------------------
        # Etape 9 : Application de la politique de revalorisation
        #---------------------------------------------------------------
        
        # Calcul de la politique de revalorisation
        result_revalo <- calc_revalo(x, passif_av_pb, tra, actif_realloc[["plac_moy_vnc"]], resultat_tech)
        
        # Mise a jour de la PPB
        x@ppb <- result_revalo[["ppb"]]
        
        #---------------------------------------------------------------
        # Etape 10 : Mise a jour des passifs
        #---------------------------------------------------------------
        
        # Evaluation du passif vieilli d'un an apres pb
        passif_ap_pb <- vieillissment_ap_pb(x@ptf_passif, result_revalo[["add_rev_nette_stock"]], x@hyp_canton@tx_soc)
        # Mise a jour des passifs a fin d'annee
        x@ptf_passif <- passif_ap_pb[["ptf"]]
        
        # Allocation des frais financiers par produit
        pm_moy <- passif_av_pb[["result_av_pb"]][["stock_agg"]][, "pm_moy"]
        sum_pm_moy <- sum(pm_moy)
        if(sum_pm_moy != 0) {
            coef_alloc <- pm_moy / sum_pm_moy
            # Les frais financiers sont mis a l'echelle des passifs et alloues
            frais_fin_prod <- frais_fin * (sum_pm_moy + x@ppb["ppb_debut"]) / actif_realloc[["plac_moy_vnc"]] * coef_alloc
        } else { # Division par 0
            frais_fin_prod <- rep(0, length(pm_moy))
        }
        
        # Frais financier associes au autres passifs
        result_autres_passifs <- passif_av_pb[["result_autres_passifs"]]
        frais_fin_hors_model <- frais_fin * ((result_autres_passifs$pm_fin + result_autres_passifs$pm_deb ) / 2) /
            actif_realloc[["plac_moy_vnc"]]
        
        #---------------------------------------------------------------
        # Etape 11 : Mise a jour des actifs
        #---------------------------------------------------------------
        
        # Realisation des eventuelles ventes de PVL actions realisees a l etape 9
        # mise a jour des actions.
        x@ptf_fin@ptf_action <- sell_pvl_action(x@ptf_fin@ptf_action, result_revalo[["pmvl_liq"]])[["action"]]
        
        # Mise a jour des PMVL Action/Immo/Oblig
        x@ptf_fin <- do_update_pmvl(x@ptf_fin)
        
        # Re-evaluation et mise a jour de la PRE
        res_pre <- calc_PRE(x@ptf_fin@pre, x@ptf_fin@pvl_action + x@ptf_fin@mvl_action +
                                x@ptf_fin@pvl_immo + x@ptf_fin@mvl_immo)
        # Mise a jour de la valeur courante de la PRE
        x@ptf_fin["pre"] <-  do_update_PRE_val_courante(x@ptf_fin@pre, res_pre[["pre_courante"]])
        
        #---------------------------------------------------------------
        # Etape 12 : Mise a jour des resultats financier et technique
        #---------------------------------------------------------------
        
        # Mise a jour du resultat financier des eventuelles PVL actions realisees a l etape 9
        resultat_fin <- resultat_fin + result_revalo[["pmvl_liq"]]
        # Calcul du TRA
        tra <- calc_tra(actif_realloc[["plac_moy_vnc"]], resultat_fin)
        
        # Mise a jour du resultat technique
        resultat_tech <- calc_result_technique_ap_pb(passif_av_pb, passif_ap_pb, x@ppb, res_pre[["var_pre"]])
        
        # Calcul du resultats brut et net d'impot
        result_brut <- resultat_tech + resultat_fin
        result_net <- result_brut * (1 - x@hyp_canton@tx_import) * (result_brut > 0) + result_brut * (result_brut <= 0)
        impot <- result_brut - result_net
        
        #---------------------------------------------------------------
        # Etape 13 : Mise a jour (des elements restants) du canton pour l'annee suivante
        #---------------------------------------------------------------
        
        #  Mise a jour de la tresorie : prelevement sociaux sur stock
        x@ptf_fin@ptf_treso <- update_treso(x@ptf_fin@ptf_treso , - sum(passif_ap_pb[["flux_agg"]][,"soc_stock_ap_pb"]))
        # Mise a jour des montant totaux de VM et de VNC des actifs
        x@ptf_fin <- do_update_vm_vnc_precedent(x@ptf_fin)
        
        # PPB
        x@ppb <- vieillissement_ppb(x@ppb)
        
        # PRE, RC
        x@ptf_fin@rc <-  do_update_RC_val_debut(x@ptf_fin@rc, x@ptf_fin@rc@val_courante)
        x@ptf_fin["pre"] <-  do_update_PRE_val_debut(x@ptf_fin@pre, x@ptf_fin@pre@val_courante) # On conserve la validation car pas fait dans l'objet
        # PGG, PSAP
        x@ptf_passif["autres_reserves"] <-  init_debut_pgg_psap(x@ptf_passif@autres_reserves) # On conserve la validation car pas fait dans l'objet
        
        # # Controle que l'actif en valeur de marche n'est pas negatif
        # if(.subset2(print_alloc(x@ptf_fin), 1)[5] < 0)
        #   warning("Attention, la valeur de marche des actifs est negative.")
        
        #---------------------------------------------------------------
        # Etape 14 : Gestion des fins de projection
        #---------------------------------------------------------------
        if(annee == annee_fin){
            # Calcul des fins de projection
            fin_proj <- calc_fin_proj(x, resultat_fin, resultat_tech, passif_ap_pb[["stock_agg"]][,"pm_fin_ap_pb"],
                                      result_revalo[["tx_pb"]], result_revalo[["tx_enc_moy"]])
            
            # Extraction des resultats
            flux_fin_passif <- fin_proj[["flux_fin_passif"]]
            resultat_tech <- fin_proj[["result_tech"]]
            resultat_fin <- fin_proj[["resultat_fin"]]
            result_brut <- fin_proj[["result_brut"]]
            result_net <- fin_proj[["result_net"]]
            impot <- fin_proj[["impot"]]
            
        } else { # Sinon le flux de fin est nul
            flux_fin_passif = rep(0, length(passif_av_pb[["nom_produit"]]))
        }
        
        #---------------------------------------------------------------
        # Etape 15 : Creation de listes stockant les flux de BE
        #---------------------------------------------------------------
        
        # Noms des produits
        nom_produits <- passif_av_pb[["nom_produit"]]
        
        # Reprendre les flux de resultats
        flux_produit <- cbind(passif_av_pb[["result_av_pb"]][["flux_agg"]],
                              passif_ap_pb[["flux_agg"]], frais_fin = frais_fin_prod)
        
        stock_produit <- cbind(passif_av_pb[["result_av_pb"]][["stock_agg"]],
                               passif_ap_pb[["stock_agg"]])
        
        # Reprendre les flux de resultats hors models
        hors_model <- passif_av_pb[["result_autres_passifs"]]
        
        fin <- cbind(melt(flux_fin_passif, value.name = "flux_fin_passif"), 
                     melt(result_revalo[["ppb8_ind"]], value.name = "ppb8"))
        
        
        output_produit <- list(flux_produit  = flux_produit,
                               stock_produit = stock_produit,
                               fin           = fin,
                               hors_model    = hors_model)
        
        output_be <- list(prime = c(flux_produit[, "pri_brut"], hors_model$prime),
                          prestation = c(flux_produit[, "prest"] +
                                             flux_produit[, "rev_prest_nette"] -
                                             flux_produit[,"rach_charg"] +
                                             flux_produit[,"soc_stock_ap_pb"] +
                                             flux_fin_passif, hors_model$prestation),
                          prestation_fdb = c(flux_produit[, "prest_fdb"] - ppb_init_attrib, 0),
                          frais = c(flux_produit[,"frais_var_prime"] +
                                        flux_produit[,"frais_fixe_prime"] +
                                        flux_produit[,"frais_var_prest"] +
                                        flux_produit[,"frais_fixe_prest"] +
                                        flux_produit[,"frais_var_enc"] +
                                        flux_produit[,"frais_fixe_enc"] +
                                        flux_produit[,"frais_fin"], hors_model$frais + frais_fin_hors_model))
        
        
        #---------------------------------------------------------------
        # Etape 16 : Creation de listes stockant les flux des actifs
        #---------------------------------------------------------------
        
        flux_ptf_fin <- list(action = cbind(annee = annee, actif = "Action", x@ptf_fin@ptf_action@ptf_action),
                             immo   = cbind(annee = annee, actif = "Immobilier", x@ptf_fin@ptf_immo@ptf_immo),
                             oblig  = cbind(annee = annee, actif = "Obligation", x@ptf_fin@ptf_oblig@ptf_oblig),
                             treso  = cbind(annee = annee, actif = "Tresorerie", x@ptf_fin@ptf_treso@ptf_treso))
        
        flux_fin <- data.frame(annee = annee, revenu_oblig = revenu_fin_det[["oblig"]],  revenu_action = revenu_fin_det[["action"]],  
                               revenu_immo = revenu_fin_det[["immo"]], var_vnc_oblig = var_vnc_oblig, 
                               pmvr_oblig = pmvr[["oblig"]],  pmvr_action = pmvr[["action"]],  pmvr_immo = pmvr[["immo"]],
                               frais_fin = frais_fin, var_rc = actif_realloc[["var_rc"]])
        
        
        #---------------------------------------------------------------
        # Etape 17 : Creation du DF stockant la PB
        #---------------------------------------------------------------
        
        output_pb <- data.frame(annee = annee, 
                                ppb8 = sum(result_revalo[["ppb8_ind"]]), 
                                stock_ppb  = result_revalo[["pb_attrib"]][["stock_ppb"]], 
                                tot_pb_rep = result_revalo[["pb_attrib"]][["pb_rep"]], 
                                tot_pb_dot = result_revalo[["pb_attrib"]][["pb_dot"]])
        
        # validation de l'objet
        # validObject(x)
        
        # Output
        return(list(canton         = x,
                    annee          = annee,
                    nom_produit    = c(nom_produits, "hors_model"),
                    output_produit = output_produit,
                    output_be      = output_be,
                    output_pb      = output_pb,
                    flux_ptf_fin   = flux_ptf_fin,
                    flux_fin       = flux_fin,
                    result_tech    = resultat_tech,
                    result_fin     = resultat_fin,
                    tra            = tra,
                    result_brut    = result_brut,
                    result_net     = result_net))
    }
)
