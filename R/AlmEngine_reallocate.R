#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction reallocate
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Realise les operations d'achats ventes
##'
##' \code{reallocate} est une methode permettant d'ajuster l'allocation du \code{\link{PortFin}} de l'assureur.
##' @name reallocate
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}.
##' @param ptf_reference est le portefeuille de reinvestissement. C'est un objet de la classe \code{\link{PortFin}}.
##' @param alloc_cible est un vecteur de type \code{numeric} constitue de 4 elements, il contient les proportions cibles d'allocations
##' action, immobilier, obligataire et de tresorerie.
##' @return \code{portFin} l'objet initial de la classe \code{\link{PortFin}} realloue a l'allocation cible.
##' @return \code{pmvr} le montant total des plus ou moins values realisees.
##' @return \code{pmvr_oblig} le montant des plus ou moins values obligataires realisees lors de la reallocation.
##' @return \code{pmvr_action} le montant des plus ou moins values action realisees lors de l'etape de reallocation.
##' @return \code{pmvr_immo}  le montant des plus ou moins values immobilieres realisees lors de l'etape de reallocation.
##' @return \code{var_rc} la variation de la reserve de capitalisation induite par la reallocation.
##' @return \code{var_pre} la variation de la provision pour risque d'exigibilite induite par la reallocation.
##' @return \code{plac_moy_vm} la valeur de marche moyenne des placements de l'assureur au cours de l'operation de reallocation.
##' @return \code{plac_moy_vnc} la valeur nette comptable moyenne des placements de l'assureur au cours de l'operation de reallocation.
##' @note Les operations d'achat/vente sont effectuees en termes de nombre d'unite d'achat/vente.
##' @author Prim'Act
##' @seealso La classe \code{\link{PortFin}}.
##' @export
##' @include AlmEngine_class.R PortFin_class.R

setGeneric(name = "reallocate", def = function(x, ptf_reference, alloc_cible) {
    standardGeneric("reallocate")
})
setMethod(
    f = "reallocate",
    signature = c(x = "PortFin", ptf_reference = "PortFin", alloc_cible = "numeric"),
    definition = function(x, ptf_reference, alloc_cible) {
        # Verification des inputs
        if (sum(alloc_cible) != 1L | sum(alloc_cible < 0) > 0L) {
            stop("[AlmEngine : reallocate] : L'allocation cible doit etre un vecteur constitue
           d'elements positifs ou nuls, dont la somme totale vaut 1. \n")
        }

        # memoire n'est pas branche pour l'instant du fait de la necessite de coder les operations en cas de vente pour chaque portefeuille
        # (le plus simple est de faire sortir les portefeuilles vendues par chaque fonction sell de base (Action Immo ou Oblig)),
        # une fois ceci code il permettra de conserver une memorisation de cacune des operations effectuees dans un dataframe
        # Initialisation du stock de plus ou moins value realisee
        pmvr <- 0
        pmvr_oblig <- 0
        pmvr_action <- 0
        pmvr_immo <- 0

        # L'allocation cible est contenue dans le portefeuille de reference
        # 1 : calcul des montants a reallouer
        alloc_init <- print_alloc(x)
        # Calcul du vecteur cible
        # Premier element : VM cible action
        # Second element : VM cible Immo
        # Troisieme element : VM cible Oblig
        vm_cible <- .subset2(alloc_init, 5L) * alloc_cible
        # Calcul du montant de vm a acheter ou vendre pour chacune des classes
        vm_achat <- vm_cible - alloc_init[1L:4L, "alloc_valeur"]

        # Action
        vm_achat_action <- vm_achat[1L]
        if (vm_achat_action > 0) {
            # Achat
            # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf action de ref
            # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
            ptf_ref_action <- ptf_reference@ptf_action
            slot_ptf_ref_action <- ptf_ref_action@ptf_action
            nom_ptf <- names(slot_ptf_ref_action)
            num_nb_unit <- which(nom_ptf == "nb_unit")
            num_val_marche <- which(nom_ptf == "val_marche")

            coeff_mult <- vm_achat_action * .subset2(slot_ptf_ref_action, num_nb_unit) / .subset2(slot_ptf_ref_action, num_val_marche)
            ptf_bought <- create_ptf_bought_action(ptf_ref_action, coeff_mult)
            x@ptf_action <- buy_action(x@ptf_action, ptf_bought)
        } else if (vm_achat_action < 0) {
            # Vente
            # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
            ptf_action <- x@ptf_action
            montant_vente <- (-vm_achat_action)
            inputs_vente <- do_calc_nb_sold_action(ptf_action, montant_vente, "proportionnelle")
            temp_operation <- sell_action(ptf_action, inputs_vente[, "num_mp"], inputs_vente[, "nb_sold"])
            x@ptf_action <- temp_operation[["action"]]
            pmvr_action <- temp_operation[["pmvr"]]
            pmvr <- pmvr + pmvr_action
        }

        # Immo
        vm_achat_immo <- vm_achat[2L]
        if (vm_achat_immo > 0) {
            # Achat
            # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf immo de ref
            # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
            ptf_ref_immo <- ptf_reference@ptf_immo
            slot_ptf_ref_immo <- ptf_ref_immo@ptf_immo
            nom_ptf <- names(slot_ptf_ref_immo)
            num_nb_unit <- which(nom_ptf == "nb_unit")
            num_val_marche <- which(nom_ptf == "val_marche")

            coeff_mult <- vm_achat_immo * .subset2(slot_ptf_ref_immo, num_nb_unit) / .subset2(slot_ptf_ref_immo, num_val_marche)
            ptf_bought <- create_ptf_bought_immo(ptf_ref_immo, coeff_mult)
            x@ptf_immo <- buy_immo(x@ptf_immo, ptf_bought)
        } else if (vm_achat_immo < 0) {
            # Vente
            # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
            ptf_immo <- x@ptf_immo
            montant_vente <- (-vm_achat_immo)
            inputs_vente <- do_calc_nb_sold_immo(ptf_immo, montant_vente, "proportionnelle")
            temp_operation <- sell_immo(ptf_immo, inputs_vente[, "num_mp"], inputs_vente[, "nb_sold"])
            x@ptf_immo <- temp_operation[["immo"]]
            pmvr_immo <- temp_operation[["pmvr"]]
            pmvr <- pmvr + pmvr_immo
        }

        # Oblig
        vm_achat_oblig <- vm_achat[3L]
        if (vm_achat_oblig > 0) { # Achat

            # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf action de ref
            # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
            ptf_ref_oblig <- ptf_reference@ptf_oblig
            slot_ptf_ref_oblig <- ptf_ref_oblig@ptf_oblig
            nom_ptf <- names(slot_ptf_ref_oblig)
            num_nb_unit <- which(nom_ptf == "nb_unit")
            num_val_marche <- which(nom_ptf == "val_marche")

            coeff_mult <- vm_achat_oblig * .subset2(slot_ptf_ref_oblig, num_nb_unit) / .subset2(slot_ptf_ref_oblig, num_val_marche)
            ptf_bought <- create_ptf_bought_oblig(ptf_ref_oblig, coeff_mult)
            x@ptf_oblig <- buy_oblig(x@ptf_oblig, ptf_bought)
        } else if (vm_achat_oblig < 0) { # Vente

            # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
            ptf_oblig <- x@ptf_oblig
            montant_vente <- (-vm_achat_oblig)
            inputs_vente <- do_calc_nb_sold_oblig(ptf_oblig, montant_vente, "proportionnelle")
            temp_operation <- sell_oblig(ptf_oblig, inputs_vente[, "num_mp"], inputs_vente[, "nb_sold"])
            x@ptf_oblig <- temp_operation[["oblig"]]
            pmvr_oblig <- temp_operation[["pmvr"]]
            pmvr <- pmvr + pmvr_oblig
        }

        # Tresorerie
        x@ptf_treso <- update_treso(x@ptf_treso, vm_achat[4L])

        # Mise a jour des PMVL Action/Immo/Oblig
        x <- do_update_pmvl(x)

        # Re-evaluation et mise a jour de la Reserve de capitalisation
        res_rc <- calc_RC(x@rc, pmvr_oblig)
        # Mise a jour de la valeur courante
        x@rc <- do_update_RC_val_courante(x@rc, res_rc[["rc_courante"]])

        # Re-evaluation et mise a jour de la PRE
        res_pre <- calc_PRE(x@pre, x@pvl_action + x@mvl_action + x@pvl_immo + x@mvl_immo)
        # Mise a jour de la valeur courante
        x@pre <- do_update_PRE_val_courante(x@pre, res_pre[["pre_courante"]])

        # Calcul des valeurs moyennes
        alloc_cour <- print_alloc(x)

        # EXtraction de donnees
        vm_vnc_precedent <- x@vm_vnc_precedent

        # Valeur moyenne des placements en valeur de marche
        plac_moy_vm <- (.subset2(alloc_cour, 5L) + sum(unlist(vm_vnc_precedent[["vm"]]))) / 2
        # Valeur moyenne des placements en valeur nette comptable
        plac_moy_vnc <- (.subset2(alloc_cour, 3L * 5L) + sum(unlist(vm_vnc_precedent[["vnc"]]))) / 2


        return(list(
            portFin = x, pmvr = pmvr, pmvr_oblig = pmvr_oblig, pmvr_action = pmvr_action, pmvr_immo = pmvr_immo,
            var_rc = res_rc[["var_rc"]], var_pre = res_pre[["var_pre"]],
            plac_moy_vm = plac_moy_vm, plac_moy_vnc = plac_moy_vnc
        ))
    }
)
