
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

setGeneric(name = "reallocate", def = function(x, ptf_reference, alloc_cible){standardGeneric("reallocate")})
setMethod(
  f = "reallocate",
  signature = c(x = "PortFin", ptf_reference = "PortFin", alloc_cible = "numeric"),
  definition = function(x, ptf_reference, alloc_cible){

    # Verification des inputs
    if(sum(alloc_cible) != 1 | sum(alloc_cible < 0) > 0) {
      stop("[AlmEngine : reallocate] : L'allocation cible doit etre un vecteur constitue
           d'elements positifs ou nuls, dont la somme totale vaut 1. \n")}

    # memoire n'est pas branche pour l'instant du fait de la necessite de coder les operations en cas de vente pour chaque portefeuille (le plus simple est de faire sortir les portefeuilles vendues par chaque fonction sell de base (Action Immo ou Oblig)),
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
    vm_cible <- alloc_init["alloc_total","alloc_valeur"] * alloc_cible
    # Calcul du montant de vm a acheter ou vendre pour chacune des classes
    vm_achat <-  vm_cible - alloc_init[1:4,"alloc_valeur"]

    # Action
    if(vm_achat[1] > 0){
      # Achat
        # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf action de ref
        # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
        ptf_ref_action  <- ptf_reference@ptf_action
        coeff_mult      <- vm_achat[1] * ptf_ref_action@ptf_action$nb_unit / ptf_ref_action@ptf_action$val_marche
        ptf_bought      <- create_ptf_bought_action(ptf_ref_action, coeff_mult)
        x@ptf_action <- buy_action(x@ptf_action, ptf_bought)

    } else if(vm_achat[1] < 0) {
      # Vente
        # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
        montant_vente   <- -vm_achat[1]
        inputs_vente    <- do_calc_nb_sold_action(x@ptf_action, montant_vente, "proportionnelle")
        temp_operation  <- sell_action(x@ptf_action, inputs_vente$num_mp, inputs_vente$nb_sold)
        x@ptf_action <- temp_operation[["action"]]
        pmvr_action <- temp_operation[["pmvr"]]
        pmvr <- pmvr + pmvr_action
    }

    # Immo
    if(vm_achat[2] > 0){
      # Achat
        # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf immo de ref
        # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
        ptf_ref_immo  <- ptf_reference@ptf_immo
        coeff_mult    <- vm_achat[2] * ptf_ref_immo@ptf_immo$nb_unit / ptf_ref_immo@ptf_immo$val_marche
        ptf_bought    <- create_ptf_bought_immo(ptf_ref_immo, coeff_mult)
        x@ptf_immo <- buy_immo(x@ptf_immo, ptf_bought)
    } else if(vm_achat[2] < 0){
      # Vente
        # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
        montant_vente  <- -vm_achat[2]
        inputs_vente   <- do_calc_nb_sold_immo(x@ptf_immo, montant_vente, "proportionnelle")
        temp_operation <- sell_immo(x@ptf_immo, inputs_vente$num_mp, inputs_vente$nb_sold)
        x@ptf_immo  <- temp_operation[["immo"]]
        pmvr_immo <- temp_operation[["pmvr"]]
        pmvr <- pmvr + pmvr_immo
    }

    # Oblig
    if(vm_achat[3] > 0){
      # Achat
        # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf action de ref
        # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
        ptf_ref_oblig  <- ptf_reference@ptf_oblig
        coeff_mult     <- vm_achat[3] * ptf_ref_oblig@ptf_oblig$nb_uni / ptf_ref_oblig@ptf_oblig$val_marche
        ptf_bought     <- create_ptf_bought_oblig(ptf_ref_oblig, coeff_mult)
        x@ptf_oblig <- buy_oblig(x@ptf_oblig, ptf_bought)
    } else if(vm_achat[3] < 0) {
      # Vente
        # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
        montant_vente  <- -vm_achat[3]
        inputs_vente   <- do_calc_nb_sold_oblig(x@ptf_oblig, montant_vente, "proportionnelle")
        temp_operation <- sell_oblig(x@ptf_oblig, inputs_vente$num_mp, inputs_vente$nb_sold)
        x@ptf_oblig <- temp_operation[["oblig"]]
        pmvr_oblig <- temp_operation[["pmvr"]]
        pmvr <- pmvr + pmvr_oblig
    }
    # Tresorerie
      x@ptf_treso <- update_treso(x@ptf_treso, vm_achat[4])

      # Mise a jour des PMVL Action/Immo/Oblig
      x <- do_update_pmvl(x)

      # Re-evaluation et mise a jour de la Reserve de capitalisation
      res_rc <- calc_RC(x@rc, pmvr_oblig)
      # Mise a jour de la valeur courante
      x@rc <-  do_update_RC_val_courante(x@rc, res_rc[["rc_courante"]])

      # Re-evaluation et mise a jour de la PRE
      res_pre <- calc_PRE(x@pre, x@pvl_action + x@mvl_action + x@pvl_immo + x@mvl_immo)
      # Mise a jour de la valeur courante
      x@pre <-  do_update_PRE_val_courante(x@pre, res_pre[["pre_courante"]])

      # Calcul des valeurs moyennes
      alloc_cour <- print_alloc(x)
      # Valeur moyenne des placements en valeur de marche
      plac_moy_vm <- (.subset2(alloc_cour, 1)[5] + sum(unlist(x@vm_vnc_precedent[["vm"]]))) /2
      # Valeur moyenne des placements en valeur nette comptable
      plac_moy_vnc <- (.subset2(alloc_cour, 3)[5] + sum(unlist(x@vm_vnc_precedent[["vnc"]]))) /2


   return(list(portFin = x, pmvr = pmvr, pmvr_oblig = pmvr_oblig, pmvr_action = pmvr_action, pmvr_immo = pmvr_immo,
               var_rc = res_rc[["var_rc"]], var_pre = res_pre[["var_pre"]],
               plac_moy_vm = plac_moy_vm, plac_moy_vnc = plac_moy_vnc))
  }
)

