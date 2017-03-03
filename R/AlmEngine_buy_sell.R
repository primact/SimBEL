# Script contenant la methode d'achat vente

# Fonctionnement du porefeuille de reference
#   - Le ptf de reference est un PTF Fin dont seul les attributs Action/Immo/Oblig sont renseignes
#   - Chacun de ces sous portefeuilles est compose de plusieurs elements
#       - La valeur de marche du portefeuille de reference est la quantite que l'on utilise (pour chaque ligne de chacun de ces sous portefeuille elle evolue chaque annee)
#       - Le nb d'unite de chacune de ces lignes est initialement determine de telle sorte a respecter ...
#          ... le rapport allocation/valeur d'achat initial tel que renseigne par l'utilisateur dans les fichiers d'input Ptf_ref
#     => Si l'assureur doit ajuster son portefeuille action de +50Ke et qu'a cette date le portefeuille action de reference vaut 25Ke il achetera le ptf action de reference (dont les unites et valeurs auront ete multipliees par 2)
#
# Le reinvestissement au sein des differentes lignes du portefeuille est pour l'instant effectue proportionnellement a celle decrite dans le portefeuille de reference. Cette vision est statique en termes de nb_unit et evolue seulement en VM (grosse limite ?)
# L'allocation cible est determinee par le poids respectifs des VM de chacun des ptfs?


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction reallocate
#----------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name = "reallocate", def = function(x, ptf_reference, alloc_cible){standardGeneric("reallocate")})
setMethod(
  f = "reallocate",
  signature = c(x = "PortFin", ptf_reference = "PortFin", alloc_cible = "numeric"),
  definition = function(x, ptf_reference, alloc_cible){

    # Verification des inputs
    if(sum(alloc_cible) != 1 | sum(alloc_cible < 0) > 0) { stop("[AlmEngine : reallocate] : L'allocation cible doit etre un vecteur constitue d'elements positifs ou nuls, dont la somme totale vaut 1. \n")}

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
        ptf_ref_action  <- ptf_reference["ptf_action"]
        coeff_mult      <- vm_achat[1] * ptf_ref_action["ptf_action"][,"nb_unit"] / ptf_ref_action["ptf_action"][,"val_marche"]
        ptf_bought      <- create_ptf_bought_action(ptf_ref_action, coeff_mult)
        x@ptf_action <- buy_action(x@ptf_action, ptf_bought)

    } else if(vm_achat[1] < 0) {
      # Vente
        # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
        montant_vente   <- -vm_achat[1]
        inputs_vente    <- do_calc_nb_sold_action(x@ptf_action, montant_vente, "proportionnelle")
        temp_operation  <- sell_action(x@ptf_action, inputs_vente[,"num_mp"], inputs_vente[,"nb_sold"])
        x@ptf_action <- temp_operation[["action"]]
        pmvr_action <- temp_operation[["pmvr"]]
        pmvr <- pmvr + pmvr_action
    }

    # Immo
    if(vm_achat[2] > 0){
      # Achat
        # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf immo de ref
        # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
        ptf_ref_immo  <- ptf_reference["ptf_immo"]
        coeff_mult    <- vm_achat[2] * ptf_ref_immo["ptf_immo"][,"nb_unit"] / ptf_ref_immo["ptf_immo"][,"val_marche"]
        ptf_bought    <- create_ptf_bought_immo(ptf_ref_immo, coeff_mult)
        x@ptf_immo <- buy_immo(x@ptf_immo, ptf_bought)
    } else if(vm_achat[2] < 0){
      # Vente
        # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
        montant_vente  <- -vm_achat[2]
        inputs_vente   <- do_calc_nb_sold_immo(x@ptf_immo, montant_vente, "proportionnelle")
        temp_operation <- sell_immo(x@ptf_immo, inputs_vente[,"num_mp"], inputs_vente[,"nb_sold"])
        x@ptf_immo  <- temp_operation[["immo"]]
        pmvr_immo <- temp_operation[["pmvr"]]
        pmvr <- pmvr + pmvr_immo
    }

    # Oblig
    if(vm_achat[3] > 0){
      # Achat
        # Creation du ptf action d'achat : Premiere etape raisonner sur le ptf action de ref
        # POINT IMPORTANT : RAPPEL DE CONVENTION LE NB_UNIT DU PTF_REFERENCE EST LA PROPORTION DE CHAQUE ACTIF DANS LE PTF DE REF
        ptf_ref_oblig  <- ptf_reference["ptf_oblig"]
        coeff_mult     <- vm_achat[3] * ptf_ref_oblig["ptf_oblig"][,"nb_unit"] / ptf_ref_oblig["ptf_oblig"][,"val_marche"]
        ptf_bought     <- create_ptf_bought_oblig(ptf_ref_oblig, coeff_mult)
        x@ptf_oblig <- buy_oblig(x@ptf_oblig, ptf_bought)
    } else if(vm_achat[3] < 0) {
      # Vente
        # Calculer le nombre, hypothese de vente proportionnelle precisee dans la fonction : do_calc_nb_sold
        montant_vente  <- -vm_achat[3]
        inputs_vente   <- do_calc_nb_sold_oblig(x@ptf_oblig, montant_vente, "proportionnelle")
        temp_operation <- sell_oblig(x@ptf_oblig, inputs_vente[,"num_mp"], inputs_vente[,"nb_sold"])
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


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonctions do_calc_nb_sold : a replacer au bon endroit
#----------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name = "do_calc_nb_sold_action", def = function(x, montant_vente, method_vente){standardGeneric("do_calc_nb_sold_action")})
setMethod(
  f = "do_calc_nb_sold_action",
  signature = c(x = "Action", montant_vente = "numeric", method_vente = "character"),
  definition = function(x, montant_vente, method_vente){
    if(method_vente == "proportionnelle"){
      alloc         <- x@ptf_action[,"val_marche"] / sum(x@ptf_action[,"val_marche"])
      montant_vente <- montant_vente * alloc
      vm_unitaire   <- x@ptf_action[,"val_marche"] / x@ptf_action[,"nb_unit"]

      num_mp  <- x@ptf_action[,"num_mp"]
      nb_sold <- montant_vente / vm_unitaire
    }
    return(data.frame(num_mp = num_mp, nb_sold = nb_sold))
  }
)

setGeneric(name = "do_calc_nb_sold_immo", def = function(x, montant_vente, method_vente){standardGeneric("do_calc_nb_sold_immo")})
setMethod(
  f = "do_calc_nb_sold_immo",
  signature = c(x = "Immo", montant_vente = "numeric", method_vente = "character"),
  definition = function(x, montant_vente, method_vente){
    if(method_vente == "proportionnelle"){
      alloc         <- x@ptf_immo[,"val_marche"] / sum(x@ptf_immo[,"val_marche"])
      montant_vente <- montant_vente * alloc
      vm_unitaire   <- x@ptf_immo[,"val_marche"] / x@ptf_immo[,"nb_unit"]

      num_mp  <- x@ptf_immo[,"num_mp"]
      nb_sold <- montant_vente / vm_unitaire
    }
    return(data.frame(num_mp = num_mp, nb_sold = nb_sold))
  }
)

setGeneric(name = "do_calc_nb_sold_oblig", def = function(x, montant_vente, method_vente){standardGeneric("do_calc_nb_sold_oblig")})
setMethod(
  f = "do_calc_nb_sold_oblig",
  signature = c(x = "Oblig", montant_vente = "numeric", method_vente = "character"),
  definition = function(x, montant_vente, method_vente){
    if(method_vente == "proportionnelle"){
      alloc         <- x@ptf_oblig[,"val_marche"] / sum(x@ptf_oblig[,"val_marche"])
      montant_vente <- montant_vente * alloc
      vm_unitaire   <- x@ptf_oblig[,"val_marche"] / x@ptf_oblig[,"nb_unit"]

      num_mp  <- x@ptf_oblig[,"num_mp"]
      nb_sold <- montant_vente / vm_unitaire
    }
    return(data.frame(num_mp = num_mp, nb_sold = nb_sold))
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonctions create_ptf_bought : a replacer au bon endroit
#----------------------------------------------------------------------------------------------------------------------------------------------------

# x : objet de la classe Action ( ptf de reference action construit en valeurs unitaires)
# y : coefficient multiplicateur
setGeneric(name = "create_ptf_bought_action", def = function(x, coefficient){standardGeneric("create_ptf_bought_action")})
setMethod(
  f = "create_ptf_bought_action",
  signature = c(x = "Action", coefficient = "numeric"),
  definition = function(x, coefficient){
    if (length(coefficient) != nrow(x@ptf_action)){stop("[Action : create_ptf_bought_action] : Les inputs sont de dimensions distinctes \n")}
    x@ptf_action[,"val_marche"] <- coefficient * x@ptf_action[,"val_marche"]
    x@ptf_action[,"val_nc"]     <- coefficient * x@ptf_action[,"val_nc"]
    x@ptf_action[,"val_achat"]  <- coefficient * x@ptf_action[,"val_achat"]
    x@ptf_action[,"nb_unit"]    <- coefficient * x@ptf_action[,"nb_unit"]
    return(x)
  }
)

# x : objet de la classe Immo ( ptf de reference immo construit en valeurs unitaires)
# y : coefficient multiplicateur
setGeneric(name = "create_ptf_bought_immo", def = function(x, coefficient){standardGeneric("create_ptf_bought_immo")})
setMethod(
  f = "create_ptf_bought_immo",
  signature = c(x = "Immo", coefficient = "numeric"),
  definition = function(x, coefficient){
    if (length(coefficient) != nrow(x@ptf_immo)){stop("[Immo : create_ptf_bought_immo] : Les inputs sont de dimensions distinctes \n")}
    x@ptf_immo[,"val_marche"] <- coefficient * x@ptf_immo[,"val_marche"]
    x@ptf_immo[,"val_nc"]     <- coefficient * x@ptf_immo[,"val_nc"]
    x@ptf_immo[,"val_achat"]  <- coefficient * x@ptf_immo[,"val_achat"]
    x@ptf_immo[,"nb_unit"]    <- coefficient * x@ptf_immo[,"nb_unit"]
    return(x)
  }
)

# x : objet de la classe Oblig( ptf de reference oblig construit en valeurs unitaires)
# y : coefficient multiplicateur
setGeneric(name = "create_ptf_bought_oblig", def = function(x, coefficient){standardGeneric("create_ptf_bought_oblig")})
setMethod(
  f = "create_ptf_bought_oblig",
  signature = c(x = "Oblig", coefficient = "numeric"),
  definition = function(x,coefficient){
    if (length(coefficient) != nrow(x@ptf_oblig)){stop("[Oblig : create_ptf_bought_oblig] : Les inputs sont de dimensions distinctes \n")}
    x@ptf_oblig[,"val_marche"] <- coefficient * x@ptf_oblig[,"val_marche"]
    x@ptf_oblig[,"val_nc"]     <- coefficient * x@ptf_oblig[,"val_nc"]
    x@ptf_oblig[,"val_achat"]  <- coefficient * x@ptf_oblig[,"val_achat"]
    x@ptf_oblig[,"nb_unit"]    <- coefficient * x@ptf_oblig[,"nb_unit"]
    x@ptf_oblig[,"cc"]         <- coefficient * x@ptf_oblig[,"cc"]
    x@ptf_oblig[,"sd"]         <- coefficient * x@ptf_oblig[,"sd"]
    # Il n'y a pas a ajuster les durations et Zsp car un mouvement parallele des flux n'impacte pas ces quantites

    # Validation de l'objet
    validObject(x)

    return(x)
  }
)

