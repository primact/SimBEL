#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de mettre a jour les elements d"un objet PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 26/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_pmvl
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Fonctions servant a mettre a jour l'ensemble des attributs pvl et pml d'un objet PortFin
setGeneric(name = "do_update_pmvl", def = function(x){standardGeneric("do_update_pmvl")})
setMethod(
  f = "do_update_pmvl",
  signature = "PortFin",
  definition = function(x){

    # Affectation des valeurs dans l'objet PortFin
    pmvl_action <- calc_pmvl_action(x["ptf_action"])
    pmvl_immo   <- calc_pmvl_immo(x["ptf_immo"])
    pmvl_oblig  <- calc_pmvl_oblig(x["ptf_oblig"])

    x["pvl_action"] <- pmvl_action[["pvl"]]
    x["mvl_action"] <- pmvl_action[["mvl"]]
    x["pvl_immo"]   <- pmvl_immo[["pvl"]]
    x["mvl_immo"]   <- pmvl_immo[["mvl"]]
    x["pvl_oblig"]  <- pmvl_oblig[["pvl"]]
    x["mvl_oblig"]  <- pmvl_oblig[["mvl"]]
    return(x)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_vm_vnc_precedent
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Ancienne version : verifier qu 'elle ne sert pas

# Fonctions servant a mettre a jour l'ensemble des attributs pvl et pml d'un objet PortFin
# setGeneric(name = "do_update_vm_vnc_precedent", def = function(x, vm_prec, vnc_prec){standardGeneric("do_update_vm_vnc_precedent")})
# setMethod(
#     f = "do_update_vm_vnc_precedent",
#     signature = c(x = "PortFin", vm_prec = "numeric", vnc_prec = "numeric"),
#     definition = function(x, vm_prec, vnc_prec){
#         # Verification des inputs
#         if (length(vm_prec) != length(vnc_prec) | length(vnc_prec) != 3) {stop("[PortFin : do_update_vm_vnc_precedent] : Les vecteurs vm_prec et vnc_prec doivent etre de longueur 3 (chaque valeur correspondant au total des lignes actions / immobiliers / obligataires \n")}
#
#         # Operation de mise a jour :
#         x["vm_vnc_precedent"] <- list(vm = list(action = vm_prec[1],
#                                                 immo   = vm_prec[2],
#                                                 oblig  = vm_prec[3]),
#                                       vnc = list(action = vnc_prec[1],
#                                                  immo   = vnc_prec[2],
#                                                  oblig  = vnc_prec[3]))
#         return(x)
#     }
# )

# Fonction anterieure a une operation de vieillissement
setGeneric(name = "do_update_vm_vnc_precedent", def = function(x){standardGeneric("do_update_vm_vnc_precedent")})
setMethod(
    f = "do_update_vm_vnc_precedent",
    signature = c(x = "PortFin"),
    definition = function(x){

        # Calcul des VM et VNC courantes
        table_alloc <- print_alloc(x)

        # Operation de mise a jour, on affecte a l'attribut VM/VNC precedente les valeurs de VM/VNC actuelle :
        x["vm_vnc_precedent"] <- list(vm = list(action = table_alloc["alloc_action", "alloc_valeur"],
                                                immo   = table_alloc["alloc_immo",   "alloc_valeur"],
                                                oblig  = table_alloc["alloc_oblig",  "alloc_valeur"]),
                                      vnc = list(action = table_alloc["alloc_action", "alloc_valeur_nc"],
                                                 immo   = table_alloc["alloc_immo",   "alloc_valeur_nc"],
                                                 oblig  = table_alloc["alloc_oblig",  "alloc_valeur_nc"]))
        return(x)
    }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Fonction Main d'appel update du portefeuille financier
setGeneric(name = "update_PortFin", def = function(an, x, new_mp_ESG, flux_milieu, flux_fin){standardGeneric("update_PortFin")})
setMethod(
    f = "update_PortFin",
    signature = c(an = "numeric", x = "PortFin", new_mp_ESG = "ModelPointESG", flux_milieu = "numeric", flux_fin = "numeric"),
    definition = function(an, x, new_mp_ESG, flux_milieu, flux_fin){
        # Viellissement du portefeuille financier
        # Mise a jour de l'annee
        x["annee"] <- as.integer(an)

        # Vieillissement des obligations
        liste_res  <- vieillissement_oblig_PortFin(x, new_mp_ESG)
        # Mise a jour du portefeuille
        x          <- liste_res[["portFin"]]
        # Extraction des echeances et des coupons
        echeance   <- liste_res[["echeance"]]
        coupon   <- liste_res[["coupon"]]
        # Extraction de la variation de VNC obligataire
        var_vnc_oblig <- liste_res[["var_vnc_oblig"]]

        # Calcul de la table des rdt actions, tresorerie et immobiliers avec les nouveaux cours
        table_rdt <- calc_rdt(x, new_mp_ESG)

        # Vieillissement des actions
        liste_res <- vieillissement_action_PortFin(x, table_rdt)
        # Mise a jour du portefeuille
        x          <- liste_res[["portFin"]]
        # Extraction des dividendes
        dividende   <- liste_res[["dividende"]]

        # Vieillissement de l'immobilier
        liste_res <- vieillissement_immo_PortFin(x, table_rdt)
        # Mise a jour du portefeuille
        x          <- liste_res[["portFin"]]
        # Extraction des loyers
        loyer   <- liste_res[["loyer"]]

        # Calcul des revenus financiers de milieu d'annne
        revenu_fin <- coupon + dividende + loyer

        # Mise a jour des PMVL Action/Immo/Oblig
        x <- do_update_pmvl(x)
        # Revenu de la treso
        x <- vieillissement_treso_PortFin(x, revenu_fin + flux_milieu, echeance + flux_fin, table_rdt)

        # # Reallocation a l'allocation cible
        # # Verification : le total de l'assiette ne doit pas etre modifie
        # liste_res <- reallocate(x, param_alm_engine["ptf_reference"], param_alm_engine["alloc_cible"])
        # x    <- liste_res[["portFin"]]
        # pmvr <- liste_res[["pmvr"]]

        # Eventuel realisation de PVL (achat vente specifique =>?)
        # Necessite une fonction permettant d'isoler un sous portefeuille constitue uniquement des actifs en plus value
        # Regle de selection des lignes en PVL a vendre?
        # Reequilibrage post vente des actifs specifiques en PVL ?

        # Prise en compte du flux specifique de charges sociales sur la treso (considere comme du tout debut d'annee)
        # A voir avec quentin ???
        # chg_soc =?
        # x["ptf_treso"][,"val_marche"] <- x["ptf_treso"][,"val_marche"] - chg_soc
        # x["ptf_treso"][,"val_nc"]     <- x["ptf_treso"][,"val_nc"] - chg_soc
        return(list(ptf = x, revenu_fin = revenu_fin, var_vnc_oblig = var_vnc_oblig))
    })

# Fonction de mise a jour du portefeuille de reference

setGeneric(name = "update_PortFin_reference", def = function(an, x, mp_ESG){standardGeneric("update_PortFin_reference")})
setMethod(
    f = "update_PortFin_reference",
    signature = c(an = "numeric", x = "PortFin", mp_ESG = "ModelPointESG"),
    definition = function(an, x, mp_ESG){

        table_rdt <- calc_rdt(x, mp_ESG)

        x@annee <- as.integer(an)
        # Update Action

        table_rdt[["rdt_action"]][["rdt"]]
            # VM
            x@ptf_action@ptf_action$val_marche  <- calc_vm_action(x@ptf_action, table_rdt[["rdt_action"]]$rdt)
            # VNC
            x@ptf_action@ptf_action$val_nc      <- x@ptf_action@ptf_action$val_marche
            # VA
            x@ptf_action@ptf_action$val_achat   <- x@ptf_action@ptf_action$val_marche
        # Update Immo
            # VM
            x@ptf_immo@ptf_immo$val_marche  <-  calc_vm_immo(x@ptf_immo,  table_rdt[["rdt_immo"]]$rdt)
            # VNC
            x@ptf_immo@ptf_immo$val_nc      <- x@ptf_immo@ptf_immo$val_marche
            # VA
            x@ptf_immo@ptf_immo$val_achat   <- x@ptf_immo@ptf_immo$val_marche
        # Update Oblig
            # VM
            x@ptf_oblig@ptf_oblig$val_marche  <- calc_vm_oblig(x@ptf_oblig, mp_ESG@yield_curve)
            # VNC
            x@ptf_oblig@ptf_oblig$val_nc      <- x@ptf_oblig@ptf_oblig$val_marche
            # VA
            x@ptf_oblig@ptf_oblig$val_achat   <- x@ptf_oblig@ptf_oblig$val_marche
        return(x)
    }
)
