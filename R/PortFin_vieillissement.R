#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_oblig_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Fonction permettant d'effectuer une operation de vieillissement du portefeuille obligataire du portefeuille financier
# Prend en input le PortFin (x), le model point des conditions de l'annee (new_mp_ESG : objet de la classe ModelPointESG).
setGeneric(name = "vieillissement_oblig_PortFin", def = function(x, new_mp_ESG){standardGeneric("vieillissement_oblig_PortFin")})
setMethod(
  f = "vieillissement_oblig_PortFin",
  signature = c(x = "PortFin", new_mp_ESG = "ModelPointESG"),
  definition = function(x, new_mp_ESG){
    # Verification input :
    if(nrow(x["ptf_oblig"]["ptf_oblig"]) > 0) { 
      
        # Recalcul des VM obligs avec nouvelle courbe de taux et vieillissement du pas de temps,
        # valeur nette comptable debut
        vnc_debut <- x@vm_vnc_precedent[["vnc"]][["oblig"]]
    
        # Calcul des flux
        flux <- calc_flux_annee(x["ptf_oblig"])
        coupon <- sum(flux[["tombee_coupon"]]) # Tombee de coupons
        echeance <- sum(flux[["tombee_echeance"]]) # Tombee de coupons

        # Viellissement des maturites et suppression des lignes obligataires arrivees au terme
        x["ptf_oblig"] <- update_mat_res(x["ptf_oblig"])
        # Mise a jour des VM
        if( nrow(x["ptf_oblig"]["ptf_oblig"])>0) x["ptf_oblig"] <- update_vm_oblig(x["ptf_oblig"], calc_vm_oblig(x["ptf_oblig"], new_mp_ESG["yield_curve"]))
        # Mise a jour des VNC
        if( nrow(x["ptf_oblig"]["ptf_oblig"])>0) x["ptf_oblig"] <- update_vnc_oblig(x["ptf_oblig"], calc_vnc(x["ptf_oblig"], x["ptf_oblig"]["ptf_oblig"][,"sd"]))
        
        # Calcul la VNC de fin et sa variation
        if( nrow(x["ptf_oblig"]["ptf_oblig"])>0)  vnc_fin       <- sum(x@ptf_oblig@ptf_oblig$val_nc)
        if( nrow(x["ptf_oblig"]["ptf_oblig"])==0) vnc_fin <- 0
        
        var_vnc_oblig <- vnc_fin - vnc_debut

        # Renvoi le portfeuille financier dont les lignes obligataires sont a jour, et les echeances (remboursement obligataire) a venir en fin d'annee
        return(list(portFin = x, coupon = coupon, echeance = echeance, var_vnc_oblig = var_vnc_oblig))
    } else {
        return(list(portFin = x, coupon = 0, echeance = 0, var_vnc_oblig = 0))
    }
  })

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_action_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------


# Fonction permettant d'effectuer une operation de vieillissement des portefeuilles action et immo du portefeuille financier
# Prend en input le PortFin (x), les tables d'evolution des cours et rdt (calcules par la fonction calc_rdt).
setGeneric(name = "vieillissement_action_PortFin", def = function(x, table_rdt){standardGeneric("vieillissement_action_PortFin")})
setMethod(
  f = "vieillissement_action_PortFin",
  signature = c(x = "PortFin", table_rdt = "list"),
  definition = function(x, table_rdt){
    # Verification input :
    if(nrow(x["ptf_action"]["ptf_action"]) > 0) {
    
        # Calcul des dividendes
        dividende  <- sum(table_rdt[["rdt_action"]][["div"]])

        # Mise a jour de la VM Action en fin d'annee
        x["ptf_action"] <- update_vm_action(x["ptf_action"], calc_vm_action(x["ptf_action"], table_rdt[["rdt_action"]][["rdt"]] ))
        # Mise a jour des durees de detention Action/Immo
        x["ptf_action"] <- update_dur_det_action(x["ptf_action"])
        return(list(portFin = x, dividende = dividende))
    } else {
        return(list(portFin = x, dividende = 0))
    }
})
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_immo_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------


# Fonction permettant d'effectuer une operation de vieillissement des portefeuilles action et immo du portefeuille financier
# Prend en input le PortFin (x), les tables d'evolution des cours et rdt (calcules par la fonction calc_rdt).
setGeneric(name = "vieillissement_immo_PortFin", def = function(x, table_rdt){standardGeneric("vieillissement_immo_PortFin")})
setMethod(
  f = "vieillissement_immo_PortFin",
  signature = c(x = "PortFin", table_rdt = "list"),
  definition = function(x, table_rdt){
    # Verification input :
    if(nrow(x["ptf_immo"]["ptf_immo"]) > 0) {
      
        # Calcul des loyers
        loyer <- sum(table_rdt[["rdt_immo"]][["loyer"]])

        # Mise a jour de la VM Immo en fin d'annee
        x["ptf_immo"]   <- update_vm_immo(x["ptf_immo"], calc_vm_immo(x["ptf_immo"], table_rdt[["rdt_immo"]][["rdt"]] ))
        # Mise a jour des durees de detention Action/Immo
        x["ptf_immo"]   <- update_dur_det_immo(x["ptf_immo"])
        return(list(portFin = x, loyer = loyer))
    } else {
        return(list(portFin = x, loyer = 0))
    }
  })
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_treso_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Fonction permettant d'effectuer une operation de vieillissement de la tresorerie du portefeuille financier
# Prend en input le PortFin (x), les tombees de milieu d'annee (flux_milieu = Coupon + Loyer + Div), les tombees de fin d'annee (echeance = remboursement obligataire), les tables d'evolution des cours et rdt (calcules par la fonction calc_rdt)..
setGeneric(name = "vieillissement_treso_PortFin", def = function(x, flux_milieu, flux_fin, table_rdt){standardGeneric("vieillissement_treso_PortFin")})
setMethod(
  f = "vieillissement_treso_PortFin",
  signature = c(x = "PortFin", flux_milieu = "numeric", flux_fin = "numeric", table_rdt = "list"),
  definition = function(x, flux_milieu, flux_fin, table_rdt){
    # Verification input :
    if(nrow(x["ptf_treso"]["ptf_treso"]) > 0) {
        # Revenu de la treso
        rdt_treso <-  revenu_treso(x["ptf_treso"],table_rdt[["rdt_treso"]],flux_milieu)
        # Mise a jour de la treso en fin d'annee (revalo de la treso  en tenant compte des flux de loyer/div/coupons de milieu d'annee+ prise en comtpe des flux de tombee d'echeance percus en fin d'annee)
        x["ptf_treso"] <- update_treso(x["ptf_treso"], rdt_treso + flux_fin + flux_milieu)
    }
    return(x)
  })
