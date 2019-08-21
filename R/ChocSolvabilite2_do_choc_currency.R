#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_currency
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque de device.
##'
##' \code{do_choc_currency} est une methode permettant d'appliquer le choc currency de la formule standard Solvabilite 2
##'  a un canton. Cette methode s'applique uniquement aux obligations, actions et titres en immobilier.
##' @name do_choc_currency
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param nom_currency object \code{character} correspondant au nom de la devise du choc.
##' @param direction    object \code{character} prenant pour valeur "up" pour le choc a la hausse et "down" pour le choc a la baisse.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central)
##' de l'assureur.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario choque
##'  currency au sens de la formule standard Solvabilite 2.
##' Cette parametrisation est effectuee dans les fichiers d'inputs utilisateurs.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R

setGeneric(name = "do_choc_currency", def = function(x, nom_currency, direction, canton){standardGeneric("do_choc_currency")})
setMethod(
    f = "do_choc_currency",
    signature = c("ChocSolvabilite2", "character", "character", "Canton"),
    definition = function(x, nom_currency, direction, canton){

      # Verification des inputs
      if (nrow(canton@ptf_fin@ptf_action@ptf_action) == 0) {
        stop("[choc Mket : Currency] : tentative de calcul du choc currency avec un objet Action vide impossible. \n")
      }
      if (nrow(canton@ptf_fin@ptf_oblig@ptf_oblig) == 0) {
        stop("[choc Mket : Currency] : tentative de calcul du choc currency avec un objet Oblig vide impossible. \n")
      }
      if (nrow(canton@ptf_fin@ptf_immo@ptf_immo) == 0) {
        stop("[choc Mket : Currency] : tentative de calcul du choc currency avec un objet Immo vide impossible. \n")
      }
      if(direction != "up" & direction != "down"){
        stop("[choc Mket : Currency] : La valeur de 'direction' doit etre 'up' ou 'down'. \n")
      }

      # Extraction du choc correspondant a la devise
      tab_choc_currency <- x@param_choc_mket@table_choc_currency
      # Verification que le devise existe
      row_currency <- which(tab_choc_currency$currency == nom_currency)
      if(length(row_currency) == 0){
        stop("[choc Mket : Currency] : La table de choc ne contient pas la devise demandee. \n")
      }
      choc_currency <- tab_choc_currency[row_currency, paste0("choc_currency_", direction)]

      # Extraction des portefeuilles actions, obligations et immobilier
      ptf_action <- canton@ptf_fin@ptf_action@ptf_action
      ptf_oblig  <- canton@ptf_fin@ptf_oblig@ptf_oblig
      ptf_immo   <- canton@ptf_fin@ptf_immo@ptf_immo

      #### Application au porfeteuille
      # Selection des indices qui interessent
      index_action <- which(ptf_action$currency == nom_currency)
      index_oblig <- which(ptf_oblig$currency == nom_currency)
      index_immo <- which(ptf_immo$currency == nom_currency)

      if(length(index_action) > 0){
        ptf_action[index_action, ]$val_marche <- ptf_action[index_action, ]$val_marche * (1 - choc_currency)
      }
      if(length(index_oblig) > 0){
        ptf_oblig[index_oblig, ]$val_marche <- ptf_oblig[index_oblig, ]$val_marche * (1 - choc_currency)
      }
      if(length(index_immo) > 0){
        ptf_immo[index_immo, ]$val_marche <- ptf_immo[index_immo, ]$val_marche * (1 - choc_currency)
      }

      # Reintegration dans les objects
      canton@ptf_fin@ptf_action <- new("Action", ptf_action)
      canton@ptf_fin@ptf_oblig <- new("Oblig", ptf_oblig)
      canton@ptf_fin@ptf_immo <- new("Immo", ptf_immo)

      # Mise a jour des PMVL Action/Immo/Oblig
      # Convention : on ne remet pas a jour la valeur de la PRE
      canton@ptf_fin <- do_update_pmvl(canton@ptf_fin)

      # Mise a jour des montant totaux de VM et de VNC des actifs
      canton@ptf_fin <- do_update_vm_vnc_precedent(canton@ptf_fin)

      # Mise a jour des zspreads PtfFin
      zspread <- calc_z_spread(canton@ptf_fin@ptf_oblig, canton@mp_esg@yield_curve)
      canton@ptf_fin@ptf_oblig <- update_zsp_oblig(canton@ptf_fin@ptf_oblig, zspread)

      ################################
      # GESTION PORT FIN REFERENCE
      # Verification des inputs
      if(nrow(canton@param_alm@ptf_reference@ptf_action@ptf_action) == 0) {
        stop("[choc Mket : Currency] : Portefeuille de reinvestissement - tentative de calcul du choc currency avec un objet Action vide impossible. \n")
      }
      if(nrow(canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig) == 0) {
        stop("[choc Mket : Currency] : Portefeuille de reinvestissement - tentative de calcul du choc currency avec un objet Oblig vide impossible. \n")
      }
      if(nrow(canton@param_alm@ptf_reference@ptf_immo@ptf_immo) == 0) {
        stop("[choc Mket : Currency] : Portefeuille de reinvestissement - tentative de calcul du choc currency avec un objet Immo vide impossible. \n")
      }

      # Extraction des portefeuilles actions, obligations et immobilier
      ptf_action_ref <- canton@param_alm@ptf_reference@ptf_action@ptf_action
      ptf_oblig_ref  <- canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig
      ptf_immo_ref   <- canton@param_alm@ptf_reference@ptf_immo@ptf_immo

      #### Application au porfeteuille
      # Selection des indices qui interessent
      index_action_ref <- which(ptf_action_ref$currency == nom_currency)
      index_oblig_ref <- which(ptf_oblig_ref$currency == nom_currency)
      index_immo_ref <- which(ptf_immo_ref$currency == nom_currency)

      if(length(index_action_ref) > 0){
        ptf_action_ref[index_action_ref, ]$val_marche <- ptf_action_ref[index_action_ref, ]$val_marche * (1 - choc_currency)
        ptf_action_ref$val_achat <- ptf_action_ref$val_marche
        ptf_action_ref$val_nc <- ptf_action_ref$val_marche
      }
      if(length(index_oblig_ref) > 0){
        ptf_oblig_ref[index_oblig_ref, ]$val_marche <- ptf_oblig_ref[index_oblig_ref, ]$val_marche * (1 - choc_currency)
        ptf_oblig_ref$val_achat <- ptf_oblig_ref$val_marche
        ptf_oblig_ref$val_nc <- ptf_oblig_ref$val_marche
      }
      if(length(index_immo_ref) > 0){
        ptf_immo_ref[index_immo_ref, ]$val_marche <- ptf_immo_ref[index_immo_ref, ]$val_marche * (1 - choc_currency)
        ptf_immo_ref$val_achat <- ptf_immo_ref$val_marche
        ptf_immo_ref$val_nc <- ptf_immo_ref$val_marche
      }

      # Reintegration dans les objects
      canton@param_alm@ptf_reference@ptf_action <- new("Action", ptf_action_ref)
      canton@param_alm@ptf_reference@ptf_oblig <- new("Oblig", ptf_oblig_ref)
      canton@param_alm@ptf_reference@ptf_immo <- new("Immo", ptf_immo_ref)

      # Mise a jour des PMVL Action/Immo/Oblig
      canton@param_alm@ptf_reference <- do_update_pmvl(canton@param_alm@ptf_reference)

      # Convention : on ne remet pas a jour la valeur de la PRE
      # Mise a jour des montant totaux de VM et de VNC des actifs
      canton@param_alm@ptf_reference <- do_update_vm_vnc_precedent(canton@param_alm@ptf_reference)

      # Mise a jour des zspreads PtfFin reference
      zspread <- calc_z_spread(canton@param_alm@ptf_reference@ptf_oblig, canton@mp_esg@yield_curve)
      canton@param_alm@ptf_reference@ptf_oblig <- update_zsp_oblig(canton@param_alm@ptf_reference@ptf_oblig, zspread)

      return(canton)
    }
)
