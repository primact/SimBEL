
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_taux
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant d'appliquer le choc de taux a un Canton.
##'
##' \code{do_choc_taux} est une methode permettant d'appliquer le choc de taux de la formule standard Solvabilite 2 a un canton.
##' @name do_choc_taux
##' @docType methods
##' @param canton un objet de la classe \code{Canton}, correspondant au canton auquel on souhaite appliquer le choc de taux.
##' @return canton l'objet de la classe \code{Canton}, mis a jour du choc de taux. 
##' @author Prim'Act
##' @export
##' @aliases ChocSolvabilite2
##' @include ChocSolvabilite2_class.R Canton_class.R


setGeneric(name = "do_choc_taux", def = function(canton){standardGeneric("do_choc_taux")})
setMethod(
    f = "do_choc_taux",
    signature = c("Canton"),
    definition = function(canton){
        # Verification des inputs
        if(nrow(canton@ptf_fin@ptf_oblig@ptf_oblig) == 0) {stop("[choc Mket : Taux] :
                                                            tentative de calcul du choc taux avec un objet Oblig vide
                                                            impossible. \n")}
        # Mise a jour des valeurs de marche du portefeuille
        canton@ptf_fin@ptf_oblig@ptf_oblig$val_marche <- calc_vm_oblig(canton@ptf_fin@ptf_oblig,
                                                                       canton@mp_esg@yield_curve)
        # Mise a jour des valeurs de marche du portefeuille de reference
        canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig$val_marche <- calc_vm_oblig(canton@param_alm@ptf_reference@ptf_oblig,
                                                                                       canton@mp_esg@yield_curve)
        canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig$val_nc    <- canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig$val_marche
        canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig$val_achat <- canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig$val_marche
        
        # Mise a jour des PMVL Action/Immo/Oblig
        canton@ptf_fin <- do_update_pmvl(canton@ptf_fin)
        
        # Convention : on ne remet pas a jour la valeur de la PRE
        
        # Mise a jour des montant totaux de VM et de VNC des actifs
        canton@ptf_fin <- do_update_vm_vnc_precedent(canton@ptf_fin)
        
        return(canton)
    }
)