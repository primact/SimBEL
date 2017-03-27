#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_oblig_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Effectue le vieillissement/la projection du portefeuille obligataire d'un portefeuille financier.
##'
##' \code{vieillissement_oblig_PortFin} est une methode permettant de projeter la composante obligataire d'un portefeuille financier.
##' @name vieillissement_oblig_PortFin
##' @docType methods
##' @param x objet de la classe \code{PortFin}, correspondant au portefeuille financier de l'assureur avant l'etape de vieillissement de son atribut \code{ptf_oblig} de la classe \code{Oblig}.
##' @param new_mp_ESG est un objet de type \code{ModelPointESG}, correspondant aux conditions economiques de l'annee du vieillissement.
##' @return Le format de la liste renvoyee est :
##' \describe{
##' \item{\code{portFin} : }  {le portefeuille financier dont l'attribut \code{ptf_oblig} a ete vieilli d'une annee.}
##' \item{\code{loyer} : }{le montant de loyer percus en milieu d'annee suite au vieillissement du portefeuille obligataire.}
##' }
##' @author Prim'Act
##' @export
##' @seealso La fonction de calcul des rendements des actifs \code{\link{calc_rdt}}.
##' @aliases PortFin
##' @include PortFin_class.R ModelPointESG_class.R


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
