#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_spread
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque spread.
##'
##' \code{do_choc_spread} est une methode permettant d'appliquer le choc spread de la formule standard Solvabilite 2
##'  a un canton. Cette methode s'applique uniquement aux obligations de type \code{corp}.
##' @name do_choc_spread
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central)
##' de l'assureur.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario choque
##'  spread au sens de la formule standard Solvabilite 2.
##' @note Il est possible d'appliquer des chocs de spreads distincts a chaque ligne du portefeuille obligataire
##'  selon le numero de rating et la duration de l'obligation.
##' Cette parametrisation est effectuee dans les fichiers d'inputs utilisateurs.
##' @author Prim'Act
##' @seealso L'application du choc de spread a une ligne obligataire : \code{\link{do_choc_spread_unitaire}}.
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R

setGeneric(name = "do_choc_spread", def = function(x, canton){standardGeneric("do_choc_spread")})
setMethod(
    f = "do_choc_spread",
    signature = c("ChocSolvabilite2", "Canton"),
    definition = function(x, canton){

        # GESTION PORT FIN ASSUREUR
        # Verification des inputs
        if(nrow(canton@ptf_fin@ptf_oblig@ptf_oblig) == 0) {stop("[choc Mket : Spread] : tentative de calcul du choc spread avec un objet Oblig vide impossible. \n")}
        temp <- canton@ptf_fin@ptf_oblig@ptf_oblig

        # Methode bourrine
        table_choc_spread <- x@param_choc_mket@table_choc_spread
        temp$val_marche <- unlist(lapply(1:nrow(temp), function(x){do_choc_spread_unitaire(table_choc_spread, temp[x,])}))
        canton@ptf_fin@ptf_oblig <- new("Oblig", temp)

        # Mise a jour des PMVL Action/Immo/Oblig
        canton@ptf_fin <- do_update_pmvl(canton@ptf_fin)

        # Convention : on ne remet pas a jour la valeur de la PRE

        # Mise a jour des montant totaux de VM et de VNC des actifs
        canton@ptf_fin <- do_update_vm_vnc_precedent(canton@ptf_fin)

        # Mise a jour des zspreads PtfFin
        zspread <- calc_z_spread(canton@ptf_fin@ptf_oblig, canton@mp_esg@yield_curve)
        canton@ptf_fin@ptf_oblig <- update_zsp_oblig(canton@ptf_fin@ptf_oblig, zspread)

        # GESTION PORT FIN REFERENCE
        # Verification des inputs
        if(nrow(canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig) == 0) {stop("[choc Mket : Spread] : Portefeuille de reinvestissement - tentative de calcul du choc spread avec un objet Oblig vide impossible. \n")}
        temp <- canton@param_alm@ptf_reference@ptf_oblig@ptf_oblig

        # Methode bourrine
        table_choc_spread <- x@param_choc_mket@table_choc_spread
        temp$val_marche <- unlist(lapply(1:nrow(temp), function(x){do_choc_spread_unitaire(table_choc_spread, temp[x,])}))
        temp$val_achat  <- temp$val_marche
        temp$val_nc     <- temp$val_marche
        canton@param_alm@ptf_reference@ptf_oblig <- new("Oblig", temp)

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
