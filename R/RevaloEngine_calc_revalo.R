#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_revalo : Methode permettant d'appliquer la politique de revalorisation
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique la politique de revalorisation d'un canton.
##'
##' \code{calc_revalo} est une methode permettant de
##'  d'appliquer l'ensemble de la politique de revalorisation d'un assureur.
##' @name calc_revalo
##' @docType methods
##' @param x un objet de la classe \code{\link{Canton}}.
##' @param passif_av_pb est une liste produit par la methode \code{\link{viellissement_av_pb}}
##' appliquee a un portefeuille de passif.
##' @param tra est la valeur \code{numeric} du taux de rendement de l'actif.
##' @param plac_moy_vnc est la valeur \code{numeric} moyenne des actifs en valeur nette comptable.
##' @param result_tech est la valeur \code{numeric} du resultat technique prise en compte avant distribution
##' de la PB.
##' @param annee est la valeur \code{integer} correspondant a l'annee de projection.
##' @return \code{add_rev_nette_stock} un vecteur \code{numeric} avec la valeur de la revalorisation nette servie par
##'  produit au titre de la participation aux benefices.
##' @return \code{pmvl_liq} le montant de plus-values latentes en actions a realiser.
##' @return \code{ppb} un objet \code{\link{Ppb}} correspondant a la PPB mise a jour.
##' @return \code{ppb8_ind} un vecteur \code{numeric} avec la valeur de la PPB devant sortir au titre de la regle des 8 ans,
##'  alloue par produit.
##' @return \code{conso_ppb_init} un vecteur \code{numeric} avec la valeur de la PPB initiale consommee servant au calcul du BEG.
##' @return \code{tx_pb} un vecteur reprenant les taux de PB par produit renseigne dans l'objet \code{x}.
##' @return \code{tx_enc_moy} un vecteur reprenant les taux de chargement sur encours theoriques moyens
##' par produit.
##' @author Prim'Act
##' @seealso Le calcul du TRA : \code{\link{calc_tra}}.
##' Le vieillissemennt des passifs avant PB : \code{\link{viellissement_av_pb}}.
##' Le calcul du resultat technique avant PB : \code{\link{calc_result_technique}}.
##' Le calcul de la base de produits financiers : \code{\link{base_prod_fin}}.
##' Le calcul de la PB contractuelle : \code{\link{pb_contr}}.
##' Le financement des TMG par la PPB : \code{\link{finance_tmg}}.
##' Le financement du taux cible par la PPB : \code{\link{finance_cible_ppb}}
##' Le financement du taux cible par la realisation plus-values latentes actions : \code{\link{finance_cible_pmvl}}
##' Le financement du taux cible par la compression de la marge de l'assureur : \code{\link{finance_cible_marge}}
##' Le calcul de la marge de l'assureur : \code{\link{calc_marge_fin}}
##' L'application de la contrainte legale de participation aux benefices : \code{\link{finance_contrainte_legale}}
##' @export
##' @include Canton_class.R

setGeneric(name = "calc_revalo", def = function(x, passif_av_pb, tra, plac_moy_vnc, result_tech, annee) {
    standardGeneric("calc_revalo")
})
setMethod(
    f = "calc_revalo",
    signature = c(x = "Canton", passif_av_pb = "list", tra = "numeric", plac_moy_vnc = "numeric", result_tech = "numeric", annee = "integer"),
    definition = function(x, passif_av_pb, tra, plac_moy_vnc, result_tech, annee) {
        # Extraction de donnees
        result_av_pb <- passif_av_pb[["result_av_pb"]]
        passif_av_pb_stock <- result_av_pb[["stock_agg"]]
        passif_av_pb_flux <- result_av_pb[["flux_agg"]]
        rev_stock_brute <- passif_av_pb_flux[, "rev_stock_brut"]

        # Stock de PPB encore present depuis la date de lancement
        nb_annee_res <- max(length(x@ppb@hist_ppb) - annee + 1L, 0) # 8 ans - nombre d'annees passees
        stock_res_ppb_init <- ifelse(nb_annee_res > 0, sum(rev(x@ppb@hist_ppb)[1:nb_annee_res]), 0)


        #---------------------------------------------------------------
        # Etape 1 : Evaluation de la base de produits financier
        #---------------------------------------------------------------

        # Base de produits financiers
        base_fin <- base_prod_fin(tra, passif_av_pb_stock[, "pm_moy"], x@ppb)
        base_prod_fin <- base_fin[["base_prod_fin"]]

        #---------------------------------------------------------------
        # Etape 2 : Calcul de la PB contractuelle
        #---------------------------------------------------------------

        # Extraction et verification des taux de PB
        ptf_passif <- x@ptf_passif
        tx_pb <- ptf_passif@tx_pb@mp # Extrait le vecteur des taux de PB contractuelle
        names_class_prod <- ptf_passif@names_class_prod # Nom des classes de produits
        nom_prod <- sapply(names_class_prod, function(i) {
            names(ptf_passif[i])
        }) # Extrait les numeros de produits
        tx_pb <- tx_pb[which(tx_pb$nom_prod == nom_prod), "taux_pb"] # Taux de PB reordonnees selon le nom des produits

        # Chargement sur encours theorique par produit
        ch_enc_th <- passif_av_pb_flux[, "enc_charg_base_th"] + passif_av_pb_flux[, "enc_charg_rmin_th"]
        base_enc_th <- passif_av_pb_flux[, "base_enc_th"]

        # Evaluation du taux de chargement sur encours moyen par produit
        tx_enc_moy <- ch_enc_th / base_enc_th

        # Gestion des divisions par 0
        tx_enc_moy[which(base_enc_th == 0)] <- 0

        # Evaluation de la PB contractuelle nette
        revalo_contr <- pb_contr(base_prod_fin, tx_pb, rev_stock_brute, ch_enc_th, tx_enc_moy)

        #---------------------------------------------------------------
        # Etape 3 : Financement des TMG par la PPB
        #---------------------------------------------------------------

        # Financement des TMG par la PPB
        financement_tmg <- finance_tmg(passif_av_pb_flux[, "bes_tmg_prest"], passif_av_pb_flux[, "bes_tmg_stock"], x@ppb)

        # Mise a jour de la PPB
        x@ppb <- financement_tmg[["ppb"]]


        #---------------------------------------------------------------
        # Etape 4 : Application de la regle des 8 ans
        #---------------------------------------------------------------

        # Recuperation de la PPB de l'annee an-8
        ppb_8 <- ppb_8ans(x@ppb)

        # Mise a jour de la PPB
        x@ppb <- ppb_8[["ppb"]]

        # Calcul de la PPB8 a attribuer par contrat : Attribution proportionnelle a la PM
        som <- sum(passif_av_pb_stock[, "pm_fin"])
        if (som != 0) {
            ppb8_ind <- ppb_8$ppb_8 * passif_av_pb_stock[, "pm_fin"] / som
        } else {
            l <- length(passif_av_pb_stock[, "pm_fin"])
            ppb8_ind <- ppb_8$ppb_8 * rep(1, l) / l
        }


        #---------------------------------------------------------------
        # Etape 5 : Financement du taux cible par la PPB
        #---------------------------------------------------------------
        bes_tx_cible <- passif_av_pb_flux[, "bes_tx_cible"] # Besoin taux cible

        # Financement du taux cible par la PPB
        tx_cibl_ppb <- finance_cible_ppb(bes_tx_cible, revalo_contr[["rev_stock_nette_contr"]], x@ppb, ppb8_ind)

        # Mise a jour de la PPB
        x@ppb <- tx_cibl_ppb[["ppb"]]

        #---------------------------------------------------------------
        # Etape 6 : Financement du taux cible par des cessions de PVL actions
        #---------------------------------------------------------------

        # Extraction de donnees
        base_prod_fin_port <- base_fin[["base_prod_fin_port"]]

        # Calcul du coefficient d'ajustement pour ajustement a l'echelle du passif
        coef_ajust <- base_prod_fin_port / plac_moy_vnc

        # Calcul du seuil de PMVL action
        seuil_pmvl <- x@param_alm@seuil_realisation_PVL * x@ptf_fin@pvl_action * coef_ajust

        # Financement du taux cible par des cessions de PVL actions
        tx_cibl_pmvl <- finance_cible_pmvl(bes_tx_cible, tx_cibl_ppb[["rev_stock_nette"]], base_prod_fin, seuil_pmvl, tx_pb)
        pmvl_liq <- tx_cibl_pmvl[["pmvl_liq"]]

        # Met a jour la base financiere
        if (base_prod_fin_port != 0) {
            # Allocation par produit
            base_prod_fin <- base_prod_fin + pmvl_liq * base_prod_fin / base_prod_fin_port
            base_prod_fin_port <- sum(base_prod_fin)
        }

        # Extraction et remise a l'echelle de l'actif de la PMVL action realisee
        if (coef_ajust != 0) {
            pmvl_liq <- pmvl_liq / coef_ajust
        } else {
            pmvl_liq <- 0
        }


        #---------------------------------------------------------------
        # Etape 7 : Financement du taux cible par la marge de l'assureur
        #---------------------------------------------------------------

        # Marge minimum que veut realisee l'assureur
        marge_min <- passif_av_pb_stock[, "pm_moy"] * x@param_revalo@tx_marge_min

        # Calcul de la marge financiere de l'assureur
        marge_fin <- calc_marge_fin(
            base_prod_fin, passif_av_pb_flux[, "rev_prest_nette"],
            tx_cibl_pmvl[["rev_stock_nette"]], financement_tmg[["contrib_tmg_prest"]],
            financement_tmg[["contrib_tmg_stock"]], tx_cibl_ppb[["reprise"]]
        )

        # Financement du taux cible par la marge de l'assureur
        tx_cibl_marge <- finance_cible_marge(marge_fin, bes_tx_cible, tx_cibl_pmvl[["rev_stock_nette"]], marge_min)


        #---------------------------------------------------------------
        # Etape 8 : Application de la contrainte de PB reglementaire
        #---------------------------------------------------------------

        # Donnees
        result_autres_passifs <- passif_av_pb[["result_autres_passifs"]]
        nom_result_autres_passifs <- names(result_autres_passifs)
        pm_deb <- which(nom_result_autres_passifs == "pm_deb")
        pm_fin <- which(nom_result_autres_passifs == "pm_fin")

        # Calcul des PM moyennes
        pm_moy <- (.subset2(result_autres_passifs, pm_fin) + .subset2(result_autres_passifs, pm_deb)) / 2

        # Evaluation d'une base de produits financiers etendue qui comprend le hors modele
        base_fin_etendu <- base_prod_fin_port + tra * pm_moy


        revalo_finale <- finance_contrainte_legale(
            base_prod_fin, base_fin_etendu,
            result_tech, passif_av_pb_flux[, "it_tech_stock"],
            tx_cibl_marge[["rev_stock_nette"]],
            passif_av_pb_flux[, "rev_prest_nette"],
            tx_cibl_ppb[["dotation"]], tx_cibl_marge[["marge_fin"]], x@ppb,
            x@param_revalo
        )

        # Mise a jour de la PPB
        x@ppb <- revalo_finale[["ppb"]]

        # Mise a jour du parametres de revalorisation (solde de PB reglementaire)
        x@param_revalo <- revalo_finale[["param_revalo"]]

        # Extraction de donnees
        rev_stock_nette <- revalo_finale[["rev_stock_nette"]]

        # On calcule le montant de revalorisation nette au dela de la revalorisation nette au taux minimum.
        add_rev_nette_stock <- rev_stock_nette - (rev_stock_brute - ch_enc_th)

        # Permet de gerer le cas ou la revalo nette apres PB est positive et la revalo nette avant est negative
        ind <- ((rev_stock_brute - ch_enc_th) <= 0) & (rev_stock_nette > 0)


        add_rev_nette_stock <- pmax(0, add_rev_nette_stock) * (1 - ind) + rev_stock_nette * ind


        # Ce montant ne doit pas etre negatif
        if (!all(add_rev_nette_stock >= 0)) {
            stop("[RevaloEngine_calc_revalo] Le montant de le revalorisation additionnelle ne peut pas etre negatif.")
        }

        #---------------------------------------------------------------
        # Etape 9 : Montant total de PB attribuee
        #---------------------------------------------------------------

        pb_attrib <- calc_pb_attrib(x@ppb)

        #---------------------------------------------------------------
        # Etape 10 : calcul la PPB encore presente depuis le lancement des calcul qui a ete consommees
        #---------------------------------------------------------------
        stock_res_ppb_init_ap_distr <- ifelse(nb_annee_res > 0, sum(rev(x@ppb@hist_ppb)[1:nb_annee_res]), 0)
        conso_ppb_init <- stock_res_ppb_init - stock_res_ppb_init_ap_distr

        # Allocation au prorata de l'allocation des besoins
        som_bes <- sum(add_rev_nette_stock)
        som <- sum(passif_av_pb_stock[, "pm_fin"])
        if (som_bes != 0) {
            conso_ppb_init <- conso_ppb_init * add_rev_nette_stock / som_bes
        } else if (som != 0) {
            # Sinon calcul de la PPB a attribuer par contrat : Attribution proportionnelle a la PM
            conso_ppb_init <- conso_ppb_init * passif_av_pb_stock[, "pm_fin"] / som
        } else {
            l <- length(passif_av_pb_stock[, "pm_fin"])
            conso_ppb_init <- conso_ppb_init * rep(1, l) / l
        }


        # Output
        return(list(
            add_rev_nette_stock = add_rev_nette_stock,
            pmvl_liq            = pmvl_liq,
            ppb                 = x@ppb,
            pb_attrib           = pb_attrib,
            ppb8_ind            = ppb8_ind,
            conso_ppb_init      = conso_ppb_init,
            tx_pb               = tx_pb,
            tx_enc_moy          = tx_enc_moy
        ))
    }
)
