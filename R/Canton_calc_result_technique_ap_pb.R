
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_result_technique_ap_pb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' calcule le resultat technique apres prise en compte de la participation aux benefices.
##'
##' \code{calc_result_technique_ap_pb} est une methode permettant de calculer le resultat technique
##' apres attribution de participation aux benefices.
##' @name calc_result_technique_ap_pb
##' @docType methods
##' @param passif_av_pb est une liste produit par la methode \code{\link{viellissement_av_pb}}.
##' @param passif_ap_pb est une liste produit par la methode \code{\link{viellissement_ap_pb}}.
##' @param ppb est un objet de la classe \code{\link{Ppb}} qui renvoie l'etat courant de la PPB.
##' @param var_pre est une valeur \code{numeric} correspondant a la variation de PRE.
##' @return Le resultat technique apres participation aux benefices.
##' @export
##' @include Canton_class.R

setGeneric(name = "calc_result_technique_ap_pb", def = function(passif_av_pb, passif_ap_pb, ppb, var_pre){
    standardGeneric("calc_result_technique_ap_pb")})

setMethod(
    f = "calc_result_technique_ap_pb",
    signature = c(passif_av_pb = "list", passif_ap_pb = "list", ppb = "Ppb", var_pre = "numeric"),

    definition = function(passif_av_pb, passif_ap_pb, ppb, var_pre){

        # Evaluation du resultats

        result_tech <-  passif_av_pb[["flux_milieu"]] +
            passif_av_pb[["flux_fin"]] -
            sum(passif_ap_pb[["stock_agg"]][, "pm_fin_ap_pb"] - # Variation PM  sur les produits inclus dans le modele
                    passif_av_pb[["result_av_pb"]][["stock_agg"]][, "pm_deb"]
            ) -
            (passif_av_pb[["result_autres_passifs"]]$pm_fin - passif_av_pb[["result_autres_passifs"]]$pm_deb) -
            var_pre - passif_av_pb[["var_psap"]] - passif_av_pb[["var_pgg"]] +
            - (ppb@valeur_ppb - ppb@ppb_debut)

        # Output
        return(result_tech)
    }
)


