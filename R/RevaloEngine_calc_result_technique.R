#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend la fonction permettant de calculer le resultat technique
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_result_technique
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' calcule le resultat technique
##'
##' \code{calc_result_technique} est une methode permettant de calculer le resultat technique avant attribution de
##' participation aux benefices.
##' @name calc_result_technique
##' @docType methods
##' @param passif_av_pb est une liste produit par la methode \code{viellissement_av_pb}
##' appliquee a un portefeuille de passif.
##' @param var_pre est une valeur \code{numeric} correspondant a la variation de PRE.
##' @return Le resulat technique
##' @export
##' @aliases RevaloEngine

setGeneric(name = "calc_result_technique", def = function(passif_av_pb, var_pre){
  standardGeneric("calc_result_technique")})

setMethod(
  f = "calc_result_technique",
  signature = c(passif_av_pb = "list", var_pre = "numeric"),

  definition = function(passif_av_pb, var_pre){

    # Evaluation du resultats

    result_tech <-  passif_av_pb[["flux_milieu"]] +
      passif_av_pb[["flux_fin"]] -
      sum(passif_av_pb[["result_av_pb"]][["stock_agg"]][, "pm_fin"][[1]] - # Variation PM  sur les produits inclus dans le modele
        passif_av_pb[["result_av_pb"]][["stock_agg"]][, "pm_deb"][[1]]
        ) -
      (passif_av_pb[["result_autres_passifs"]]$pm_fin - passif_av_pb[["result_autres_passifs"]]$pm_deb) -
      var_pre - passif_av_pb[["var_psap"]] - passif_av_pb[["var_pgg"]]

    # Output
    return(result_tech)
    }
)


