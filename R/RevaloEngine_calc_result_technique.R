#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_result_technique : methode permettant de calculer le resultat technique
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' calcule le resultat technique
##'
##' \code{calc_result_technique} est une methode permettant de calculer le resultat technique avant attribution de
##' participation aux benefices.
##' @name calc_result_technique
##' @docType methods
##' @param passif_av_pb est une liste produite par la methode \code{\link{viellissement_av_pb}}
##' appliquee a un portefeuille de passif.
##' @param var_pre est une valeur \code{numeric} correspondant a la variation de PRE.
##' @return Le resultat technique.
##' @author Prim'Act
##' @seealso \code{\link{PRE}}, \code{\link{viellissement_av_pb}}.
##' @export

setGeneric(name = "calc_result_technique", def = function(passif_av_pb, var_pre){standardGeneric("calc_result_technique")})
setMethod(
    f = "calc_result_technique",
    signature = c(passif_av_pb = "list", var_pre = "numeric"),
    definition = function(passif_av_pb, var_pre){
        
        # Donnees
        stock_agg <- passif_av_pb[["result_av_pb"]][["stock_agg"]]
        result_autres_passifs <- passif_av_pb[["result_autres_passifs"]]
        nom_passif <- names(result_autres_passifs)
        num_pm_deb <- which(nom_passif == "pm_deb")
        num_pm_fin <- which(nom_passif == "pm_fin")
        
        # Evaluation du resultats
        result_tech <-  passif_av_pb[["flux_milieu"]] + passif_av_pb[["flux_fin"]] -
            sum(stock_agg[, "pm_fin"] - stock_agg[, "pm_deb"]) - # Variation PM  sur les produits inclus dans le modele
            (.subset2(result_autres_passifs, num_pm_fin) - .subset2(result_autres_passifs, num_pm_deb)) -
            var_pre - passif_av_pb[["var_psap"]] - passif_av_pb[["var_pgg"]]
        
        # Output
        return(result_tech)
    }
)


