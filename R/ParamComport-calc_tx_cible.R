#----------------------------------------------------------
# Ce script comprend les methodes de la classe ParamComport
#----------------------------------------------------------

##' Calcul des differents taux cible pour chaque model point de la classe EpEuroInd
##'
##' \code{calc_tx_cible_ref_marche} est une methode permettant de calculer un taux cible.
##' @name calc_tx_cible_ref_marche
##' @docType methods
##' @param param_comport un objet de la classe \code{ParamComport}.
##' @param list_rd une liste qui comprend les taux de rendement de reference.
##' @param tx_cible_prec le taux cible individuel de l'annee precedente.
##' @return Le taux cible annuel.
##' @author Prim'Act
##' @export
##' @aliases ParamComport

setGeneric(name = "calc_tx_cible_ref_marche",
           def = function(param_comport, list_rd, tx_cible_prec)
          {standardGeneric("calc_tx_cible_ref_marche")})
#--------------------------------------------------------

setMethod(
  f = "calc_tx_cible_ref_marche",
  signature = c(param_comport = "ParamComport", list_rd = "list", tx_cible_prec = "numeric"),
  def = function(param_comport, list_rd, tx_cible_prec){

  tx_mar <- sum(param_comport@alloc_mar * c(list_rd[["rdt_oblig"]],
                                            list_rd[["rdt_action"]],
                                            list_rd[["rdt_immo"]],
                                            list_rd[["rdt_tre"]])
                )

  tx_cible_an <- param_comport@w_n * (1 - param_comport@marge_mar) *
    max(0, tx_mar - param_comport@ch_enc_mar) +
    (1 - param_comport@w_n) * tx_cible_prec

  # Output
  return(tx_cible_an)

  }
)


