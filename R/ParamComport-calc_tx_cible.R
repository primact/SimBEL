#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_tx_cible_ref_marche : methode pour le calcul du taux cible.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de revalorisation cible.
##'
##' \code{calc_tx_cible_ref_marche} est une methode permettant de calculer le taux de revalorisation cible
##' en evaluant le taux de rendement des assureurs sur le marche.
##' @name calc_tx_cible_ref_marche
##' @docType methods
##' @param param_comport un objet de la classe \code{\link{ParamComport}} contenant les parametres
##'  comportementaux.
##' @param list_rd une liste contenant les rendements de reference. Le format de cette liste est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @param tx_cible_prec une valeur \code{numeric} correspondant au taux cible de la periode precedente.
##' @return La valeur du taux cible.
##' @author Prim'Act
##' @export
##' @include ParamComport-class.R

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


