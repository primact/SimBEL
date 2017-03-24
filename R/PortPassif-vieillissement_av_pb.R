#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_av_pb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Vieillissement du portefeuille sur l'annee avant attribution
##' de participation aux benefices.
##'
##' \code{viellissement_av_pb} est une methode permettant de vieillir l'objet \code{\link{PortPassif}}
##' sur l'annee avant attribution de participation aux benefices.
##' @name vieillissement_av_pb
##' @docType methods
##' @param an une valeur \code{numeric} correspondant a l'annee de projection.
##' @param x un objet de la classe \code{\link{PortPassif}} contenant l'ensemble des produits de passifs.
##' @param coef_inf une valeur \code{numeric} correspondant au coefficient d'inflation
##'  considere pour le traitement des frais.
##' @param list_rd une liste contenant les rendements de reference. Le format de cette liste est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @param tx_soc une valeur \code{numeric} correspondant au taux de charges sociales.
##' @return Une liste comprenant  :
##' \describe{
##' \item{\code{ptf} : }{Le portefeuille \code{x} mis a jour.}
##' \item{\code{result_av_pb} : }{Une liste dont le premier element designe les noms des produits,
##'  puis deux matrices de resultats aggreges : une pour les flux et une pour le stock. Le format de cette sortie
##'  decoule de celui de la methode \code{\link{proj_annee_av_pb}}.}
##' \item{\code{result_autres_passifs} : }{un vecteur contenant les resultats des passifs non modelises.}
##' \item{\code{var_psap} : }{la variation de PSAP sur l'annee.}
##' \item{\code{var_pgg} : }{la variation de PGG sur l'annee.}
##' \item{\code{flux_milieu} : }{les flux de milieu d'annee entrant en tresorerie en milieu de periode.}
##' \item{\code{flux_fin} : }{les flux de fin d'annee entrant en tresorerie en fin de periode.}
##' }
##' @author Prim'Act
##' @seealso La projection des passifs sur un an avant PB : \code{\link{proj_annee_av_pb}}.
##' La projection des autres passifs : \code{\link{proj_annee_autres_passifs}}.
##' La mise a jour des autres reserves : \code{\link{update_reserves}}.
##' @export
##' @aliases PortPassif
##' @include PortPassif-class.R
##'
setGeneric(name = "viellissement_av_pb", def = function(an, x, coef_inf, list_rd, tx_soc){
  standardGeneric("viellissement_av_pb")})
setMethod(
  f = "viellissement_av_pb",
  signature = c(an = "integer", x = "PortPassif", coef_inf = "numeric",
                list_rd = "list", tx_soc = "numeric"),
  def = function(an, x, coef_inf, list_rd, tx_soc){

    # Mise a jour de l'annee
    x["annee"] <- as.integer(an)
    #---------------------------------------------------------------
    # Etape 1 : Evaluation des flux et de PM avant PB sur les produits
    #---------------------------------------------------------------
    # Evaluation des flux et la PM avant PB
    result_av_pb <- proj_annee_av_pb(an, x, tx_soc, coef_inf, list_rd)

    # Mise a jour des tableaux de resultats intermediaires
    x <- result_av_pb[["x"]]

    # Totaux
    prest_tot <- sum(result_av_pb[["flux_agg"]][, "prest"])
    pm_av_pb_tot <- sum(result_av_pb[["stock_agg"]][, "pm_fin"])

    #---------------------------------------------------------------
    # Etape 2 : Evaluation des flux et de PM sur les produits hors modeles
    #---------------------------------------------------------------

    # Extraction des passifs hors modeles. Specifique MGP
    result_autres_passifs <- proj_annee_autres_passifs(an, x@autres_passifs, coef_inf)

    #---------------------------------------------------------------
    # Etape 3 : Traitement de la PGG et de la PSAP --> Specifique MGP
    #---------------------------------------------------------------

    # Evaluation et mise a jour de la PGG et de la PSAP.
    op_autres_reserves <- update_reserves(x@autres_reserves,
                                          prest_tot, result_autres_passifs$prestation,
                                          pm_av_pb_tot, result_autres_passifs$pm_fin)
    # Mise a jour des provisions
    x["autres_reserves"] <- op_autres_reserves[["x"]]

    #---------------------------------------------------------------
    # Etape 4 : Evaluation des besoins de financement des TMG
    #---------------------------------------------------------------

    # Evaluation du besoin pour le financement des TMG sur prestations et  stock
    result_av_pb[["flux_agg"]] <- cbind(result_av_pb[["flux_agg"]],
                                        bes_tmg_prest = result_av_pb[["flux_agg"]][,"rev_prest"] -
                                          result_av_pb[["flux_agg"]][,"it_tech_prest"],
                                        bes_tmg_stock = result_av_pb[["flux_agg"]][,"rev_stock_brut"] -
                                          result_av_pb[["flux_agg"]][,"it_tech_stock"]
                                        )

    #---------------------------------------------------------------
    # Etape 5 : Evaluation des flux interagissant avec l'actif
    #---------------------------------------------------------------
    # Calcul des flux de mileu d'annee
    flux_milieu <- sum(result_av_pb[["flux_agg"]][, "pri_brut"] - # Primes
      ( # Prestations
        result_av_pb[["flux_agg"]][,"rev_prest_nette"] +
          result_av_pb[["flux_agg"]][,"prest"] -
          result_av_pb[["flux_agg"]][,"rach_charg"]
        ) - # Frais sur primes et sur prestations
      (
        result_av_pb[["flux_agg"]][,"frais_var_prime"] +
        result_av_pb[["flux_agg"]][,"frais_fixe_prime"] +
        result_av_pb[["flux_agg"]][,"frais_var_prest"] +
        result_av_pb[["flux_agg"]][,"frais_fixe_prest"]
        ))

    # Ajout des flux hors modele
    flux_milieu <- flux_milieu +
      result_autres_passifs$prime -
      result_autres_passifs$prestation -
      # result_autres_passifs$it - # Interet technique deja inclus dans les prestations
      result_autres_passifs$frais


    # Calcul du flux de fin d'annee
    flux_fin <- -sum( # Frais sur encours
      result_av_pb[["flux_agg"]][,"frais_var_enc"] +
      result_av_pb[["flux_agg"]][,"frais_fixe_enc"]
    )


    # Retrait des elements en doublon dans
    result_av_pb[["x"]] <- NULL
    nom_produit <- result_av_pb[["nom_produit"]]
    result_av_pb[["nom_produit"]] <- NULL

    # Output
    return(list(
      ptf = x,
      nom_produit = nom_produit,
      result_av_pb = result_av_pb,
      result_autres_passifs = result_autres_passifs,
      var_psap = op_autres_reserves[["var_psap"]], var_pgg = op_autres_reserves[["var_pgg"]],
      flux_milieu = flux_milieu, flux_fin = flux_fin
      )
      )
  }
)
