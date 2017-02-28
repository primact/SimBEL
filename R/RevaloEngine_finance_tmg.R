#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le financement de la PB contractuelle.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_tmg
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la contribution de la PPB au financement des taux minimum garanti.
##'
##' \code{finance_tmg} est une methode permettant d'evaluer le contribution de la PPB
##' au financement des taux minimum garanti sur prestations et sur stock.
##' @name base_prod_fin
##' @docType methods
##' @param bes_tmg_prest est un vecteur de type \code{numeric} comprenant
##'  par produit le besoin de finance des TMG sur prestations.
##'  @param bes_tmg_stock est un vecteur de type \code{numeric} comprenant
##'  par produit le besoin de finance des TMG sur le stock.
##' @param ppb est un objet de la classe \code{Ppb} qui renvoie l'etat courant de la PPB.
##' @return Une liste comprenant la valeur de la contribution au financement des TMG sur prestations et sur stock.
##' Cette liste comprend egalement la PPB mise a jour.
##' @details Cette fonction priorise le financement des TMG prestations. Elle met a jour la PPB des reprises effectuees.
##' @author Prim'Act
##' @export
##' @aliases RevaloEngine


setGeneric(name = "finance_tmg", def = function(bes_tmg_prest, bes_tmg_stock, ppb){standardGeneric("finance_tmg")})
setMethod(
  f = "finance_tmg",
  signature = c(bes_tmg_prest = "numeric", bes_tmg_stock = "numeric", ppb = "Ppb"),
  definition = function(bes_tmg_prest, bes_tmg_stock, ppb){

    # Controle
    if(prod(bes_tmg_prest < 0)){
      stop("[RevaloEngine-finance_tmg] : les besoins de financements par produit doivent etre positifs.")
    }

    # Controle
    if(prod(bes_tmg_stock < 0)){
      stop("[RevaloEngine-finance_tmg] : les besoins de financements par produit doivent etre positifs.")
    }

    # Calcul du besoin en TMG du portefeuille pour les prestations et le stock
    bes_tmg_prest_port <- sum(bes_tmg_prest)
    bes_tmg_stock_port <- sum(bes_tmg_stock)

    # Etape 1 : financement des TMG sur prestations
    # Operation de reprise sur PPB
    op_ppb <- calc_reprise_ppb(ppb, bes_tmg_prest_port)
    # Mise a jour de la PPB
    ppb <- op_ppb[["ppb"]]
    reprise <- op_ppb[["reprise"]]

    # Contribution par la PPB pour le financement des TMG sur prestations
    contrib_tmg_prest <- reprise

    # Etape 2 : financement des TMG sur stock

    # Operation de reprise sur PPB
    op_ppb <- calc_reprise_ppb(ppb, bes_tmg_stock_port)
    # Mise a jour de la PPB
    ppb <- op_ppb[["ppb"]]
    reprise <- op_ppb[["reprise"]]

    # Evaluation du besoin non prise en charge par la PPB pour le financement des TMG sur prestations
    contrib_tmg_stock <- reprise


    # Output
    return(list(contrib_tmg_prest = contrib_tmg_prest, contrib_tmg_stock = contrib_tmg_stock, ppb = ppb))
  }
)

