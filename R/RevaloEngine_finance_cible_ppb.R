#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le financement de la PB contractuelle.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_cible_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue le financement d'un taux cible par la PPB.
##'
##' \code{finance_cible_ppb} est une methode permettant de
##'  determiner le financement de taux cibles par la provision pour participation aux benefices.
##' @name base_prod_fin
##' @docType methods
##' @param bes_cible est un vecteur de type \code{numeric} comprenant par produit le besoin de financement aux taux cible.
##' @param rev_stock_nette est un vecteur de type \code{numeric} comprenant par produit le montant de revalorisation nette atteint.
##' @param ppb est un objet de la classe \code{Ppb} qui renvoie l'etat courant de la PPB.
##' @return Une liste avec la valeur de la revalorisation nette servie par produit, les montants de dotation
##' et reprise sur PPB et la PPB mise à jour.
##' @details Lorsque la revalorisation nette est superieure au besoin de financement des taux cibles, on sert le taux cible
##' et on dote le reste a la PPB dans la limite du plafond de dotation annuel. A l'inverse, les taux cible sont finances par
##' les compensations entre produits lorsque certains prevoient une revalorisation superieure au taux cible, et
##' par une reprise sur PPB.
##' @author Prim'Act
##' @export
##' @aliases RevaloEngine

setGeneric(name = "finance_cible_ppb", def = function(bes_cible, rev_stock_nette, ppb){standardGeneric("finance_cible_ppb")})
setMethod(
  f = "finance_cible_ppb",
  signature = c(bes_cible = "numeric", rev_stock_nette = "numeric", ppb = "Ppb"),
  definition = function(bes_cible, rev_stock_nette, ppb){

    # Controle
    if(prod(bes_cible < 0)){
      stop("[RevaloEngine-finance_cible_ppb] : les besoins de financements par produit doivent etre positifs.")
    }

    if(length(bes_cible) != length(rev_stock_nette)){
      stop("[RevaloEngine-finance_cible_ppb] : les vecteurs en entree ne sont pas de meme longueur.")
    }


    # Analyse du besoin de financement sur l'ensemble des produits
    bes_add_ind <- bes_cible - rev_stock_nette
    bes_add <- sum(bes_add_ind)

    # Montant pour lequelle la PB contractuelle est plus genereuse
    avantage_pb_contr <- pmax(0, - bes_add_ind)
    # Produit pour lesquelle la  PB contractuelle est plus genereuse
    prod_avantage <- (avantage_pb_contr >  0)

    # Initialise les montants de dotation et reprise
    dotation <- 0 ; reprise <- 0

    # Dotation a la PPB si la revalorisation nette sur le stock est superieure au besoin de taux cible
    # Dans ce cas, comme 'bes_cible' est necessairement positif, on a 'rev_stock_nette' qui est positif
    if(bes_add < 0){

      # Dotation
      op_ppb <- calc_dotation_ppb(ppb, - bes_add)
      # Mise a jour de la PPB
      ppb <- op_ppb[["ppb"]]
      dotation <- op_ppb[["dotation"]]

      # Revalorisation totale attribuee au portefeuille : ce qui n'a pas pu etre dote
      rev_stock_nette_port <- sum(rev_stock_nette) - dotation
      # Surplus par rapport au financement des taux cibles
      add_rev_tx_cib <- rev_stock_nette_port -  sum(bes_cible)

      # Application de la revalorisation par produit
      rev_stock_nette_cible <- bes_cible

      # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
      # et on affecte sur ces produits ce qui n'a pas pu etre dote
      if(add_rev_tx_cib >0){
        rev_stock_nette_cible[prod_avantage] <- bes_cible[prod_avantage] +
          add_rev_tx_cib *  bes_add_ind[prod_avantage] / sum(bes_add_ind[prod_avantage])
      }

    }else{
      # Dans le cas oppose, le besoin cible est finance par la PPB et par les compensations possibles
      # entre produits
      op_ppb <- calc_reprise_ppb(ppb, bes_add) # La reprise de PPB prend en compte les compensations entre produits
      # Mise a jour de la PPB
      ppb <- op_ppb[["ppb"]]
      reprise <- op_ppb[["reprise"]]

      # Contribution des produits sur lesquels la PB contractuelle est plus genereuse que le taux cible
      compens <- sum(avantage_pb_contr)
      # Application de la revalorisation par produit
      rev_stock_nette_cible <- rev_stock_nette
      # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
      rev_stock_nette_cible[prod_avantage] <- bes_cible[prod_avantage]

      # Pour les autres, affectation de la reprise et des compensations entre produits
      if(compens + reprise > 0){
        rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette[! prod_avantage] +
          (reprise + compens) * bes_add_ind[! prod_avantage] / sum(bes_add_ind[! prod_avantage])
      }

      # if(sum(bes_add[! prod_avantage]) == 0){ # Division par 0
      #   rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette[! prod_avantage] +
      #     (reprise + compens) * rep(1, length(rev_stock_nette[! prod_avantage])) / length(rev_stock_nette[! prod_avantage])
      # }else{
      #   rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette[! prod_avantage] +
      #     (reprise + compens) * bes_cible[! prod_avantage] / sum(bes_cible[! prod_avantage])
      # }
    }

    # Controle
    if(prod(is.na(rev_stock_nette_cible)) | prod(is.nan(rev_stock_nette_cible))){
      stop("[RevaloEngine-finance_cible_ppb] : erreur de division par zero.")
    }

    # Output
    return(list(rev_stock_nette = rev_stock_nette_cible, dotation = dotation, reprise = reprise, ppb = ppb))
  }
)


