#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_cible_pmvl : methode permettant de calculer le financement de PB par la realisation de PVL actions
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue le financement d'une revalorisation au taux cible par des cessions de plus-values latentes.
##'
##' \code{finance_cible_pmvl} est une methode permettant de
##'  determiner le financement d'une revalorisation au taux cible par une cession de plus-values latentes en actions.
##' @name finance_cible_pmvl
##' @docType methods
##' @param bes_cible un vecteur \code{numeric} correspondant au besoin de financement necessaire pour atteindre
##' le taux cible part produit.
##' @param rev_stock_nette un vecteur \code{numeric} comprenant par produit
##'  le montant de revalorisation nette au titre de le PB atteint.
##' @param base_fin un vecteur \code{numeric} comprenant par produit la base de produits financiers.
##' @param seuil_pmvl une valeur \code{numeric} correspondant au montant de plus-values latentes
##'  qui peut etre liquidee. Ce montant doit etre exprime en tenant compte de l'abattement (mise a l'echelle) realise
##'   pour rapport aux plus-values latentes de l'actif general au passif.
##' @param tx_pb un vecteur \code{numeric} comprenant par produit les taux de participation aux benefices contractuels.
##' @return \code{rev_stock_nette} la valeur de la revalorisation nette servie par produit apres cession.
##' @return \code{pmvl_liq} le montant de plus-values a liquider, ramene a la valeur du passif, pour financer
##'  la revalorisation.
##' @details Lorsque la revalorisation nette est superieure au besoin de financement des taux cibles, on sert le taux cible
##' et on partage le surplus. A l'inverse, les taux cible sont finances par
##' les compensations entre produits lorsque certains prevoient une revalorisation superieure au taux cible, et
##' par une liquidation de plus-values latentes.
##' @author Prim'Act
##' @export

setGeneric(name = "finance_cible_pmvl", def = function(bes_cible, rev_stock_nette, base_fin, seuil_pmvl, tx_pb){
  standardGeneric("finance_cible_pmvl")})
setMethod(
  f = "finance_cible_pmvl",
  signature = c(bes_cible = "numeric", rev_stock_nette = "numeric", base_fin = "numeric",
                seuil_pmvl = "numeric", tx_pb = "numeric"),
  definition = function(bes_cible, rev_stock_nette, base_fin, seuil_pmvl, tx_pb){

    # Controle
    if(prod(bes_cible < 0)){
      stop("[RevaloEngine-finance_cible_pmvl] : les besoins de financements par produit doivent etre positifs.")
    }
    if(prod(tx_pb < 0)){
      stop("[RevaloEngine-finance_cible_pmvl] : les taux de PB doivent etre positifs.")
    }
    if(length(bes_cible) != length(rev_stock_nette) | length(tx_pb) != length(rev_stock_nette)){
      stop("[RevaloEngine-finance_cible_pmvl] : les vecteurs en entree ne sont pas de meme longueur.")
    }

    # Analyse du besoin de financement sur l'ensemble des produits
    bes_add_ind <- bes_cible - rev_stock_nette
    bes_add <- sum(bes_add_ind)

    # Montant pour lequelle la PB contractuelle est plus genereuse
    avantage_pb_contr <- pmax(0, - bes_add_ind)
    # Produit pour lesquelle la  PB contractuelle est plus genereuse
    prod_avantage <- (avantage_pb_contr >  0)


    # Aucune operation si la revalorisation nette sur le stock est superieure au besoin de taux cible
    # On realise simplement des compensations entre produits
    # Dans ce cas, comme 'bes_cible' est necessairement positif, on a 'rev_stock_nette' qui est positif
    if(bes_add < 0){

      # Mise a zero des PMVL liquidees
      pmvl_liq <- 0

      # Surplus par rapport au financement des taux cibles
      add_rev_tx_cib <- (- bes_add)


      # Application de la revalorisation par produit
      rev_stock_nette_cible <- bes_cible

      # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
      # et on affecte sur ces produits ce qui n'a pas pu etre dote
      if(add_rev_tx_cib >0){
        rev_stock_nette_cible[prod_avantage] <- bes_cible[prod_avantage] +
          add_rev_tx_cib *  bes_add_ind[prod_avantage] / sum(bes_add_ind[prod_avantage])
      }

    }else{
      # Dans le cas oppose, on liquide une PMVL

      # Contribution des produits sur lesquels la PB contractuelle est plus genereuse que le taux cible
      compens <- sum(avantage_pb_contr)
      # Application de la revalorisation par produit
      rev_stock_nette_cible <- rev_stock_nette
      # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
      rev_stock_nette_cible[prod_avantage] <- bes_cible[prod_avantage]

      # Pour les autres, affectation des compensations entre produits
      if(compens > 0){
        rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette[! prod_avantage] +
          (compens) * bes_add_ind[! prod_avantage] / sum(bes_add_ind[! prod_avantage])
      }

      # Les PMVL sont calcules apres compensation
      bes_add_ind <- (bes_cible - rev_stock_nette_cible)
      if(sum(bes_add_ind) !=0){
        # Montant pour lequelle la PB contractuelle est plus genereuse
        avantage_pb_contr <- pmax(0, - bes_add_ind)
        # Produit pour lesquelle la  PB contractuelle est plus genereuse
        prod_avantage <- (avantage_pb_contr >  0)

        # Montant de PMVL qu'il faudrait liquider
        if(sum(base_fin) !=0){
          # Calcul du taux de PB moyen
          tx_pb_moy <- sum(base_fin * tx_pb / sum(base_fin))
        }else{
          tx_pb_moy <- mean(tx_pb)
        }


        pmvl_liq <- sum(bes_add_ind) * 1 / tx_pb_moy # PMVL a liquider
        if(pmvl_liq == 0){ # Gestion des division par 0
          # Supplement de revalorisation
          suppl_revalo <-0
        } else{
          coef_echelle <- pmvl_liq / sum(bes_add_ind) # Coefficient de passage passif vers actif
          # Application du seuil de liquidation
          pmvl_liq <- min(seuil_pmvl, pmvl_liq)
          # Supplement de revalorisation
          suppl_revalo <- pmvl_liq / coef_echelle
        }

        # Pour les autres, affectation de la pmvl et des compensations entre produits
        if(suppl_revalo > 0){
          rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette_cible[! prod_avantage] +
            (suppl_revalo) * bes_add_ind[! prod_avantage] / sum(bes_add_ind[! prod_avantage])
        }
      }else{ # Cession nulle
        pmvl_liq <- 0
      }

    }

      # Controle
      if(prod(is.na(rev_stock_nette_cible)) | prod(is.nan(rev_stock_nette_cible))){
        stop("[RevaloEngine-finance_cible_pmvl] : erreur de division par zero.")
      }

    # Output
    return(list(rev_stock_nette = rev_stock_nette_cible, pmvl_liq = pmvl_liq))
  }
)

