#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_cible_marge : methode permettant de calculer le financement de PB par la marge financiere de l'assureur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue le financement d'une revalorisation au taux cible par la marge de l'assureur
##'
##' \code{finance_cible_marge} est une methode permettant de
##'  determiner le financement d'une revalorisation au taux cible en comprimant la marge financiere
##'  de l'assureur
##' @name finance_cible_marge
##' @docType methods
##' @param marge_fin une valeur \code{numeric} donnant le montant courant de la marge financiere de l'assureur.
##' @param bes_cible un vecteur \code{numeric} correspondant au besoin de financement necessaire pour atteindre
##' le taux cible part produit.
##' @param rev_stock_nette un vecteur \code{numeric} comprenant par produit
##'  le montant de revalorisation nette au titre de le PB atteint.
##' @param marge_min est une valeur \code{numeric} correspondant
##'  au montant minimum de marge financiere souhaite par l'assureur.
##' @return \code{rev_stock_nette} la valeur de la revalorisation nette servie par produit apres reduction de marge.
##' @return \code{marge_fin} le montant de marge de l'assureur apres reduction.
##' @author Prim'Act
##' @export

setGeneric(name = "finance_cible_marge", def = function(marge_fin, bes_cible, rev_stock_nette, marge_min){
  standardGeneric("finance_cible_marge")})
setMethod(
  f = "finance_cible_marge",
  signature = c(marge_fin = "numeric", bes_cible = "numeric", rev_stock_nette = "numeric", marge_min = "numeric"),
  definition = function(marge_fin, bes_cible, rev_stock_nette, marge_min){

    # Controle
    if(prod(bes_cible < 0)){
      stop("[RevaloEngine-finance_cible_marge] : les besoins de financements par produit doivent etre positifs.")
    }
    if(length(bes_cible) != length(rev_stock_nette)){
      stop("[RevaloEngine-finance_cible_marge] : les vecteurs en entree ne sont pas de meme longueur.")
    }

    # Analyse du besoin de financement sur l'ensemble des produits
    bes_add_ind <- bes_cible - rev_stock_nette
    bes_add <- sum(bes_add_ind)


    # Montant pour lequelle la PB contractuelle est plus genereuse
    avantage_pb_contr <- pmax(0, - bes_add_ind)
    # Produit pour lesquelle la  PB contractuelle est plus genereuse
    prod_avantage <- (avantage_pb_contr >  0)


    # Evaluation du montant que peut financer la marge de l'assureur
    finance_marg <- max(0, marge_fin - marge_min) # Montant pouvant etre utilise pour financer le besoin supplementaire de revalorisation


    # Aucune operation si la revalorisation nette sur le stock est superieure au besoin de taux cible
    # On realise simplement des compensations entre produits
    # Dans ce cas, comme 'bes_cible' est necessairement positif, on a 'rev_stock_nette' qui est positif
    if(bes_add < 0){

      # Initialisation
      bes_finance <-0

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

      # Besoin finance par la marge
      bes_finance <- min(finance_marg, bes_add)

      # Contribution des produits sur lesquels la PB contractuelle est plus genereuse que le taux cible
      compens <- sum(avantage_pb_contr)
      # Application de la revalorisation par produit
      rev_stock_nette_cible <- rev_stock_nette
      prod_avantage <- (avantage_pb_contr > 0)
      # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
      rev_stock_nette_cible[prod_avantage] <- bes_cible[prod_avantage]
      # Pour les autres, affectation des compensations et du besoin finance par la marge entre produits

      if(compens + bes_finance > 0){
      rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette[! prod_avantage] +
        (bes_finance + compens) * bes_add_ind[! prod_avantage] / sum(bes_add_ind[! prod_avantage])
      }
    }

    # Output
    return(list(rev_stock_nette = rev_stock_nette_cible, marge_fin = marge_fin - bes_finance))
    }
)


