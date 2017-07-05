#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_cible_ppb : methode permettant de calculer le financement de PB par reprise de PPB.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue le financement d'une revalorisation au taux cible par une reprise de PPB. Cette methode permet aussi de redistribuer
##' la PPB stockee 8 annees auparavant.
##'
##' \code{finance_cible_ppb} est une methode permettant de
##'  determiner le financement d'une revalorisation au taux cible par la reprise de provision pour participation
##'   aux benefices (PPB). Cette methode evalue egalement si une dotation est effectuee.
##' @name finance_cible_ppb
##' @docType methods
##' @param bes_cible un vecteur \code{numeric} correspondant au besoin de financement necessaire pour atteindre
##' le taux cible par produit.
##' @param rev_stock_nette un vecteur \code{numeric} comprenant par produit
##'  le montant de revalorisation nette au titre de le PB atteint.
##' @param ppb un objet de la classe \code{\link{Ppb}} qui renvoie l'etat courant de la PPB.
##' @param ppb8_ind une valeur \code{numeric} correspondant ? la ppb de l'annee t-8.
##' @details Lorsque la revalorisation nette est superieure au besoin de financement des taux cibles,
##' on sert le taux cible et on dote le reste a la PPB dans la limite du plafond de dotation annuel.
##' A l'inverse, les taux cible sont finances par les compensations entre produits lorsque certains
##' prevoient une revalorisation superieure au taux cible, puis
##' par une reprise sur PPB.
##' @return \code{rev_stock_nette} la valeur de la revalorisation nette servie apres une eventuelle reprise de PPB.
##' @return \code{dotation} le montant de dotation a la PPB.
##' @return \code{reprise} le montant de reprise sur la PPB.
##' @return \code{ppb} l'objet \code{ppb} mis a jour.
##' @author Prim'Act
##' @export
##' @include Ppb_class.R

setGeneric(name = "finance_cible_ppb", def = function(bes_cible, rev_stock_nette, ppb, ppb8_ind){standardGeneric("finance_cible_ppb")})
setMethod(
    f = "finance_cible_ppb",
    signature = c(bes_cible = "numeric", rev_stock_nette = "numeric", ppb = "Ppb", ppb8_ind = "numeric"),
    definition = function(bes_cible, rev_stock_nette, ppb, ppb8_ind){

        # Controle
        if(! all(bes_cible >= 0)) stop("[RevaloEngine-finance_cible_ppb] : les besoins de financements par produit doivent etre positifs.")
        if(length(bes_cible) != length(rev_stock_nette)) stop("[RevaloEngine-finance_cible_ppb] : les vecteurs en entree ne sont pas de meme longueur.")
        if(length(ppb8_ind) != length(rev_stock_nette)) stop("[RevaloEngine-finance_cible_ppb] : les vecteurs en entree ne sont pas de meme longueur.")


        # Total de PPB8ans ? attribuer
        ppb_8 <- sum(ppb8_ind)

        bes_add_ind <- pmax(bes_cible - ppb8_ind, 0) - rev_stock_nette
        # bes_add <- max(sum(bes_cible) - ppb_8, 0) - sum(rev_stock_nette)
        bes_add <- sum(bes_add_ind)



        # Montant pour lequel la PB contractuelle est plus genereuse
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

            # Revalorisation totale attribuee au portefeuille : ce qui n'a pas pu etre dote ainsi que la PPB8
            rev_stock_nette_port <- sum(rev_stock_nette) - dotation
            # Surplus par rapport au financement des taux cibles
            # add_rev_tx_cib <- rev_stock_nette_port - sum(bes_cible)
            add_rev_tx_cib <- rev_stock_nette_port - sum(pmax(bes_cible - ppb8_ind, 0))

            # Application de la revalorisation par produit
            rev_stock_nette_cible <- pmax(bes_cible, ppb8_ind)
            # rev_stock_nette_cible <- ppb8_ind + pmax(bes_cible - rev_stock_nette , 0)

            # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
            # et on affecte sur ces produits ce qui n'a pas pu etre dote
            if(add_rev_tx_cib >0){
                rev_stock_nette_cible[prod_avantage] <- rev_stock_nette_cible[prod_avantage] +
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
            # compens <- sum(pmax(0, rev_stock_nette - bes_cible))

            # Application de la revalorisation par produit
            rev_stock_nette_cible <- rev_stock_nette + ppb8_ind

            # Les produits qui prevoient une revalo contractuelle plus avantageuse que le taux cible sont revalorises au taux cible
            rev_stock_nette_cible[prod_avantage] <- bes_cible[prod_avantage]

            # Pour les autres, affectation de la reprise et des compensations entre produits
            if(compens + reprise > 0){
                rev_stock_nette_cible[! prod_avantage] <- rev_stock_nette_cible[! prod_avantage] +
                    (reprise + compens) * bes_add_ind[! prod_avantage] / sum(bes_add_ind[! prod_avantage])
            }

        }

        # Controle
        if(prod(is.na(rev_stock_nette_cible)) | prod(is.nan(rev_stock_nette_cible))){
            stop("[RevaloEngine-finance_cible_ppb] : erreur de division par zero.")
        }

        # Output
        return(list(rev_stock_nette = rev_stock_nette_cible,
                    dotation = dotation,
                    reprise = reprise,
                    ppb = ppb))
    }
)


