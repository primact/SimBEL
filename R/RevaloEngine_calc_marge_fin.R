#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_marge_fin : methode permettant de calculer la marge financiere de l'assureur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la marge financiere de l'assureur.
##'
##' \code{calc_marge_fin} est une methode permettant
##'  de calculer la marge financiere de l'assureur apres attribution d'un certain niveau de revalorisation.
##' @name calc_marge_fin
##' @docType methods
##' @param base_fin est un vecteur de type \code{numeric} comprenant par produit la base de produits financiers.
##' @param rev_prest_nette est un vecteur de type \code{numeric} comprenant par produit
##' la revalorisation nette sur prestations.
##' @param rev_stock_nette est un vecteur de type \code{numeric} comprenant par produit
##' la revalorisation nette sur stock.
##' @param contrib_tmg_prest est une valeur \code{numeric} comprenant par produit
##' la contribution de la PPB au financement des TMG sur prestations.
##' @param contrib_tmg_stock est une valeur \code{numeric} comprenant par produit
##' la contribution de la PPB au financement des TMG sur stock.
##' @param contrib_ppb_tx_cible une valeur de type \code{numeric} comprenant par produit
##' la contribution de la PPB au financement du taux cible sur stock.
##' @return Le montant de la marge de l'assureur.
##' @author Prim'Act
##' @export

setGeneric(name = "calc_marge_fin", def = function(base_fin,
                                                   rev_prest_nette, rev_stock_nette,
                                                   contrib_tmg_prest, contrib_tmg_stock,
                                                   contrib_ppb_tx_cible){
  standardGeneric("calc_marge_fin")})
setMethod(
  f = "calc_marge_fin",
  signature = c(base_fin = "numeric",
                rev_prest_nette = "numeric", rev_stock_nette = "numeric",
                contrib_tmg_prest = "numeric", contrib_tmg_stock = "numeric",
                contrib_ppb_tx_cible = "numeric"),
  definition = function(base_fin,
                        rev_prest_nette, rev_stock_nette,
                        contrib_tmg_prest, contrib_tmg_stock,
                        contrib_ppb_tx_cible){


    # Controle
    if(length(base_fin) != length(rev_prest_nette) |
       length(rev_stock_nette) != length(rev_prest_nette)){
      stop("[RevaloEngine-calc_marge_fin] : les vecteurs en entree ne sont pas de meme longueur.")
    }

    if(length(contrib_tmg_prest) > 1  | length(contrib_tmg_stock) > 1 | length(contrib_ppb_tx_cible) > 1){
      stop("[RevaloEngine-calc_marge_fin] : les vecteurs correspondant au
           contribution de la PPB doivent etre de taille 1.")
    }


    # Calcul de la marge financiere avant revalorisation du stock
    marge_fin <- sum(base_fin)
    # if(sum(base_fin) < 0){ # Perte financiere globale
    #   marge_fin <- sum(base_fin)
    # }else{
    #   marge_fin <- sum(base_fin * (1 - tx_pb)) # Attribution apres PB contractuelle
    # }
    marge_fin <- marge_fin -
      sum(rev_prest_nette) + contrib_tmg_prest - # Retrait de la charge pour financement revalorisation des prestations
      sum(rev_stock_nette) + contrib_tmg_stock +
      contrib_ppb_tx_cible # Ajout du financement venant de la PPB sur les TMG du stock et reprise pour taux cible

    # Output
    return(marge_fin)
    }
)


