#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le financement de la PB contractuelle.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           pb_contr
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la PB contractuelle.
##'
##' \code{pb_contr} est une methode permettant de calculer la participation aux benefices contractuelle par produit.
##' @name pb_contr
##' @docType methods
##' @param base_fin est un vecteur de type \code{numeric} comprenant par produit la base de produits financiers.
##' @param tx_pb est un vecteur de type \code{numeric} comprenant par produit les taux de participation aux benefices contractuels.
##' @param rev_stock_brut est un vecteur de type \code{numeric} comprenant par produit la revalorisation
##' appliquee sur le stock au taux minimum.
##' @param ch_enc_th est un vecteur de type \code{numeric} comprenant par produit le montant total
##'  des chargements sur encours appliques stock, revalorise au taux minimum.
##' @param tx_enc_moy est un vecteur de type \code{numeric} comprenant par produit
##' les taux de chargements sur encours moyens.
##' @return Une liste composee de deux vecteurs comprenant par produit les chargements sur encours appliques et la
##' revalorisation contractuelle nette.
##' @details Le montant des chargements \code{ch_enc_th} est theorique et peut
##' conduire a l'application d'une revalorisation nette negative.
##'
##' @author Prim'Act
##' @export
##' @aliases RevaloEngine

setGeneric(name = "pb_contr", def = function(base_fin, tx_pb, rev_stock_brut, ch_enc_th,
                                             tx_enc_moy){standardGeneric("pb_contr")})
setMethod(
  f = "pb_contr",
  signature = c(base_fin = "numeric", tx_pb = "numeric", rev_stock_brut = "numeric", ch_enc_th = "numeric",
                tx_enc_moy  = "numeric"),
  definition = function(base_fin, tx_pb, rev_stock_brut, ch_enc_th, tx_enc_moy){

    # Controle
    if(prod(tx_pb < 0)){
      stop("[RevaloEngine-pb_contr] : les taux de PB doivent etre positifs.")
    }
    if(length(base_fin) != length(tx_pb) | length(tx_pb) != length(rev_stock_brut) |
       length(rev_stock_brut) != length(ch_enc_th) | length(ch_enc_th) != length(tx_enc_moy)){
      stop("[RevaloEngine-pb_contr] : les vecteurs en entree ne sont pas de meme longueur.")
    }



    # Calcul de la PB contractuelle par produit
    # base_fin <- c(base_fin, base_fin_hm) # Ajout de la base financiere hors modele
    pb_contr <- pmax(0, base_fin) * tx_pb # Calcul de la PB contractuelle

    # Calcul du supplement de revalorisation par produit
    add_pb_contr <- pmax(0, pb_contr - rev_stock_brut)

    # Chargements sur encours
    # Calcul des chargements sur encours theoriques par produit
    ch_enc_ap_pb_contr <- ch_enc_th + add_pb_contr * tx_enc_moy
    # Calcul de la revalorisation nette sans application de limite sur les chargements sur encours
    rev_stock_nette_contr <- rev_stock_brut + add_pb_contr - ch_enc_ap_pb_contr

    # Output
    return(list(ch_enc_ap_pb_contr = ch_enc_ap_pb_contr, rev_stock_nette_contr = rev_stock_nette_contr))

  }
)
