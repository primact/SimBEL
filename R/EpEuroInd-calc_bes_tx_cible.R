# #----------------------------------------------------------
# # Ce script comprend les methodes de la classe EpEuroInd
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 02/02/2017. Fait par MT : initialisation
# #----------------------------------------------------------
#
#
# ##' Calcul des differents besoins en taux cible pour chaque model point de la classe EpEuroInd
# ##'
# ##' \code{calc_bes_tx_cible} est une methode permettant de calculer un taux cible
# ##'   pour chaque model point de la classe EpEuroInd.
# ##' @name calc_bes_tx_cible
# ##' @docType methods
# ##' @param epi un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
# ##' @param mpESG un objet de la classe \code{ModelPointESG} contenant les model points ESG.
# ##' @param mat_oblig numerique : maturité de référence pour le calcul du rendement obligataire servant à calculer le taux de marché.
# ##' @param alloc_mar vecteur numerique de 4 éléments : relatif à l'allocation de marché.
# ##' @param w_n numerique : poids de pondération du taux de marché dans le calcul du taux cible.
# ##' @param marge_mar numerique : marge assureur marché
# ##' @param ch_enc_mar numerique : chargmeent encours marché
# ##' @param ind_ref_action numerique : indice de reference action
# ##' @param ind_ref_immo numerique : indice de reference immobilier
# ##' @return Une data.frame contenant :
# ##' \describe{
# ##' \item{\code{calc_bes_tx_cible} : }{un vecteur contenant les taux cible pour chaque model point}
# ##' }
# ##' @author Prim'Act
# ##' @export
# ##' @aliases EpEuroInd
#
# setGeneric(name = "calc_bes_tx_cible",
#            def = function(epi, tx_cible, tab_prime, tab_prest, tab_pm)
#           {standardGeneric("calc_bes_tx_cible")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "calc_bes_tx_cible",
#   signature = c(epi = "EpEuroInd", tab_prest = "data.frame", tab_pm = "data.frame"),
#   def = function(epi, tx_cible, tab_prime, tab_prest, tab_pm){
#
#
#
#
#     return(bes_tx_cible = bes_tx_cible)
#   }
# )
