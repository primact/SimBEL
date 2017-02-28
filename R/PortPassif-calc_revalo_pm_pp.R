# #----------------------------------------------------------
# # Ce script comprend les methodes de la classe PortPassif
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 03/02/2017. Fait par MT : initialisation
# #----------------------------------------------------------
#
#
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Fonction de calcul de la revalo de la pm pour le portfeuille constituté des differents produits
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Calcul les PM pour le portfeuille constituté des differents produits.
# ##'
# ##' \code{calc_revalo_pm_pp} est une methode permettant de calculer le besoin de taux cible pour le portefeuille
# ##' @name calc_revalo_pm_pp
# ##' @docType methods
# ##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
# ##' @param tx_sortie une liste contenant les taux de sortie associes a chaque ligne de model points :
# ##'  \describe{
# ##'  \item{\code{qx_rach_tot : }}{un vecteur avec les taux de rachat total structurel}
# ##'  \item{\code{qx_rach_tot_dyn : }}{un vecteur avec les taux de rachat total dynamique}
# ##'  \item{\code{qx_dc : }}{un vecteur avec les taux de deces}
# ##'  \item{\code{qx_rach_part : }}{un vecteur avec les taux de rachats partiel structurel}
# ##'  \item{\code{qx_rach_part_dyn : }}{un vecteur avec les taux de rachats partiel dynamique}
# ##'  }
# ##' @param tx_min un data.frame contenant le taux de revalorisation minimum associes a chaque ligne de model points :
# ##'  \describe{
# ##'  \item{\code{tx_min_an : }}{un vecteur de taux annuels}
# ##'  \item{\code{tx_min_se : }}{un vecteur de taux semestriels}
# ##'  }
# ##' @param an un numeric represantant l'annee de projection courante
# ##' @param method un character prenant pour valeur \code{normal} pour le calcul des flux avec application de la revalorisation au titre
# ##' participation aux benefices, et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).
# ##' @return Une liste contenant :
# ##' \describe{
# ##' \item{\code{method} : }{la valeur de l'argument \code{method}}
# ##' \item{\code{flux} : }{un data.frame comprenant les flux de l'annee}
# ##' \item{\code{nb} : }{un data.frame comprenant les nombres de sorties}
# ##' }
# ##' Le format de \code{flux} est :
# ##' \describe{
# ##' \item{\code{ech} : }{un vecteur contenant les flux de sortie en echeance de l'annee}
# ##' \item{\code{rach_tot} : }{un vecteur contenant les flux de rachat totaux de l'annee}
# ##' \item{\code{dc} : }{un vecteur contenant les flux de deces de l'annee}
# ##' \item{\code{rach_part} : }{un vecteur contenant les flux de rachat partiel de l'annee}
# ##' \item{\code{prest} : }{un vecteur contenant les flux prestations de l'annee}
# ##' \item{\code{rev_ech} : }{un vecteur contenant la revalorisation des echeances de l'annee}
# ##' \item{\code{rev_rach_tot} : }{un vecteur contenant la revalorisation des rachats totaux de l'annee}
# ##' \item{\code{rev_dc} : }{un vecteur contenant la revalorisation des deces de l'annee}
# ##' \item{\code{rev_rach_part} : }{un vecteur contenant la revalorisation des rachats partiels de l'annee}
# ##' \item{\code{rev} : }{un vecteur contenant la revalorisation des prestations de l'annee}
# ##' }
# ##' Le format de \code{nb} est :
# ##' \describe{
# ##' \item{\code{nb_ech : }}{un vecteur contenant le nombre de sorties en echeance de l'annee}
# ##' \item{\code{nb_rach_tot : }}{un vecteur contenant le nombre de rachats totaux de l'annee}
# ##' \item{\code{nb_dc : }}{un vecteur contenant le nombre de deces de l'annee}
# ##' \item{\code{nb_sortie : }}{un vecteur contenant le nombre de sorties de l'annee}
# ##' \item{\code{nb_contr_fin : }}{un vecteur contenant le nombre de contrats en cours en fin d'annee}
# ##' }
# ##'
# ##' @author Prim'Act
# ##' @export
# ##' @aliases PortPassif
# ##'
# setGeneric(name = "calc_revalo_pm_pp", def = function(x, rev_net_alloue_pp, bes_tx_cible_pp, tab_pm_pp, tab_prime_pp, tab_prest_pp, an , tx_soc)
# {standardGeneric("calc_revalo_pm_pp")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "calc_revalo_pm_pp",
#   signature = c(x = "PortPassif",rev_net_alloue_pp ="numeric", bes_tx_cible_pp = "list", tab_pm_pp = "list", tab_prime_pp = "list", tab_prest_pp = "list", an = "numeric", tx_soc = "numeric"),
#   def = function(x, rev_net_alloue_pp, bes_tx_cible_pp, tab_pm_pp, tab_prime_pp, tab_prest_pp, an , tx_soc){
#
#
#     list_revalo_pm_pp_det <- NULL
#     list_revalo_pm_pp_agg <- NULL
#     revalo_pm_pp_tot <- NULL
#     nbProd <- length(x["names_class_prod"])
#
#     for(i in 1:nbProd)
#     {
#       ncpi <- x["names_class_prod"][i]
#       nprdi <- names(x[ncpi])
#       longi <- length(x[ncpi])
#
#       for(j in 1:longi)
#       {
#         nprdij <- nprdi[j]
#         mpij <- x[ncpi][[nprdij]]
#
#         list_revalo_pm_pp_det[[i+j-1]] <- calc_revalo_pm(mpij,rev_net_alloue_pp[i+j-1], bes_tx_cible_pp$list_bes_tx_cible_det[[i+j-1]],
#                                                          tab_pm_pp$list_pm_det[[i+j-1]], tab_prime_pp$list_primes_det[[i+j-1]],
#                                                          tab_prest_pp$list_prest_det[[i+j-1]]$flux, an, tx_soc)
#
#         list_revalo_pm_pp_agg[[i+j-1]] <- colSums(list_revalo_pm_pp_det[[i+j-1]])
#
#         if (i == 1 & j == 1) {
#           revalo_pm_pp_tot <- colSums(list_revalo_pm_pp_det[[i+j-1]])
#         }
#         else {
#           revalo_pm_pp_tot <- revalo_pm_pp_tot + colSums(list_revalo_pm_pp_det[[i+j-1]])
#         }
#
#         names(list_revalo_pm_pp_det)[i+j-1] <- names(x[ncpi])[j]
#         names(list_revalo_pm_pp_agg)[i+j-1] <- names(x[ncpi])[j]
#       }
#     }
#
#
#     # output
#     return(list(list_revalo_pm_pp_det = list_revalo_pm_pp_det, list_revalo_pm_pp_agg = list_revalo_pm_pp_agg, revalo_pm_pp_tot = revalo_pm_pp_tot))
#
#   }
# )
