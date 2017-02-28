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
# #           Fonction de mise à jour du portefeuille de passifs
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Mise à jour du portefeuille de passifs.
# ##'
# ##' \code{maj_mp_pp} est une methode permettant la mise à jour du portefeuille de passifs
# ##' @name maj_mp_pp
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
# setGeneric(name = "maj_mp_pp", def = function(x,tab_pm_pp, tab_prime_pp, tab_prest_pp, revalo_pm_ap_pb_pp)
# {standardGeneric("maj_mp_pp")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "maj_mp_pp",
#   signature = c(x = "PortPassif", tab_pm_pp = "list", tab_prime_pp = "list", tab_prest_pp = "list", revalo_pm_ap_pb_pp = "list"),
#   def = function(x,tab_pm_pp, tab_prime_pp, tab_prest_pp, revalo_pm_ap_pb_pp){
#
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
#         tx_cible_mp <- calc_tx_cible(mpij,list_rd, list_param_mar)
#
#         x[ncpi][[nprdij]] <- maj_mp(x[i][[j]], tab_pm_pp$list_pm_det[[i+j-1]],  tab_prime_pp$list_primes_det[[i+j-1]],
#                             tab_prest_pp$list_prest_det[[i+j-1]]$nb, tab_prest_pp$list_prest_det[[i+j-1]]$flux,
#                             revalo_pm_ap_pb_pp$list_revalo_pm_pp_det[[i+j-1]], tx_cible_mp)
#       }
#     }
#
#     # output
#     return(x)
#
#   }
# )
