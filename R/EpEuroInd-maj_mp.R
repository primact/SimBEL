# #----------------------------------------------------------
# # Ce script comprend les methodes de la classe EpEuroInd
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 03/02/2017. Fait par MT : initialisation
# #----------------------------------------------------------
#
#
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Fonction de mise à jour des model points
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Calcul la revalo pour des PM d'un model point epargne en euros.
# ##'
# ##' \code{maj_mp} est une methode permettant la mise à jour des model points
# ##' @name maj_mp
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
# ##' @aliases EpEuroInd
# ##'
# setGeneric(name = "maj_mp", def = function(x, tab_pm, tab_prime, tab_prest_nb, tab_prest_flux, revalo_pm_ap_pb, tx_cible)
#   {standardGeneric("maj_mp")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "maj_mp",
#   signature = c(x = "EpEuroInd", tab_pm = "list", tab_prime = "data.frame", tab_prest_nb = "data.frame", tab_prest_flux = "data.frame",revalo_pm_ap_pb = "data.frame", tx_cible = "data.frame"),
#   def = function(x, tab_pm, tab_prime, tab_prest_nb, tab_prest_flux, revalo_pm_ap_pb, tx_cible){
#
#     #Mise à jour age et anciennete
#     x <- vieilli_mp(x)
#
#     #Mise a jour du nombre de contrats
#     x["mp"]$nb_contr <- tab_prest_nb$nb_contr_fin
#
#     #Mise a jour du tx_cible_prec
#     x["mp"]$tx_cible_prec <- tx_cible$tx_cible_an
#
#     #Mise a jour du tx_revalo_prec
#     x["mp"]$tx_revalo_prec <- revalo_pm_ap_pb$tx_rev_net
#
#     #Mise a jour de la PM
#     x["mp"]$pm <- tab_pm$stock$pm_deb - tab_prest_flux$prest + tab_prime$pri_net + revalo_pm_ap_pb$revalo_stock_nette_ap_pb - revalo_pm_ap_pb$soc_stock
#
#     # output
#     return(x)
#   }
# )
