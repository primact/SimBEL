# #----------------------------------------------------------
# # Ce script comprend les methodes de la classe PortPassif
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 31/01/2017. Fait par MT : initialisation
# #----------------------------------------------------------
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Fonction de calcul des flux de prestations pour un portefeuille de passif
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Calcul les flux de prestations pour un portefeuille de passif.
# ##'
# ##' \code{calc_prest_pp} est une methode permettant de calculer les flux de prestations sur
# ##'  une annee.
# ##' @name calc_prest_pp
# ##' @docType methods
# ##' @param x un objet de la classe \code{PortPassif} contenant les portefeuilles du passif
# ##' @param tx_sortie une liste contenant les taux de sortie
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
# ##' \item{\code{ech} : }{le montant des flux de sortie en echeance de l'annee}
# ##' \item{\code{rach_tot} : }{le montant des  flux de rachat totaux de l'annee}
# ##' \item{\code{dc} : }{le montant des  flux de deces de l'annee}
# ##' \item{\code{rach_part} : }{le montant des  flux de rachat partiel de l'annee}
# ##' \item{\code{prest} : }{le montant des  flux prestations de l'annee}
# ##' \item{\code{rev_ech} : }{le montant de la revalorisation des echeances de l'annee}
# ##' \item{\code{rev_rach_tot} : }{le montant de la revalorisation des rachats totaux de l'annee}
# ##' \item{\code{rev_dc} : }{le montant de la revalorisation des deces de l'annee}
# ##' \item{\code{rev_rach_part} : }{le montant de la revalorisation des rachats partiels de l'annee}
# ##' \item{\code{rev_prest} : }{le montant de la revalorisation des prestations de l'annee}
# ##' \item{\code{rev_prest_nette} : }{le montant de la revalorisation des prestations nette de l'annee}
# ##' \item{\code{enc_charg} : }{le montant des chargements sur l'encours de l'annee}
# ##' \item{\code{rach_charg} : }{le montant des chargements sur les rachats de l'annee}
# ##' \item{\code{soc_prest} : }{le montant des prelevements sociaux sur prestations de l'annee}
# ##' }
# ##' Le format de \code{nb} est :
# ##' \describe{
# ##' \item{\code{nb_ech : }}{le nombre de sorties en echeance de l'annee}
# ##' \item{\code{nb_rach_tot : }}{le nombre de rachats totaux de l'annee}
# ##' \item{\code{nb_dc : }}{le nombre de deces de l'annee}
# ##' \item{\code{nb_sortie : }}{le nombre de sorties de l'annee}
# ##' \item{\code{nb_contr_fin : }}{le nombre de contrats en cours en fin d'annee}
# ##' }
# ##'
# ##' @author Prim'Act
# ##' @export
# ##' @aliases PortPassif
# ##'
# setGeneric(name = "calc_prest_pp", def = function(x, an, method, tx_soc, mt_ap, coef_inf){standardGeneric("calc_prest_pp")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "calc_prest_pp",
#   signature = c(x = "PortPassif", an = "numeric", method = "character", tx_soc = "numeric", mt_ap = "numeric", coef_inf = "numeric"),
#   def = function(x, an, method, tx_soc, mt_ap, coef_inf){
#
#     list_prest_det <- NULL
#     list_prest_agg_flux <- NULL
#     list_prest_agg_nb <- NULL
#     prest_tot_flux <- NULL
#     prest_tot_nb <- NULL
#     ht <- x["ht"]
#     nbProd <- length(x["names_class_prod"])
#
#     for(i in 1:nbProd)
#     {
#
#       ncpi <- x["names_class_prod"][i]
#       nprdi <- names(x[ncpi])
#       longi <- length(x[ncpi])
#
#       for(j in 1:longi)
#       {
#         nprdij <- nprdi[j]
#         mpij <- x[ncpi][[nprdij]]
#         prest <- calc_prest(mpij, ht, an, method, tx_soc)
#
#         # Calcul des frais sur prest
#         frais <- calc_frais(x["fp"], nprdij, "prest",mpij["mp"]["nb_contr"],prest$flux["prest"],coef_inf)
#         # Ajout des frais sur prest ? l'output prest
#         prest$flux <-cbind(prest$flux,frais)
#
#         list_prest_det[[i+j-1]] <- prest
#         list_prest_agg_flux[[i+j-1]] <- colSums(prest$flux)
#         list_prest_agg_nb[[i+j-1]] <- colSums(prest$nb)
#
#         if (i == 1 & j == 1) {
#                               prest_tot_flux <- list_prest_agg_flux[[i+j-1]]
#                               prest_tot_nb <- list_prest_agg_nb[[i+j-1]]
#                               }
#                         else {
#                               prest_tot_flux <- prest_tot_flux + list_prest_agg_flux[[i+j-1]]
#                               prest_tot_nb <- prest_tot_nb + list_prest_agg_nb[[i+j-1]]
#                               }
#         names(list_prest_det)[i+j-1] <- names(x[ncpi])[j]
#         names(list_prest_agg_flux)[i+j-1] <- names(x[ncpi])[j]
#         names(list_prest_agg_nb)[i+j-1] <- names(x[ncpi])[j]
#       }
#     }
#
#     prest_tot_flux["prest"] <- prest_tot_flux["prest"] +  mt_ap
#
#
#     # output
#     return(list(list_prest_det = list_prest_det, list_prest_agg_flux = list_prest_agg_flux, list_prest_agg_nb = list_prest_agg_nb,
#                 prest_tot_flux = prest_tot_flux, prest_tot_nb = prest_tot_nb))
#   }
# )
