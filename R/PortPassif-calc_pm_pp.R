# #----------------------------------------------------------
# # Ce script comprend les methodes de la classe PortPassif
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 01/02/2017. Fait par MT : initialisation
# #----------------------------------------------------------
#
#
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Fonction de calcul des flux de pm d un model point
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Calcul les PM d'un model point epargne en euros.
# ##'
# ##' \code{calc_pm_pp} est une methode permettant de calculer les PM sur une annee.
# ##' @name calc_pm_pp
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
# setGeneric(name = "calc_pm_pp", def = function(x, tab_prime_pp, tab_prest_pp, an, method, tx_soc, mt_ap, coef_inf){standardGeneric("calc_pm_pp")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "calc_pm_pp",
#   signature = c(x = "PortPassif", tab_prime_pp = "list", tab_prest_pp = "list",
#                 an = "numeric", method = "character", tx_soc = "numeric", mt_ap = "numeric", coef_inf = "numeric"),
#   def = function(x, tab_prime_pp, tab_prest_pp,
#                  an, method, tx_soc, mt_ap, coef_inf){
#
#     # Initialisation des listes de resultats
#     list_pm_det <- NULL
#     list_pm_agg_stock <- NULL
#     list_pm_agg_flux <- NULL
#     pm_tot_stock <- NULL
#     pm_tot_flux <- NULL
#     nbProd <- length(x@names_class_prod)
#
#     # Boucle sur les types de produits
#     for(i in 1:nbProd)
#     {
#       # Liste des produits
#       ncpi <- x@names_class_prod[i] # Nom du produits i
#       list_prodi <- x[ncpi] #
#
#       # Noms des produits de la liste
#
#
#       nprdi <- names()
#       # Noms des produits de la liste
#       longi <- length(x[ncpi])
#
#       for(j in 1:longi)
#       {
#
#         nprdij <- nprdi[j]
#         mpij <- x[ncpi][[nprdij]]
#         pm <- calc_pm(x[ncpi][[nprdij]], tab_prime_pp$list_primes_det[[i+j-1]],tab_prest_pp$list_prest_det[[i+j-1]]$flux, an, method, tx_soc)
#
#         # Calcul des frais sur encours
#         frais <- calc_frais(x["fp"], nprdij, "enc",mpij["mp"]["nb_contr"], pm$stock["pm_fin"],coef_inf)
#         # Ajout des frais sur pm ? l'output pm
#         pm$flux <-cbind(pm$flux,frais)
#
#         list_pm_det[[i+j-1]] <- pm
#         list_pm_agg_stock[[i+j-1]] <- colSums(list_pm_det[[i+j-1]]$stock)
#         list_pm_agg_flux[[i+j-1]] <- colSums(list_pm_det[[i+j-1]]$flux)
#
#         if (i == 1 & j == 1) {
#           pm_tot_stock <- colSums(list_pm_det[[i+j-1]]$stock)
#           pm_tot_flux <- colSums(list_pm_det[[i+j-1]]$flux)
#         }
#         else {
#           pm_tot_stock <- pm_tot_stock + colSums(list_pm_det[[i+j-1]]$stock)
#           pm_tot_flux <- pm_tot_flux + colSums(list_pm_det[[i+j-1]]$flux)
#         }
#
#         names(list_pm_det)[i+j-1] <- names(x[ncpi])[j]
#         names(list_pm_agg_stock)[i+j-1] <- names(x[ncpi])[j]
#         names(list_pm_agg_flux)[i+j-1] <- names(x[ncpi])[j]
#       }
#     }
#
#     pm_tot_stock["pm_fin"] <- pm_tot_stock["pm_fin"] +  mt_ap
#
#
#     # output
#     return(list(list_pm_det = list_pm_det, list_pm_agg_stock = list_pm_agg_stock,list_pm_agg_flux = list_pm_agg_flux,
#                 pm_tot_stock = pm_tot_stock, pm_tot_flux = pm_tot_flux))
#
#   }
# )
