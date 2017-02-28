# #----------------------------------------------------------
# # Ce script comprend les methodes de la classe PortPassif
# #----------------------------------------------------------
# # Suivi version
# # Version 1.0 du 31/01/2017. Fait par MT : initialisation
# #----------------------------------------------------------
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           Fonction de calcul des flux de primes pour un portefeuille de passif
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Calcul les flux de primes pour un portefeuille de passif.
# ##'
# ##' \code{calc_primes_pp} est une methode permettant de calculer les flux de primes sur
# ##'  une annee.
# ##' @name calc_primes_pp
# ##' @docType methods
# ##' @param x un objet de la classe \code{PortPassif} contenant les portefeuilles du passif
# ##' @return Un data.frame contenant :
# ##' \describe{
# ##' \item{\code{pri_brut} : }{un vecteur contenant les flux de primes brutes de chargements de l'annee}
# ##' \item{\code{pri_net} : }{un vecteur contenant les flux de primes nettes de chargements de l'annee}
# ##' \item{\code{pri_chgt} : }{un vecteur contenant les flux de chargements de l'annee}
# ##' @return Un data.frame contenant :
# ##' \describe{
# ##' \item{\code{pri_brut} : } {le montant de primes brutes de chargements de l'annee}
# ##' \item{\code{pri_net} : }  {le montant de primes nettes de chargements de l'annee}
# ##' \item{\code{pri_chgt} : } {le montant de chargements de l'annee}
# ##' }
# ##' @author Prim'Act
# ##' @export
# ##' @aliases PortPassif
# ##'
# setGeneric(name = "calc_primes_pp", def = function(x, mt_ap, coef_inf){standardGeneric("calc_primes_pp")})
# #--------------------------------------------------------
#
# setMethod(
#   f = "calc_primes_pp",
#   signature = c(x = "PortPassif", mt_ap = "numeric", coef_inf = "numeric"),
#   def = function(x, mt_ap, coef_inf){
#
#   list_primes_det <- NULL
#   list_prime_agg <- NULL
#   prime_tot <- NULL
#   nbProd <- length(x["names_class_prod"])
#
#   for(i in 1: nbProd){
#
#     ncpi <- x["names_class_prod"][i]
#     nprdi <- names(x[ncpi])
#     longi <- length(x[ncpi])
#
#     for(j in 1:longi){
#
#       nprdij <- nprdi[j]
#       mpij <- x[ncpi][[nprdij]]
#       prime <- calc_primes(mpij)
#
#       # Calcul des frais sur primes
#       frais <- calc_frais(x["fp"], nprdij, "prime",mpij["mp"]["nb_contr"],prime["pri_brut"],coef_inf)
#       # Ajout des frais sur primes ? l'output primes
#       prime <-cbind(prime,frais)
#
#       list_primes_det[[i+j-1]] <- prime
#       list_prime_agg [[i+j-1]] <- colSums(prime)
#       if (i == 1 & j == 1) {
#         prime_tot <- list_prime_agg [[i+j-1]]
#       }else{
#           prime_tot <- prime_tot + list_prime_agg [[i+j-1]]
#           }
#       names(list_primes_det)[i+j-1] <- names(x[ncpi])[j]
#       names(list_prime_agg)[i+j-1] <- names(x[ncpi])[j]
#     }
#   }
#
#   prime_tot["pri_brut"] <- prime_tot["pri_brut"]+  mt_ap
#   prime_tot["pri_net"] <- prime_tot["pri_net"] + mt_ap
#
#   # output
#   return(list(list_primes_det = list_primes_det, list_prime_agg = list_prime_agg, prime_tot = prime_tot))
#
#   }
# )
