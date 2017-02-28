#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 22/01/2017. Fait par QG : initialisation
#----------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de primes d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux de primes d'un model point epargne en euros.
##'
##' \code{calc_primes} est une methode permettant de calculer les flux de primes sur
##'  une annee.
##' @name calc_primes
##' @docType methods
##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros
##' @return Un data.frame contenant :
##' \describe{
##' \item{\code{pri_brut} : }{un vecteur contenant les flux de primes brutes de chargements de l'annee}
##' \item{\code{pri_net} : }{un vecteur contenant les flux de primes nettes de chargements de l'annee}
##' \item{\code{pri_chgt} : }{un vecteur contenant les flux de chargements de l'annee}
##' }
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd
##'
setGeneric(name = "calc_primes", def = function(x){standardGeneric("calc_primes")})
#--------------------------------------------------------

setMethod(
  f = "calc_primes",
  signature = c(x = "EpEuroInd"),
  def = function(x){

    # Nombre de versements
    nb_vers <- x@mp$nb_contr * (x@mp$prime > 0)

    # Calcul les primes de l'annee
    pri_brut <- x@mp$prime * x@mp$nb_contr # primes brutes
    pri_net <- pri_brut * (1 - x@mp$chgt_prime) # primes nettes
    pri_chgt <- pri_brut * x@mp$chgt_prime # Chargements sur primes

    # output
    return(list(stock = list(nb_vers = nb_vers),
                flux = list(
                  pri_brut = pri_brut,
                  pri_net = pri_net,
                  pri_chgt = pri_chgt
                  )
                )
           )
  }
)
