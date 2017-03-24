#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de primes d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux de primes pour des contrats epargne en euros.
##'
##' \code{calc_primes} est une methode permettant de calculer les flux de primes sur
##'  une periode.
##' @name calc_primes
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @details Cette fonction permet de projeter uniquement des primes periodiques de contrats epargne en euros.
##' @return \code{stock} : une liste contenent le nombre de versements \code{nb_vers} associe a chaque model point.
##' @return \code{flux} : une liste contenant pour chaque model point les montants de primes brutes \code{pri_brut},
##' les montants de primes nettes \code{pri_net} et les chargemenets sur primes \code{pri_chgt}.
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd
##' @include EpEuroInd-class.R
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
