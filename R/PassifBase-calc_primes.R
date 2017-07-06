#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de primes d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les flux de primes pour des contrats epargne en euros et des retraites en phase de restitution.
##'
##' \code{calc_primes} est une methode permettant de calculer les flux de primes sur
##'  une periode.
##' @name calc_primes
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}}
##'  contenant les model points epargne euros ou de retraite en phase de restitution.
##' @details Cette fonction permet de projeter uniquement des primes periodiques de contrats epargne en euros. Pour
##' la retraite en phase de restitution, il n'y a pas de prime et la methode renvoie des valeurs nulles.
##' @return \code{stock} : une liste contenent le nombre de versements \code{nb_vers} associe a chaque model point.
##' @return \code{flux} : une liste contenant pour chaque model point les montants de primes brutes \code{pri_brut},
##' les montants de primes nettes \code{pri_net} et les chargements sur primes \code{pri_chgt}.
##' @author Prim'Act
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R
##'

#--------------------------------------------------------
setGeneric(name = "calc_primes", def = function(x){standardGeneric("calc_primes")})

#--------------------------------------------------------
setMethod(
    f = "calc_primes",
    signature = c(x = "EpEuroInd"),
    def = function(x){

        # Tableau des ModelPoint
        mp      <- x@mp
        nom_mp  <- names(mp)

        # Extraction des donnees
        prime       <- .subset2(mp, which(nom_mp == "prime"))
        nb_contr    <- .subset2(mp, which(nom_mp == "nb_contr"))
        chgt_prime  <- .subset2(mp, which(nom_mp == "chgt_prime"))

        # Nombre de versements
        nb_vers <- nb_contr * (prime > 0)

        # Calcul les primes de l'annee
        pri_brut <- prime * nb_contr # primes brutes
        pri_net <- pri_brut * (1 - chgt_prime) # primes nettes
        pri_chgt <- pri_brut * chgt_prime # Chargements sur primes

        # output
        return(list(stock = list(nb_vers = nb_vers),
                    flux = list(
                        pri_brut = pri_brut,
                        pri_net = pri_net,
                        pri_chgt = pri_chgt)))
    }
)


#--------------------------------------------------------
setMethod(
    f = "calc_primes",
    signature = c(x = "RetraiteEuroRest"),
    def = function(x){

        # Tableau des ModelPoint
        mp      <- x@mp
        nom_mp  <- names(mp)

        # Nombre de versements
        nb_vers <- .subset2(mp, which(nom_mp == "nb_contr"))

        # Output zero
        out_zero <- rep(0, nrow(mp))

        # output
        return(list(stock = list(nb_vers = nb_vers),
                    flux = list(
                        pri_brut = out_zero,
                        pri_net  = out_zero,
                        pri_chgt = out_zero)))
    }
)
