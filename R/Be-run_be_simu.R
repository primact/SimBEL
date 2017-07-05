#----------------------------------------------------------------------------------------------------------------------------------------------------
#           run_be_simu : metheode executant les operations de calcul d'un BE pour une simulation.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul d'un BE par une simulation.
##'
##' \code{run_be_simu} est une methode permettant de calculer un best estimate
##' pour une simulation donnee.
##' @name run_be_simu
##' @docType methods
##' @param x un objet de type \code{\link{Be}}.
##' @param i un entier (\code{integer}) correspondant au numero de la simulation.
##' @param pre_on  une valeur \code{logical} qui lorsqu'elle vaut \code{TRUE} prend en compte la variation
##' de PRE dans le resultat technique utilisee pour le calcul de la participation aux benefices reglementaires.
##' @details Pour une simulation donnee, cette methode projette un \code{\link{Canton}} jusqu'au terme, parametre dans
##' l'objet \code{x}.
##' @return resultats une liste dont le format est le suivant :
##' \describe{
##' \item{\code{nom_produit} : }{un vecteur contenant le liste des noms de produits..}
##' \item{\code{prime} : }{une matrice contenant les flux de primes par produit.}
##' \item{\code{prestation} : }{une matrice contenant les flux de prestations par produit.}
##' \item{\code{prestation_fdb} : }{une matrice contenant les flux de prestations discretionnaires par produit.}
##' \item{\code{frais} : }{une matrice contenant les flux de frais par produit.}
##' \item{\code{flux_be} : }{une matrice contenant les flux de best estimate par produit.}
##' \item{\code{prime_actu} : }{une matrice contenant la valeur des primes actualisees par produit.}
##' \item{\code{prestation_actu} : }{une matrice contenant la valeur des prestations actualisees par produit.}
##' \item{\code{prestation_fdb_actu} : }{une matrice contenant la valeur des prestations
##' discretionnaires actualisees par produit.}
##' \item{\code{frais_actu} : }{une matrice contenant la valeur des frais actualisees par produit.}
##' \item{\code{be} : }{une matrice contenant la valeur du best estimate par produit.}
##' }
##' @return canton un objet de type \code{\link{Canton}}.
##' @author Prim'Act
##' @seealso La methode de projection d'un \code{\link{Canton}} : \code{\link{proj_an}}.
##' L'extraction d'une simulation de l'\code{\link{ESG}} :\code{\link{extract_ESG}}.
##' La classe \code{\link{Be}}.
##' @export
##' @include Be_class.R
setGeneric(name = "run_be_simu", def = function(x, i, pre_on){standardGeneric("run_be_simu")})
setMethod(
    f = "run_be_simu",
    signature = c(x = "Be", i = "integer", pre_on = "logical"),
    definition = function(x, i, pre_on){

        # Controle
        if(i < 0L | i > x@esg@nb_simu)
            stop("[Be-run_be_simu] : le numero de simulation est incorrect. \n")


        # Initilisation des listes de stockage de resultats
        prime <- vector("list", x@param_be@nb_annee)
        prestation <- prime
        prestation_fdb <- prime
        frais <- prime
        deflateur <- NULL

        # Canton en date initial
        canton <- x@canton

        # Boucle sur les annees de projection
        for(an in 1L:x@param_be@nb_annee){

            # Extraction du model point ESG
            mp_esg <- extract_ESG(x@esg, i, an)
            deflateur <- c(deflateur, mp_esg@deflateur)
            canton@mp_esg <- mp_esg

            # Projection d'un canton sur une annee
            result_proj_an <- proj_an(canton, x@param_be@nb_annee, pre_on)

            # Gestion des simulation pour lesquel l'actif devient negatif
            # On retourne le numero de simulation
            if(is.logical(result_proj_an)){
                if(! result_proj_an){
                    return(list(erreur = i))
                }
            }

            # Mise a jour du canton
            canton <- result_proj_an[["canton"]]

            # Mise a jour des resultats
            output_be <- result_proj_an[["output_be"]]
            prime[[an]] <- output_be[["prime"]]
            prestation[[an]] <- output_be[["prestation"]]
            prestation_fdb[[an]] <- output_be[["prestation_fdb"]]
            frais[[an]] <- output_be[["frais"]]
        }

        # Mise a jour du booleen de calcul des probas
        if(x@canton@ptf_passif@calc_proba)
            x@canton@ptf_passif@calc_proba <- FALSE

        # Mise sous forme de matrice
        prime <- do.call("rbind", prime)
        prestation <- do.call("rbind", prestation)
        prestation_fdb <- do.call("rbind", prestation_fdb)
        frais <- do.call("rbind", frais)
        flux_be <- prestation + frais - prime

        # Suppression des noms parasites de colonnes de matrice
        colnames(prime) <- NULL
        colnames(prestation) <- NULL
        colnames(prestation_fdb) <- NULL
        colnames(frais) <- NULL
        colnames(flux_be) <- NULL

        # Actualisation
        prime_actu <- deflateur %*% prime
        prestation_actu <- deflateur %*% prestation
        prestation_fdb_actu <- deflateur %*% prestation_fdb
        frais_actu <- deflateur %*% frais
        be <- prestation_actu + frais_actu - prime_actu

        # Output
        return(list(resultats = list(nom_produit = result_proj_an[["nom_produit"]],
                                     prime = prime,
                                     prestation = prestation,
                                     prestation_fdb = prestation_fdb,
                                     frais = frais,
                                     flux_be = flux_be,
                                     prime_actu = prime_actu,
                                     prestation_actu = prestation_actu,
                                     prestation_fdb_actu = prestation_fdb_actu,
                                     frais_actu = frais_actu,
                                     be = be),
                    canton = canton))
    }
)

