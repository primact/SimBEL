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
        
        # Nombre d'annees de projection
        nb_annee <- x@param_be@nb_annee
        
        # Initilisation des listes de stockage de resultats
        prime <- vector("list", nb_annee)
        prestation <- prime
        prestation_fdb <- prime
        frais <- prime
        flux_produit <- prime
        stock_produit <- prime
        fin <- prime
        hors_model <- prime
        action <- prime
        immo <- prime
        oblig <- prime
        treso <- prime
        flux_fin <- prime
        output_pb <- prime
        deflateur <- NULL
        
        # Canton en date initial
        canton <- x@canton
        
        # Boucle sur les annees de projection
        for(an in 1L:nb_annee){
            
            # Extraction du model point ESG
            mp_esg <- extract_ESG(x@esg, i, an)
            deflateur <- c(deflateur, mp_esg@deflateur)
            canton@mp_esg <- mp_esg
            
            # Projection d'un canton sur une annee
            result_proj_an <- proj_an(canton, nb_annee, pre_on)
            
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
            
            # Recuperation des donnees a ecrire dans la base
            output_produit <- result_proj_an[["output_produit"]]
            flux_produit[[an]] <- output_produit[["flux_produit"]]
            stock_produit[[an]] <- output_produit[["stock_produit"]]
            hors_model[[an]] <- output_produit[["hors_model"]]
            fin[[an]] <- output_produit[["fin"]]
            
            output_pb[[an]] <- result_proj_an[["output_pb"]]
            
            # Recuperation des flux financier
            flux_fin[[an]] <- result_proj_an[["flux_fin"]]
            flux_ptf_fin <- result_proj_an[["flux_ptf_fin"]]
            action[[an]] <- flux_ptf_fin[["action"]]
            immo[[an]]   <- flux_ptf_fin[["immo"]]
            oblig[[an]]  <- flux_ptf_fin[["oblig"]]
            treso[[an]]  <- flux_ptf_fin[["treso"]]
        }
        
        # Mise a jour du booleen de calcul des probas
        if(x@canton@ptf_passif@calc_proba)
            x@canton@ptf_passif@calc_proba <- FALSE
        
        # Aggregation de donnees
        prime           <- do.call("rbind", prime)
        prestation      <- do.call("rbind", prestation)
        prestation_fdb  <- do.call("rbind", prestation_fdb)
        frais           <- do.call("rbind", frais)
        flux_produit    <- do.call("rbind", flux_produit)
        stock_produit   <- do.call("rbind", stock_produit)
        hors_model      <- do.call("rbind", hors_model)
        fin             <- do.call("rbind", fin)
        flux_fin        <- do.call("rbind", flux_fin)
        action          <- do.call("rbind", action)
        immo            <- do.call("rbind", immo)
        oblig           <- do.call("rbind", oblig)
        treso           <- do.call("rbind", treso)
        output_pb       <- do.call("rbind", output_pb)
        flux_be <- prestation + frais - prime
        
        # Suppression des noms parasites de colonnes de matrice
        colnames(prime) <- NULL
        colnames(prestation) <- NULL
        colnames(prestation_fdb) <- NULL
        colnames(frais) <- NULL
        colnames(flux_be) <- NULL
        
        # Noms des produits
        nom_produit <- result_proj_an[["nom_produit"]]
        
        # Actualisation
        prime_actu <- deflateur %*% prime
        prestation_actu <- deflateur %*% prestation
        prestation_fdb_actu <- deflateur %*% prestation_fdb
        frais_actu <- deflateur %*% frais
        be <- prestation_actu + frais_actu - prime_actu
        
        # Aggregation des bases
        table_output_be <- merge_table_be(prime, frais, prestation, prestation_fdb, i, nom_produit)
        table_output_produits <- merge_table_produit(flux_produit, stock_produit, hors_model, fin, nb_annee, i, nom_produit)
        table_be <- merge_be(be, i, nom_produit)
        table_actifs <- merge_actifs(action, immo, oblig, treso, i)
        table_flux_fin <- merge_flux_fin(flux_fin, i)
        table_output_pb <- merge_pb(output_pb, i)
        
        # Output
        return(list(resultats = list(nom_produit = nom_produit,
                                     prime = prime,
                                     prestation = prestation,
                                     prestation_fdb = prestation_fdb,
                                     frais = frais,
                                     flux_be = flux_be,
                                     prime_actu = prime_actu,
                                     prestation_actu = prestation_actu,
                                     prestation_fdb_actu = prestation_fdb_actu,
                                     frais_actu = frais_actu,
                                     be = be,
                                     tables = list(table_output_be = table_output_be,
                                                   table_output_produit_model = table_output_produits[["df_model"]],
                                                   table_output_produit_hors_model = table_output_produits[["df_hors_model"]],
                                                   table_output_pb = table_output_pb,
                                                   table_be = table_be,
                                                   table_actifs = table_actifs,
                                                   table_flux_fin = table_flux_fin)),
                    canton = canton))
    }
)

