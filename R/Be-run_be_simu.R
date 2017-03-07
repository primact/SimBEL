#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script execute les operations a effectuer les operations de calcul d'un BE pour une simulation.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           run_be_simu
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul d'un BE par simulation
##'
##' \code{run_be_simu} est une methode intermediare permettant de calcul un best estimate
##' pour une simulation donnee.
##' @name run_be_simu
##' @docType methods
##' @param x est un objet de type \code{Be}.
##' @param i est un entier (\code{integer}) correspondant au numero de la simulation.
##' @param pre_on est une valeur \code{logical} qui lorsqu'elle vaut \code{TRUE} prend en compte la variation
##' de PRE dans le resultat technique utilisee pour le calcul de la participation aux benefices reglementaires.
##' @return Une liste contenant les noms de produits calcules, les flux par produits et par annee pour une simulation sous
##' forme de matrice et les flux actualises par produits.
##' @author Prim'Act
##' @export
##' @aliases Be
##'
setGeneric(name = "run_be_simu", def = function(x, i, pre_on){standardGeneric("run_be_simu")})
setMethod(
  f = "run_be_simu",
  signature = c(x = "Be", i = "integer", pre_on = "logical"),
  definition = function(x, i, pre_on){

    # Controle
    if(i < 0 | i > x@esg@nb_simu){
      stop("[Be-run_be_simu] : le numero de simulation est incorrect. \n")
    }

    # Initilisation des listes de stockage de resultats
    prime <- vector("list", x@param_be@nb_annee)
    prestation <- prime
    prestation_fdb <- prime
    frais <- prime
    deflateur <- NULL

    # Canton en date initial
    canton <- x@canton

    # Boucle sur les annees de projection
    for(an in 1:x@param_be@nb_annee){

      # Extraction du model point ESG
      mp_esg <- extract_ESG(x@esg, i, an)
      deflateur <- c(deflateur, mp_esg@deflateur)
      canton@mp_esg <- mp_esg

      # Projection d'un canton sur une annee
      result_proj_an <- proj_an(canton, x@param_be@nb_annee, pre_on)

      # Mise a jour du canton
      canton <- result_proj_an[["canton"]]

      # Mise a jour des resultats
      output_be <- result_proj_an[["output_be"]]
      prime[[an]] <- output_be[["prime"]]
      prestation[[an]] <- output_be[["prestation"]]
      prestation_fdb[[an]] <- output_be[["prestation_fdb"]]
      frais[[an]] <- output_be[["frais"]]
      }

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
    return(list(nom_produit = result_proj_an[["nom_produit"]],
                prime = prime,
                prestation = prestation,
                prestation_fdb = prestation_fdb,
                frais = frais,
                flux_be = flux_be,
                prime_actu = prime_actu,
                prestation_actu = prestation_actu,
                prestation_fdb_actu = prestation_fdb_actu,
                frais_actu = frais_actu,
                be = be))
  }
)

