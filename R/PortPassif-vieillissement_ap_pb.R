#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul de flux et de prestations la classe PortPassif apres PB
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Vieillissement du portefeuille sur l'annee apres attribution
##' de participation aux benefices.
##'
##' \code{vieillissment_ap_pb} est une methode permettant de calculer les PM et les flux sur une annee apres PB.
##' Cette methode vieilli le portefeuille de passifs apres attribution de PB.
##' @name vieillissment_ap_pb
##' @docType methods
##' @param x un objet de la classe \code{\link{PortPassif}} contenant l'ensemble des produits de passifs.
##' @param rev_nette_alloue un vecteur \code{numeric} contenant par produit
##'  le supplement de revalorisation par rapport au taux minimum.
##' @param tx_soc une valeur \code{numeric} correspondant au taux de charges sociales.
##' @return \code{x} l'objet \code{x} mis a jour.
##' @return \code{nom_produit} un vecteur de \code{character} contenant les noms des produits.
##' @return \code{flux_agg} une matrice contenant les flux aggreges par produits.
##' @return \code{stock_agg} une matrice contenant les stocks aggreges par produits.
##' @author Prim'Act
##' @seealso L'attribution de la revalorisation par model point : \code{\link{calc_revalo_pm}}
##' Le viellissement des model points : \code{\link{vieilli_mp}}.
##' @export
##' @include PortPassif-class.R
##'

setGeneric(name = "vieillissment_ap_pb", def = function(x, rev_nette_alloue, tx_soc)
{standardGeneric("vieillissment_ap_pb")})
setMethod(
  f = "vieillissment_ap_pb",
  signature = c(x = "PortPassif", rev_nette_alloue = "numeric",
                tx_soc = "numeric"),
  def = function(x, rev_nette_alloue, tx_soc){

    # Initialisation des listes de resultats
    list_res_stock_agg <- NULL # Liste des stocks aggregees par produit
    list_res_flux_agg <- NULL # Liste des flux aggregees par produit
    index <- NULL # Liste des noms de produits

    # Nombre de type de produits modelises
    nb_type <- length(x@names_class_prod)

    # Boucle sur les types de produits
    k <- 0 # Compteur de boucle
    for(i in 1:nb_type)
    {
      # Liste des produits
      ncpi <- x@names_class_prod[i] # Nom du type i
      list_prodi <- x[ncpi] # Liste de produits pour le type i
      longi <- length(list_prodi) # Nombre de produits
      noms_prodi <- names(list_prodi)

      # Boucle sur les produits de type i
      for(j in 1:longi)
      {
        k <- k + 1 # Compteur pour les listes de stockage
        # Nom du produit
        nom_prod <- noms_prodi[j]

        # Calcul de la revalorisation par produit
        revalo_prod <- calc_revalo_pm(list_prodi[[j]], rev_nette_alloue[k], tx_soc)

        # Vieilli les passifs et mise a jour
        list_prodi[[j]] <- vieilli_mp(list_prodi[[j]], revalo_prod[["stock"]][["pm_fin_ap_pb"]],
                                      revalo_prod[["tx_rev_net"]])
        # Alimentation des listes de stock et de flux, puis et aggregation
        list_res_flux_agg[[k]] <- lapply(revalo_prod[["flux"]], sum)
        list_res_stock_agg[[k]] <- lapply(revalo_prod[["stock"]], sum)

        # Nom des elements de la liste
        names(list_res_flux_agg)[k] <- nom_prod
        names(list_res_stock_agg)[k] <- nom_prod
        index <- c(index, nom_prod) # enregistrement des noms de produits
      }
      # Re-affectation des resultats a chaque objet
      x[ncpi] <- list_prodi
    }

    # Mise au format matrice des listes de flug_agg et stock_agg
    flux_agg = matrix(unlist(do.call("rbind",list_res_flux_agg)), length(index), byrow = F)
    stock_agg = matrix(unlist(do.call("rbind",list_res_stock_agg)), length(index), byrow = F)

    # Nom des colonnes matrices
    colnames(flux_agg) <- names(list_res_flux_agg[[1]])
    colnames(stock_agg) <- names(list_res_stock_agg[[1]])

    # Stockage dans une liste generale en aggregeant par produit
    res <- list(ptf = x,
                nom_produit = index,
                flux_agg = flux_agg,
                stock_agg = stock_agg
                )
    # output
    return(res)

  }
)


