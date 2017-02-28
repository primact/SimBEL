#----------------------------------------------------------
# Ce script comprend les methodes de flux et de prestations la classe PortPassif avant PB
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux et de pm d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux et les PM des produits modelises
##'
##' \code{proj_annee_av_pb} est une methode permettant de calculer les PM et les flux sur une annee avant PB.
##' Cette me
##' thode calcule egalement les frais sur flux et PM.
##' @name proj_annee_av_pb
##' @docType methods
##' @param an est l'annee de projection.
##' @param x un objet de la classe \code{PortPassif} contenant l'ensemble des produits de passifs.
##' @param tx_soc le taux de charges sociales.
##' @param coef_inf est le coefficient d'inflation considere pour le traitement des frais.
##' @param list_rd est une liste contenant les rendements des actifs de references.
##' @details L'annee de projection est utilisee pour gerer les produits dont les clauses dependent de l'annee
##' @return Une liste dont le premier element designe les noms des produits, puis deux matrices de resultats aggreges : une pour les flux
##' et une pour le stock. Retourne egalement le portefeuille avec une mise a jour des tables de resultats intermediaires.
##' @author Prim'Act
##' @export
##' @aliases PortPassif
##'

setGeneric(name = "proj_annee_av_pb", def = function(an, x, tx_soc, coef_inf, list_rd)
{standardGeneric("proj_annee_av_pb")})
setMethod(
  f = "proj_annee_av_pb",
  signature = c(an = "numeric", x = "PortPassif",
                tx_soc = "numeric", coef_inf = "numeric", list_rd = "list"),
  def = function(an, x,
                 tx_soc, coef_inf, list_rd){


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
      for(j in 1:longi){
        k <- k + 1 # Compteur pour les listes de stockage
        # Nom du produit
        nom_prod <- noms_prodi[j]

        # Calcul des primes du produits par model points
        prime <- calc_primes(list_prodi[[j]])

        # Calcul du taux minimum
        tx_min <-  calc_tx_min(list_prodi[[j]], an)

        # Calcul des taux de sortie
        tx_sortie <- calc_tx_sortie(list_prodi[[j]], x@ht)

        # Calcul des prestations du produits par model points
        prest <- calc_prest(list_prodi[[j]], tx_sortie, tx_min, an, method = "normal", tx_soc)
        prest_gar <- calc_prest(list_prodi[[j]], tx_sortie, tx_min, an, method = "gar", tx_soc)

        # Calcul des taux cible
        tx_cible <- calc_tx_cible(list_prodi[[j]], x@ht, list_rd)

        # Calcul des PM avant revalorisation par model points
        pm <- calc_pm(list_prodi[[j]],prime[["flux"]], prest[["flux"]], tx_cible, tx_min, an, method = "normal", tx_soc)
        pm_gar <- calc_pm(list_prodi[[j]],prime[["flux"]], prest_gar[["flux"]], tx_cible, tx_min, an, method = "gar", tx_soc)

        # Mise a jour du tableau de sauvegarde
        list_prodi[[j]]@tab["tab"][["num_mp"]] <- list_prodi[[j]]@mp$num_mp
        list_prodi[[j]]@tab["tab"][["pri_net"]] <- prime[["flux"]][["pri_net"]]
        list_prodi[[j]]@tab["tab"][["prest"]] <- prest[["flux"]][["prest"]]
        list_prodi[[j]]@tab["tab"][["pm_deb"]] <- pm[["stock"]][["pm_deb"]]
        list_prodi[[j]]@tab["tab"][["pm_fin"]] <- pm[["stock"]][["pm_fin"]]
        list_prodi[[j]]@tab["tab"][["enc_charg_base_th"]] <- pm[["flux"]][["enc_charg_base_th"]]
        list_prodi[[j]]@tab["tab"][["enc_charg_rmin_th"]] <- pm[["flux"]][["enc_charg_rmin_th"]]
        list_prodi[[j]]@tab["tab"][["rev_stock_brut"]] <- pm[["flux"]][["rev_stock_brut"]]
        list_prodi[[j]]@tab["tab"][["bes_tx_cible"]] <- pm[["flux"]][["bes_tx_cible"]]
        list_prodi[[j]]@tab["tab"][["nb_contr"]] <- prest[["stock"]][["nb_contr_fin"]]
        list_prodi[[j]]@tab["tab"][["tx_cible"]] <- tx_cible[["tx_cible_an"]]
        list_prodi[[j]]@tab["tab"][["pm_gar"]] <- pm_gar[["stock"]][["pm_fin"]]

        # Alimentation des listes de stock et de flux, puis et aggregation
        list_res_flux_agg[[k]] <- lapply(c(prime[["flux"]], prest[["flux"]], pm[["flux"]]), sum)
        list_res_stock_agg[[k]] <- lapply(c(prime[["stock"]], prest[["stock"]], pm[["stock"]]), sum)

        # Prestations fdb
        prest_fdb <- sum(prest[["flux"]][["prest"]]) + sum(prest[["flux"]][["rev_prest_nette"]]) -
          (sum(prest_gar[["flux"]][["prest"]]) + sum(prest_gar[["flux"]][["rev_prest_nette"]]))

        # Ajout de la FDB
        list_res_flux_agg[[k]] <- c(list_res_flux_agg[[k]], prest_fdb = prest_fdb)


        # Calcul des frais sur primes
        frais_primes <- calc_frais(x@fp, "prime", nom_prod, list_res_stock_agg[[k]][["nb_vers"]],
                                      list_res_flux_agg[[k]][["pri_brut"]], coef_inf)

        # Calcul des frais sur prestations
        frais_prest <- calc_frais(x@fp, "prest", nom_prod, list_res_stock_agg[[k]][["nb_sortie"]],
                                    list_res_flux_agg[[k]][["prest"]], coef_inf)

        # Calcul des frais sur encours
        frais_enc <- calc_frais(x@fp, "enc", nom_prod, list_res_stock_agg[[k]][["nb_contr_moy"]],
                                   list_res_stock_agg[[k]][["pm_moy"]], coef_inf)

        # Ajout des frais au flux
        list_res_flux_agg[[k]] <- c(list_res_flux_agg[[k]], frais_primes, frais_prest, frais_enc)


        # Nom des elements de la liste
        names(list_res_flux_agg)[k] <- nom_prod
        names(list_res_stock_agg)[k] <- nom_prod
        index <- c(index, nom_prod) # enregistrement des noms de produits
      }
      # Re-affectation des resultats a chaque objet
      x[ncpi] <- list_prodi
    }

    # Stockage dans une liste generale en aggregeant par produit
    res <- list(x = x,
                nom_produit = index,
                flux_agg = do.call("rbind",list_res_flux_agg),
                stock_agg = do.call("rbind",list_res_stock_agg)
                )
    # output
    return(res)

  }
)



