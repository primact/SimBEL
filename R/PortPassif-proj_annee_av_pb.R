#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux et de pm d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les flux et les PM des produits modelises
##'
##' \code{proj_annee_av_pb} est une methode permettant de calculer les PM et les flux sur une annee avant PB.
##' Cette methode calcule egalement les frais sur flux et sur primes.
##' @name proj_annee_av_pb
##' @docType methods
##' @param an une valeur \code{integer} correspondant a l'annee de projection.
##' @param x un objet de la classe \code{\link{PortPassif}} contenant l'ensemble des produits de passifs.
##' @param tx_soc une valeur \code{numeric} correspondant au taux de charges sociales.
##' @param coef_inf une valeur \code{numeric} correspondant au coefficient d'inflation
##'  considere pour le traitement des frais.
##' @param list_rd un vecteur contenant les rendements de reference. Le format de cette liste est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @details L'annee de projection est utilisee pour gerer les produits dont les clauses dependent de l'annee.
##' Cette methode calcule deux fois les prestations et les PM pour permettre de calculer le montant de FDB.
##' @return \code{x} l'objet pour lequel les tableaux de resultats des objets \code{\link{EpEuroInd}} sont mis a jour.
##' @return \code{nom_produit} un vecteur de \code{character} contenant les noms des produits.
##' @return \code{flux_agg} une matrice contenant les flux aggreges par produits.
##' @return \code{stock_agg} une matrice contenant les stocks aggreges par produits.
##' @author Prim'Act
##' @seealso La classe \code{\link{EpEuroInd}} et ses methodes.
##' La classe \code{\link{FraisPassif}} et ses methodes.
##' @export
##' @include PortPassif-class.R
##'

setGeneric(name = "proj_annee_av_pb", def = function(an, x, tx_soc, coef_inf, list_rd) {standardGeneric("proj_annee_av_pb")})
setMethod(
    f = "proj_annee_av_pb",
    signature = c(an = "integer", x = "PortPassif", tx_soc = "numeric", coef_inf = "numeric", list_rd = "numeric"),
    def = function(an, x, tx_soc, coef_inf, list_rd){


        # Initialisation des listes de resultats
        list_res_stock_agg <- NULL # Liste des stocks aggregees par produit
        list_res_flux_agg <- NULL # Liste des flux aggregees par produit
        index <- NULL # Liste des noms de produits

        # Nombre de type de produits modelises
        nb_type <- length(x@names_class_prod)

        # Hypotheses techniques
        ht <- x@ht

        # Frais passifs
        fp <- x@fp

        # Calcul des probabilites
        calc_proba <- x@calc_proba

        # Boucle sur les types de produits
        k <- 0L # Compteur de boucle
        for(i in 1:nb_type) {

            # Liste des produits
            ncpi <- .subset2(x@names_class_prod, i) # Nom du type i
            list_prodi <- x[ncpi] # Liste de produits pour le type i
            longi <- length(list_prodi) # Nombre de produits
            noms_prodi <- names(list_prodi)

            # Boucle sur les produits de type i
            for(j in 1:longi){
                k <- k + 1L # Compteur pour les listes de stockage
                # Nom du produit
                nom_prod <- .subset2(noms_prodi, j)

                # Extraction du produit de la liste
                prodi <- .subset2(list_prodi, j)
                mp <- prodi@mp

                if ( class(prodi) == "EpEuroInd" ) {

                    # Donnees
                    num_mp <- which(names(mp) == "num_mp")

                    # Calcul des primes du produits par model points
                    prime <- calc_primes(prodi)

                    # Extraction de donnees (primes)
                    prime_flux  <- prime[["flux"]]
                    prime_stock <- prime[["stock"]]

                    # Calcul du taux minimum
                    tx_min <-  calc_tx_min(prodi, an)

                    # Calcul des probas lorsqu'elles n'ont pas encore ete calculees
                    if (calc_proba) {
                        # Calcul des probabilites portant sur les flux
                        proba_flux <- calc_proba_flux(x = prodi, ht = ht)

                        # Mise a jour du tableau de probas
                        prodi@tab_proba <- update_tab_proba(x = prodi@tab_proba, an = an, y = list(proba_flux = proba_flux))
                    }

                    # Calcul des probabilites dynamiques
                    proba_dyn <- calc_proba_dyn(prodi, ht = ht)

                    # Calcul des prestations du produits par model points
                    prest <- calc_prest(prodi, method = "normal", an = an, y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = tx_soc))
                    prest_gar <- calc_prest(prodi, method = "gar", an = an, y = list(proba_dyn = proba_dyn, tx_min = tx_min, tx_soc = tx_soc))

                    # Extraction de donnees (prest)
                    prest_flux  <- prest[["flux"]]
                    prest_stock <- prest[["stock"]]
                    prest_gar_flux <- prest_gar[["flux"]]

                    # Calcul des taux cible
                    tx_cible <- calc_tx_cible(prodi, list(ht = ht, list_rd = list_rd))

                    # Calcul des PM avant revalorisation par model points
                    pm <- calc_pm(prodi, method = "normal", an = an, tx_cible = tx_cible,
                                  list(tab_prime = prime_flux, tab_prest = prest_flux, tx_min = tx_min, tx_soc = tx_soc))
                    pm_gar <- calc_pm(prodi, method = "gar", an = an, tx_cible = tx_cible,
                                      list(tab_prime = prime_flux, tab_prest = prest_gar_flux, tx_min = tx_min, tx_soc = tx_soc))


                    # Extraction de donnees (PM)
                    pm_stock <- pm[["stock"]]
                    pm_flux <- pm[["flux"]]


                    # Mise a jour du tableau de sauvegarde
                    tab <- prodi@tab["tab"]
                    tab[["num_mp"]] <- .subset2(mp, num_mp)
                    tab[["pri_net"]] <- prime_flux[["pri_net"]]
                    tab[["prest"]] <- prest_flux[["prest"]]
                    tab[["pm_deb"]] <- pm_stock[["pm_deb"]]
                    tab[["pm_fin"]] <- pm_stock[["pm_fin"]]
                    tab[["enc_charg_base_th"]] <- pm_flux[["enc_charg_base_th"]]
                    tab[["enc_charg_rmin_th"]] <- pm_flux[["enc_charg_rmin_th"]]
                    tab[["rev_stock_brut"]] <- pm_flux[["rev_stock_brut"]]
                    tab[["bes_tx_cible"]] <- pm_flux[["bes_tx_cible"]]
                    tab[["nb_contr"]] <- prest_stock[["nb_contr_fin"]]
                    tab[["tx_cible"]] <- tx_cible[["tx_cible_an"]]
                    tab[["pm_gar"]] <- pm_gar[["stock"]][["pm_fin"]]

                    # Mise a jour de la liste de produits
                    list_prodi[[j]] <- prodi
                    list_prodi[[j]]@tab["tab"] <- tab



                } else if (class(prodi) == "RetraiteEuroRest") {

                    # Donnees
                    nom_mp <- names(mp)
                    num_mp <- which(nom_mp == "num_mp")
                    num_age <- which(nom_mp == "age")
                    num_age_rvs <- which(nom_mp == "age_rvs")

                    # Calcul des primes
                    prime <- calc_primes(prodi)

                    # Calcul des probas lorsqu'elles n'ont pas encore ete calculees
                    if (calc_proba) {
                        # Calcul des probabilites portant sur les flux
                        proba_flux <- calc_proba_flux(x = prodi, ht = ht)

                        # Mise a jour du tableau de probas
                        prodi@tab_proba <- update_tab_proba(x = prodi@tab_proba, an = an, y = list(proba_flux = proba_flux, coef_rente = NULL))
                    }

                    # Calcul des prestations
                    prest <- calc_prest(prodi, method = "normal", an = an)
                    prest_gar <- calc_prest(prodi, method = "gar", an = an)


                    # Extraction de donnees (prest)
                    prest_flux  <- prest[["flux"]]
                    prest_stock <- prest[["stock"]]
                    prest_gar_flux <- prest_gar[["flux"]]

                    # Calcul des taux cible
                    tx_cible <- calc_tx_cible(prodi, list(ht = ht, list_rd = unlist(list_rd)))

                    # viellissement de 1 an
                    prodi@mp$age      <- .subset2(mp, num_age) + 1L
                    prodi@mp$age_rvs  <- .subset2(mp, num_age_rvs) + 1L
                    prodi@mp$nb_contr <- prest_stock[["nb_contr_fin"]]

                    # Calcul des coefficients de rente et insertion des donnees dans la table
                    if (calc_proba) {
                        # Calcul des coefficients actuariels
                        coef_rente <- get_coef_rente(prodi, ht)

                        # Mise a jour du tableau de probas
                        prodi@tab_proba <- update_tab_proba(x = prodi@tab_proba, an = an, y = list(proba_flux = NULL, coef_rente = coef_rente))
                    }

                    # Calcul des PM avant revalorisation
                    pm <- calc_pm(prodi, method = "normal", an = an, tx_cible = tx_cible)
                    pm_gar <- calc_pm(prodi, method = "gar", an = an, tx_cible = tx_cible)


                    # Extraction de donnees
                    pm_stock <- pm[["stock"]]
                    pm_flux <- pm[["flux"]]
                    prime_flux  <- prime[["flux"]]
                    prime_stock  <- prime[["stock"]]

                    # Mise a jour du tableau de sauvegarde
                    tab <- prodi@tab["tab"]
                    tab[["num_mp"]] <- .subset2(mp, num_mp)
                    tab[["prest"]] <-  prest_flux[["prest"]]
                    tab[["pm_deb"]] <- pm_stock[["pm_deb"]]
                    tab[["pm_fin"]] <- pm_stock[["pm_fin"]]
                    tab[["bes_tx_cible"]] <- pm_flux[["bes_tx_cible"]]
                    tab[["nb_contr"]] <- prest_stock[["nb_contr_fin"]]
                    tab[["tx_cible"]] <- tx_cible[["tx_cible_an"]]
                    tab[["pm_gar"]] <- pm_gar[["stock"]][["pm_fin"]]

                    # Mise a jour de la liste de produits
                    list_prodi[[j]] <- prodi
                    list_prodi[[j]]@tab["tab"] <- tab

                }
                else
                    stop("[PortPassif : proj_annee_av_pb] : La liste list_prodi comporte au moins un element non instancie. \n")


                # Alimentation des listes de stock et de flux, puis et aggregation
                list_res_flux_agg_k <- lapply(c(prime_flux, prest_flux, pm_flux), sum)
                list_res_stock_agg_k <- lapply(c(prime_stock, prest_stock, pm_stock), sum)

                # Prestations fdb
                prest_fdb <- sum(prest_flux[["prest"]]) + sum(prest_flux[["rev_prest_nette"]]) -
                    (sum(prest_gar_flux[["prest"]]) + sum(prest_gar_flux[["rev_prest_nette"]]))

                # Ajout de la FDB
                list_res_flux_agg_k <- c(list_res_flux_agg_k, prest_fdb = prest_fdb)

                # Calcul des frais sur primes
                frais_primes <- calc_frais(fp, "prime", nom_prod, nb = list_res_stock_agg_k[["nb_vers"]],
                                           mt = list_res_flux_agg_k[["pri_brut"]], coef_inf)

                # Calcul des frais sur prestations
                frais_prest <- calc_frais(fp, "prest", nom_prod, nb = list_res_stock_agg_k[["nb_sortie"]],
                                          mt = list_res_flux_agg_k[["prest"]], coef_inf)

                # Calcul des frais sur encours
                frais_enc <- calc_frais(fp, "enc", nom_prod, nb = list_res_stock_agg_k[["nb_contr_moy"]],
                                        mt = list_res_stock_agg_k[["pm_moy"]], coef_inf)

                # Ajout des frais au flux
                list_res_flux_agg_k <- c(list_res_flux_agg_k, frais_primes, frais_prest, frais_enc)


                # Mise a jour des listes
                list_res_flux_agg[[k]]    <- list_res_flux_agg_k
                list_res_stock_agg[[k]] <- list_res_stock_agg_k


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
        colnames(flux_agg) <- names(.subset2(list_res_flux_agg, 1L))
        colnames(stock_agg) <- names(.subset2(list_res_stock_agg, 1L))


        # Stockage dans une liste generale en aggregeant par produit
        res <- list(x = x,
                    nom_produit = index,
                    flux_agg = flux_agg,
                    stock_agg = stock_agg
        )
        # output
        return(res)

    }
)



