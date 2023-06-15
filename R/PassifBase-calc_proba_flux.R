#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des probas de mouvement de flux d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Calcul les probabilites de mouvement de flux pour des contrats epargne en euros et de retraite.
##'
##' \code{calc_proba_flux} est une methode permettant de calculer les differents taux de sortie
##'   sur une periode.
##' @name calc_proba_flux
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros.
##' @param ht un objet de la classe \code{\link{HypTech}} contenant differentes tables de mortalite et differentes lois de rachat.
##'
##' @return Pour un objet \code{\link{EpEuroInd}}, renvoie une liste contenant pour chaque model points en ligne :
##' \describe{
##' \item{\code{qx_rach_tot} : }{un vecteur contenant les taux de rachats totaux}
##' \item{\code{qx_dc} : }{un vecteur contenant les taux de deces}
##' \item{\code{qx_rach_part} : }{un vecteur contenant les taux de rachats partiels}
##' }
##' @return Pour un objet \code{\link{RetraiteEuroRest}}, renvoie une liste contenant pour chaque model points en ligne :
##' \describe{
##' \item{\code{proba_sortie_retraite} : }{un vecteur contenant les probabilites de deces}
##' \item{\code{proba_survie_un_an} : }{un vecteur contenantles probabilites de survie}
##' }
##' @author Prim'Act
##' @seealso La recuperation des taux de rachat structurel : \code{\link{get_qx_rach}}.
##' La recuperation des taux de rachat dynamique : \code{\link{get_rach_dyn}}.
##' La recuperation des taux de deces : \code{\link{get_qx_mort}}.
##' @export
##' @include EpEuroInd-class.R HypTech-class.R RetraiteEuroRest_class.R

#--------------------------------------------------------
setGeneric(name = "calc_proba_flux", def = function(x, ht) {
    standardGeneric("calc_proba_flux")
})
# Epargne y = list(ht)
# Retraite y = list(ht)
#--------------------------------------------------------

setMethod(
    f = "calc_proba_flux",
    signature = c(x = "EpEuroInd", ht = "HypTech"),
    def = function(x, ht) {
        # ModelPoint
        epi_mp <- x@mp

        # Nombre de lignes
        nb_mp <- nrow(epi_mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_epi <- names(epi_mp)
        age <- which(nom_epi == "age")
        anc <- which(nom_epi == "anc")
        gen <- which(nom_epi == "gen")
        num_tab_mort <- which(nom_epi == "num_tab_mort")
        num_rach_part <- which(nom_epi == "num_rach_part")
        num_rach_tot <- which(nom_epi == "num_rach_tot")


        # Donnees des MPs
        age <- .subset2(epi_mp, age)
        anc <- .subset2(epi_mp, anc)
        gen <- .subset2(epi_mp, gen)
        tab_mort <- .subset2(epi_mp, num_tab_mort)
        rach_tot <- .subset2(epi_mp, num_rach_tot)
        rach_part <- .subset2(epi_mp, num_rach_part)

        # Tables
        tab_mort_unique <- levels(tab_mort)
        rach_tot_unique <- levels(rach_tot)
        rach_part_unique <- levels(rach_part)

        # Initialisation des vecteurs
        qx_dc <- vector("numeric", length = nb_mp)
        qx_rach_tot <- vector("numeric", length = nb_mp)
        qx_rach_part <- vector("numeric", length = nb_mp)

        # Calcul des qx_rach_tot
        for (tab in tab_mort_unique) {
            row <- which(tab_mort == tab)
            qx_dc[row] <- get_qx_mort(ht, tab, age[row], gen[row])
        }

        # Calcul des qx_dc
        for (tab in rach_tot_unique) {
            row <- which(rach_tot == tab)
            qx_rach_tot[row] <- get_qx_rach(ht, tab, age[row], anc[row])
        }

        # Calcul des qx_dc
        for (tab in rach_part_unique) {
            row <- which(rach_part == tab)
            qx_rach_part[row] <- get_qx_rach(ht, tab, age[row], anc[row])
        }

        # Output
        return(list(
            qx_rach_tot = qx_rach_tot,
            qx_dc = qx_dc,
            qx_rach_part = qx_rach_part
        ))
    }
)

#-----------------------------------------------------------
setMethod(
    f = "calc_proba_flux",
    signature = c(x = "RetraiteEuroRest", ht = "HypTech"),
    def = function(x, ht) {
        # Model Point
        epi_mp <- x@mp

        # Nombre de lignes
        nb_mp <- nrow(x@mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_epi <- names(epi_mp)

        # Recuperer numeros colonnes
        num_tab_mort <- which(nom_epi == "num_tab_mort")
        num_tab_mort_rvs <- which(nom_epi == "num_tab_mort_rvs")
        num_age <- which(nom_epi == "age")
        num_age_rvs <- which(nom_epi == "age_rvs")
        num_gen <- which(nom_epi == "gen")
        num_gen_rvs <- which(nom_epi == "gen_rvs")
        num_statut <- which(nom_epi == "statut_rvs")
        num_tx_rvs <- which(nom_epi == "tx_rvs")

        # Extraction des donnees
        tab_mort <- .subset2(epi_mp, num_tab_mort)
        tab_mort_rvs <- .subset2(epi_mp, num_tab_mort_rvs)
        age <- .subset2(epi_mp, num_age)
        age_rvs <- .subset2(epi_mp, num_age_rvs)
        gen <- .subset2(epi_mp, num_gen)
        gen_rvs <- .subset2(epi_mp, num_gen_rvs)
        statut <- .subset2(epi_mp, num_statut)
        tx_rvs <- .subset2(epi_mp, num_tx_rvs)

        # Tables
        tab_mort_unique <- levels(tab_mort)
        tab_mort_rvs_unique <- levels(tab_mort_rvs)
        bool_tab <- vector(mode = "logical", length = nlevels(tab_mort_rvs))
        names(bool_tab) <- tab_mort_rvs_unique

        # Initialisation des vecteurs
        proba_sortie_retraite <- vector("numeric", length = nb_mp)
        proba_survie_un_an <- vector("numeric", length = nb_mp)


        # Calcul des probas
        for (tab in tab_mort_unique) {
            ## Statut = 1 (Pas de reversataire)
            # Ligne de donnees
            row <- which((tab_mort == tab) & (statut == 1L))

            # Calcul des probas
            qx_a <- get_qx_mort(ht, tab, age[row], gen[row])
            proba_sortie_retraite[row] <- qx_a
            proba_survie_un_an[row] <- 1 - qx_a


            for (tab_rvs in tab_mort_rvs_unique) {
                ## Statut vaut 2 (Assure + reversataire)
                # Lignes de donnees
                row <- which((tab_mort == tab) & (tab_mort_rvs == tab_rvs) & (statut == 2L))

                # Calcul des probas
                qx_a <- get_qx_mort(ht, tab, age[row], gen[row])
                qx_r <- get_qx_mort(ht, tab_rvs, age_rvs[row], gen_rvs[row])
                proba_sortie_retraite[row] <- qx_a * qx_r
                proba_survie_un_an[row] <- (1 - qx_a) + tx_rvs[row] * qx_a * (1 - qx_r)



                ## Statut = 3 (Que reversataire)
                if (!bool_tab[tab_rvs]) {
                    # Lignes de donnees
                    row <- which((tab_mort_rvs == tab_rvs) & (statut == 3L))

                    # Calcul des probas
                    qx_r <- get_qx_mort(ht, tab_rvs, age_rvs[row], gen_rvs[row])
                    proba_sortie_retraite[row] <- qx_r
                    proba_survie_un_an[row] <- tx_rvs[row] * (1 - qx_r)

                    # Modification du tableau de booleen
                    bool_tab[tab_rvs] <- TRUE
                }
            }
        }


        # Output
        return(list(
            proba_sortie_retraite = proba_sortie_retraite,
            proba_survie_un_an = proba_survie_un_an
        ))
    }
)
