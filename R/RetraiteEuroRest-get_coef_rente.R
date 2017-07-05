#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_coef_rente : Methode de calcul des ax
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recupere le coefficient d'ax ou d'axy
##'
##' \code{get_coef_rente} est une methode permettant d'obtenir le coefficient de survie esperee actualise au taux technique de rente.
##' @name get_coef_rente
##' @docType methods
##' @param x un objet de la classe \code{\link{RetraiteEuroRest}}.
##' @param ht un objet de la classe \code{\link{HypTech}}.
##' @return Le coefficient \code{numeric} d'ax ou axy selon le statut du model point dont les caracteristiques sont precisees en input.
##' @author Prim'Act
##' @export
##' @include RetraiteEuroRest_class.R
##'
setGeneric("get_coef_rente", function(x, ht) {standardGeneric("get_coef_rente")})
setMethod(
    f = "get_coef_rente",
    signature = c(x = "RetraiteEuroRest", ht = "HypTech"),
    def = function(x, ht){

        epi_mp <- x@mp

        # Nombre de lignes
        nb_mp <- nrow(epi_mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_epi <- names(epi_mp)

        # Recuperer numeros colonnes
        num_tab_mort     <- which(nom_epi == "num_tab_mort")
        num_tab_mort_rvs <- which(nom_epi == "num_tab_mort_rvs")
        num_age          <- which(nom_epi == "age")
        num_age_rvs      <- which(nom_epi == "age_rvs")
        num_gen          <- which(nom_epi == "gen")
        num_gen_rvs      <- which(nom_epi == "gen_rvs")
        num_statut       <- which(nom_epi == "statut_rvs")
        num_tx_rvs       <- which(nom_epi == "tx_rvs")
        num_tx_tech      <- which(nom_epi == "tx_tech")
        num_freq         <- which(nom_epi == "freq_rente")
        num_echu         <- which(nom_epi == "echu")

        # Extraction des donnees
        tab_mort     <- as.character(.subset2(epi_mp, num_tab_mort))
        tab_mort_rvs <- as.character(.subset2(epi_mp, num_tab_mort_rvs))
        age          <- .subset2(epi_mp, num_age)
        age_rvs      <- .subset2(epi_mp, num_age_rvs)
        gen          <- .subset2(epi_mp, num_gen)
        gen_rvs      <- .subset2(epi_mp, num_gen_rvs)
        statut       <- .subset2(epi_mp, num_statut)
        tx_rvs       <- .subset2(epi_mp, num_tx_rvs)
        tx_tech      <- .subset2(epi_mp, num_tx_tech)
        freq         <- .subset2(epi_mp, num_freq)
        echu         <- .subset2(epi_mp, num_echu)

        # Recuperation de la table de mortalite
        tables_mort <- ht@tables_mort
        nom_tab_mort <- names(tables_mort)

        # Fonction d'extraction des taux de sortie
        calc_coef_rente_mp <- function(i) {

            #Age du model point i
            tab_mort_i     <- tab_mort[i]
            tab_mort_rvs_i <- tab_mort_rvs[i]
            age_i          <- age[i]
            age_rvs_i      <- age_rvs[i]
            gen_i          <- gen[i]
            gen_rvs_i      <- gen_rvs[i]
            statut_i       <- statut[i]
            tx_rvs_i       <- tx_rvs[i]
            tx_tech_i      <- tx_tech[i]
            freq_i         <- freq[i]
            echu_i         <- echu[i]

            # Ajout d un test de presence du nom
            if (! (tab_mort_i %in% nom_tab_mort) | ! (tab_mort_rvs_i %in% nom_tab_mort))
                stop("[RetraiteEuroRest : get_coef_rente] : Nom de table de mortalite non trouve.")


            # Calcul du coefficient
            if (statut_i == 1L) # Cas avec une seule tete
                return(calc_ax(tx_tech_i, freq_i, echu_i, tables_mort[[tab_mort_i]], age_i, gen_i))
            else if (statut_i == 2L) # Cas avec deux tetes
                return(calc_axy(tx_tech_i, tx_rvs_i, freq_i, echu_i, tables_mort[[tab_mort_i]], age_i, gen_i, tables_mort[[tab_mort_rvs_i]], age_rvs_i, gen_rvs_i))
            else if (statut_i == 3L) # Cas du reversataire seul
                return(tx_rvs_i * calc_ax(tx_tech_i, freq_i, echu_i, tables_mort[[tab_mort_rvs_i]], age_rvs_i, gen_rvs_i))
            else
                stop("[RetraiteEuroRest : get_coef_rente] : Statut non reconnu.")
        }

        # Output
        return(t(sapply(1:nb_mp, calc_coef_rente_mp)))
    }
)
