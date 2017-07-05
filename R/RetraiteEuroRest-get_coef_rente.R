#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_coef_rente : Methode de calcul des ax
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recupere le coefficient d'ax ou d'axy
##'
##' \code{get_coef_rente} est une methode permettant d'obtenir le coefficient de survie esperee actualise au taux technique de rente.
##' @name get_coef_rente
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table_1 un \code{character} designant le nom de la table de mortalite de la tete principale.
##' @param age_1 la valeur \code{integer} de l'age de la tete principale.
##' @param gen_1 la valeur \code{integer} de la generation de la tete principale.
##' @param statut l'indicatrice \code{integer} permettant de determiner le statut de la rente
##'  (1 : une tete, 2 : une tete principale et un reversataire, 3 : seul le reversataire est en vie).
##' @param tx_rvs un \code{numeric} correspondant au taux de reversion de la rente. 
##' @param tx_tech un \code{numeric} correspondant au taux technique d'actualisation du coefficient actuariel.
##' @param nom_table_2 un \code{character} designant le nom de la table de mortalite de la tete secondaire.
##' @param age_2 la valeur \code{integer} de l'age de la tete secondaire.
##' @param gen_2 la valeur \code{integer} de la generation de la tete secondaire.
##' @param freq_rente un \code{integer} correspondant au nombre de versements dans l'annee.
##' @param echu un \code{logical} correspond a la methode de calcul du coefficient actuariel.
##' @return Le coefficient \code{numeric} d'ax ou axy selon le statut du model point dont les caractéristiques sont precisees en input.
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
        nb_mp <- nrow(x@mp)
        
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
        
        
        # Fonction d'extraction des taux de sortie
        calc_coef_rente_mp <- function(i) {
            
            #Age du model point i
            tab_mort_i     <- as.character(.subset2(epi_mp, num_tab_mort)[i])
            tab_mort_rvs_i <- as.character(.subset2(epi_mp, num_tab_mort_rvs)[i])
            age_i          <- .subset2(epi_mp, num_age)[i]
            age_rvs_i      <- .subset2(epi_mp, num_age_rvs)[i]
            gen_i          <- .subset2(epi_mp, num_gen)[i]
            gen_rvs_i      <- .subset2(epi_mp, num_gen_rvs)[i]
            statut_i       <- .subset2(epi_mp, num_statut)[i]
            tx_rvs_i       <- .subset2(epi_mp, num_tx_rvs)[i]
            tx_tech_i      <- .subset2(epi_mp, num_tx_tech)[i]
            freq_i         <- .subset2(epi_mp, num_freq)[i]
            echu_i         <- .subset2(epi_mp, num_echu)[i]
            
            # Ajout d un test de presence du nom
            if (! (tab_mort_i %in% names(ht@tables_mort)) | ! (tab_mort_rvs_i %in% names(ht@tables_mort))) {
                stop("[RetraiteEuroRest : get_coef_rente] Nom de table de mortalite non trouve")
            } else {
                if (statut_i == 1) { # Cas avec une seule tete
                    return(calc_ax(tx_tech_i, freq_i, echu_i, ht@tables_mort[[tab_mort_i]], age_i, gen_i))
                } else if (statut_i == 2) { # Cas avec deux tetes
                    return(calc_axy(tx_tech_i, tx_rvs_i, freq_i, echu_i, ht@tables_mort[[tab_mort_i]], age_i, gen_1, ht@tables_mort[[tab_mort_rvs_i]], age_rvs_i, gen_rvs_i))
                } else if (statut_i == 3) { # Cas du reversataire seul
                    return(tx_rvs_i * calc_ax(tx_tech_i, freq_i, echu_i, ht@tables_mort[[tab_mort_rvs_i]], age_rvs_i, gen_rvs_i))
                } else{
                    stop("[RetraiteEuroRest : get_coef_rente] Statut non reconnu")
                }
            }
        }
        
        
        return(t(sapply(1:nb_mp, calc_coef_rente_mp)))
    }
)
