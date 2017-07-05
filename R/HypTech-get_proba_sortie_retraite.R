#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_proba_sortie_retraite : Methode de calcul de la probabilite de sortie de contrat
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recupere la probabilite de sortie d'un contrat retraite.
##'
##' \code{get_proba_sortie_retraite} est une methode permettant de calculer la probabilite de sortie
##' pour des contrats avec ou sans reversion.
##' @name get_proba_sortie_retraite
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table_1 un \code{character} designant le nom de la table de mortalite de la tete principale.
##' @param age_1 la valeur \code{integer} de l'age de la tete principale.
##' @param gen_1 la valeur \code{integer} de la generation de la tete principale.
##' @param statut l'indicatrice \code{integer} permettant de determiner le statut de la rente
##'  (1 : une tete, 2 : une tete principale et un reversataire, 3 : seul le reversataire est en vie).
##' @param nom_table_2 un \code{character} designant le nom de la table de mortalite de la tete secondaire.
##' @param age_2 la valeur \code{integer} de l'age de la tete secondaire.
##' @param gen_2 la valeur \code{integer} de la generation de la tete secondaire.
##' @return La probabilite de fin de contrat calculee.
##' @author Prim'Act
##' @seealso Le calcul des taux de deces \code{\link{calc_qx}}.
##' @export
##' @include HypTech-class.R
##'
setGeneric("get_proba_sortie_retraite", function(x, nom_table_1, age_1, gen_1, statut, nom_table_2, age_2, gen_2){standardGeneric("get_proba_sortie_retraite")})
setMethod(
    f = "get_proba_sortie_retraite",
    signature = c(x = "HypTech",  nom_table_1 = "character", age_1 = "integer", gen_1 = "integer", statut = "integer",
                  nom_table_2 = "character", age_2 = "integer", gen_2 = "integer"),
    def = function(x, nom_table_1, age_1, gen_1, statut,
                   nom_table_2, age_2, gen_2){

        # Ajout d un test de presence du nom
        if (! nom_table_1 %in% names(x@tables_mort) | ! nom_table_2 %in% names(x@tables_mort)) {
            stop("[HypTech : get_proba_sortie_retraite] Nom de table de mortalite non trouve")
        } else {
            if (statut == 1) { # Cas avec une seule tete
                return(calc_qx(x@tables_mort[[nom_table_1]], age_1, gen_1))
            } else if (statut == 2) { # Cas avec deux tetes
                return(calc_qx(x@tables_mort[[nom_table_1]], age_1, gen_1) * calc_qx(x@tables_mort[[nom_table_2]], age_2, gen_2))
            } else if(statut == 3) { # Cas du reversataire seul
                return(calc_qx(x@tables_mort[[nom_table_2]], age_2, gen_2))
            } else {
                stop("[HypTech : get_proba_sortie_retraite] La variable 'statut' peut ne peut prendre que les valeurs 1 (pas de reversataire), 2 (tete principale et reversataire vivants) ou 3 (seule la tete reversataire est vivante)")
            }
        }
    }
)
