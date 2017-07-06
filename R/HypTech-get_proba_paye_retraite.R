#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recupere la probabilite de payer un flux de rente pour un produit de retraite.
##'
##' \code{get_proba_paye_retraite} est une methode qui calcule un flux de rente pour un produit de retraite
##' en tenant compte de la possible reversion.
##' @name get_proba_paye_retraite
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table_1 un \code{character} designant le nom de la table de mortalite de la tete principale.
##' @param age_1 une valeur \code{integer} correspondant a l'age de la tete principale.
##' @param gen_1 une valeur \code{integer} correspondant a la generation de la tete principale.
##' @param statut l'indicatrice \code{integer} permettant de determiner le statut de la rente
##'  (1 : une tete, 2 : une tete principale et un reversataire, 3 : seul le reversataire est en vie).
##' @param tx_rvs une valur \code{numeric} correspondant au taux de reversion.
##' @param nom_table_2 un \code{character} designant le nom de la table de mortalite de la tete secondaire.
##' @param age_2 une valeur \code{integer} correspondant a l'age de la tete secondaire (renseigner la meme valeur que pour la tete principale en l'absence de tete secondaire).
##' @param gen_2 une valeur \code{integer} correspondant a la generation de la tete secondaire (renseigner la meme valeur que pour la tete principale en l'absence de tete secondaire).
##' @return La probabilite de payer un flux de rente, ponderee du taux de reversion si le staut est 2 ou 3.
##' @author Prim'Act
##' @seealso Recuperer le taux de deces \code{\link{get_qx_mort}}.
##' @export
##' @include HypTech-class.R
setGeneric("get_proba_paye_retraite", function(x, nom_table_1, age_1, gen_1, statut, tx_rvs, nom_table_2, age_2, gen_2){
    standardGeneric("get_proba_paye_retraite")})
setMethod(
    f = "get_proba_paye_retraite",
    signature = c(x = "HypTech",  nom_table_1 = "character", age_1 = "integer", gen_1 = "integer",
                  statut = "integer", tx_rvs = "numeric", nom_table_2 = "character", age_2 = "integer", gen_2 = "integer"),
    def = function(x, nom_table_1, age_1, gen_1, statut, tx_rvs, nom_table_2, age_2, gen_2){

        # Ajout d un test de presence du nom
        if (! (nom_table_1 %in% names(x@tables_mort) | nom_table_2 %in% names(x@tables_mort)))
            stop("[ParamTableComport : get_proba_paye_retraite] Nom de table de mortalite non trouve")


        # Calcul de la probabilite en fonction du statut
        if (statut == 1L)
            return(1 - get_qx_mort(x, nom_table_1, age_1, gen_1))
        else if (statut == 2L) {
            qx_1 <- get_qx_mort(x, nom_table_1, age_1, gen_1)
            return((1 - qx_1) + tx_rvs * qx_1 * (1 - get_qx_mort(x, nom_table_2, age_2, gen_2)))
        } else if(statut == 3L)
            return(tx_rvs * (1 - get_qx_mort(x, nom_table_2, age_2, gen_2)))
        else
            stop("[ParamTableComport : get_proba_paye_retraite] La variable statut peut prendre les valeurs 1 (pas de reversataire),
                                              2 (tete principale et reversataire vivants) ou 3 (seule la tete reversataire est vivante)")

    }

)
