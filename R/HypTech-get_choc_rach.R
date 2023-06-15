#----------------------------------------------------------------------------------------------------------------------------------------------------
# Methode pour l'application des chocs de rachat
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique les chocs de rachat de la formule standard.
##'
##' \code{get_choc_rach} est une methode permettant d'appliquer a l'ensemble des lois de rachat
##' structurelle d'un objet \code{\link{HypTech}} les chocs a la hausse ou a la baisse de la formule standard.
##' @name get_choc_rach
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}} contenant differentes
##' lois de rachat partielles et totales.
##' @param type_choc_rach est un character renseignant le type de choc a applique : \code{up}
##' pour le choc a la hausse, et \code{down} pour le choc a la baisse.
##' @param choc une valeur \code{numeric} indiquant le taux de choc.
##' @param choc_lim une valeur \code{numeric} indiquant la limite haute pour le choc a la hausse,
##'  ou une limite basse pour le choc a baisse.
##' @return L'objet \code{x} apres choc.
##' @export
##' @include HypTech-class.R

setGeneric("get_choc_rach", function(x, type_choc_rach, choc, choc_lim) {
    standardGeneric("get_choc_rach")
})
setMethod(
    f = "get_choc_rach",
    signature = c(x = "HypTech", type_choc_rach = "character", choc = "numeric", choc_lim = "numeric"),
    def = function(x, type_choc_rach, choc, choc_lim) {
        # Recuperation de donnees
        tables <- x@tables_rach
        nom_tables <- names(tables)
        nb_tables <- length(tables)

        # Passage de chacune des tables
        for (i in 1:nb_tables) {
            # Extraction de donnees
            param_rach <- tables[[nom_tables[i]]]
            df_rach <- param_rach@table
            ancmin <- param_rach@anc_min
            ancmax <- param_rach@anc_max
            agemin <- param_rach@age_min
            agemax <- param_rach@age_max

            # Passage de chacune des lignes
            for (anc in ancmin:ancmax) {
                for (age in agemin:agemax) {
                    # Calcul du nouveau qx
                    if (type_choc_rach == "up") qx_choc <- min((calc_rach(param_rach, age, anc) * (1 + choc)), choc_lim)
                    if (type_choc_rach == "down") {
                        qx_choc <- max(
                            calc_rach(param_rach, age, anc) * (1 + choc),
                            calc_rach(param_rach, age, anc) + choc_lim
                        )
                    }

                    # Mise a jour du qx
                    df_rach[df_rach$anc == anc & df_rach$age == age, "taux_rachat"] <- qx_choc
                }
            }

            # Mise a jour de la table
            x@tables_rach[[nom_tables[i]]]@table <- df_rach
        }

        return(x)
    }
)
