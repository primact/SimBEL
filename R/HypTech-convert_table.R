#----------------------------------------------------------------------------------------------------------------------------------------------------
# convert_table : Tranforme les tables de mortalite en qx
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Tranforme une table contenant des "l_x" en une table contenant des "q_x"
##'
##' \code{convert_table} est une methode permettant de calculer le vecteur des probabilites cumulees de survie.
##' @name convert_table
##' @docType methods
##' @param table_mort un objet de la classe \code{data.frame} contenant la table de mortalite.
##' @param type un objet de la classe \code{character} correspondant au type de la table de mortalite passee en parametre (qx ou lx).
##' @return La table transformee.
##' @author Prim'Act
##' @export

setGeneric("convert_table", function(table_mort, type) {
    standardGeneric("convert_table")
})
setMethod(
    f = "convert_table",
    signature = c(table_mort = "data.frame", type = "character"),
    def = function(table_mort, type) {
        # Ajout de test sur le format
        if (!type %in% c("qx", "lx")) stop("[HypTech : convert_table] Le type doit etre une chaine de caractere correspondant a qx ou lx.")

        # Initialisation de la table
        table_fin <- table_mort

        # Ajout d'une colonne en fonction de type de donnees deja present
        if (type == "lx") { # Cas pour les tables contenant des lx

            # Gestion des noms de colonnes du data.frame
            nom_table <- names(table_fin)
            age_num <- which(nom_table == "age")
            gen_num <- which(nom_table == "gen")
            lx_num <- which(nom_table %in% c("lx", "valeur"))

            # Liste des generations
            generation <- unique(.subset2(table_fin, gen_num))

            # Initialisation des qx
            table_fin$qx <- 1

            # Boucle sur chaque generations
            for (gen in generation) {
                # Lignes data pourlesquelles les lx sont non nuls
                row_data_diff0 <- which((.subset2(table_fin, gen_num) == gen) & (.subset2(table_fin, lx_num) != 0))

                # Nombre de lignes ou lx != 0
                nrow <- length(row_data_diff0)

                # Extraction des donnees
                lxplus1 <- table_fin[row_data_diff0[2L:(nrow)], 3L]
                lx <- table_fin[row_data_diff0[1L:(nrow - 1L)], 3L]

                # Calcul des qx
                qx <- 1 - (lxplus1 / lx)

                # Insertion des donnees dans la table
                table_fin[row_data_diff0[1L:(nrow - 1L)], "qx"] <- qx
            }

            # Renommage des colonnes
            colnames(table_fin) <- c("gen", "age", "lx", "qx")
        } else if (type == "qx") { # Cas pour les tables contenant des qx

            # Gestion des noms de colonnes du data.frame
            nom_table <- names(table_fin)
            age_num <- which(nom_table == "age")
            gen_num <- which(nom_table == "gen")
            qx_num <- which(nom_table %in% c("qx", "valeur"))

            # Liste des generations
            generation <- unique(.subset2(table_fin, gen_num))

            # Initialisation des qx
            table_fin$lx <- 0

            # Boucle sur chaque generations
            for (gen in generation) {
                # Lignes data pour une generation
                row_data <- which(.subset2(table_fin, gen_num) == gen)
                qx <- .subset2(table_fin, qx_num)[row_data]

                # Calcul des px
                px <- 1 - qx

                # Calcul de la longueur des donnees
                l <- length(qx)

                # Calcul du produit des px
                prod_px <- sapply(1L:(l - 1L), function(i) {
                    return(prod(px[1L:i]))
                })

                # Calcul des Lx
                lx <- 100000 * c(1, prod_px)

                # Insertion des donnees dans la table
                table_fin[row_data, "lx"] <- lx
            }

            # Renommage des colonnes
            colnames(table_fin) <- c("gen", "age", "qx", "lx")
        }

        # Output
        return(table_fin)
    }
)
