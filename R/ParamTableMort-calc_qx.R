#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_qx : Methode de calcul des taux de deces.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de deces.
##'
##' \code{calc_qx} est une methode permettant de calculer le taux de deces.
##' @name calc_qx
##' @docType methods
##' @param table_mort un objet de la classe \code{\link{ParamTableMort}} contenant la table de mortalite.
##' @param age une valeur \code{integer} correspondant a l'age.
##' @param gen une valeur \code{integer} correspondant a la generation.
##' @return La valeur du taux de deces calcule.
##' @author Prim'Act
##' @include ParamTableMort-class.R
##' @export

setGeneric("calc_qx", function(table_mort, age, gen){standardGeneric("calc_qx")})
setMethod(
    f = "calc_qx",
    signature = c(table_mort = "ParamTableMort", age = "integer", gen = "integer"),
    def = function(table_mort, age, gen){

        # Ajout de test sur le format
        if(! all(age >= table_mort@age_min)) {
            stop("L'age doit etre superieur a l'age minimum de la table")
        }

        # Age applique
        age_app <- pmin(age, table_mort@age_max)

        # Generation applique
        gen_app <- pmax(pmin(gen, table_mort@gen_max), table_mort@gen_min)


        # Gestion des noms de colonnes du data.frame de donnnees
        nom_table <- names(table_mort@table)
        age_name  <- which(nom_table == "age")
        gen_name  <- which(nom_table == "gen")
        qx_name   <- which(nom_table == "qx")

        # Numero de ligne du qx (cle : gen-app)
        row_qx <- match(paste0(gen_app, age_app), paste0(.subset2(table_mort@table, gen_name), .subset2(table_mort@table, age_name)))

        # Calcul de la probabilite de survie
        q_x    <- .subset2(table_mort@table, qx_name)[row_qx]

        return(q_x)
    }
)
