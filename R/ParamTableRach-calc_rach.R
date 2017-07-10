#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_rach : Methode de calcul des taux de rachat.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de rachat.
##'
##' \code{calc_rach} est une methode permettant de calculer le taux de rachat.
##' @name calc_rach
##' @docType methods
##' @param table_rach un objet de la classe \code{\link{ParamTableRach}} contenant la table de rachat.
##' @param age une valeur \code{integer} correspondant a l'age.
##' @param anc une valeur \code{integer} correspondant a l'anciennete.
##' @return La valeur du taux de rachat calcule.
##' @author Prim'Act
##' @include ParamTableRach-class.R
##' @export
setGeneric("calc_rach", function(table_rach, age, anc){standardGeneric("calc_rach")})
setMethod(
    f = "calc_rach",
    signature = c(table_rach = "ParamTableRach", age = "integer", anc = "integer"),
    def = function(table_rach, age, anc){

        # Ajout de test sur le format
        if(! all(age >= table_rach@age_min)) stop("L'age doit etre superieur a l'age minimum de la table")
        if(! all(anc >= table_rach@anc_min)) stop("L'anciennete doit etre superieure a l'anciennete minimum de la table")

        # Gestion des ages superieur a l age maximum de la table
        age_app <- pmin(age, table_rach@age_max)

        # Generation applique
        anc_app <- pmax(pmin(anc, table_rach@anc_max), table_rach@anc_min)

        # Gestion des noms de colonnes du data.frame de donnnees
        table <- table_rach@table
        nom_table <- names(table)
        age_name  <- which(nom_table == "age")
        anc_name  <- which(nom_table == "anc")
        tx_name   <- which(nom_table == "taux_rachat")
        age_tab <- .subset2(table, age_name)
        anc_tab <- .subset2(table, anc_name)


        age_uniq <- unique(age_tab)
        anc_uniq <- unique(anc_tab)

        # Test si l age est bien present dans la table
        if (! all(age_app %in% age_tab)) stop("L'age doit etre present dans la table")


        # Numero de ligne
        row_x <- match(paste0(anc_app, age_app), paste0(anc_tab, age_tab))

        # Extraction du taux de rachat
        res <- .subset2(table, tx_name)[row_x]

        return(res)
    }
)

