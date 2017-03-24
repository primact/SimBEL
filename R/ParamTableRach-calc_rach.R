#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_rach : Methode de calcul des taux de rachat.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de rachat.
##'
##' \code{calc_rach} est une methode permettant de calculer le taux de rachat.
##' @name calc_rach
##' @docType methods
##' @param table_rach un objet de la classe \code{\link{ParamTableRach}} contenant la table de rachat.
##' @param age une valeur \code{numeric} correspondant a l'age.
##' @param anc une valeur \code{numeric} correspondant a l'anciennete.
##' @return La valeur du taux de rachat calcule.
##' @author Prim'Act
##' @aliases ParamTableRach
##' @include ParamTableRach-class.R
##' @export
setGeneric("calc_rach",
           function(table_rach, age, anc){standardGeneric("calc_rach")})
setMethod(
  f = "calc_rach",
  signature = c(table_rach = "ParamTableRach", age = "numeric", anc = "numeric"),
  def = function(table_rach, age, anc){
    # Ajout de test sur le format
    if(age < table_rach@age_min){
      stop("L'age doit etre superieur a l'age minimum de la table")
    }
    if(anc < table_rach@anc_min){
      stop("L'anciennete doit etre superieure a l'anciennete minimum de la table")
    }

    # Gestion des ages superieur a l age maximum de la table
    age_app <- min(age, table_rach@age_max)


    # Gestion des noms de colonnes du data.frame de donnnees
    nom_table <- names(table_rach@table)
    age_name <- which(nom_table == "age")
    anc_name <- which(nom_table == "anc")
    tx_name <- which(nom_table == "taux_rachat")

    # Test si l age est bien present dans la table
    if (!(age_app %in%  .subset2(table_rach@table, age_name))){
      stop("L'age doit etre present dans la table")
    }

    # Numero de ligne
    row_age <- which(.subset2(table_rach@table, age_name) == age)

    # On calcule l'anciennete max pour l'age demande
    anc_max <- max(.subset2(table_rach@table, anc_name)[row_age], 0)
    #on calcule l'anciennete appliquee
    anc_app <- min(anc, anc_max)

    # Test si l anciennete est bien presente dans la table
    if (!(anc_app %in% .subset2(table_rach@table, anc_name))){
      stop("L'anciennete doit etre presente dans la table")
    }

    # Numero de ligne
    row_x <- which(.subset2(table_rach@table, age_name) == age_app &
                     .subset2(table_rach@table, anc_name) == anc_app)

    # Extraction du taux de rachat
    res <- .subset2(table_rach@table, tx_name)[row_x]

    return(res)
  }
)

