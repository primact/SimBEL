#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_qx : Methode de calcul des taux de deces.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le taux de deces.
##'
##' \code{calc_qx} est une methode permettant de calculer le taux de deces.
##' @name calc_qx
##' @docType methods
##' @param table_mort un objet de la classe \code{\link{ParamTableMort}} contenant la table de mortalite.
##' @param age une valeur \code{numeric} correspondant a l'age.
##' @param gen une valeur \code{numeric} correspondant a la generation.
##' @return La valeur du taux de deces calcule.
##' @author Prim'Act
##' @include ParamTableMort-class.R
##' @export

setGeneric("calc_qx", function(table_mort, age, gen){standardGeneric("calc_qx")})
setMethod(
  f = "calc_qx",
  signature = c(table_mort = "ParamTableMort", age = "numeric", gen = "numeric"),
  def = function(table_mort, age, gen){

    # Ajout de test sur le format
    if(age < table_mort@age_min){
      stop("L'age doit etre superieur a l'age minimum de la table")
    }
    # Age applique
    age_app <- min(age, table_mort@age_max - 1)

    # Generation applique
    gen_app <- max(min(gen, table_mort@gen_max), table_mort@gen_min)


    # Gestion des noms de colonnes du data.frame de donnnees
    nom_table <- names(table_mort@table)
    age_name <- which(nom_table == "age")
    gen_name <- which(nom_table == "gen")

    # Numero de ligne pour les Lx et Lx+1
    row_x <- which(.subset2(table_mort@table, gen_name) == gen_app &
                     .subset2(table_mort@table, age_name) == age_app)

    row_xplusun <- which(.subset2(table_mort@table, gen_name) == gen_app &
                     .subset2(table_mort@table, age_name) == (age_app + 1))

    #Calcul des probabilites de deces
    l_x <- .subset2(table_mort@table, 3)[row_x]
    l_xplusun <- .subset2(table_mort@table, 3)[row_xplusun]

    if(l_x == 0){
      return(0)
    }else{
      return((l_x - l_xplusun) / l_x)
    }
  }
)








