#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_proba_survie : Methode de calcul du vecteur des probabilites de survies.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le vecteur des probabilites cumulees de survies.
##'
##' \code{calc_proba_survie} est une methode permettant de calculer le vecteur des probabilites cumulees de survie.
##' @name calc_proba_survie
##' @docType methods
##' @param table_mort un objet de la classe \code{\link{ParamTableMort}} contenant la table de mortalite.
##' @param age une valeur \code{integer} correspondant a l'age.
##' @param gen une valeur \code{integer} correspondant a la generation.
##' @param n_periodes une valeur \code{integer} correspondant a la borne de sommation.
##' @return Le vecteur des taux de deces calcules.
##' @author Prim'Act
##' @include ParamTableMort-class.R
##' @export
setGeneric("calc_proba_survie", function(table_mort, age, gen, n_periodes){standardGeneric("calc_proba_survie")})
setMethod(
    f = "calc_proba_survie",
    signature = c(table_mort = "ParamTableMort", age = "integer", gen = "integer", n_periodes = "integer"),
    def = function(table_mort, age, gen, n_periodes){

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
        age_name  <- which(nom_table == "age")
        gen_name  <- which(nom_table == "gen")

        # Numero de ligne pour les Lx et Lx+1
        index_denominateur <- which(.subset2(table_mort@table, gen_name) == gen_app &
                                    .subset2(table_mort@table, age_name) == age_app)
        index_numerateur   <- which(.subset2(table_mort@table, gen_name) == gen_app &
                                    .subset2(table_mort@table, age_name) == age_app + 1)
        
        index_max <- which(.subset2(table_mort@table, gen_name) == gen_app &
                             .subset2(table_mort@table, age_name) == table_mort@age_max)

        index_numerateur <- index_numerateur : index_max

        # Calcul des probabilites de deces
        numerateur   <- .subset2(table_mort@table, 3)[index_numerateur]
        denominateur <- .subset2(table_mort@table, 3)[index_denominateur]
        if(length(numerateur) >= n_periodes){
            numerateur <- numerateur[1:n_periodes]
        } else {
            numerateur <- c(numerateur, rep(0, n_periodes - length(numerateur)))
        }

        if(denominateur == 0){
            return(rep(0, n_periodes))
        }else{
            return(numerateur / denominateur)
        }
    }
)

