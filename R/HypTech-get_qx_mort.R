#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_qx_mort : Methode de calcul des taux de mortalite
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Recuperer les taux de deces calcules.
##'
##' \code{get_qx_mort} est une methode permettant d'executer le calcul des taux de deces.
##' @name get_qx_mort
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}}.
##' @param nom_table un \code{character} designant le nom d'une table de mortalite.
##' @param age est la valeur \code{integer} de l'age.
##' @param gen est la valeur \code{integer} de la generation.
##' @return Le taux de deces.
##' @author Prim'Act
##' @seealso Le calcul du taux de deces \code{\link{calc_qx}}.
##' @export
##' @include HypTech-class.R
setGeneric("get_qx_mort", function(x, nom_table, age, gen){standardGeneric("get_qx_mort")})
setMethod(
    f = "get_qx_mort",
    signature = c(x = "HypTech",  nom_table = "character", age = "integer", gen = "integer"),
    def = function(x, nom_table, age, gen){
        
        # Ajout d un test de presence du nom
        if (! nom_table %in% names(x@tables_mort)) stop("[Hyptech : get_qx_mort] Nom de table de mortalite non trouve")
        
        
        return(calc_qx(x@tables_mort[[nom_table]],age,gen))
        
    }
)
