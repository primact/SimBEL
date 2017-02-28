#----------------------------------------------------------------------------------------------------------------------------------------------------
# get_qx_mort : Methode de la classe de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Calcule le taux de mortalite
##'
##' \code{get_qx_mort} est une methode permettant de calculer le taux de mortalite.
##' @name get_qx_mort
##' @docType methods
##' @param x un objet de la classe \code{HypTech}.
##' @param nom_table : nom de la table de mortalite consideree
##' @param age : age en numerique
##' @param gen : generation en numerique
##' @return le taux de mortalite
##' @author Prim'Act
##' @export
##' @aliases HypTech


setGeneric("get_qx_mort", function(x, nom_table, age, gen){standardGeneric("get_qx_mort")})
setMethod(
  f = "get_qx_mort",
  signature = c(x = "HypTech",  nom_table = "character", age = "numeric", gen = "numeric"),
  def = function(x, nom_table, age, gen){

    # Ajout d un test de presence du nom
    if (! nom_table %in% names(x@tables_mort)){"Nom de table de mortalite non trouve"}
    else{return(calc_qx(x@tables_mort[[nom_table]],age,gen))}
  }
)
