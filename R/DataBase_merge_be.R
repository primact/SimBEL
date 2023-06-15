#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_be
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Insertion des BE dans la table \code{BE} de la base de donnees.
##'
##' @name merge_be
##' @docType methods
##' @param be une \code{matrix} a inserer dans la base.
##' @param sim un \code{integer} representant le numero de la simulation.
##' @param nom_produit une liste de \code{character} contenant les noms des differents produits.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_be", def = function(be, sim, nom_produit) {
  standardGeneric("merge_be")
})
setMethod(
  f = "merge_be",
  signature = c(be = "matrix", sim = "integer", nom_produit = "character"),
  definition = function(be, sim, nom_produit) {
    # Creation et mise en forme du dataframe a inserer dans la base
    nb_prod <- length(nom_produit)
    df <- data.frame(num_sim = rep(sim, nb_prod), prod = nom_produit, be = melt(be)$value)

    # Output
    return(df)
  }
)
