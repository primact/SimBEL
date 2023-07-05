#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_table_be
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Jointure et insertion des donnees dans la base de donnees. Les donnees sont les ouputs (\code{output_be}) de la fonction \code{\link{proj_an}}
##'
##' @name merge_table_be
##' @docType methods
##' @param prime une \code{matrix} a inserer dans la base.
##' @param frais une \code{matrix} a inserer dans la base.
##' @param prestation une \code{matrix} a inserer dans la base.
##' @param prestation_fdb une \code{matrix} a inserer dans la base.
##' @param sim un \code{integer} representant le numero de la simulation.
##' @param nom_produit une liste de \code{character} contenant les noms des differents produits.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_table_be", def = function(prime, frais, prestation, prestation_fdb, sim, nom_produit) {
    standardGeneric("merge_table_be")
})
setMethod(
    f = "merge_table_be",
    signature = c(
        prime = "matrix", frais = "matrix", prestation = "matrix", prestation_fdb = "matrix",
        sim = "integer", nom_produit = "character"
    ),
    definition = function(prime, frais, prestation, prestation_fdb, sim, nom_produit) {
        # Creation et mise en forme du dataframe a inserer dans la base
        nb_annee <- nrow(prime)
        nb_prod <- length(nom_produit)
        df <- data.frame(
            num_sim = rep(sim, nb_annee * nb_prod), annee = rep(1L:nb_annee, nb_prod), prod = rep(nom_produit, each = nb_annee),
            prime = melt(prime)$value, frais = melt(frais)$value, prestation = melt(prestation)$value, prestation_fdb = melt(prestation_fdb)$value
        )

        # Output
        return(df)
    }
)
