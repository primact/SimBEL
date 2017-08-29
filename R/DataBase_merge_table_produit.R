#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_table_produit
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Jointure et insertion de donnees dans la \code{DataBase}. Les donnees sont les ouputs \code{output_produit} de la fonction \code{\link{proj_an}}
##'
##' @name merge_table_produit
##' @docType methods
##' @param flux une \code{matrix} a inserer dans la base.
##' @param stock une \code{matrix} a inserer dans la base.
##' @param hors_model un \code{data.frame} a inserer dans la base.
##' @param fin un \code{data.frame} a inserer dans la base.
##' @param nb_annee un \code{integer} indiquant le nombre d'annees de projection.
##' @param sim un \code{integer} representant le numero de la simulation.
##' @param nom_produit une liste de \code{character} contenant les noms des differents produits.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_table_produit", def = function(flux, stock, hors_model, fin, nb_annee, sim, nom_produit) {standardGeneric("merge_table_produit")})
setMethod(
    f = "merge_table_produit",
    signature = c(flux = "matrix", stock = "matrix", hors_model = "data.frame", fin = "data.frame",
                  nb_annee = "integer", sim = "integer", nom_produit = "character"),
    definition = function(flux, stock, hors_model, fin, nb_annee, sim, nom_produit){

        # Noms des produits modelises (sauf les 'hors_model')
        nom_prod_mod <- nom_produit[-length(nom_produit)]

        # Creation du DF
        df_model <- data.frame(num_sim = sim, annee = rep(1L:nb_annee, 1L, each = length(nom_prod_mod)), prod = rep(nom_prod_mod, nb_annee), flux, stock, fin)
        df_hors_model <- data.frame(num_sim = sim, prod = rep("hors_model", nb_annee), hors_model)

        # Output
        return(list(df_model = df_model,
                    df_hors_model = df_hors_model))
    }
)
