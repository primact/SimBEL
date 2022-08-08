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
##' @param result_tech une \code{matrix} contenant le resultat technique.
##' @param result_fin une \code{matrix} contenant le resultat financier.
##' @param result_brut une \code{matrix} contenant le resultat brut.
##' @param result_net une \code{matrix} contenant le resultat net.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_table_produit", def = function(flux, stock, hors_model, fin, nb_annee, sim, nom_produit,
                                                        result_tech, result_fin, result_brut, result_net) {standardGeneric("merge_table_produit")})
setMethod(
    f = "merge_table_produit",
    signature = c(flux = "matrix", stock = "matrix", hors_model = "data.frame", fin = "data.frame",
                  nb_annee = "integer", sim = "integer", nom_produit = "character",
                  result_tech = "matrix", result_fin = "matrix", result_brut = "matrix", result_net = "matrix"),
    definition = function(flux, stock, hors_model, fin,
                          nb_annee, sim, nom_produit, result_tech, result_fin, result_brut, result_net){

        # Noms des produits modelises (sauf les 'hors_model')
        nom_prod_mod <- nom_produit[-length(nom_produit)]

        # Creation du DF
        df_model <- data.frame(num_sim = sim,
                               annee = rep(1L:nb_annee, 1L, each = length(nom_prod_mod)),
                               prod = rep(nom_prod_mod, nb_annee),
                               flux,
                               stock,
                               fin)

        # Creation d'un DF aggregeant les produits
        num_prod <- which(names(df_model) == "prod")
        df_model_agg <- aggregate(.~ num_sim + annee, data = df_model[, -num_prod], FUN = sum)
        df_model_agg <- data.frame(df_model_agg,
                                   result_tech = result_tech,
                                   result_fin = result_fin,
                                   result_brut = result_brut,
                                   result_net = result_net)

        # DF hors model
        df_hors_model <- data.frame(num_sim = sim, prod = rep("hors_model", nb_annee), hors_model)

        # Output
        return(list(df_model = df_model,
                    df_model_agg = df_model_agg,
                    df_hors_model = df_hors_model))
    }
)
