#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_van_agg
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Insertion des VAN dans la table \code{VAN_AGG} de la base de donnees.
##'
##' @name merge_van_agg
##' @docType methods
##' @param sim un \code{integer} representant le numero de la simulation.
##' @param result_tech une \code{matrix} contenant le resultat technique.
##' @param result_fin une \code{matrix} contenant le resultat financier.
##' @param result_brut une \code{matrix} contenant le resultat brut.
##' @param result_net une \code{matrix} contenant le resultat net.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_van_agg", def = function(result_tech, result_fin, result_brut, result_net, sim){standardGeneric("merge_van_agg")})
setMethod(
  f = "merge_van_agg",
  signature = c( result_tech = "matrix", result_fin = "matrix", result_brut = "matrix", result_net = "matrix", sim = "integer"),
  definition = function(result_tech, result_fin, result_brut, result_net, sim){

    # Creation et mise en forme du dataframe a inserer dans la base
    df <- data.frame(num_sim = sim,
                     result_tech = result_tech,
                     result_fin = result_fin,
                     result_brut = result_brut,
                     result_net = result_net)

    # Output
    return(df)
  }
)
