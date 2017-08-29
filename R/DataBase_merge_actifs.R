#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_actifs
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Aggregation des donnees relatives aux actifs.
##'
##' @name merge_actifs
##' @docType methods
##' @param action un \code{data.frame} contenant les donnees des actions a inserer dans la base..
##' @param immo un \code{data.frame} contenant les donnees des immo a inserer dans la base.
##' @param oblig un \code{data.frame} contenant les donnees des obligations a inserer dans la base.
##' @param treso un \code{data.frame} contenant les donnees treso a inserer dans la base.
##' @param sim un \code{integer} representant le numero de la simulation.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_actifs", def = function(action, immo, oblig, treso, sim) {standardGeneric("merge_actifs")})
setMethod(
    f = "merge_actifs",
    signature = c(action = "data.frame", immo = "data.frame", oblig = "data.frame", treso = "data.frame", sim = "integer"),
    definition = function(action, immo, oblig, treso, sim){

        # Aggregation des donnes
        df <- cbind(num_sim = sim, rbind.fill(action, immo, oblig, treso))

        # Output
        return(df)
    }
)
