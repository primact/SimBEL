#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_flux_fin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Aggregation des donnees relatives aux flux financiers.
##'
##' @name merge_flux_fin
##' @docType methods
##' @param flux_fin est un objet de type \code{matrix}.
##' @param sim un \code{integer} correspondant au numero de simulation.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_flux_fin", def = function(flux_fin, sim) {
    standardGeneric("merge_flux_fin")
})
setMethod(
    f = "merge_flux_fin",
    signature = c(flux_fin = "data.frame", sim = "integer"),
    definition = function(flux_fin, sim) {
        # Aggregation des donnes
        df <- cbind(num_sim = sim, flux_fin)

        # Output
        return(df)
    }
)
