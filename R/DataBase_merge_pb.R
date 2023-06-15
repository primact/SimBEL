#----------------------------------------------------------------------------------------------------------------------------------------------------
#           merge_pb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Aggregation des donnees relatives a la PB.
##'
##' @name merge_pb
##' @docType methods
##' @param output_pb est un objet de type \code{data.frame}.
##' @param sim un \code{integer} correspondant au numero de simulation.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "merge_pb", def = function(output_pb, sim) {
    standardGeneric("merge_pb")
})
setMethod(
    f = "merge_pb",
    signature = c(output_pb = "data.frame", sim = "integer"),
    definition = function(output_pb, sim) {
        # Aggregation des donnes
        df <- cbind(num_sim = sim, output_pb)

        # Output
        return(df)
    }
)
