#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_table_output_pb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la table des PB. Ajoute une colonne "diff_pb".
##'
##' @name update_table_output_pb
##' @docType methods
##' @param table est un objet de type \code{data.frame}.
##' @author Prim'Act
##' @return La table mise a jour.
##' @export
##' @include DataBase_class.R

setGeneric(name = "update_table_output_pb", def = function(table) {
    standardGeneric("update_table_output_pb")
})
setMethod(
    f = "update_table_output_pb",
    signature = c(table = "data.frame"),
    definition = function(table) {
        # Creation et mise en forme du dataframe a inserer dans la base
        table["diff_pb"] <- table["tot_pb_rep"] - table["ppb8"]

        # Output
        return(table)
    }
)
