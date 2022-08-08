#----------------------------------------------------------------------------------------------------------------------------------------------------
#           insert_tables
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Fusionne et insere les donnees dans la \code{\link{DataBase}}
##'
##' @name insert_tables
##' @docType methods
##' @param x est un objet de type \code{\link{DataBase}}.
##' @param result_simu une \code{liste} contenant les tables a inserer dans la \code{DataBase}.
##' @param ens_simu un vecteur de type \code{numeric} contenant les indices des elements de la liste a inserer dans la \code{DataBase}.
##' @author Prim'Act
##' @export
##' @include DataBase_class.R

setGeneric(name = "insert_tables", def = function(x, result_simu, ens_simu){standardGeneric("insert_tables")})
setMethod(
    f = "insert_tables",
    signature = c(x = "DataBase", result_simu = "list", ens_simu = "numeric"),
    definition = function(x, result_simu, ens_simu){

        # Initialisation des tables
        table_output_be <- NULL ; table_output_produit_model <- NULL ; table_output_produit_hors_model <- NULL ;
        table_output_produit_model_agg <- NULL ;table_van_agg <- NULL; table_be <- NULL ;
        table_actifs <- NULL ; table_flux_fin <- NULL ; table_output_pb <- NULL

        # Aggregartion des resultats
        for(j in ens_simu) {

            tables <- result_simu[[j]][["tables"]]
            table_van_agg <- rbind(table_van_agg, tables[["table_van_agg"]])
            table_output_be <- rbind(table_output_be, tables[["table_output_be"]])
            table_output_produit_model <- rbind(table_output_produit_model, tables[["table_output_produit_model"]])
            table_output_produit_hors_model <- rbind(table_output_produit_hors_model, tables[["table_output_produit_hors_model"]])
            table_output_produit_model_agg <- rbind(table_output_produit_model_agg, tables[["table_output_produit_model_agg"]])
            table_be <- rbind(table_be, tables[["table_be"]])
            table_actifs <- rbind(table_actifs, tables[["table_actifs"]])
            table_flux_fin <- rbind2(table_flux_fin, tables[["table_flux_fin"]])
            table_output_pb <- rbind(table_output_pb, tables[["table_output_pb"]])
        }

        # Mise a jour table
        table_output_produit_model <- update_table_output_produit(table_output_produit_model)
        table_output_pb <- update_table_output_pb(table_output_pb)

        # Ecriture dans la base
        dbWriteTable(conn = x@database, name = "OUTPUT_BE", value = table_output_be, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "OUTPUT_VAN_AGG", value = table_van_agg, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "OUTPUT_PRODUIT", value = table_output_produit_model, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "HORS_MODEL", value = table_output_produit_hors_model, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "OUTPUT_PRODUIT_AGG", value = table_output_produit_model_agg, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "BE", value = table_be, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "ACTIF", value = table_actifs, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "FLUX_FIN", value = table_flux_fin, row.names=FALSE, append=TRUE)
        dbWriteTable(conn = x@database, name = "PB", value = table_output_pb, row.names=FALSE, append=TRUE)
    }
)
