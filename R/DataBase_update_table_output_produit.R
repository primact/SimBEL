#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_table_output_produit
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la table OUTPUT_PRODUIT. Ajoute plusieurs colonnes : chgt, delta_pm, frais, credit, debit, resulat.
##'
##' @name update_table_output_produit
##' @docType methods
##' @param table est un objet de type \code{data.frame}.
##' @author Prim'Act
##' @return La table mise a jour.
##' @export
##' @include DataBase_class.R

setGeneric(name = "update_table_output_produit", def = function(table) {
    standardGeneric("update_table_output_produit")
})
setMethod(
    f = "update_table_output_produit",
    signature = c(table = "data.frame"),
    definition = function(table) {
        # Creation et mise en forme du dataframe a inserer dans la base
        table["chgt"] <- table["arr_charg"] + table["enc_charg_stock"] + table["rach_charg"]
        table["delta_pm"] <- table["pm_fin_ap_pb"] - table["pm_deb"]
        table["frais"] <- table["frais_fixe_prime"] + table["frais_var_prime"] + table["frais_fixe_prest"] + table["frais_var_prest"] +
            table["frais_fixe_enc"] + table["frais_var_enc"] + table["frais_fin"]
        table["credit"] <- table["pri_brut"] + table["chgt"]
        table["debit"] <- table["frais"] + table["prest"] + table["flux_fin_passif"] + table["delta_pm"] + table["soc_stock_ap_pb"]
        table["resultat"] <- table["credit"] - table["debit"]

        # Output
        return(table)
    }
)
