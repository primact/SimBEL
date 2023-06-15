#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update ptf manuelle
#----------------------------------------------------------------------------------------------------------------------------------------------------


setGeneric(name = "update_journal", def = function(x, operation_achat_vente) {
    standardGeneric("update_journal")
})
setMethod(
    f = "update_journal",
    signature = c(x = "AlmEngine", operation_achat_vente = data.frame()),
    definition = function(x, operation_achat_vente = data.frame(
                              annee = integer(),
                              operation = "",
                              type_actif = "",
                              num_mp = integer(),
                              num_index = integer(),
                              montant = double(),
                              nb_unit = double(),
                              pmvr = double()
                          )) {
        x["journal_achat_vente"] <- rbind(x["journal_achat_vente"], operation_achat_vente)
        return(x)
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update ptf action
#----------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name = "update_journal_action", def = function(x, ptf, annee, operation, pmvr) {
    standardGeneric("update_journal_action")
})
setMethod(
    f = "update_journal_action",
    signature = c(x = "AlmEngine", ptf = "Action", annee = "numeric", operation = "character"),
    definition = function(x, ptf, annee, operation) {
        if (operation == "achat") {
            operation_achat_vente <- data.frame(
                annee = rep(annee, nrow(ptf["ptf_action"])),
                operation = rep(operation, nrow(ptf["ptf_action"])),
                type_actif = "action",
                num_mp = ptf["ptf_action"][, "num_mp"],
                num_index = ptf["ptf_action"][, "num_index"],
                montant = ptf["ptf_action"][, "val_marche"],
                nb_unit = ptf["ptf_action"][, "nb_unit"],
                pmvr = rep(0, nrow(x["ptf_action"]))
            )
        } else if (operation == "vente") {
            operation_achat_vente <- data.frame(
                annee = rep(annee, nrow(ptf["ptf_action"])),
                operation = rep(operation, nrow(ptf["ptf_action"])),
                type_actif = "action",
                num_mp = ptf["ptf_action"][, "num_mp"],
                num_index = ptf["ptf_action"][, "num_index"],
                montant = ptf["ptf_action"][, "val_marche"],
                nb_unit = ptf["ptf_action"][, "nb_unit"],
                pmvr = pmvr
            )
        } else {
            stop("[AlmEngine : update_journal_action] : L'operation realisee est soit un achat soit une vente \n")
        }


        x["journal_achat_vente"] <- rbind(x["journal_achat_vente"], operation_achat_vente)
        return(x)
    }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update ptf immo
#----------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name = "update_journal_immo", def = function(x, ptf, annee, operation, pmvr) {
    standardGeneric("update_journal_immo")
})
setMethod(
    f = "update_journal_immo",
    signature = c(x = "AlmEngine", ptf = "Immo", annee = "numeric", operation = "character", pmvr = "numeric"),
    definition = function(x, ptf, annee, operation, pmvr) {
        if (operation == "achat") {
            operation_achat_vente <- data.frame(
                annee = rep(annee, nrow(ptf["ptf_immo"])),
                operation = rep(operation, nrow(ptf["ptf_immo"])),
                type_actif = "immo",
                num_mp = ptf["ptf_immo"][, "num_mp"],
                num_index = ptf["ptf_immo"][, "num_index"],
                montant = ptf["ptf_immo"][, "val_marche"],
                nb_unit = ptf["ptf_immo"][, "nb_unit"],
                pmvr = rep(0, nrow(x["ptf_immo"]))
            )
        } else if (operation == "vente") {
            operation_achat_vente <- data.frame(
                annee = rep(annee, nrow(ptf["ptf_immo"])),
                operation = rep(operation, nrow(ptf["ptf_immo"])),
                type_actif = "immo",
                num_mp = ptf["ptf_immo"][, "num_mp"],
                num_index = ptf["ptf_immo"][, "num_index"],
                montant = ptf["ptf_immo"][, "val_marche"],
                nb_unit = ptf["ptf_immo"][, "nb_unit"],
                pmvr = pmvr
            )
        } else {
            stop("[AlmEngine : update_journal_immo] : L'operation realisee est soit un achat soit une vente \n")
        }


        x["journal_achat_vente"] <- rbind(x["journal_achat_vente"], operation_achat_vente)
        return(x)
    }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update ptf oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name = "update_journal_action", def = function(x, ptf, annee, operation, pmvr) {
    standardGeneric("update_journal_action")
})
setMethod(
    f = "update_journal_action",
    signature = c(x = "AlmEngine", ptf = "Action", annee = "numeric", operation = "character", pmvr = "numeric"),
    definition = function(x, ptf, annee, operation, pmvr) {
        if (operation == "achat") {
            operation_achat_vente <- data.frame(
                annee = rep(annee, nrow(ptf["ptf_action"])),
                operation = rep(operation, nrow(ptf["ptf_action"])),
                type_actif = "action",
                num_mp = ptf["ptf_action"][, "num_mp"],
                num_index = ptf["ptf_action"][, "num_index"],
                montant = ptf["ptf_action"][, "val_marche"],
                nb_unit = ptf["ptf_action"][, "nb_unit"],
                pmvr = rep(0, nrow(x["ptf_action"]))
            )
        } else if (operation == "vente") {
            operation_achat_vente <- data.frame(
                annee = rep(annee, nrow(ptf["ptf_action"])),
                operation = rep(operation, nrow(ptf["ptf_action"])),
                type_actif = "action",
                num_mp = ptf["ptf_action"][, "num_mp"],
                num_index = ptf["ptf_action"][, "num_index"],
                montant = ptf["ptf_action"][, "val_marche"],
                nb_unit = ptf["ptf_action"][, "nb_unit"],
                pmvr = pmvr
            )
        } else {
            stop("[AlmEngine : update_journal_action] : L'operation realisee est soit un achat soit une vente \n")
        }


        x["journal_achat_vente"] <- rbind(x["journal_achat_vente"], operation_achat_vente)
        return(x)
    }
)
