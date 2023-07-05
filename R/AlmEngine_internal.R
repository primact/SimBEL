#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "AlmEngine",
    definition = function(x, i) {
        switch(EXPR = i,
            # Data frame Financier
            "journal_achat_vente" = {
                return(x@journal_achat_vente)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)
# Setteur
setReplaceMethod(
    f = "[",
    signature = "AlmEngine",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "journal_achat_vente" = {
                x@journal_achat_vente <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)

# Constructeur generique
journal <- function(annee = integer(),
                    operation = "",
                    type_actif = "",
                    num_mp = integer(),
                    num_index = integer(),
                    montant = double(),
                    nb_unit = double(),
                    pmvr = double()) {
    x <- new("AlmEngine")
    x["journal_achat_vente"] <- data.frame(
        annee = annee, operation = operation, type_actif = type_actif,
        num_mp = num_mp, num_index = num_index, montant = montant, nb_unit = nb_unit, pmvr = pmvr
    )
    return(x)
}
