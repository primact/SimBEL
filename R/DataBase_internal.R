# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
#           - Erreur autrement

setMethod(
    f = "initialize",
    signature = "DataBase",
    definition = function(.Object,
                          file_adress = "character",
                          ecriture_base = "logical",
                          choc_name = "character") {
        if (!(missing(ecriture_base) | missing(file_adress) | missing(choc_name))) {
            .Object@ecriture_base <- ecriture_base
            if (ecriture_base) {
                .Object@database <- dbConnect(RSQLite::SQLite(), paste(file_adress, paste("SimBEL", choc_name, "base.sqlite", sep = "_"), sep = "/"))
            }
        } else {
            .Object@ecriture_base <- TRUE
            .Object@database <- dbConnect(RSQLite::SQLite())
        }

        # Test de validite
        validObject(.Object)

        # Output
        return(.Object)
    }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "DataBase",
    definition = function(x, i) {
        switch(EXPR = i,
            "database" = {
                return(x@database)
            },
            stop("[DataBase] : Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "DataBase",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "database" = {
                x@database <- value
            },
            stop("[DataBase] : Cet attribut n'existe pas!")
        )

        # Test de validite
        validObject(x)

        # Output
        return(x)
    }
)
