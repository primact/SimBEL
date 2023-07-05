# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement

setMethod(
    f = "initialize",
    signature = "RevaloEngine",
    definition = function(.Object,
                          param_revalo = "ParamRevaloEngine") {
        .Object@param_revalo <- param_revalo
        validObject(.Object)
        return(.Object)
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "RevaloEngine",
    definition = function(x, i) {
        switch(EXPR = i,
            "param_revalo" = {
                return(x@param_revalo)
            },
            stop("[RevaloEngine] : Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "RevaloEngine",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "param_revalo" = {
                x@param_revalo <- value
            },
            stop("[RevaloEngine] : Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
