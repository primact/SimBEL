# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement

setMethod(
    f = "initialize",
    signature = "Ppb",
    definition = function(.Object,
                          hist_ppb = vector(mode = "numeric", length = 8),
                          seuil_rep = numeric(),
                          seuil_dot = numeric(),
                          compte_rep = numeric(),
                          compte_dot = numeric()) {
        if (!missing(hist_ppb) & !missing(seuil_rep) & !missing(seuil_dot)) {
            .Object@hist_ppb <- hist_ppb
            .Object@valeur_ppb <- sum(hist_ppb)
            .Object@ppb_debut <- sum(hist_ppb)
            .Object@seuil_rep <- seuil_rep
            .Object@seuil_dot <- seuil_dot
            .Object@compte_rep <- 0
            .Object@compte_dot <- 0
        } else {
            # Traitement du cas vide
            .Object@hist_ppb <- rep(0, 8L)
            .Object@valeur_ppb <- 0
            .Object@ppb_debut <- 0
            .Object@seuil_rep <- 0
            .Object@seuil_dot <- 0
            .Object@compte_rep <- 0
            .Object@compte_dot <- 0
        }
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
    signature = "Ppb",
    definition = function(x, i) {
        switch(EXPR = i,
            "hist_ppb" = {
                return(x@hist_ppb)
            },
            "valeur_ppb" = {
                return(x@valeur_ppb)
            },
            "ppb_debut" = {
                return(x@ppb_debut)
            },
            "seuil_rep" = {
                return(x@seuil_rep)
            },
            "seuil_dot" = {
                return(x@seuil_dot)
            },
            "compte_rep" = {
                return(x@compte_rep)
            },
            "compte_dot" = {
                return(x@compte_dot)
            },
            stop("[Ppb] : Cet attribut n'existe pas!")
        )
    }
)

# Setteur
setReplaceMethod(
    f = "[",
    signature = "Ppb",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "hist_ppb" = {
                x@hist_ppb <- value
                x@valeur_ppb <- sum(value)
            },
            "valeur_ppb" = {
                x@valeur_ppb <- value
            },
            "ppb_debut" = {
                x@ppb_debut <- value
            },
            "seuil_rep" = {
                x@seuil_rep <- value
            },
            "seuil_dot" = {
                x@seuil_dot <- value
            },
            "compte_rep" = {
                x@compte_rep <- value
            },
            "compte_dot" = {
                x@compte_dot <- value
            },
            stop("[Ppb] : Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
