#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe TabProbaRetEuroRest
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "TabProbaRetEuroRest",
    definition = function(.Object, num_mp = "numeric"){
        
        .Object@ax              <- data.frame(num_mp)
        .Object@sortie_retraite <- data.frame(num_mp)
        .Object@survie_un_an    <- data.frame(num_mp)
        return(.Object)
    }
)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "TabProbaRetEuroRest",
    definition = function(x, i){
        switch(EXPR = i,
               "ax"               = {return(x@ax)},
               "sortie_retraite"  = {return(x@sortie_retraite)},
               "survie_un_an"     = {return(x@survie_un_an)},
               stop("Cet attribut n'existe pas!")
        )
    }
)


# Setteur
setReplaceMethod(
    f = "[",
    signature = "TabProbaRetEuroRest",
    definition = function(x, i, value){
        switch(EXPR = i,
               "ax"               = {x@ax <- value},
               "sortie_retraite"  = {x@sortie_retraite <- value},
               "survie_un_an"     = {x@survie_un_an <- value},
               stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)

