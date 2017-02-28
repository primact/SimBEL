#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe ParamTableRach
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation de ParamTableRach
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
setMethod(
  f = "initialize",
  signature = "ParamTableRach",
  definition = function(.Object, df){
    if(!missing(df) ){ 
      .Object@table <- df
      .Object@age_min <- as.integer(min(df["age"]))
      .Object@age_max <- as.integer(max(df["age"]))
      .Object@anc_min <- as.integer(min(df["anc"]))
      .Object@anc_max <- as.integer(max(df["anc"]))  
      validObject(.Object)
    }else 
    {stop("[ParamTableRach] : Veuillez renseigner l'ensemble des parametres pour l'initalisation")}
    return(.Object)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "ParamTableRach",
  definition = function(x, i){
    switch(EXPR = i,
           "table" = {return(x@table)},
           "age_min" = {return(x@age_min)},
           "age_max" = {return(x@age_max)},
           "anc_min" = {return(x@anc_min)},
           "anc_max" = {return(x@anc_max)},
           stop("Cet attribut n'existe pas!")
    )
  }
)

# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamTableRach",
  definition = function(x, i, value){
    switch(EXPR = i,
           "table" = {x@table <- value},
           "age_min" = {x@age_min <- value},
           "age_max" = {x@age_max <- value},
           "anc_min" = {x@anc_min <- value},
           "anc_max" = {x@anc_max <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)