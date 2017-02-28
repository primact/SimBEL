#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe ParamTableMort
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation de ParamTableMort
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
setMethod(
  f = "initialize",
  signature = "ParamTableMort",
  definition = function(.Object,df){
    if(!missing(df)){
      .Object@table <- df
      .Object@age_min <- as.integer(min(df["age"]))
      .Object@age_max <- as.integer(max(df["age"]))
      .Object@gen_min <- as.integer(min(df["gen"]))
      .Object@gen_max <- as.integer(max(df["gen"]))
      validObject(.Object)
    } else 
    {stop("[ParamTableMort] : Veuillez renseigner l'ensemble des parametres pour l'initalisation")}
    return(.Object)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "ParamTableMort",
  definition = function(x,i){
    switch(EXPR = i,
           "table" = {return(x@table)},
           "age_min" = {return(x@age_min)},
           "age_max" = {return(x@age_max)},
           "gen_min" = {return(x@gen_min)},
           "gen_max" = {return(x@gen_max)},
           stop("Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamTableMort",
  definition = function(x,i,value){
    switch(EXPR = i,
           "table" = {x@table <- value},
           "age_min" = {x@age_min <- value},
           "age_max" = {x@age_max <- value},
           "gen_min" = {x@gen_min <- value},
           "gen_max" = {x@gen_max <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
