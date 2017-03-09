#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe ParamChoc
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 22/01/2017. Fait par MT : initialisation
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "ParamChocSousc",
  definition = function(.Object, df_mp){
    if( missing(df_mp)){
      stop("L'objet 'df_mp' doit etre renseigne")
    }else{
      .Object@mp <- df_mp
      validObject(.Object)}
    return(.Object)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "ParamChocSousc",
  definition = function(x, i){
    switch(EXPR = i,
           "mp" = {return(x@mp)},
           stop("Cet attribut n'existe pas!")
    )
  }
)


# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamChocSousc",
  definition = function(x, i, value){
    switch(EXPR = i,
           "mp" = {x@mp <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
