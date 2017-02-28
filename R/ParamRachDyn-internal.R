#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe ParamRachDyn
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par MT : initialisation
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation de ParamRachDynt
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
setMethod(
  f = "initialize",
  signature = "ParamRachDyn",
  definition = function(.Object,vec_param){
    if(!missing(vec_param)){    
      .Object@vec_param <- vec_param
      validObject(.Object)
    } else 
    {stop("[ParamRachDyn] : Veuillez renseigner l'ensemble des parametres pour l'initalisation")}
    return(.Object)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "ParamRachDyn",
  definition = function(x,i){
    switch(EXPR = i,
           "vec_param" = {return(x@vec_param)},
           stop("Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamRachDyn",
  definition = function(x,i,value){
    switch(EXPR = i,
           "vec_param" = {x@vec_param <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)






	


