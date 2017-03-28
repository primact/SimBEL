
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation de HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "HypTech",
  definition = function(.Object,
                        tables_mort = list(),
                        tables_rach = list(),
                        param_rach_dyn =list(),
                        param_comport = list()
                        ){

    if(! missing(tables_mort) & ! missing(tables_rach) & ! missing(param_rach_dyn) & ! missing(param_comport)){
      .Object@tables_mort <- tables_mort
      .Object@tables_rach <- tables_rach
      .Object@param_rach_dyn <- param_rach_dyn
      .Object@param_comport <- param_comport

      # Validation du format
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@tables_mort <- list()
      .Object@tables_rach <- list()
      .Object@param_rach_dyn <- list()
      .Object@param_comport <- list()
    }
    # Output
    return(.Object)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "HypTech",
  definition = function(x, i){
    switch(EXPR = i,
           "tables_mort" = {return(x@tables_mort)},
           "tables_rach" = {return(x@tables_rach)},
           "param_rach_dyn" = {return(x@param_rach_dyn)},
           "param_comport" = {return(x@param_comport)},
           stop("Cet attribut n'existe pas!")
    )
  }
)


# Setteur
setReplaceMethod(
  f = "[",
  signature = "HypTech",
  definition = function(x, i, value){
    switch(EXPR = i,
           "tables_mort" = {x@tables_mort <- value},
           "tables_rach" = {x@tables_rach <- value},
           "param_rach_dyn" = {x@param_rach_dyn <- value},
           "param_comport" = {x@param_comport <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
