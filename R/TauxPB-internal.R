
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "TauxPB",
  definition = function(.Object, mp = data.frame()){

    if(! missing(mp)){
      .Object@mp <- mp

      # Validation du format
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@mp <- data.frame(nom_prod = character(),
                               taux_pb = numeric())
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
  signature = "TauxPB",
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
  signature = "TauxPB",
  definition = function(x, i, value){
    switch(EXPR = i,
           "mp" = {x@mp <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

