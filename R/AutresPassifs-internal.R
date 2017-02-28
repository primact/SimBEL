#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe AutresPassifs
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 01/02/2017. Fait par MT : initialisation
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "AutresPassifs",
  definition = function(.Object, mp = data.frame()){

    if(! missing(mp)){
      .Object@mp <- mp

      # Validation du format
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@mp <- data.frame(annee = numeric(),
                               prime = numeric(),
                               prestation = numeric(),
                               frais = numeric(),
                               pm_deb = numeric(),
                               pm_fin = numeric(),
                               it = numeric()
                                 )
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
  signature = "AutresPassifs",
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
  signature = "AutresPassifs",
  definition = function(x, i, value){
    switch(EXPR = i,
           "mp" = {x@mp <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

