# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement

setMethod(
  f = "initialize",
  signature = "ParamBe",
  definition = function(.Object,
                        nb_annee = integer()) {
    .Object@nb_annee <- nb_annee
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
  signature = "ParamBe",
  definition = function(x, i) {
    switch(EXPR = i,
      "nb_annee" = {
        return(x@nb_annee)
      },
      stop("[ParamBe] : Cet attribut n'existe pas!")
    )
  }
)

# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamBe",
  definition = function(x, i, value) {
    switch(EXPR = i,
      "nb_annee" = {
        x@nb_annee <- value
      },
      stop("[ParamBe] : Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
