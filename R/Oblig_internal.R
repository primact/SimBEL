#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les methodes internes de la classe Oblig
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "Oblig",
  definition = function(x,i){
    switch(EXPR = i,
           # Data frame Financier
           "ptf_oblig" = {return(x@ptf_oblig)},
           stop("Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "Oblig",
  definition = function(x,i,value){
    switch(EXPR = i,
           "ptf_oblig" = {x@ptf_oblig <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)


