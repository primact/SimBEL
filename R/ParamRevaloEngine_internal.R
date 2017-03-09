
# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement

setMethod(
  f = "initialize",
  signature = "ParamRevaloEngine",
  definition = function(.Object,
                        taux_pb_fi = numeric(),
                        taux_pb_tech = numeric(),
                        tx_marge_min = numeric(),
                        solde_pb_regl = numeric()){
    .Object@taux_pb_fi  <- taux_pb_fi
    .Object@taux_pb_tech  <- taux_pb_tech
    .Object@tx_marge_min  <- tx_marge_min
    .Object@solde_pb_regl  <- solde_pb_regl
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
  signature = "ParamRevaloEngine",
  definition = function(x, i){
    switch(EXPR = i,
           "taux_pb_fi" = {return(x@taux_pb_fi)},
           "taux_pb_tech" = {return(x@taux_pb_tech)},
           "tx_marge_min" = {return(x@tx_marge_min)},
           "solde_pb_regl" = {return(x@solde_pb_regl)},
          stop("[ParamRevaloEngine] : Cet attribut n'existe pas!")
    )
  }
)

# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamRevaloEngine",
  definition = function(x, i, value){
    switch(EXPR = i,
           "taux_pb_fi" = {x@taux_pb_fi <- value},
           "taux_pb_tech" = {x@taux_pb_tech <- value},
           "tx_marge_min" = {x@tx_marge_min <- value},
           "solde_pb_regl" = {x@solde_pb_regl <- value},
           stop("[ParamRevaloEngine] : Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
