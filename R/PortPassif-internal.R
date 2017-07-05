
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement

setMethod(
  f = "initialize",
  signature = "PortPassif",
  definition = function(.Object,
                        annee = integer(),
                        eei = list(),
                        rer = list(),
                        names_class_prod = character(),
                        ht = new("HypTech"),
                        fp = new("FraisPassif"),
                        tx_pb = new("TauxPB"),
                        autres_passifs = new("AutresPassifs"),
                        autres_reserves = new("AutresReserves")
                        ){
    if(! missing(annee) & ! missing(names_class_prod) & ! missing(ht) & ! missing(fp)  & ! missing(tx_pb) &
       ! missing(autres_passifs) & ! missing(autres_reserves)){
      .Object@annee <- annee
      .Object@eei   <- eei
      .Object@rer   <- rer
      .Object@names_class_prod <- names_class_prod
      .Object@ht   <- ht
      .Object@fp  <- fp
      .Object@tx_pb  <- tx_pb
      .Object@autres_passifs <- autres_passifs
      .Object@autres_reserves   <- autres_reserves
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@annee      <- as.integer(0)
      .Object@eei <- list(eei_1 = new("EpEuroInd"))
      .Object@rer <- list(rer_1 = new("RetraiteEuroRest"))
      .Object@names_class_prod <- character()
      .Object@ht <- new("HypTech")
      .Object@fp <- new("FraisPassif")
      .Object@tx_pb <- new("TauxPB")
      .Object@autres_passifs <- new("AutresPassifs")
      .Object@autres_reserves <- new("AutresReserves")
    }
    return(.Object)
  }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------

setMethod(
  f = "[",
  signature = "PortPassif",
  definition = function(x, i){
    switch(EXPR = i,
           "annee" = {return(x@annee)},
           "eei" = {return(x@eei)},
           "rer" = {return(x@rer)},
           "names_class_prod" = {return(x@names_class_prod)},
           "ht" = {return(x@ht)},
           "fp" = {return(x@fp)},
           "tx_pb" = {return(x@tx_pb)},
           "autres_passifs" = {return(x@autres_passifs)},
           "autres_reserves" = {return(x@autres_reserves)},
            stop("Cet attribut n'existe pas!")
    )
  }
)


# Setteur
setReplaceMethod(
  f = "[",
  signature = "PortPassif",
  definition = function(x, i, value){
    switch(EXPR = i,
           "annee" = {x@annee <- value},
           "eei" = {x@eei <- value},
           "rer" = {x@rer <- value},
           "names_class_prod" = {x@names_class_prod <- value},
           "ht" = {x@ht <- value},
           "fp" = {x@fp <- value},
           "tx_pb" = {x@tx_pb <- value},
           "autres_passifs" = {x@autres_passifs <- value},
           "autres_reserves" = {x@autres_reserves <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
