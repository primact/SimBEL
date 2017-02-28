#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe AutresReserves
#----------------------------------------------------------


# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement

setMethod(
  f = "initialize",
  signature = "AutresReserves",
  definition = function(.Object,
                        pgg_debut = numeric(),
                        psap_debut = numeric(),
                        pgg_valeur = numeric(),
                        psap_valeur = numeric(),
                        tx_pgg_ep = numeric(),
                        tx_pgg_autres = numeric(),
                        tx_psap_ep = numeric(),
                        tx_psap_autres = numeric()){
    if(!missing(pgg_debut) & !missing(psap_debut) & ! missing(pgg_valeur) & !missing(psap_valeur)  & !missing(tx_pgg_ep)  & !missing(tx_pgg_autres)
       & ! missing(tx_psap_ep) & !missing(tx_psap_autres)){
      .Object@pgg_debut <- pgg_debut
      .Object@psap_debut <- psap_debut
      .Object@pgg_valeur <- pgg_valeur
      .Object@psap_valeur <- psap_valeur
      .Object@tx_pgg_ep  <- tx_pgg_ep
      .Object@tx_pgg_autres  <- tx_pgg_autres
      .Object@tx_psap_ep  <- tx_psap_ep
      .Object@tx_psap_autres  <- tx_psap_autres
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@pgg_debut <- numeric()
      .Object@psap_debut <- numeric()
      .Object@pgg_valeur <- numeric()
      .Object@psap_valeur <- numeric()
      .Object@tx_pgg_ep  <- numeric()
      .Object@tx_pgg_autres  <- numeric()
      .Object@tx_psap_ep  <- numeric()
      .Object@tx_psap_autres  <- numeric()
    }
    return(.Object)
  }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "AutresReserves",
  definition = function(x, i){
    switch(EXPR = i,
           "pgg_debut" = {return(x@pgg_debut)},
           "psap_debut" = {return(x@psap_debut)},
           "pgg_valeur" = {return(x@pgg_valeur)},
           "psap_valeur"   = {return(x@psap_valeur)},
           "tx_pgg_ep"  = {return(x@tx_pgg_ep)},
           "tx_pgg_autres"  = {return(x@tx_pgg_autres)},
           "tx_psap_ep"  = {return(x@tx_psap_ep)},
           "tx_psap_autres"  = {return(x@tx_psap_autres)},
            stop("[AutresReserves] : Cet attribut n'existe pas!")
    )
  }
)

# Setteur
setReplaceMethod(
  f = "[",
  signature = "AutresReserves",
  definition = function(x, i, value){
    switch(EXPR = i,
           "pgg_debut" = {x@pgg_debut <- value},
           "psap_debut" = {x@psap_debut <- value},
           "pgg_valeur" = {x@pgg_valeur <- value},
           "psap_valeur"   = {x@psap_valeur <- value},
           "tx_pgg_ep"  = {x@tx_pgg_ep <- value},
           "tx_pgg_autres"  = {x@tx_pgg_autres <- value},
           "tx_psap_ep"  = {x@tx_psap_ep <- value},
           "tx_psap_autres"  = {x@tx_psap_autres <- value},
           stop("[AutresReserves]  : Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
