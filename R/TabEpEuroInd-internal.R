#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe TabEpEuroInd
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "TabEpEuroInd",
  definition = function(.Object, tab = list()){
    if( ! missing(tab)){
      .Object@tab <- tab
      validObject(.Object)
    }  else {
      #Traitement du cas vide
      .Object@tab <- list(num_mp = 0,
                          pri_net = 0,
                          prest = 0,
                          pm_deb = 0,
                          pm_fin = 0,
                          enc_charg_base_th = 0,
                          enc_charg_rmin_th = 0,
                          rev_stock_brut = 0,
                          bes_tx_cible = 0,
                          nb_contr = 0,
                          tx_cible = 0,
                          pm_gar = 0)}
    return(.Object)
  }
)



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "TabEpEuroInd",
  definition = function(x, i){
    switch(EXPR = i,
           "tab" = {return(x@tab)},
           stop("Cet attribut n'existe pas!")
    )
  }
)


# Setteur
setReplaceMethod(
  f = "[",
  signature = "TabEpEuroInd",
  definition = function(x, i, value){
    switch(EXPR = i,
           "tab" = {x@tab <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

