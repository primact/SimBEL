#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "ESG",
  definition = function(x,i){
    switch(EXPR = i,
           "nb_simu"       = {return(x@nb_simu)},
           "ind_action"    = {return(x@ind_action)},
           "ind_immo"      = {return(x@ind_immo)},
           "ind_inflation" = {return(x@ind_inflation)},
           "yield_curve"   = {return(x@yield_curve)},
           "deflateur"     = {return(x@deflateur)},
           stop("Cet attribut n'existe pas!")
    )
  }
)
# Setteur
# Quoi qu'il arrive le setteur ecrasera
# Pour ajouter des elements a la liste courante renseigner value comme : list(liste_actuelle, list(valeur_a_ajouter))
setReplaceMethod(
  f = "[",
  signature = "ESG",
  definition = function(x,i,value){
    switch(EXPR = i,
           "nb_simu"       = {x@nb_simu <- value},
           "ind_action"    = {x@ind_action <- value},
           "ind_immo"      = {x@ind_immo <- value},
           "ind_inflation" = {x@ind_inflation <- value},
           "yield_curve"   = {x@yield_curve <- value},
           "deflateur"     = {x@deflateur <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

