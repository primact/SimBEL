
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "EpEuroInd",
  definition = function(.Object, mp, tab){
    if( ! missing(mp) & ! missing(tab)){
      .Object@mp <- mp
      .Object@tab <- tab
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@tab <- new("TabEpEuroInd")
      .Object@mp <- data.frame(num_mp = integer(),
                               num_canton = integer(),
                               num_prod = integer(),
                               age = integer(),
                               gen = integer(),
                               num_tab_mort = factor(),
                               chgt_enc = numeric(),
                               ind_chgt_enc_pos = logical(),
                               pm = numeric(),
                               nb_contr = numeric(),
                               anc = integer(),
                               terme = integer(),
                               type_cot = factor(),
                               periode_cot = factor(),
                               tx_cible = factor(),
                               chgt_prime = numeric(),
                               prime = numeric(),
                               tx_tech = numeric(),
                               terme_tx_tech = integer(),
                               tmg = numeric(),
                               terme_tmg = integer(),
                               num_rach_tot = factor(),
                               num_rach_part = factor(),
                               num_rach_dyn_tot = factor(),
                               num_rach_dyn_part = factor(),
                               chgt_rach = numeric(),
                               pm_gar = numeric(),
                               tx_revalo_prec = numeric(),
                               tx_cible_prec = numeric()
                               )
    }
      return(.Object)
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "EpEuroInd",
  definition = function(x, i){
    switch(EXPR = i,
           "mp" = {return(x@mp)},
           "tab" = {return(x@tab)},
           stop("Cet attribut n'existe pas!")
    )
  }
)

# Setteur
setReplaceMethod(
  f = "[",
  signature = "EpEuroInd",
  definition = function(x, i, value){
    switch(EXPR = i,
           "mp" = {x@mp <- value},
           "tab" = {x@tab <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
