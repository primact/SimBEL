#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "ParamComport",
  definition = function(.Object,
                        mat_oblig = numeric(),
                        alloc_mar = numeric(),
                        w_n = numeric(),
                        marge_mar = numeric(),
                        ch_enc_mar = numeric(),
                        ind_ref_action = numeric(),
                        ind_ref_immo = numeric()
                        ){

    if(! missing(mat_oblig) & ! missing(alloc_mar) & ! missing(w_n) & ! missing(marge_mar)  & ! missing(ch_enc_mar) &
       ! missing(ind_ref_action) & ! missing(ind_ref_immo)){
      .Object@mat_oblig <- mat_oblig
      .Object@alloc_mar <- alloc_mar
      .Object@w_n <- w_n
      .Object@marge_mar <- marge_mar
      .Object@ch_enc_mar <- ch_enc_mar
      .Object@ind_ref_action <- ind_ref_action
      .Object@ind_ref_immo <- ind_ref_immo
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@mat_oblig <- numeric()
      .Object@alloc_mar <- numeric()
      .Object@w_n <- numeric()
      .Object@marge_mar <- numeric()
      .Object@ch_enc_mar <- numeric()
      .Object@ind_ref_action <- numeric()
      .Object@ind_ref_immo <- numeric()
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
  signature = "ParamComport",
  definition = function(x, i){
    switch(EXPR = i,
           "mat_oblig" = {return(x@mat_oblig)},
           "alloc_mar" = {return(x@alloc_mar)},
           "w_n" = {return(x@w_n)},
           "marge_mar" = {return(x@marge_mar)},
           "ch_enc_mar" = {return(x@ch_enc_mar)},
           "ind_ref_action" = {return(x@ind_ref_action)},
           "ind_ref_immo" = {return(x@ind_ref_immo)},

           stop("Cet attribut n'existe pas!")
    )
  }
)


# Setteur
setReplaceMethod(
  f = "[",
  signature = "ParamComport",
  definition = function(x, i, value){
    switch(EXPR = i,
           "mat_oblig" = {x@mat_oblig <- value},
           "alloc_mar" = {x@alloc_mar <- value},
           "w_n" = {x@w_n <- value},
           "marge_mar" = {x@marge_mar <- value},
           "ch_enc_mar" = {x@ch_enc_mar <- value},
           "ind_ref_action" = {x@ind_ref_action <- value},
           "ind_ref_immo" = {x@ind_ref_immo <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

