



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "Canton",
  definition = function(x,i){
    switch(EXPR = i,
           "annee"      = {return(x@annee)},
           "ptf_fin" = {return(x@ptf_fin)},
           "ptf_passif" = {return(x@ptf_passif)},
           "mp_esg" = {return(x@mp_esg)},
           "ppb" = {return(x@ppb)},
           "hyp_canton" = {return(x@hyp_canton)},
           "param_alm" = {return(x@param_alm)},
           "param_revalo" = {return(x@param_revalo)},
           stop("[PortFin] : Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "Canton",
  definition = function(x,i,j,value){
    switch(EXPR = i,
           "annee"      = {x@annee <- value},
           "ptf_fin" = {x@ptf_fin <- value},
           "ptf_passif" = {x@ptf_passif <- value},
           "mp_esg" = {x@mp_esg <- value},
           "ppb" = {x@ppb <- value},
           "hyp_canton" = {x@hyp_canton <- value},
           "param_alm" = {x@param_alm <- value},
           "param_revalo" = {x@param_revalo <- value},
           stop("[PortFin] : Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
