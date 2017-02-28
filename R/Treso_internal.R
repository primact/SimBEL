#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "Treso",
  definition = function(x,i){
    switch(EXPR = i,
           # Data frame Financier
           "ptf_treso" = {return(x@ptf_treso)},
           stop("Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "Treso",
  definition = function(x,i,value){
    switch(EXPR = i,
           "ptf_treso" = {x@ptf_treso <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)
