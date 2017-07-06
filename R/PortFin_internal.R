#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur et Constructeur grand public
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "PortFin",
  definition = function(x,i){
    switch(EXPR = i,
           # Data frame Financier
           "annee"      = {return(x@annee)},
           "ptf_action" = {return(x@ptf_action)},
           "ptf_immo"   = {return(x@ptf_immo)},
           "ptf_oblig"  = {return(x@ptf_oblig)},
           "ptf_treso"  = {return(x@ptf_treso)},
           "pre"  = {return(x@pre)},
           "rc"  = {return(x@rc)},
           "frais_fin"  = {return(x@frais_fin)},
           "pvl_action" = {return(x@pvl_action)},
           "pvl_immo"   = {return(x@pvl_immo)},
           "pvl_oblig"  = {return(x@pvl_oblig)},
           "mvl_action" = {return(x@mvl_action)},
           "mvl_immo"   = {return(x@mvl_immo)},
           "mvl_oblig"  = {return(x@mvl_oblig)},
           "vm_vnc_precedent" = {return(x@vm_vnc_precedent)},
           stop("[PortFin] : Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "PortFin",
  definition = function(x,i,j,value){
    switch(EXPR = i,
           "annee"      = {x@annee <- value},
           "ptf_action" = {x@ptf_action <- value},
           "ptf_immo"   = {x@ptf_immo   <- value},
           "ptf_oblig"  = {x@ptf_oblig  <- value},
           "ptf_treso"  = {x@ptf_treso  <- value},
           "pre"  = {x@pre  <- value},
           "rc"  = {x@rc  <- value},
           "frais_fin"  = {x@frais_fin  <- value},
           "pvl_action" = {x@pvl_action<- value},
           "pvl_immo"   = {x@pvl_immo  <- value},
           "pvl_oblig"  = {x@pvl_oblig  <- value},
           "mvl_action" = {x@mvl_action<- value},
           "mvl_immo"   = {x@mvl_immo  <- value},
           "mvl_oblig"  = {x@mvl_oblig  <- value},
           "vm_vnc_precedent" = {x@vm_vnc_precedent <- value},
           stop("[PortFin] : Cet attribut n'existe pas!")
    )
    # validObject(x)
    return(x)
  }
)
