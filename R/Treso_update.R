
# Fonction qui permet d'accroitre ou de decroitre un objet tresorerie d'un certain flux
setGeneric(name = "update_treso", def = function(x,flux){standardGeneric("update_treso")})
setMethod(
  f = "update_treso",
  signature = c(x = "Treso", flux = "numeric"),
  definition = function(x, flux){
    if (length(flux) > 1) {stop("[Treso : update_treso] : Le flux d'input doit etre compose d'un unique element \n")}
    x["ptf_treso"][,"val_marche"] <- x["ptf_treso"][,"val_marche"] + flux
    x["ptf_treso"][,"val_nc"]     <- x["ptf_treso"][,"val_nc"] + flux
    return(x)
  }
)
