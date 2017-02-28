setGeneric(name = "update_vm_immo", def = function(x,vm){standardGeneric("update_vm_immo")})
setMethod(
  f = "update_vm_immo",
  signature = c(x = "Immo", vm = "numeric"),
  definition = function(x,vm){
    # Verification des inputs
    if (nrow(x["ptf_immo"]) != length(vm)) { stop("[Immo : update_vm_immo] Les inputs ne sont pas de memes dimensions")}
    if(sum(vm < 0) > 0) { stop("[Immo : update_vm_immo] :  Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_immo"][,"val_marche"] <- vm
    return(x)
  }
)

setGeneric(name = "update_dur_det_immo", def = function(x){standardGeneric("update_dur_det_immo")})
setMethod(
  f = "update_dur_det_immo",
  signature = "Immo",
  definition = function(x){
    if(nrow(x["ptf_immo"]) == 0) {cat("[WARNING : Immo : update_dur_det_immo] : Le portefeuille immo initial est vide. \n")
                                    return(x)}
    x["ptf_immo"][,"dur_det"] <- x["ptf_immo"][,"dur_det"] + 1
    return(x)
  }
)
