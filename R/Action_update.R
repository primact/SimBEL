setGeneric(name = "update_vm_action", def = function(x,vm){standardGeneric("update_vm_action")})
setMethod(
  f = "update_vm_action",
  signature = c(x = "Action", vm = "numeric"),
  definition = function(x,vm){
    # Verification des inputs
    if (nrow(x["ptf_action"]) != length(vm)) { stop("[Action : update_vm_action] Les inputs ne sont pas de memes dimensions")}
    if(sum(vm < 0) > 0) { stop("[Action : update_vm_action] :  Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_action"][,"val_marche"] <- vm
    return(x)
  }
)

setGeneric(name = "update_dur_det_action", def = function(x){standardGeneric("update_dur_det_action")})
setMethod(
  f = "update_dur_det_action",
  signature = "Action",
  definition = function(x){
    if(nrow(x["ptf_action"]) == 0) { cat("[WARNING : Action : update_dur_det_action] : Le portefeuille action initial est vide. \n")
                                    return(x)}
    x["ptf_action"][,"dur_det"] <- x["ptf_action"][,"dur_det"] + 1
    return(x)
  }
)
