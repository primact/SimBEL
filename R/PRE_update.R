setGeneric("do_update_PRE_val_debut", def = function(x, val_debut){standardGeneric("do_update_PRE_val_debut")})
setMethod(
    f = "do_update_PRE_val_debut",
    signature = c(x = "PRE", val_debut = "numeric"),
    definition = function(x, val_debut){
        x@val_debut <- val_debut
        return(x)
    }
)

setGeneric("do_update_PRE_val_courante", def = function(x, val_courante){standardGeneric("do_update_PRE_val_courante")})
setMethod(
    f = "do_update_PRE_val_courante",
    signature = c(x = "PRE", val_courante = "numeric"),
    definition = function(x, val_courante){
        x@val_courante <- val_courante
        return(x)
    }
)