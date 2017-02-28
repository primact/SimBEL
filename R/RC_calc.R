
setGeneric(name = "calc_RC", def = function(x, pmvr_oblig){standardGeneric("calc_RC")})
setMethod(
    f = "calc_RC",
    signature = c(x = "RC", pmvr_oblig = "numeric"),
    definition = function(x, pmvr_oblig){
        # Verification des inputs
        if (length(pmvr_oblig) !=  1) { stop("[RC : calc_RC] : Le montant de plus ou moins value realise doit etre de dimension 1. \n")}
        val_courante <- max(x@val_courante + pmvr_oblig, 0)
        delta_RC <- val_courante - x@val_debut
        return (list(rc_courante = val_courante, var_rc = delta_RC))
    }
)
