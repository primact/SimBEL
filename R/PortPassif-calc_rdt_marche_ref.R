#----------------------------------------------------------
# Ce script comprend les methodes de calcul du taux de rendement de reference au niveau du marche
#----------------------------------------------------------

##' Calcul du taux de rendement de reference au niveau du marche
##'
##' \code{calc_rdt_marche_ref} est une methode permettant de calculer un taux cible.
##' @name calc_rdt_marche_ref
##' @docType methods
##' @param x un objet de la classe \code{\link{ParamComport}}.
##' @param mp_esg est un objet de type \code{\link{ModelPointESG}}, qui represente la situation courante
##' en annee et simulations des valeurs de l'ESG.
##' @return Une liste contenant les rendements de reference du marche.
##' @author Prim'Act
##' @include ModelPointESG_class.R ParamComport-class.R
##' @export

setGeneric(name = "calc_rdt_marche_ref", def = function(x, mp_esg) {
    standardGeneric("calc_rdt_marche_ref")
})
setMethod(
    f = "calc_rdt_marche_ref",
    signature = c(x = "ParamComport", mp_esg = "ModelPointESG"),
    definition = function(x, mp_esg) {
        # Recuperation de donnees
        ind_ref_action <- x@ind_ref_action
        indice_action <- mp_esg@indice_action
        ind_ref_immo <- x@ind_ref_immo
        indice_immo <- mp_esg@indice_immo

        # Verification des inputs
        if (ind_ref_action > nrow(indice_action)) stop("[calc_rdt_passif] :  L'indice action renseigne n'est pas present dans le model point ESG. \n")
        if (ind_ref_immo > nrow(indice_immo)) stop("[calc_rdt_passif] :  L'indice immo renseigne n'est pas present dans le model point ESG. \n")


        # Calcul de rdt actions
        indice_action <- mp_esg@indice_action
        rdt_action <- (indice_action[ind_ref_action, "S_action"] / indice_action[ind_ref_action, "S_prev_action"]) - 1

        # Calcul rdt immo
        indice_immo <- mp_esg@indice_immo
        rdt_immo <- (indice_immo[ind_ref_immo, "S_immo"] / indice_immo[ind_ref_immo, "S_prev_immo"]) - 1

        # Calcul rdt oblig
        mat_oblig <- x@mat_oblig
        if (mat_oblig > 1) {
            rdt_oblig <- mp_esg["yield_curve"][mat_oblig + 1]
        } else {
            rdt_oblig <- 0
        }

        # Calcul rdt treso
        rdt_treso <- mp_esg["yield_curve"][2L]


        # Output
        return(c(
            rdt_oblig = rdt_oblig,
            rdt_action = rdt_action,
            rdt_immo = rdt_immo,
            rdt_tre = rdt_treso
        ))
    }
)
