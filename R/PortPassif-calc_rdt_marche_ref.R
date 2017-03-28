#----------------------------------------------------------
# Ce script comprend les methodes de calcul du taux de rendement de reference au niveau du marche
#----------------------------------------------------------

##' Calcul du taux de rendement de reference au niveau du marche
##'
##' \code{calc_rdt_marche_ref} est une methode permettant de calculer un taux cible.
##' @name calc_rdt_marche_ref
##' @docType methods
##' @param param_comport un objet de la classe \code{ParamComport}.
##' @param mp_esg est un objet de type \code{ModelPointESG}, qui represente la situation courante
##' en annee et simulations des valeurs de l'ESG.
##' @return Une liste contenant les rendements de reference du marche.
##' @author Prim'Act
##' @export

setGeneric(name = "calc_rdt_marche_ref", def = function(x, mp_esg){
  standardGeneric("calc_rdt_marche_ref")})
setMethod(
    f = "calc_rdt_marche_ref",
    signature = c(x = "ParamComport", mp_esg = "ModelPointESG"),
    definition = function(x, mp_esg){

        # Verification des inputs
        if(x@ind_ref_action > nrow(mp_esg@indice_action)) {stop("[calc_rdt_passif] :  L'indice action renseigne n'est pas present dans le model point ESG. \n")}
        if(x@ind_ref_immo   > nrow(mp_esg@indice_immo))   {stop("[calc_rdt_passif] :  L'indice immo renseigne n'est pas present dans le model point ESG. \n")}

        # Calcul de rdt actions, immobilier et treso
        rdt_action <- mp_esg@indice_action[x@ind_ref_action,"S_action"] /
          mp_esg@indice_action[x@ind_ref_action,"S_prev_action"] - 1
        rdt_immo   <- mp_esg@indice_immo[x@ind_ref_immo,"S_immo"] /
          mp_esg@indice_immo[x@ind_ref_immo,"S_prev_immo"] - 1
        if (x@mat_oblig > 1) { rdt_oblig  <- mp_esg["yield_curve"][x@mat_oblig + 1]
        } else {            rdt_oblig  <- 0}
        rdt_treso <- mp_esg["yield_curve"][2]

        return(list(rdt_oblig = rdt_oblig,
                    rdt_action = rdt_action,
                    rdt_immo = rdt_immo,
                    rdt_tre = rdt_treso))
    }
)
