#
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           calc_flux_milieu
# #----------------------------------------------------------------------------------------------------------------------------------------------------
#
# # Cette fonction permet de calculer le revenu de l'actif de milieu d'annee (coupon, loyer, dividende)
# setGeneric(name = "calc_flux_milieu", def = function(x, new_mp_ESG){standardGeneric("calc_flux_milieu")})
# setMethod(
#   f = "calc_flux_milieu",
#   signature = c(x = "PortFin", new_mp_ESG = "ModelPointESG"),
#   definition = function(x, new_mp_ESG){
#     # Calcul des tombees + coupons  + suppression des maturites residuelles negatives => a besoin de renvoyer le portefeuille dans son etat
#     flux_oblig <- calc_flux_annee(x["ptf_oblig"])
#     coupon     <- sum(flux_oblig[[1]])
#
#     # Calcul des dividendes et loyers
#     table_rdt  <- calc_rdt(x, new_mp_ESG)
#     dividende  <- sum(table_rdt[["rdt_action"]][["div"]])
#     loyer      <- sum(table_rdt[["rdt_immo"]][["loyer"]])
#
#     revenu <- coupon + dividende + loyer
#     return(revenu)
#   }
# )

# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           calc_term_flow
# #----------------------------------------------------------------------------------------------------------------------------------------------------
#
# # Cette fonction calcule le montant des flux d'echeance de remboursement obligataire de l'annee pour un objet PortFin
# setGeneric(name = "calc_term_flow", def = function(x){standardGeneric("calc_term_flow")})
# setMethod(
#   f = "calc_term_flow",
#   signature = c(x = "PortFin"),
#   definition = function(x){
#     # Mise a jour de l'attribut ptf_oblig suite aux eventuelles tombees d'echeances
#     echeance <- sum(calc_flux_annee(x["ptf_oblig"])[[2]])
#     return(echeance)
#   }
# )
