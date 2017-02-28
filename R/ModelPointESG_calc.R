# setGeneric("calc_coef_inflation", def = function(mp_ESG, annee){standardGeneric("calc_coef_inflation")})
# setMethod(
#     f = "calc_coef_inflation",
#     signature = c(mp_ESG = "ModelPointESG", annee = "integer"),
#     definition = function(mp_ESG, annee){
#         # Verification des inputs
#         if(length(annee) != 1) {stop("[ModelPoint_ESG : calc_deflateur_inflation] : L'input d'annee est incorrectement renseigne \n")}
#         if(length(mp_ESG@indice_inflation) > annee) { stop("[ModelPoint_ESG : calc_deflateur_inflation] : La courbe d'inflation ne contient pas assez d'element")}
#
#         return((1 + mp_ESG@indice_inflation[annee]) ^ (annee))
#     }
# )
#
# #
# # setGeneric("calc_rd_ref", def = function(mp_ESG, nom_index_ref){standardGeneric("calc_rd_ref")})
# # setMethod(
# #   f = "calc_coef_inflation",
# #   signature = c(mp_ESG = "ModelPointESG", nom_index_ref = "list"),
# #   definition = function(mp_ESG, nom_index_ref){
# #
# #     # Verification des inputs
# #     if(length(nom_index_ref) != 4) {stop("[ModelPoint_ESG : calc_rd_ref] : quatre noms d'indice doivent etre renseignes \n")}
# #
# #     # Numero d'indice
# #     index_action
# #     index_immo
# #     index_treso
# #
# #
# #     rdt_action <- mp_ESG@indice_action$
# #
# #       [index_action[1:length(index_action)], "S_action"]
# #     indice_action[1:length(index_action), "S_prev_action"] <- mp_ESG["indice_action"][index_action[1:length(index_action)], "S_prev_action"]
# #
# #
# #     return((1 + mp_ESG@indice_inflation[annee]) ^ (annee))
# #   }
# # )
