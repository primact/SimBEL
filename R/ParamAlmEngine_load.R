# Script permettant de charger la valeur initiale des ppb dans un objet de type Ppb

setGeneric(name = "param_alm_engine_load", def = function(file_alm_address, ptf_fin_ref){standardGeneric("param_alm_engine_load")})
setMethod(
    f = "param_alm_engine_load",
    signature = c("character","PortFin"),
    definition = function(file_alm_address, ptf_fin_ref){
        temp          <- read.csv2(file_alm_address)
        param_alm     <- new("ParamAlmEngine",
                             ptf_reference = ptf_fin_ref,
                             alloc_cible   = c(temp[,"alloc_action"],
                                               temp[,"alloc_immo"],
                                               temp[,"alloc_oblig"],
                                               temp[,"alloc_treso"]), 
                             seuil_realisation_PVL  = temp[,"seuil_realisation_PVL"])
        return(param_alm)
    }
)