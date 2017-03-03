
#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_rach_dyn : Methode de la classe ParamRachDynt
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la composante rachats dynamique
##'
##' \code{calc_rach_dyn} est une methode permettant de calculer la composante rachats dynamique.
##' @name calc_rach_dyn
##' @docType methods
##' @param p un objet de la classe \code{ParamRachDyn} contenant les parametres de rachats dynamiques.
##' @param tx_cible : taux cible en numerique
##' @param tx_serv : taux servi en numerique
##' @return la composante rachats dynamique en numerique
##' @author Prim'Act
##' @export
##' @aliases ParamRachDyn


setGeneric("calc_rach_dyn",function(p,tx_cible,tx_serv){standardGeneric("calc_rach_dyn")})
setMethod(
  f = "calc_rach_dyn",
  signature = c(p="ParamRachDyn",tx_cible="numeric",tx_serv="numeric"),
  def = function(p,tx_cible,tx_serv){

    # # Ajout de test sur le format
    # if(tx_cible < 0){
    #   stop("Le taux cible doit etre superieur a 0")
    # }
    #
    # # Ajout de test sur le format
    # if(tx_serv < 0){
    #   stop("Le taux servi doit etre superieur a 0")
    # }

    RC <- 0
    v <- as.numeric(p@vec_param)

    if((tx_serv-tx_cible)<=v[1]){
      RC <- v[6]

    }else if(((tx_serv-tx_cible)<=v[2]) & ((tx_serv-tx_cible)>v[1])){
      RC <- v[6]*(tx_serv-tx_cible-v[2])/(v[1]-v[2])

    }else if(((tx_serv-tx_cible)<=v[3]) & ((tx_serv-tx_cible)>v[2])){
      RC <- 0

    }else if(((tx_serv-tx_cible)<=v[4]) & ((tx_serv-tx_cible)>v[3])){
      RC <- v[5]*(tx_serv-tx_cible-v[3])/(v[4]-v[3])

    }else if((tx_serv-tx_cible)>v[4]){
      RC <- v[5]
    }
    return(RC)

  }
)








