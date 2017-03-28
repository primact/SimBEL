#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_rach_dyn : Methode de calcul des taux de rachat dynamique.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la composante rachats dynamique.
##'
##' \code{calc_rach_dyn} est une methode permettant de calculer la composante rachat dynamique
##' selon la methodologie transmise dans le ONC de l'ACPR de 2013.
##' @name calc_rach_dyn
##' @docType methods
##' @param p un objet de la classe \code{\link{ParamRachDyn}} contenant les parametres de rachats dynamiques.
##' @param tx_cible une valeur \code{numeric} correspondant au taux de revalorisation cible.
##' @param tx_serv une valeur \code{numeric} correspondant au taux de revalorisation servi.
##' @return La valeur du taux rachat.
##' @author Prim'Act
##' @include ParamRachDyn-class.R
##' @export


setGeneric("calc_rach_dyn", function(p, tx_cible, tx_serv){standardGeneric("calc_rach_dyn")})
setMethod(
  f = "calc_rach_dyn",
  signature = c(p = "ParamRachDyn", tx_cible = "numeric", tx_serv = "numeric"),
  def = function(p, tx_cible, tx_serv){

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








