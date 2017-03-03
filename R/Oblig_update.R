
setGeneric(name = "update_vm_oblig", def = function(x,vm){standardGeneric("update_vm_oblig")})
setMethod(
  f = "update_vm_oblig",
  signature = c(x = "Oblig", vm = "numeric"),
  definition = function(x, vm){
    # Verification des inputs
    if (nrow(x["ptf_oblig"]) != length(vm)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
    if(sum(vm < 0) > 0) { stop("[Oblig : update_vm_oblig] :  Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_oblig"][,"val_marche"] <- vm
    return(x)
  }
)

setGeneric(name = "update_vnc_oblig", def = function(x,vnc){standardGeneric("update_vnc_oblig")})
setMethod(
  f = "update_vnc_oblig",
  signature = c(x = "Oblig", vnc = "numeric"),
  definition = function(x, vnc){
    # Verification des inputs
    if (nrow(x["ptf_oblig"]) != length(vnc)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
    if(sum(vnc < 0) > 0) { stop("[Oblig : update_vnc_oblig] :  Le vecteur de VNC initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_oblig"][,"val_nc"] <- vnc
    return(x)
  }
)

setGeneric(name = "update_sd_oblig", def = function(x,sd){standardGeneric("update_sd_oblig")})
setMethod(
  f = "update_sd_oblig",
  signature = c(x = "Oblig", sd = "numeric"),
  definition = function(x, sd){
    # Verification des inputs
    if (nrow(x["ptf_oblig"]) != length(sd)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
    x["ptf_oblig"][,"sd"] <- sd
    return(x)
  }
)
setGeneric(name = "update_dur_oblig", def = function(x,duration){standardGeneric("update_dur_oblig")})
setMethod(
  f = "update_dur_oblig",
  signature = c(x = "Oblig", duration = "numeric"),
  definition = function(x, duration){
    # Verification des inputs
    if (nrow(x["ptf_oblig"]) != length(duration)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
    if(sum(duration < 0) > 0) { stop("[Oblig : update_dur_oblig] :  Le vecteur de duration initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_oblig"][,"duration"] <- duration
    return(x)
  }
)
setGeneric(name = "update_zsp_oblig", def = function(x,zspread){standardGeneric("update_zsp_oblig")})
setMethod(
  f = "update_zsp_oblig",
  signature = c(x = "Oblig", zspread = "numeric"),
  definition = function(x, zspread){
    # Verification des inputs
    if (nrow(x["ptf_oblig"]) != length(zspread)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
    x["ptf_oblig"][,"zspread"] <- zspread
    return(x)
  }
)
# Cette fonction renvoie uniquement l'objet mis ajour des maturites residuelles !!! GERER LA SUPPRESSION DES OBLIGS ARRIVEES AUX TERMES
# Verifier que l'appel fait bien ressortir les
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_mat_res
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la maturite residuelle et de la duree de detention de chaque composante d'un portefeuille obligataire.
##'
##' \code{update_mat_res} est une methode permettant de mettre a jour la maturite residuelle et la duree de detention
##' de chaque composante d'un portefeuille obligataire.
##' @name update_mat_res
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuille obligataire).
##' @return L'objet x dont
##' \describe{
##' \item{\code{mat_res : } { est diminuee d'une unite (une unite correspond a un an)}}
##' \item{\code{dur_det : } { est augmentee d'une unite (une unite correspond a un an)}}
##' @author Prim'Act
##' @export
##' @aliases Oblig

setGeneric(name = "update_mat_res", def = function(x){standardGeneric("update_mat_res")})
setMethod(
  f = "update_mat_res",
  signature = c(x = "Oblig"),
  definition = function(x){
    x["ptf_oblig"][,"mat_res"] <- x["ptf_oblig"][,"mat_res"] - 1
    x["ptf_oblig"][,"dur_det"] <- x["ptf_oblig"][,"dur_det"] + 1

    # Operation de suppression des elements de maturite residuelle negative apres mise a jour
    if(length(which(x["ptf_oblig"][,"mat_res"] <= 0))>0) {
      num_del         <- x["ptf_oblig"][which(x["ptf_oblig"][,"mat_res"] <= 0),"num_mp"]
      x["ptf_oblig"]  <- x["ptf_oblig"][which(!(x["ptf_oblig"][,"num_mp"] %in% num_del)),]
    }
    # Reordonnancement des num_mp et verification que l'on ne vide pas le portefeuille
    if(nrow(x["ptf_oblig"]) == 0) {cat(" [Oblig : update_mat_res] : Attention : portefeuille obligataire vide\n")}
    return(x)
  }
)

setGeneric(name = "update_cc_oblig", def = function(x,coupon){standardGeneric("update_cc_oblig")})
setMethod(
  f = "update_cc_oblig",
  signature = c(x = "Oblig", coupon = "numeric"),
  definition = function(x, coupon){
    # Verification des inputs
    if (nrow(x["ptf_oblig"]) != length(coupon)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
    x["ptf_oblig"][,"cc"] <- coupon
    return(x)
  }
)
