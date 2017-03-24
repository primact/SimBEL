#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de mettre a jour un ptf obligataire suite a une vente/achat ou vieillissement d'oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille obligation suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_oblig} est une methode permettant de mettre a jour chaque composante d'un portefeuille obligation suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_oblig
##' @docType methods
##' @param x objet de la classe \code{Oblig} (decrivant le portefeuille obligation en detention).
##' @param num_sold vecteur de type \code{numeric} contenant le numero de model point obligation du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur de type \code{numeric} contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return L'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "sell_oblig", def = function(x, num_sold, nb_sold){standardGeneric("sell_oblig")})
setMethod(
  f = "sell_oblig",
  signature = c(x = "Oblig", num_sold = "numeric", nb_sold = "numeric"),
  definition = function(x, num_sold, nb_sold){
    #Verification des inputs
    if(length(num_sold)!=length(nb_sold)) {stop("[Oblig : sell] : Les vecteurs num_sold et nb_sold doivent etre de même longueur.")}
    if(sum(num_sold %in% x["ptf_oblig"][,"num_mp"])!= length(num_sold)) {stop("[Oblig : sell] : Tentative de vente d'une oblig non existante \n")}

    pmvr_part <- 0
    pmvr_tot  <- 0

    # Selection des lignes soumises a une operation de vente
    temp <- x["ptf_oblig"][which(x["ptf_oblig"][,"num_mp"] == num_sold),]

    # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
    if(sum(nb_sold > temp[,"nb_unit"]) > 0)   {stop("[Oblig : sell] : Vente d'oblig entraine une position negative \n")}
    if(sum(temp[,"cessible"] == F) > 0)       {stop("[Oblig : sell] : Tentative de vente d'une obligation non cessible \n")}
    if(sum(temp[,"presence"] == F) > 0)       {stop("[Oblig : sell] : Tentative de vente d'une obligation non presente \n")}

    # Premier cas : vente partielle d'une ligne obligataire
    temp_part               <- new("Oblig",temp[which(temp[,"nb_unit"] > nb_sold),])
    num_sold_part           <- temp_part["ptf_oblig"][,"num_mp"]
    nb_sold_part            <- nb_sold[which(num_sold %in% num_sold_part)]

    # Operations
    if(length(num_sold_part) > 0){
      pmvr_part <-   sum((temp_part["ptf_oblig"][,"val_marche"] - temp_part["ptf_oblig"][,"val_nc"])  * nb_sold_part / temp_part["ptf_oblig"][,"nb_unit"])
      temp_part["ptf_oblig"][,"val_achat"]  <- temp_part["ptf_oblig"][,"val_achat"]  * (1 - nb_sold_part / temp_part["ptf_oblig"][,"nb_unit"])
      temp_part["ptf_oblig"][,"val_marche"] <- temp_part["ptf_oblig"][,"val_marche"] * (1 - nb_sold_part / temp_part["ptf_oblig"][,"nb_unit"])
      temp_part["ptf_oblig"][,"val_nc"]     <- temp_part["ptf_oblig"][,"val_nc"]  * (1 - nb_sold_part / temp_part["ptf_oblig"][,"nb_unit"])
      temp_part["ptf_oblig"][,"nb_unit"]    <- temp_part["ptf_oblig"][, "nb_unit"] - nb_sold_part
      # df_surcote_decote_vnc                 <- calc_sur_dec(temp_part)
      # temp_part["ptf_oblig"][,"sd"]         <- df_surcote_decote_vnc[,"surcote_decote"]
      
    }

    # Deuxieme cas : vente totale d'une ligne obligataire
    temp_tot     <- new("Oblig",temp[which(temp[,"nb_unit"] == nb_sold),])
    num_sold_tot <- temp_tot["ptf_oblig"][,"num_mp"]

    if(length(num_sold_tot) > 0) {pmvr_tot <- sum(temp_tot["ptf_oblig"][,"val_marche"] - temp_tot["ptf_oblig"][,"val_nc"])}
    # Mise a jour de l'objet Oblig
    # 1 : Suppression des lignes obligataires integralement vendues
    if(length(num_sold_tot) > 0) {x["ptf_oblig"] <- x["ptf_oblig"][-which(x["ptf_oblig"][,"num_mp"] %in% num_sold_tot),]}
    # 2 :
    x["ptf_oblig"][which(x["ptf_oblig"][,"num_mp"] %in% num_sold_part),] <- temp_part["ptf_oblig"]

    if(nrow(x["ptf_oblig"]) == 0) {cat(" [Oblig : sell] : Attention : portefeuille obligataire vide\n")}

    pmvr <- pmvr_part + pmvr_tot
    validObject(x)
    return(list(oblig = x, pmvr = pmvr))
  }
)
