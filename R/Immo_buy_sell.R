##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de mettre a jour un ptf immo suite a une vente/achat d'immo
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------
SEUIL_DEL_IMMO <- 0.001
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           buy_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante d'un portefeuille immo suite a un achat d'un autre portefeuille immo.
##'
##' \code{buy_immo} est une methode permettant de mettre a jour le portefeuille immo suite a l'achat d'un autre portefeuille immobilier.
##' de chaque composante d'un portefeuille immo.
##' @name buy_immo
##' @docType methods
##' @param x objet de la classe Immo (decrivant le portefeuille immo en detention).
##' @param ptf_bought objet de la classe Immo (decrivant le portefeuille immo achete).
##' @return L'objet x complete des elements de ptf_bought.
##' @author Prim'Act
##' @export
##' @aliases immo

setGeneric(name = "buy_immo", def = function(x, ptf_bought){standardGeneric("buy_immo")})
setMethod(
  f = "buy_immo",
  signature = c(x = "Immo", ptf_bought = "Immo"),
  definition = function(x, ptf_bought){

    # Permet d'acheter un portefeuille lorsque l'initial est vide
    n_init <- 0
    if(nrow(x["ptf_immo"]) > 0) {n_init <- max(x["ptf_immo"][,"num_mp"])}
    n_bought <- nrow(ptf_bought["ptf_immo"])

    # Ne permet pas d'acheter un portefeuille vide :
    if(n_bought == 0) {stop("[Immo : buy] : Tentative d'acquisition d'un portefeuille vide \n")}

    ptf_bought["ptf_immo"][,"num_mp"] <- c((n_init+1):(n_init+n_bought))
    x["ptf_immo"] <- rbind(x["ptf_immo"], ptf_bought["ptf_immo"])

    validObject(x)
    return(x)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille immo suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_immo} est une methode permettant de mettre a jour chaque composante d'un portefeuille immo suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_immo
##' @docType methods
##' @param x objet de la classe Immo (decrivant le portefeuille immo en detention).
##' @param num_sold vecteur contenant le numero de model point des biens immobiliers du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return L'objet x mis a jour de l'operation de vente (suppression des lignes vendues).
##' @author Prim'Act
##' @export
##' @aliases immo

setGeneric(name = "sell_immo", def = function(x, num_sold, nb_sold){standardGeneric("sell_immo")})
setMethod(
  f = "sell_immo",
  signature = c(x = "Immo", num_sold = "numeric", nb_sold = "numeric"),
  definition = function(x, num_sold, nb_sold){
    # Verification des inputs
    if(length(num_sold)!=length(nb_sold)) {stop("[Immo : sell] : Les vecteurs num_sold et nb_sold doivent etre de même longueur.")}
    if(sum(num_sold %in% x["ptf_immo"][,"num_mp"])!= length(num_sold)) {stop("[Immo : sell] : Tentative de vente d'une immo non existante \n")}

    pmvr_part <- 0
    pmvr_tot  <- 0

    # Selection des lignes soumises a une operation de vente
    temp <- x["ptf_immo"][which(x["ptf_immo"][,"num_mp"] == num_sold),]

    # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
    if(sum(nb_sold > temp[,"nb_unit"]) > 0)   {stop("[Immo : sell] : Vente d'immo entraine une position negative \n")}
    if(sum(temp[,"cessible"] == F) > 0)       {stop("[Immo : sell] : Tentative de vente d'une immo non cessible \n")}
    if(sum(temp[,"presence"] == F) > 0)       {stop("[Immo : sell] : Tentative de vente d'une immo non presente \n")}

    # Premier cas : vente partielle d'une ligne immo
    temp_part               <- new("Immo",temp[which(temp[,"nb_unit"] > nb_sold),])
    num_sold_part           <- temp_part["ptf_immo"][,"num_mp"]
    nb_sold_part            <- nb_sold[which(num_sold %in% num_sold_part)]
    # Operations
    if(length(num_sold_part)>0){
      pmvr_part <-   sum((temp_part["ptf_immo"][,"val_marche"] - temp_part["ptf_immo"][,"val_nc"])  * nb_sold_part / temp_part["ptf_immo"][,"nb_unit"])
      temp_part["ptf_immo"][,"val_achat"]  <- temp_part["ptf_immo"][,"val_achat"]  * (1 - nb_sold_part / temp_part["ptf_immo"][,"nb_unit"])
      temp_part["ptf_immo"][,"val_marche"] <- temp_part["ptf_immo"][,"val_marche"] * (1 - nb_sold_part / temp_part["ptf_immo"][,"nb_unit"])
      temp_part["ptf_immo"][,"val_nc"]     <- temp_part["ptf_immo"][,"val_nc"]     * (1 - nb_sold_part / temp_part["ptf_immo"][,"nb_unit"])
      temp_part["ptf_immo"][,"nb_unit"]    <- temp_part["ptf_immo"][, "nb_unit"] - nb_sold_part
    }

    # Deuxieme cas : vente totale d'une ligne immo
    temp_tot     <- new("Immo",temp[which(temp[,"nb_unit"] == nb_sold),])
    num_sold_tot <- temp_tot["ptf_immo"][,"num_mp"]

    if(length(num_sold_tot) > 0) {pmvr_tot     <- sum(temp_tot["ptf_immo"][,"val_marche"] - temp_tot["ptf_immo"][,"val_nc"])}
    # Mise a jour de l'objet immo
    # 1 : Suppression des lignes immo integralement vendues
    if(length(num_sold_tot) > 0) {x["ptf_immo"] <- x["ptf_immo"][-which(x["ptf_immo"][,"num_mp"] %in% num_sold_tot),]}
    # 2 : Ajustement des lignes immo partiellement vendues
    if(length(num_sold_part) > 0) {x["ptf_immo"][which(x["ptf_immo"][,"num_mp"] %in% num_sold_part),] <- temp_part["ptf_immo"]}
    # 3 : Prevention en cas de portefeuille vide
    if(nrow(x["ptf_immo"]) == 0) {cat(" [Immo : sell] : Attention : portefeuille immo vide\n")}

    # 4 : vider les immo dont la valeur nette comptable est inferieure a 0.001
    x["ptf_immo"] <- x["ptf_immo"][which(x["ptf_immo"][,"val_nc"] > SEUIL_DEL_IMMO),]

    pmvr <- pmvr_part + pmvr_tot
    validObject(x)
    return(list(immo = x,pmvr = pmvr))
  }
)
