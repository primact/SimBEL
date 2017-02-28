# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Ce script comprend les fonctions permettant de mettre a jour un ptf treso suite a une vente/achat d'treso
# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Suivi version
# # Version 1.0 du 23/01/2017. Fait par GK : initialisation
# #--------------------------------------------------------------------------------------------------------------------
#  SERT  A RIEN ???
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           buy_treso
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Mise a jour de chaque composante d'un portefeuille treso suite a un achat d'un autre portefeuille treso.
# ##'
# ##' \code{buy_treso} est une methode permettant de mettre a jour le portefeuille treso suite a l'achat d'un autre portefeuille treso.
# ##' de chaque composante d'un portefeuille treso.
# ##' @name buy_treso
# ##' @docType methods
# ##' @param x objet de la classe treso (decrivant le portefeuille treso en detention).
# ##' @param ptf_bought objet de la classe treso (decrivant le portefeuille treso achete).
# ##' @return L'objet x complete des elements de ptf_bought.
# ##' @author Prim'Act
# ##' @export
# ##' @aliases treso
# 
# setGeneric(name = "buy_treso", def = function(x, ptf_bought){standardGeneric("buy_treso")})
# setMethod(
#   f = "buy_treso",
#   signature = c(x = "Treso", ptf_bought = "Treso"),
#   definition = function(x, ptf_bought){
# 
#     # Permet d'acheter un portefeuille lorsque l'initial est vide
#     n_init <- 0
#     if(nrow(x["ptf_treso"]) > 0) {n_init <- max(x["ptf_treso"][,"num_mp"])}
#     n_bought <- nrow(ptf_bought["ptf_treso"])
# 
#     # Ne permet pas d'acheter un portefeuille vide :
#     if(n_bought == 0) {stop("[Treso : buy] : Tentative d'acquisition d'un portefeuille vide \n")}
# 
#     ptf_bought["ptf_treso"][,"num_mp"] <- c((n_init+1):(n_init+n_bought))
#     x["ptf_treso"] <- rbind(x["ptf_treso"], ptf_bought["ptf_treso"])
# 
#     validObject(x)
#     return(x)
#   }
# )
# 
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# #           sell_treso
# #----------------------------------------------------------------------------------------------------------------------------------------------------
# ##' Mise a jour de chaque composante du portefeuille treso suite a la vente de tout ou partie de ce portefeuille.
# ##'
# ##' \code{sell_treso} est une methode permettant de mettre a jour chaque composante d'un portefeuille treso suite a la vente
# ##' de tout ou partie de ce portefeuille.
# ##' @name sell_treso
# ##' @docType methods
# ##' @param x objet de la classe treso (decrivant le portefeuille treso en detention).
# ##' @param num_sold vecteur contenant le numero de model point treso du portefeuille que l'on souhaite vendre.
# ##' @param val_sold vecteur contenant le montant que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
# ##' @return L'objet x mis a jour de l'operation de vente (suppression des lignes vendues).
# ##' @author Prim'Act
# ##' @export
# ##' @aliases treso
# 
# setGeneric(name = "sell_treso", def = function(x, num_sold, val_sold){standardGeneric("sell_treso")})
# setMethod(
#   f = "sell_treso",
#   signature = c(x = "Treso", num_sold = "numeric", val_sold = "numeric"),
#   definition = function(x, num_sold, val_sold){
#     # Verification des inputs
#     if(length(num_sold)!=length(val_sold)) {stop("[Treso : sell] : Les vecteurs num_sold et val_sold doivent etre de meme longueur \n")}
#     if(sum(num_sold %in% x["ptf_treso"][,"num_mp"])!= length(num_sold)) {stop("[Treso : sell] : Tentative de vente d'une treso non existante \n")}
# 
#     # Selection des lignes soumises a une operation de vente
#     temp <- x["ptf_treso"][which(x["ptf_treso"][,"num_mp"] %in% num_sold),]
# 
#     # Verification qu'on ne vende pas plus que ce qu'on ne possede
#     if(sum(val_sold > temp[,"val_marche"]) > 0) {stop("[Treso : sell] : Vente de treso entrainant une position negative \n")}
# 
#     # Premier cas : vente partielle d'une ligne treso
#     temp_part               <- new("Treso",temp[which(temp[,"val_marche"] > val_sold),])
#     num_sold_part           <- temp_part["ptf_treso"][,"num_mp"]
#     val_sold_part           <- val_sold[which(num_sold %in% num_sold_part)]
#     # Operations
#     if(length(num_sold_part)>0){
#       temp_part["ptf_treso"][,"val_nc"]     <- temp_part["ptf_treso"][,"val_nc"] * (1 - val_sold_part / temp_part["ptf_treso"][,"val_marche"])
#       temp_part["ptf_treso"][,"val_marche"] <- temp_part["ptf_treso"][,"val_marche"]  - val_sold_part
#     }
# 
#     # Deuxieme cas : vente totale d'une ligne treso
#     temp_tot     <- new("Treso",temp[which(temp[,"nb_unit"] == nb_sold),])
#     num_sold_tot <- temp_tot["ptf_treso"][,"num_mp"]
# 
#     # Mise a jour de l'objet treso
#     # 1 : Suppression des lignes treso integralement vendues
#     if (length(num_sold_tot) > 0) {x["ptf_treso"] <- x["ptf_treso"][-which(x["ptf_treso"][,"num_mp"] %in% num_sold_tot),]}
#     # 2 : Ajustement des lignes tresos partiellement vendues
#     if(length(num_sold_part) > 0) {x["ptf_treso"][which(x["ptf_treso"][,"num_mp"] %in% num_sold_part),] <- temp_part["ptf_treso"]}
#     # 3 : Message de prevention si le portefeuille est vide
#     if(nrow(x["ptf_treso"]) == 0) {cat(" [treso : sell] : Attention : portefeuille treso vide\n")}
# 
#     validObject(x)
#     return(x)
#   }
# )
