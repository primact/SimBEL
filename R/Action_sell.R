# 10/03/2017 Guillaume de Kervenoael

SEUIL_DEL_ACTION <- .001
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille action suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_action} est une methode permettant de mettre a jour chaque composante d'un portefeuille action suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_action
##' @docType methods
##' @param x objet de la classe \code{Action} (decrivant le portefeuille action en detention).
##' @param num_sold vecteur de type \code{numeric} contenant le numero de model point action du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur de type \code{numeric} contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return L'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @author Prim'Act
##' @export
##' @aliases Action
##' @include Action_class.R

setGeneric(name = "sell_action", def = function(x, num_sold, nb_sold){standardGeneric("sell_action")})
setMethod(
    f = "sell_action",
    signature = c(x = "Action", num_sold = "numeric", nb_sold = "numeric"),
    definition = function(x, num_sold, nb_sold){
        # Verification des inputs
        if(length(num_sold)!=length(nb_sold)) {stop("[Action : sell] : Les vecteurs num_sold et nb_sold doivent etre de meme longueur \n")}
        if(sum(num_sold %in% x["ptf_action"][,"num_mp"])!= length(num_sold)) {stop("[Action : sell] : Tentative de vente d'une action non existante \n")}
        
        pmvr_part <- 0
        pmvr_tot  <- 0
        
        # Selection des lignes soumises a une operation de vente
        temp <- x["ptf_action"][which(x["ptf_action"][,"num_mp"] %in% num_sold),]
        
        # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
        if(sum(nb_sold > temp[,"nb_unit"]) > 0)   {stop("[Action : sell] : Vente d'action entraine une position negative \n")}
        if(sum(temp[,"cessible"] == F) > 0)       {stop("[Action : sell] : Tentative de vente d'une action non cessible \n")}
        if(sum(temp[,"presence"] == F) > 0)       {stop("[Action : sell] : Tentative de vente d'une action non presente \n")}
        
        # Premier cas : vente partielle d'une ligne action
        temp_part               <- new("Action",temp[which(temp[,"nb_unit"] > nb_sold),])
        num_sold_part           <- temp_part["ptf_action"][,"num_mp"]
        nb_sold_part            <- nb_sold[which(num_sold %in% num_sold_part)]
        # Operations
        if(length(num_sold_part) > 0){
            pmvr_part <-   sum((temp_part["ptf_action"][,"val_marche"] - temp_part["ptf_action"][,"val_nc"])  * nb_sold_part / temp_part["ptf_action"][,"nb_unit"])
            temp_part["ptf_action"][,"val_achat"]  <- temp_part["ptf_action"][,"val_achat"]  * (1 - nb_sold_part / temp_part["ptf_action"][,"nb_unit"])
            temp_part["ptf_action"][,"val_marche"] <- temp_part["ptf_action"][,"val_marche"] * (1 - nb_sold_part / temp_part["ptf_action"][,"nb_unit"])
            temp_part["ptf_action"][,"val_nc"]     <- temp_part["ptf_action"][,"val_nc"]     * (1 - nb_sold_part / temp_part["ptf_action"][,"nb_unit"])
            temp_part["ptf_action"][,"nb_unit"]    <- temp_part["ptf_action"][, "nb_unit"] - nb_sold_part
        }
        
        # Deuxieme cas : vente totale d'une ligne action
        temp_tot     <- new("Action", temp[which(temp[,"nb_unit"] == nb_sold),])
        num_sold_tot <- temp_tot["ptf_action"][,"num_mp"]
        if (length(num_sold_tot) > 0) {pmvr_tot <- sum(temp_tot["ptf_action"][,"val_marche"] - temp_tot["ptf_action"][,"val_nc"])}
        
        # Mise a jour de l'objet action
        # 1 : Suppression des lignes action integralement vendues
        if (length(num_sold_tot) > 0) {x["ptf_action"] <- x["ptf_action"][-which(x["ptf_action"][,"num_mp"] %in% num_sold_tot),]}
        # 2 : Ajustement des lignes actions partiellement vendues
        if(length(num_sold_part) > 0) {x["ptf_action"][which(x["ptf_action"][,"num_mp"] %in% num_sold_part),] <- temp_part["ptf_action"]}
        # 3 : Message de prevention si le portefeuille est vide
        if(nrow(x["ptf_action"]) == 0) {cat(" [Action : sell] : Attention : portefeuille action vide\n")}
        
        # 4 : vider les actions dont la valeur nette comptable est inferieure a 0.001
        x["ptf_action"] <- x["ptf_action"][which(x["ptf_action"][,"val_nc"] > SEUIL_DEL_ACTION),]
        
        
        pmvr <- pmvr_part + pmvr_tot
        validObject(x)
        return(list(action = x, pmvr = pmvr))
    }
)
