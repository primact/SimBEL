#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de mettre a jour un ptf action suite a une vente/achat d'action
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------
SEUIL_DEL_ACTION <- 0.001
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           buy_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante d'un portefeuille action suite a un achat d'un autre portefeuille action.
##'
##' \code{buy_action} est une methode permettant de mettre a jour le portefeuille action suite a l'achat d'un autre portefeuille action.
##' de chaque composante d'un portefeuille action.
##' @name buy_action
##' @docType methods
##' @param x objet de la classe Action (decrivant le portefeuille action en detention).
##' @param ptf_bought objet de la classe Action (decrivant le portefeuille action achete).
##' @return L'objet x complete des elements de ptf_bought.
##' @author Prim'Act
##' @export
##' @aliases Action

setGeneric(name = "buy_action", def = function(x, ptf_bought){standardGeneric("buy_action")})
setMethod(
  f = "buy_action",
  signature = c(x = "Action", ptf_bought = "Action"),
  definition = function(x, ptf_bought){

    # Permet d'acheter un portefeuille lorsque l'initial est vide
    n_init <- 0
    if(nrow(x["ptf_action"]) > 0) {n_init <- max(x["ptf_action"][,"num_mp"])}
    n_bought <- nrow(ptf_bought["ptf_action"])

    # Ne permet pas d'acheter un portefeuille vide :
    if(n_bought == 0) {stop("[Action : buy] : Tentative d'acquisition d'un portefeuille vide \n")}

    ptf_bought["ptf_action"][,"num_mp"] <- c((n_init+1):(n_init+n_bought))
    x["ptf_action"] <- rbind(x["ptf_action"], ptf_bought["ptf_action"])

    validObject(x)
    return(x)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille action suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_action} est une methode permettant de mettre a jour chaque composante d'un portefeuille action suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_action
##' @docType methods
##' @param x objet de la classe Action (decrivant le portefeuille action en detention).
##' @param num_sold vecteur contenant le numero de model point action du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return L'objet x mis a jour de l'operation de vente (suppression des lignes vendues).
##' @author Prim'Act
##' @export
##' @aliases Action

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


# fonction de calcul des PMVL_action a realiser (i.e. vente puis rachat des meme elements)
# Verifier qu on a assez de PMVL
# filtrer celles qui sont en PVL
# vendre le montant proportionnellement
# remettre a jour le ptf
# x <- PtfFin_4@ptf_action
# montant <- 35000
setGeneric(name = "sell_pvl_action", def = function(x, montant){standardGeneric("sell_pvl_action")})
setMethod(
    f = "sell_pvl_action",
    signature = c(x = "Action", montant = "numeric"),
    definition = function(x, montant){
        # Verification des inputs
        if(nrow(x@ptf_action) > 0) {
            pvl_action <- calc_pmvl_action(x)$pvl 
            if(pvl_action < montant) {stop("[Action : sell_pvl_action] : Tentative de realisation de plus de plus value latente que ce que le portefeuille contient. \n")}
        
            # Selection des lignes soumises a une operation de realisation des pvl
            temp              <- x@ptf_action[which(x@ptf_action$val_marche > x@ptf_action$val_nc),]
            num_mp_pvl        <- temp$num_mp 
            vecteur_poids_pvl <- (temp$val_marche - temp$val_nc)/pvl_action
        
            # Realisation des pvl de maniere proportionnelle
            # Reviens uniquement a vendre et a racheter immediatement : seule la val_nc et la val_achat sont augmentees de montant * vecteur_poids_pvl 
            identifiant <- which(x@ptf_action$num_mp %in% num_mp_pvl)
            x@ptf_action$val_nc[identifiant]    <- x@ptf_action$val_nc[identifiant] + vecteur_poids_pvl * montant
            x@ptf_action$val_achat[identifiant] <- x@ptf_action$val_achat[identifiant] + vecteur_poids_pvl * montant
            return(list(action = x, pmvr = montant))
        } else {
            return(list(action = x, pmvr = 0))
        }
    }
)

