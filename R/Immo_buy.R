#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de mettre a jour un ptf action suite a une vente/achat d'action
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           buy_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante d'un portefeuille immo suite a un achat d'un autre portefeuille immo.
##'
##' \code{buy_immo} est une methode permettant de mettre a jour le portefeuille immo suite a l'achat d'un autre portefeuille immo.
##' de chaque composante d'un portefeuille immo.
##' @name buy_immo
##' @docType methods
##' @param x objet de la classe \code{Immo} (decrivant le portefeuille immo en detention).
##' @param ptf_bought objet de la classe \code{Immo} (decrivant le portefeuille immo achete).
##' @return L'objet \code{x} complete des elements de \code{ptf_bought}.
##' @author Prim'Act
##' @export
##' @aliases Immo
##' @include Immo_class.R

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
