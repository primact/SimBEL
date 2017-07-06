#----------------------------------------------------------------------------------------------------------------------------------------------------
#           buy_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante d'un portefeuille action suite a un achat d'un autre portefeuille immobilier.
##'
##' \code{buy_immo} est une methode permettant de mettre a jour le portefeuille action suite a l'achat d'un autre portefeuille immobilier.
##' de chaque composante d'un portefeuille action.
##' @name buy_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille immo en detention).
##' @param ptf_bought objet de la classe \code{\link{Immo}} (decrivant le portefeuille immo achete).
##' @return L'objet \code{x} complete des elements de \code{ptf_bought}.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "buy_immo", def = function(x, ptf_bought){standardGeneric("buy_immo")})
setMethod(
    f = "buy_immo",
    signature = c(x = "Immo", ptf_bought = "Immo"),
    definition = function(x, ptf_bought){

        # Donnees
        ptf_immo <- x@ptf_immo
        nom_table <- names(ptf_immo)
        num_mp    <- which(nom_table == "num_mp")

        # Permet d'acheter un portefeuille lorsque l'initial est vide
        if(nrow(ptf_immo) > 0L)
            n_init <- max(.subset2(ptf_immo, num_mp))
        else
            n_init <- 0

        # Nombre d'immo achetees
        n_bought <- nrow(ptf_bought@ptf_immo)

        # Ne permet pas d'acheter un portefeuille vide :
        if(n_bought == 0L) stop("[Immo : buy] : Tentative d'acquisition d'un portefeuille vide \n")

        # Mise a jour des numeros du PTF
        ptf_bought@ptf_immo$num_mp <- (n_init + 1L):(n_init + n_bought)

        # Mise a jour du PTF
        x@ptf_immo <- rbind(ptf_immo, ptf_bought@ptf_immo)

        # Output
        return(x)
    }
)
