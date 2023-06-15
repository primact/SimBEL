#----------------------------------------------------------------------------------------------------------------------------------------------------
#           buy_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante d'un portefeuille action suite a un achat d'un autre portefeuille action.
##'
##' \code{buy_action} est une methode permettant de mettre a jour le portefeuille action suite a l'achat d'un autre portefeuille action.
##' de chaque composante d'un portefeuille action.
##' @name buy_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}} (decrivant le portefeuille action en detention).
##' @param ptf_bought objet de la classe \code{\link{Action}} (decrivant le portefeuille action achete).
##' @return L'objet \code{x} complete des elements de \code{ptf_bought}.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "buy_action", def = function(x, ptf_bought) {
    standardGeneric("buy_action")
})
setMethod(
    f = "buy_action",
    signature = c(x = "Action", ptf_bought = "Action"),
    definition = function(x, ptf_bought) {
        # Donnees
        ptf_action <- x@ptf_action
        nom_table <- names(ptf_action)
        num_mp <- which(nom_table == "num_mp")

        # Permet d'acheter un portefeuille lorsque l'initial est vide
        if (nrow(ptf_action) > 0L) {
            n_init <- max(.subset2(ptf_action, num_mp))
        } else {
            n_init <- 0
        }

        # Nombre d'actions achetees
        n_bought <- nrow(ptf_bought@ptf_action)

        # Ne permet pas d'acheter un portefeuille vide :
        if (n_bought == 0L) stop("[Action : buy] : Tentative d'acquisition d'un portefeuille vide \n")

        # Mise a jour des numeros du PTF
        ptf_bought@ptf_action$num_mp <- (n_init + 1L):(n_init + n_bought)

        # Mise a jour PTF
        x@ptf_action <- rbind(ptf_action, ptf_bought@ptf_action)

        # Output
        return(x)
    }
)
