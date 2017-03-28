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

setGeneric(name = "buy_action", def = function(x, ptf_bought){standardGeneric("buy_action")})
setMethod(
  f = "buy_action",
  signature = c(x = "Action", ptf_bought = "Action"),
  definition = function(x, ptf_bought){

    nom_table <- names(x@ptf_action)
    num_mp    <- which(nom_table == "num_mp")

    # Permet d'acheter un portefeuille lorsque l'initial est vide
    n_init <- 0
    if(nrow(x@ptf_action) > 0) {n_init <- max(.subset2(x@ptf_action, num_mp))}
    n_bought <- nrow(ptf_bought@ptf_action)

    # Ne permet pas d'acheter un portefeuille vide :
    if(n_bought == 0) {stop("[Action : buy] : Tentative d'acquisition d'un portefeuille vide \n")}

    ptf_bought@ptf_action$num_mp <- c((n_init + 1):(n_init + n_bought))
    x@ptf_action <- rbind(x@ptf_action, ptf_bought@ptf_action)

    return(x)
  }
)
