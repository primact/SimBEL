#----------------------------------------------------------------------------------------------------------------------------------------------------
#           buy_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante d'un portefeuille obligataire suite a un achat d'un autre portefeuille obligataire.
##'
##' \code{buy_oblig} est une methode permettant de mettre a jour le portefeuille obligataire suite a l'achat d'un autre portefeuille obligataire.
##' de chaque composante d'un portefeuille obligataire.
##' @name buy_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire en detention).
##' @param ptf_bought objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire achete).
##' @return L'objet \code{x} complete des elements de \code{ptf_bought}.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R


setGeneric(name = "buy_oblig", def = function(x, ptf_bought){standardGeneric("buy_oblig")})
setMethod(
    f = "buy_oblig",
    signature = c(x = "Oblig", ptf_bought = "Oblig"),
    definition = function(x, ptf_bought){

        # Donnees
        ptf_oblig <- x@ptf_oblig
        nom_table <- names(ptf_oblig)
        num_mp <- which(nom_table == "num_mp")

        # Permet d'acheter un portefeuille lorsque l'initial est vide
        if(nrow(ptf_oblig) > 0L)
            n_init <- max(.subset2(ptf_oblig, num_mp))
        else
            n_init <- 0

        # Nombre d'oblig achetees
        n_bought <- nrow(ptf_bought@ptf_oblig)

        if(n_bought == 0L) stop("[Oblig : buy_oblig] : Tentative d'achat d'un portefeuille vide\n")

        # Mise a jour des numeros du PTF
        ptf_bought@ptf_oblig$num_mp <- (n_init + 1L):(n_init + n_bought)

        # Mise a jour de la surcote decote du portefeuille achete en date d'achat
        ptf_bought <- update_sd_oblig(ptf_bought, calc_sur_dec(ptf_bought))

        # Mise a jour du PTF
        x@ptf_oblig <- rbind(ptf_oblig, ptf_bought@ptf_oblig)

        # Output
        return(x)
    }
)
