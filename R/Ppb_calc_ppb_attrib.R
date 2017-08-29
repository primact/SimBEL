#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pb_attrib : Methode permettant de calculer le montant de PB attribuee au cours d'une annee
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le montant de PB attribuee depuis le debut d'annee.
##'
##' \code{calc_pb_attrib} est une methode permettant de calculer le montant de PB attribuee au cours d'une annee.
##' @name calc_pb_attrib
##' @docType methods
##' @param x un objet de la classe \code{\link{Ppb}}.
##' @return la valeur \code{numeric} correspondant au montant de la pb attribuee.
##' @author Prim'Act
##' @export
##' @include Ppb_class.R

setGeneric(name = "calc_pb_attrib", def = function(x){ standardGeneric("calc_pb_attrib") })
setMethod(
    f = "calc_pb_attrib",
    signature = c(x = "Ppb"),
    definition = function(x) {
        
        # Stock PPB
        stock_ppb <- x@ppb_debut
        
        # PB attribuee
        pb_rep <- x@compte_rep
        
        # PB dotee
        pb_dot <- x@compte_dot
        
        # Output
        return(list(stock_ppb = stock_ppb,
                    pb_rep = pb_rep,
                    pb_dot = pb_dot))
    }
)
