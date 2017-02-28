#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend une fonction permettant d'initialiser la PPB en debut de periode.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           init_debut_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Re-initialise un objet Ppb en debut d'annee
##'
##' \code{init_debut_ppb} est une methode permettant de re-initialiser les montants de dotation ou de reprise cumules sur l'annee et
##' de re-initialiser le montant de PPB de debut de periode.
##' @name init_debut_ppb
##' @docType methods
##' @param x objet de la classe \code{Ppb}.
#' @return L'objet x reinitialise.
##' @author Prim'Act
##' @export
##' @aliases Ppb

setGeneric(name = "init_debut_ppb", def = function(x){standardGeneric("init_debut_ppb")})
setMethod(
  f = "init_debut_ppb",
  signature = c(x = "Ppb"),
  definition = function(x){

    # Mise a jour du montant de PPB initial
    x["ppb_debut"] <- x["valeur_ppb"]

    # Remise a zero des compteurs
    x["compte_dot"] <- 0
    x["compte_rep"] <- 0

    # Output
    return(x)
  }
)
