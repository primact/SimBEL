#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de doter et de reprendre la PPB
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_dotation_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Dote la valeur de la PPB
##'
##' \code{calc_dotation_ppb} est une methode permettant de doter la PPB. La dotation est effectuee si les limites de dotation
##' de la PPB sur l'annee ne sont pas atteintes. La valeur de cette limite est mise a jour suite a la dotation.
##' @name calc_dotation_ppb
##' @docType methods
##' @param x objet de la classe \code{Ppb}.
##' @param montant valeur de la dotation.
##' @return Une liste comprenant l'objet mis a jour et le montnant de la dotation effectuee.
##' @author Prim'Act
##' @export
##' @aliases Ppb

setGeneric(name = "calc_dotation_ppb", def = function(x, montant){standardGeneric("calc_dotation_ppb")})
setMethod(
  f = "calc_dotation_ppb",
  signature = c(x = "Ppb", montant = "numeric"),
  definition = function(x, montant){

    # Limite de dotation courante
    limit <- max(x["seuil_dot"] * x["ppb_debut"] -  x["compte_dot"], 0)

    # Montant dote
    dot <- min(limit, montant)

    # Application de la dotation
    x["valeur_ppb"] <- x["valeur_ppb"] + dot

    # Mise a jour du montant dote
    x["compte_dot"] <- x["compte_dot"] + dot

    return(list(ppb = x, dotation = dot))
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_reprise_ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Reprend sur la valeur de la PPB
##'
##' \code{calc_reprise_ppb} est une methode permettant de reprendre sur la PPB. La reprise est effectuee si les limites de reprise
##' de la PPB sur l'annee ne sont pas atteintes. La valeur de cette limite est mise a jour suite a la reprise
##' @name calc_reprise_ppb
##' @docType methods
##' @param x objet de la classe \code{Ppb}.
##' @param montant valeur de la reprise.
##' @return Une liste comprenant l'objet mis a jour et le montnant de la reprise effectuee.
##' @author Prim'Act
##' @export
##' @aliases Ppb

setGeneric(name = "calc_reprise_ppb", def = function(x, montant){standardGeneric("calc_reprise_ppb")})
setMethod(
  f = "calc_reprise_ppb",
  signature = c(x = "Ppb", montant = "numeric"),
  definition = function(x, montant){

    # Limite de reprise courante
    limit <- max(x["seuil_rep"] * x["ppb_debut"] -  x["compte_rep"], 0)

    # Montant repris
    rep <- min(limit, montant, x["valeur_ppb"])

    # Application de la reprise
    x["valeur_ppb"] <- x["valeur_ppb"] - rep

    # Mise a jour du montant repris
    x["compte_rep"] <- x["compte_rep"] + rep

    return(list(ppb = x, reprise = rep))
  }
)

