#----------------------------------------------------------------------------------------------------------------------------------------------------
#           create_ptf_bought_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Ajuste les quantites d'obligations a acheter.
##'
##' Cette methode permet d'ajuster d'un coefficient les quantites
##' d'obligations a acheter. Cette methode est utilisee pour l'achat de nouveaux titres obligataires.
##' @name create_ptf_bought_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}}, correspondant au portefeuille
##'  obligataire de reinvestissement. Ce portefeuille est unitaire.
##' @param coefficient est un vecteur de type \code{numeric} qui a autant d'elements
##'  que le portefeuille de reinvestissement obligataire a de lignes. Il correspond au coefficient
##'   a appliquer au portefeuille de reinvestissement obligataire pour effectuer l'achat desire.
##' @return \code{x} un objet de la classe \code{\link{Oblig}} correspondant a une proportion
##'  precise du portefeuille de reinvestissement obligataire.
##' @author Prim'Act
##' @seealso La classe \code{\link{Oblig}}.
##' @export
##' @include AlmEngine_class.R Oblig_class.R


setGeneric(name = "create_ptf_bought_oblig", def = function(x, coefficient) {standardGeneric("create_ptf_bought_oblig")})
setMethod(
    f = "create_ptf_bought_oblig",
    signature = c(x = "Oblig", coefficient = "numeric"),
    definition = function(x,coefficient) {
        
        # Recuperation du PTF oblig
        ptf_oblig <- x@ptf_oblig
        
        # Test input
        if (length(coefficient) != nrow(ptf_oblig)) stop("[Oblig : create_ptf_bought_oblig] : Les inputs sont de dimensions distinctes \n")

        # Donnees
        nom_table  <- names(ptf_oblig)
        val_marche <- which(nom_table == "val_marche")
        val_nc     <- which(nom_table == "val_nc")
        val_achat  <- which(nom_table == "val_achat")
        nb_unit    <- which(nom_table == "nb_unit")
        cc         <- which(nom_table == "cc")
        sd         <- which(nom_table == "sd")

        # Mise a jour des donnees du PTF
        ptf_oblig$val_marche <- coefficient * .subset2(ptf_oblig, val_marche)
        ptf_oblig$val_nc     <- coefficient * .subset2(ptf_oblig, val_nc)
        ptf_oblig$val_achat  <- coefficient * .subset2(ptf_oblig, val_achat)
        ptf_oblig$nb_unit    <- coefficient * .subset2(ptf_oblig, nb_unit)
        ptf_oblig$cc         <- coefficient * .subset2(ptf_oblig, cc)
        ptf_oblig$sd         <- coefficient * .subset2(ptf_oblig, sd)
        # Il n'y a pas a ajuster les durations et Zsp car un mouvement parallele des flux n'impacte pas ces quantites
        
        # Mise a jour du PTF
        x@ptf_oblig <- ptf_oblig
        
        # Output
        return(x)
    }
)
