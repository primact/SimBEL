#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ESG
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe ChocSolvabilite2 permet d'implementer l'ensemble des scenarios de choc initiaux au sens de la formule standard de la directive Solvabilite 2.
##' Cette classe contient deux attributs qui contiennent respectivement l'ensemble des parametres necessaire a l'application des chocs Marche et Souscription,
##' au sens de la formule standard. 
##' Cette classe contient aussi l'ensemble des methodes permettant d'appliquer chacun de ces chocs individuellement a un objet de la classe \code{canton}.
##' 
##' @name ChocSolvabilite2
##' @slot param_choc_mket objet de la classe \code{ParamChocMket}
##' @slot param_choc_sousc : objet de la classe \code{ParamChocSousc}
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul et la mise a jour des autres reserves \code{\link{do_choc_action}}, \code{\link{do_choc_immo}}, \code{\link{do_choc_spread}} et
##' \code{\link{do_choc_frais}}, \code{\link{do_choc_mortalite}}, \code{\link{do_choc_longevite}}, \code{\link{do_choc_rachat_up}}, \code{\link{do_choc_rachat_down}}.
##' @export

setClass(
    Class = "ChocSolvabilite2",
    representation = representation(
        param_choc_mket  = "ParamChocMket",
        param_choc_sousc = "ParamChocSousc")
)
