
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ChocSolvabilite2}.
##'
##' La classe \code{ChocSolvabilite2} permet de realiser les principaux des scenarios de choc initiaux
##' au sens de la formule standard de la directive Solvabilite 2.
##' @name ChocSolvabilite2
##' @slot scenario un vecteur de type \code{character} comprenant les noms des chocs a initialiser
##' @slot param_choc_mket un objet de la classe \code{\link{ParamChocMket}}.
##' @slot param_choc_sousc un objet de la classe \code{\link{ParamChocSousc}}.
##' @slot matrice_choc_action une matrice de correlation utilisee pour calculer le SCR action.
##' @slot matrice_choc_mket une matrice de correlation utilisee pour calculer le SCR marche.
##' @slot matrice_choc_sousc une matrice de correlation utilisee pour calculer le SCR souscription vie.
##' @slot matrice_choc_bscr une matrice de correlation utilisee pour calculer le BSCR.
##' @docType class
##' @details Cette classe contient deux attributs
##' qui contiennent respectivement l'ensemble des parametres necessaires a l'application des chocs Marche et Souscription.
##' Cette classe contient aussi l'ensemble des methodes permettant d'appliquer chacun de ces chocs individuellement
##' a un objet de la classe \code{\link{Canton}}. Les chocs permis sont :
##' \describe{
##' \item{\code{central} : }{la situation centrale}
##' \item{\code{taux_up} : }{le choc de taux a la hausse}
##' \item{\code{taux_down} : }{le choc de taux a la baisse}
##' \item{\code{action_type1} : }{le choc action de type 1}
##' \item{\code{action_type2} : }{le choc action de type 2}
##' \item{\code{immo} : }{le choc immobilier}
##' \item{\code{spread} : }{le choc spread sur les obligations corporates}
##' \item{\code{currency_up} : }{les chocs sur devise a la hausse}
##' \item{\code{currency_down} : }{les chocs sur devise a la baisse}
##' \item{\code{mortalite} : }{le choc mortalite sur les tables de mortalite}
##' \item{\code{longevite} : }{le choc longevite sur les tables de mortalite}
##' \item{\code{frais} : }{le choc depenses sur le niveau des frais et l'inflation des frais}
##' \item{\code{rachat_up} : }{le choc de rachat a la hausse}
##' \item{\code{rachat_down} : }{le choc de rachat a la baisse}
##' \item{\code{rachat_mass} : }{le choc de rachat massif}
##' }
##' @author Prim'Act
##' @seealso L'application des chocs de \code{taux_up} et \code{taux_down} : \code{\link{do_choc_taux}}.
##' L'application des chocs de \code{action_type1} et \code{action_type2} : \code{\link{do_choc_action_type1}}, \code{\link{do_choc_action_type2}}.
##' L'application du choc de \code{immo} : \code{\link{do_choc_immo}}.
##' L'application du choc de \code{spread} : \code{\link{do_choc_spread}}.
##' L'application des chocs de \code{currency_up} et \code{currency_down} : \code{\link{do_choc_currency}}.
##' L'application du choc de \code{mortalite} : \code{\link{do_choc_mortalite}}.
##' L'application du choc de \code{longevite} : \code{\link{do_choc_longevite}}.
##' L'application du choc de \code{frais} : \code{\link{do_choc_frais}}, \code{\link{get_choc_inflation_frais}}.
##' L'application des chocs de \code{rachat_up}, \code{rachat_down} et  \code{rachat_massif}: \code{\link{do_choc_rachat_up}},
##' \code{\link{do_choc_rachat_down}} et \code{\link{do_choc_rachat_mass}}.
##'
##' @export
##' @include ParamChocMket_class.R ParamChocSousc-class.R

setClass(
    Class = "ChocSolvabilite2",
    representation = representation(
        scenario         = "character",
        param_choc_mket  = "ParamChocMket",
        param_choc_sousc = "ParamChocSousc",
        matrice_choc_action = "matrix",
        matrice_choc_mket = "matrix",
        matrice_choc_sousc = "matrix",
        matrice_choc_bscr = "matrix"),
    validity = function (object){

      retval <- NULL
      if(!validObject(object@param_choc_mket))      retval <- c(retval, "[ChocSolvabilite2] : Objet param_choc_mket non valide")
      if(!validObject(object@param_choc_sousc))       retval <- c(retval, "[ChocSolvabilite2] : Objet param_choc_sousc non valide")
      if(!is.character(object@scenario))       retval <- c(retval, "[ChocSolvabilite2] : Objet scenario non valide")
      if(!is.character(object@matrice_choc_action))       retval <- c(retval, "[ChocSolvabilite2] : Objet matrice_choc_action non valide")
      if(!is.character(object@matrice_choc_mket))       retval <- c(retval, "[ChocSolvabilite2] : Objet matrice_choc_mket non valide")
      if(!is.character(object@matrice_choc_sousc))       retval <- c(retval, "[ChocSolvabilite2] : Objet matrice_choc_sousc non valide")
      if(!is.character(object@matrice_choc_bscr))       retval <- c(retval, "[ChocSolvabilite2] : Objet matrice_choc_bscr non valide")


      if (is.null(retval)) return (TRUE)
      else return (retval)
    }
)
