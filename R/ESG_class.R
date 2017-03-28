
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ESG}.
##'
##' Une classe de parametres contenant les tables de simulation, generees par une generateur de scenarions
##' economique.
##' @name ESG
##' @slot nb_simu un entier (\code{integer}) correspondant au nombre de simulations.
##' @slot ind_action une liste contenant les differents indices actions utilises. Chaque element de la liste contient
##' \code{nb_simu} simulations de l'indice.
##' @slot ind_immo une liste contenant les differents indices immobilier utilises. Chaque element de la liste contient
##' \code{nb_simu} simulations de l'indice.
##' @slot ind_inflation une liste contenant l'indice inflation utilise. L'element de la liste contient
##' \code{nb_simu} simulations de l'indice.
##' @slot yield_curve une liste contenant les courbes de taux simulees a chaque date de projection. Chaque element
##' de la liste, correspondant a une annee de projection, contient \code{nb_simu} simulations de la courbe des taux.
##' @slot deflateur une liste contenant le deflateur stochastique a utiliser. L'element de la liste contient
##' \code{nb_simu} simulations du deflateur.
##' @docType class
##' @author Prim'Act
##' @seealso Les methodes de chargement d'un ESG \code{\link{chargement_ESG}} et d'extraction d'un model point
##' ESG \code{\link{extract_ESG}}.
##' @keywords classes
##' @export

setClass(
  Class = "ESG",
  representation = representation(
    nb_simu       = "integer",
    ind_action    = "list",
    ind_immo      = "list",
    ind_inflation = "list",
    yield_curve   = "list",
    deflateur     = "list"),
  validity = function(object){
                  retval <- NULL

                  if(!is.integer(object@nb_simu)) {retval <- "[ESG] : Objet nb_simu non valide"}
                  if(!is.list(object@ind_action))   {retval <- "[ESG] : Objet ind_action non valide"}
                  if(!is.list(object@ind_immo))   {retval <- "[ESG] : Objet ind_immo non valide"}
                  if(!is.list(object@ind_inflation))   {retval <- "[ESG] : Objet ind_inflation non valide"}
                  if(!is.list(object@yield_curve))   {retval <- "[ESG] : Objet yield_curve non valide"}
                  if(!is.list(object@deflateur))   {retval <- "[ESG] : Objet deflateur non valide"}

                  if(object@nb_simu < 1) {retval <- "[ESG] : Objet nb_sim doit etre superieur a 1."}

                  if (is.null(retval)) return (TRUE)
                  else return (retval)

  }
  )
