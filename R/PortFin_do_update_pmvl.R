#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_pmvl
#----------------------------------------------------------------------------------------------------------------------------------------------------
##'  Met a jour l'ensemble des attributs pvl et pml d'un portefeuille financier.
##'
##' \code{do_update_pmvl} est une methode permettant de mettre a jour les moins de plus ou moins-values
##' latentes d'un objet \code{\link{PortFin}}.
##' @name do_update_pmvl
##' @docType methods
##' @param x est un objet de la classe \code{\link{PortFin}},
##' @return L'objet \code{x} de la classe \code{\link{PortFin}} dont les plus-values et moins-values
##' latentes ont ete recalculees avec les elements du \code{\link{PortFin}} renseigne en input.
##' @author Prim'Act
##' @seealso Les methodes de calcul des plus ou moins-values latentes : \code{\link{calc_pmvl_action}},
##' \code{\link{calc_pmvl_immo}}, \code{\link{calc_pmvl_oblig}}.
##' @export
##' @include PortFin_class.R

setGeneric(name = "do_update_pmvl", def = function(x){standardGeneric("do_update_pmvl")})
setMethod(
    f = "do_update_pmvl",
    signature = "PortFin",
    definition = function(x){

        # Affectation des valeurs dans l'objet PortFin
        pmvl_action <- calc_pmvl_action(x@ptf_action)
        pmvl_immo   <- calc_pmvl_immo(x@ptf_immo)
        pmvl_oblig  <- calc_pmvl_oblig(x@ptf_oblig)

        x@pvl_action <- pmvl_action[["pvl"]]
        x@mvl_action <- pmvl_action[["mvl"]]
        x@pvl_immo   <- pmvl_immo[["pvl"]]
        x@mvl_immo   <- pmvl_immo[["mvl"]]
        x@pvl_oblig  <- pmvl_oblig[["pvl"]]
        x@mvl_oblig  <- pmvl_oblig[["mvl"]]
        return(x)
    }
)
