
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les valeurs de marches de chaque composante du portefeuille treso.
##'
##' \code{calc_vm_treso} est une methode permettant de calculer les valeurs de marche de chaque ligne du portefeuille treso.
##' @name calc_vm_treso
##' @docType methods
##' @param x objet de la classe \code{\link{Treso}} (decrivant le portefeuille de treso).
##' @param rdt vecteur decrivant le rendement de chacune des lignes treso du portefeuille.
##' Contient autant d'elements que le portefeuille a de lignes.
##' @param flux_milieu vecteur decrivant les flux de milieu d'annee (positif =entrants,  negatifs = sortants).
##' @param flux_fin vecteur decrivant les flux de fin d'annee (positif =entrants,  negatifs = sortants).
##' @return Les valeurs de marche mises a jour.
##' @author Prim'Act
##' @export
##' @include Treso_class.R


setGeneric(name = "calc_vm_treso", def = function(x,rdt,flux_milieu,flux_fin){standardGeneric("calc_vm_treso")})
setMethod(
    f = "calc_vm_treso",
    signature = c(x = "Treso", rdt = "numeric", flux_milieu = "numeric", flux_fin = "numeric"),
    definition = function(x,rdt,flux_milieu, flux_fin){

        # Recuperation du PTF
        ptf_treso <- x@ptf_treso
        nb_treso  <- nrow(x@ptf_treso)

        # Verification des inputs
        len_rdt <- length(rdt)
        len_flux_milieu <- length(flux_milieu)
        len_flux_fin <- length(flux_fin)
        if(nb_treso == 0) stop("[Treso : calc_vm_treso] : Tentative de calcul de VM sur un portfeuille de treso vide. \n")
        if(len_rdt != nb_treso | len_rdt != len_flux_milieu | len_flux_milieu != len_flux_fin) stop("[Treso : calc_vm_treso] : Les inputs ont des dimensions distinctes\n")
        if(len_rdt > 1 | len_flux_milieu > 1 | len_flux_fin > 1) stop("[Treso : calc_vm_treso] : Les inputs doivent etre de dimension 1. \n")

        # Calcul de la VM
        val_marche <- ptf_treso$val_marche * (1 + rdt) + flux_milieu * sqrt(1 + rdt) + flux_fin

        # Output
        return(val_marche)
    }
)
