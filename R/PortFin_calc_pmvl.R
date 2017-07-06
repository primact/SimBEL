#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mets a jour les valeurs des plus ou moins-values actions et immobilier.
##'
##' \code{calc_pmvl} est une methode permettant de calculer les valeurs des plus ou moins values
##' latentes sur actions et immobilier. Met a jour la valeur des attributs d'un objet \code{\link{PortFin}}.
##' @name calc_pmvl
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}.
##' @return L'objet \code{x} dont les attributs en PVL et en MVL a ete mise a jour.
##' @author Prim'Act
##' @export
##' @include PortFin_class.R

setGeneric(name = "calc_pmvl", def = function(x){standardGeneric("calc_pmvl")})
setMethod(
    f = "calc_pmvl",
    signature = "PortFin",
    definition = function(x){
        
        # Recuperation des PTF
        ptf_action <- x@ptf_action@ptf_action
        ptf_immo   <- x@ptf_immo@ptf_immo
        
        # Gestion des noms de colonnes du data.frame de donnnees
        nom_table_action <- names(ptf_action)
        val_marche_action <- which(nom_table_action == "val_marche")
        val_nc_action <- which(nom_table_action == "val_nc")
        
        nom_table_immo <- names(ptf_immo)
        val_marche_immo <- which(nom_table_immo == "val_marche")
        val_nc_immo <- which(nom_table_immo == "val_nc")
        
        ### Calcul des PMVL
        # Action
        pmvl_action <- .subset2(ptf_action, val_marche_action) - .subset2(ptf_action, val_nc_action)
        # Immo
        pmvl_immo <- .subset2(ptf_immo, val_marche_immo) - .subset2(ptf_immo, val_nc_immo)
        
        
        # Affectation des resultats
        x@pvl_action <- sum(max(pmvl_action, 0))
        x@mvl_action <- sum(min(pmvl_action, 0))
        x@pvl_immo   <- sum(max(pmvl_immo, 0))
        x@mvl_immo   <- sum(min(pmvl_immo, 0))
        
        # Output
        return(x)
    }
)
