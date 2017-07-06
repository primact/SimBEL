#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les plus et moins-values obligataires.
##'
##' \code{calc_pmvl_oblig} est une methode permettant de calculer les plus et moins-values du portefeuille obligataire.
##' @name calc_pmvl_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille d'obligations).
##' @return \code{pvl} correspondant a la somme des plus-values latentes obligataires.
##' @return \code{mvl} correspondant a la somme des moins-values latentes obligataires.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_pmvl_oblig", def = function(x){standardGeneric("calc_pmvl_oblig")})
setMethod(
    f = "calc_pmvl_oblig",
    signature = "Oblig",
    definition = function(x){
        
        # Donnees
        ptf_oblig  <- x@ptf_oblig
        nom_table  <- names(ptf_oblig)
        val_marche <- which(nom_table == "val_marche")
        val_nc     <- which(nom_table == "val_nc")

        # Plus ou moins value latentes
        pmvl <-.subset2(ptf_oblig, val_marche) - .subset2(ptf_oblig, val_nc)

        # Calcul des plus et moins values
        pvl <- sum( pmvl * (pmvl > 0))
        mvl <- sum( pmvl * (pmvl <= 0))
        
        # Ouput
        return(list(pvl = pvl, mvl = mvl))
    }
)
