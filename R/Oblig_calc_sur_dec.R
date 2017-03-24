
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_sur_dec
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les surcote/decote de chaque composante d'un portefeuille obligataire.
##'
##' \code{calc_sur_dec} est une methode permettant de calculer les surcotes/decotes de chaque composante d'un portefeuille obligataire.
##' @name calc_sur_dec
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuille obligataire).
##' @return Un data.frame compose de deux colonnes : 1 ere colonne : surcotes decotes ; 2de colonne : valeurs nettes comptables.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "calc_sur_dec", def = function(x){standardGeneric("calc_sur_dec")})
setMethod(
    f = "calc_sur_dec",
    signature = "Oblig",
    definition = function(x){
        if(nrow(x["ptf_oblig"]) == 0) {stop("[Oblig:calc_sur_dec_vnc] : Portefeuille obligataire vide")}
        nominal           <- calc_nominal(x) / x["ptf_oblig"][,"nb_unit"]
        vnc             <- x["ptf_oblig"][,"val_nc"] / x["ptf_oblig"][,"nb_unit"]
        maturite_initiale <- x["ptf_oblig"][,"mat_res"]
        surcote_decote    <- (nominal - vnc) / maturite_initiale
        return(surcote_decote)
    }
)
