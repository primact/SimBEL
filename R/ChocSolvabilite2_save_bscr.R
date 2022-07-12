#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script execute enregistre les resultats.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           save_bscr
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Enregistre les resultats d'une evaluation BSCR
##'
##' \code{save_bscr} est une methode permettant d'enregistrer en \code{.cvs} les resultats
##' d'une evaluation BSCR.
##' @name save_bscr
##' @docType methods
##' @param nom est un objet de type \code{character} utilise pour nommer le fichier de resultats.
##' @param path est un objet de type \code{character} utilise pour indiquer le chemin d'enregistrement des resultats.
##' @param x est un objet de type \code{list} contenant les r?sultats d'une evalution BSCR.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R
##'
setGeneric(name = "save_bscr", def = function(nom, path, x){standardGeneric("save_bscr")})
setMethod(
    f = "save_bscr",
    signature = c(nom = "character", path = "character", x = "list"),
    definition = function(nom, path, x){
        write.csv2(x, file = paste0(path,"/", nom,".csv"))
    }
)
# save(x, file = paste0(path, nom,".Rdata")

