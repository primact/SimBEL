#----------------------------------------------------------------------------------------------------------------------------------------------------
#           rc_load
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Chargement de la valeur initiale de la RC
##'
##' \code{rc_load} est une methode permettant de charger la valeur de RC initiale dans un objet de type RC.
##' @name rc_load
##' @docType methods
##' @param file_RC_address est un \code{character} correspondant a l'adresse du fichier d'input renseignant les donnees de RC
##' @return Un objet de la classe \code{\link{RC}} charge a partir des donnees du fichier dont le nom est precise en input.
##' @author Prim'Act
##' @export
##' @include RC_class.R

setGeneric(name = "rc_load", def = function(file_RC_address){standardGeneric("rc_load")})
setMethod(
    f = "rc_load",
    signature = "character",
    definition = function(file_RC_address){
        temp <- read.csv2(file_RC_address)
        rc <- new("RC",
                   val_debut    = temp[,"rc_init"],
                   val_courante = temp[,"rc_init"])
        return(rc)
    }
)
