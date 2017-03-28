#----------------------------------------------------------------------------------------------------------------------------------------------------
#           pre_load
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Chargement de la valeur initiale de la PRE
##'
##' \code{pre_load} est une methode permettant de charger la valeur de PRE initiale dans un objet de type PRE.
##' @name pre_load
##' @docType methods
##' @param file_PRE_address est un \code{character} correspondant a l'adresse du fichier d'input renseignant les donnees de PRE
##' @return Un objet de la classe \code{\link{PRE}} charge a partir des donnees du fichier dont le nom est precise en input.
##' @author Prim'Act
##' @export
##' @include PRE_class.R

setGeneric(name = "pre_load", def = function(file_PRE_address){standardGeneric("pre_load")})
setMethod(
    f = "pre_load",
    signature = "character",
    definition = function(file_PRE_address){
        temp <- read.csv2(file_PRE_address)
        if(temp[,"ryth_dot"] != round(temp[,"ryth_dot"])) {stop("[PRE : load] : L'input de rythme de dotation doit etre un entier. \n")}
        pre <- new("PRE",
                   val_debut    = temp[,"pre_init"],
                   val_courante = temp[,"pre_init"],
                   ryth_dot     = as.integer(temp[,"ryth_dot"]))
        return(pre)
    }
)
