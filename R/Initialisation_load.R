##' Chargement de certains attributs dans un objet \code{Initialisation}
##'
##' \code{initialisation_load} est la methode de chargement des attributs \code{nb_simu} et \code{nb_annee_proj} a partir des donnees de l'environnement utilisateur.
##' @name initialisation_load
##' @docType methods
##' @param x un objet de la classe \code{\link{Initialisation}}.
##' @param file_lancement_address nom complet (i.e. avec chemin d'acces et extension) du fichier contenant les parametres de lancement.
##' @return Pas de sortie.
##' @note  Cette methode permet de creer l'objet \code{\link{Canton}} initial et de le sauvegarder dans le repertoire adequat de l'architecture.
##' @author Prim'Act
##' @export
##' @include Initialisation_class.R


setGeneric(name = "initialisation_load", def = function(x, file_lancement_address){standardGeneric("initialisation_load")})
setMethod(
    f = "initialisation_load",
    signature = c(x = "Initialisation", file_lancement_address = "character"),
    definition = function(x, file_lancement_address){

        # Lecture du fichier csv
        temp            <- read.csv2(file_lancement_address, colClasses = c("integer", "integer"))
        
        # Tests
        if (! all(! is.na(temp)))
            stop("[Initialisation - load] : Presence de NA dans le fichier d'input")

        # Mise a jour des attributs
        x@nb_simu       <- temp[,"nb_simu"]
        x@nb_annee_proj <- temp[,"nb_annee_trajectoire"]

        # Output
        return(x)
    }
)
