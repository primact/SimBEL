##' Recuperation de l'objet \code{\link{Be}} et affectation d'une base de donnees.
##'
##' \code{load_be} est la methode permettant de recuper un objet \code{\link{Be}}.
##' @name load_be
##' @docType methods
##' @param x un objet de la classe \code{\link{Initialisation}}.
##' @param choc_name un \code{character} indiquant le nom du choc.
##' @return Pas de sortie.
##' @note Cette methode permet de creer l'objet \code{\link{Canton}} initial et de le sauvegarder dans le repertoire adequat de l'architecture.
##' @author Prim'Act
##' @export
##' @include Initialisation_class.R


setGeneric(name = "load_be", def = function(x, choc_name){standardGeneric("load_be")})
setMethod(
    f = "load_be",
    signature = c(x = "Initialisation", choc_name = "character"),
    definition = function(x, choc_name){

        # Chargement de l'objet BE
        best_estimate <- get(load(paste(x@address$save_folder[choc_name], "best_estimate.RData", sep = "/")))

        # Suppression des objets .Rd non necessaires
        gc()

        # Lecture du fichier csv
        temp            <- read.csv2(paste(x@address$param$base, "param_base.csv", sep = "/"), colClasses = "logical")
        
        # Tests
        if (! all(! is.na(temp)))
            stop("[BE - load] : Presence de NA dans le fichier d'input")

        # Mise a jour des attributs
        ecriture_base   <- temp[,"ecriture_base"]
        # chemin          <- temp[,"chemin"]
        chemin          <- paste(x@root_address, "internal_ws/data/database", sep = "/")

        # Mise a jour de la base
        best_estimate@base <- new("DataBase", chemin, ecriture_base, choc_name)

        if(ecriture_base)
            init_tables(best_estimate@base)
            
        # Output
        return(best_estimate)
    }
)
