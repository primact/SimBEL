# GK 06/03/2017
#---------------------------------------------------------------------------------------------------------------------
##' Creation de l'architecture de sauvegarde des scenarios et executions du code a partir de la racine renseignee.
##'
##' \code{init_create_folder} est une methode permettant de creer l'architecture de sauvegarde des scenarios et les executions du code a partir de la racine renseignee.
##' @name init_create_folder
##' @docType methods
##' @param x objet de la classe \code{\link{Initialisation}}.
##' @note Il est necessaire anterieurement a l'appel de cette fonction d'avoir dans un premier temps
##'  cree un objet \code{\link{Initialisation}} en lui ayant affecte une racine,
##' puis dans un second temps d'avoir appele la methode \code{\link{set_architecture}} a ce meme objet.
##' @return En cas de bonne execution (i.e. l'ensemble des dossiers est cree ou ecrase) la methode renvoie un \code{logical}.
##' @author Prim'Act
##' @export
##' @aliases Initialisation
##' @include Initialisation_class.R

setGeneric(name = "init_create_folder", def = function(x){standardGeneric("init_create_folder")})
setMethod(
    f = "init_create_folder",
    signature = "Initialisation",
    definition = function(x){
        # On ecrase tous les dossiers sauf le init qui contient la photo originale
        # Si les dossiers existent, on les ecrasent et on les recreent
        # Sinon on les creent
        if (dir.exists(paste(racine@root_address, "internal_ws/data/scenario", sep = "/")) == F) { stop("[Initialisation : init_create_folder] L'architecture du dossier internal_ws requiert l'existence d'un dossier data/scenario.")}
        lapply(names(x@address[["save_folder"]][names(x@address[["save_folder"]]) != "init"]), 
               function(y){ 
                    path <- x@address[["save_folder"]][[y]]                           
                    if (dir.exists(path) == F) { dir.create(path) } else { unlink(paste(path, "/*", sep = "")) }})
        return(message("[Initialisation : init_create_folder] : Creation de l'architecture des scenario Solvabilite 2 reussie."))
    }
    
)
     
     