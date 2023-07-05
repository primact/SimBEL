#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{Initialisation}.
##'
##' Une classe permettant de gerer les parametres techniques necessaire a l'initialisation d'une etude.
##'
##' @name Initialisation
##' @slot root_address ce \code{character} doit correspondre a la racine du projet. C'est dans les sous dossiers de cet emplacement que l'ensemble
##' des donnees, parametres et dossiers de sauvegarde doivent se situer,
##' en respectant l'architecture etablie par Prim'Act.
##' @slot address est une liste renseignee par la fonction \code{\link{set_architecture}} qui contient l'ensemble des adresses de l'architecture
##' physique du projet
##' (emplacement des donnnees utilisateurs, emplacement des parametres utilisateurs, emplacement des sauvegardes temporaires et definitives).
##' @slot nb_simu est un \code{integer} correspondant aux nombres de trajectoires simulees par le jeu de donnees de l'ESG Prim'Act.
##' @slot nb_annee_proj est un \code{integer} correspondant au nombre d'annee de projection de la modelisation.
##' @note Il est necessaire que l'attribut nb_annee_proj corresponde au nombre d'annee de projection des donnees de l'ESG Prim'Act.
##' @docType class
##' @author Prim'Act
##' @seealso La mise en place de l'architecture de chargement des donnees et parametres renseignes par l'utilisateur \code{\link{set_architecture}},
##' la creation et la sauvegarde du canton initial \code{\link{init_SimBEL}}, la creation de l'architecture des scenarios central,
##' de marche et de souscription de la formule standard ainsi que la creation des objets \code{\link{Be}} pour chacun de ces scenarios.
##' @keywords classes
##' @export

setClass(
    Class = "Initialisation",
    representation = representation(
        root_address  = "character",
        address       = "list",
        nb_simu       = "integer",
        nb_annee_proj = "integer"
    )
)
