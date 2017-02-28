#----------------------------------------------------------
# Ce script est la definition de la classe PortPassif dedie aux differents produits existants du passif
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 25/01/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe PortPassif
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe PortPassif
##'
##' @name PortPassif
##' @slot eei une liste d'objets de classe \code{EpEuroInd} contenant l'ensemble portefeuilles
##'  relatifs aux model points epargne en euros.
##' @slot names_class_prod un vecteur \code{character} indique les noms de classes de produits.
##' @slot ht un objet de classe \code{HypTech}.
##' @slot fp un objet de classe \code{FraisPassif}.
##' @slot tx_pb un objet de classe \code{Taux_PB}.
##' @slot autres_passifs un objet de classe \code{AutresPassifs}.
##' @slot autres_reserves un objet de classe \code{AutresReserves}.
#' @slot ere une liste d'objets de classe EpRetrInd contenant l'ensemble portefeuilles relatifs aux model points epargne retraite en euros
#' @slot erp une liste d'objets de classe EpRetrPoint contenant l'ensemble portefeuilles relatifs aux model points epargne retraite en points
#' @slot ren une liste d'objets de classe Rente contenant l'ensemble portefeuilles relatifs aux model points rente en euros

##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export

setClass(
  Class = "PortPassif",
  representation = representation(
    annee = "integer",
    eei = "list",
    names_class_prod = "character",
    ht = "HypTech",
    fp = "FraisPassif",
    tx_pb = "TauxPB",
    autres_passifs = "AutresPassifs",
    autres_reserves = "AutresReserves"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL

    # Test que les listes ne sont pas vides
    if(length(object@eei) == 0){c(retval, "[PortPassif] : 'eei' ne doit pas etre vide")}

    # Tests de non absence de noms
    if(is.null(names(object@eei))){retval <- c(retval, "[PortPassif] : 'eei' doit etre une liste avec des noms")}

    # Test si un nom n a pas ete oublie
    if(! prod(names(object@eei) != c("") )){
      retval <- c(retval, "[PortPassif] : tous les elements de 'eei' n ont pas ete nommes")}

    # Test sur le type des objets mis en parametre
    if(! validObject(object@ht)){retval <- c(retval, "[PortPassif] : Objet 'ht' non valide")}
    if(! validObject(object@fp)){retval <- c(retval, "[PortPassif] : Objet 'fp' non valide")}
    if(!validObject(object@tx_pb)){retval <- c(retval, "[PortPassif] : Objet 'tx_pb' non valide")}
    if(! validObject(object@autres_passifs)){retval <- c(retval, "[PortPassif] : Objet 'autres_passifs' non valide")}
    if(! validObject(object@autres_reserves)){retval <- c(retval, "[PortPassif] : Objet 'autres_reserves' non valide")}
    if(length(object@annee) > 1){retval <- c("[PortPassif] : 'annee' doit être entier, pas de composante vectorielle autorisee.")}
    if(! is.integer(object@annee)){retval <- c("[PortPassif] : 'annee' doit être entier.")}

    # Test que les elements des listes sont bien des objets du bon type
    for(i in 1:length(object@eei)){
      if(!validObject(object@eei[[i]])){
        retval <- c(retval, paste("[PortPassif] : l element ", i, " de 'eei' est non valide \n", sep = ""))}
    }

    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (retval)
    }

  }
)
