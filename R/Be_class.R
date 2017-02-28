#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Be
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe Be
##'
##' Classe pour le BE d'un assureur
##'
##' @name Be
##' @slot param_be est un objet de type \code{ParamBe} qui regroupe les parametres de base du best estimate.
##' @slot canton est un objet de type \code{Canton} correspond au canton en date initiale.
##' @slot esg est un objet de type \code{ESG}.
##' @slot tab_flux est une liste qui contient les flux moyens de best estimate.
##' @slot tab_be est une liste qui contient la valeur du BE et de ses composantes.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
setClass(
  Class = "Be",
  representation = representation(
    param_be = "ParamBe",
    canton = "Canton",
    esg = "ESG",
    tab_flux = "list",
    tab_be = "list"
        ),
  validity = function (object){
              retval <- NULL

              if(!validObject(object@param_be)) {retval <- c("[Be] : Objet ParamBe non valide")}
              if(!validObject(object@canton))   {retval <- c("[Be] : Objet Canton non valide")}
              if(!validObject(object@esg))   {retval <- c("[Be] : Objet ESG non valide")}
              if(!is.list(object@tab_flux))   {retval <- c("[Be] : Objet tab_flux non valide")}
              if(!is.list(object@tab_be))   {retval <- c("[Be] : Objet tab_be non valide")}

              # Nom de la liste qui permettent d'alimenter les tableaux de flux
              nom_flux <- c("nom_produit", "prime", "prestation", "prestation_fdb", "frais", "flux_be")
              # Nom de la liste qui permettent d'alimenter les tableaux de flux
              nom_be <- c("nom_produit", "prime_actu", "prestation_actu", "prestation_fdb_actu", "frais_actu", "be")

              # Test sur la longueur des listes de resultats
              if(length(object@tab_flux) != length(nom_flux)) {retval <- c(paste("[Be] : Objet tab_flux doit etre de longueur ",
                                                                           length(nom_flux),".", sep = ""))}
              if(length(object@tab_be) != length(nom_be)) {retval <- c(paste("[Be] : Objet tab_be doit etre de longueur ",
                                                                                 length(nom_be),".", sep = ""))}

              # Test sur les noms des listes de resultats
              if(! prod(names(object@tab_flux) == nom_flux)) {retval <- c("[Be] : Nom de l'objet tab_flux non valide")}
              if(! prod(names(object@tab_be) == nom_be)) {retval <- c("[Be] : Nom de l'objet tab_be non valide")}

              if (is.null(retval)) return (TRUE)
              else return (retval)
              }
            )



# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "Be",
  definition = function(.Object,
                        param_be = new("ParamBe"),
                        canton = new("Canton"),
                        esg = new("ESG"),
                        tab_flux = list(),
                        tab_be = list()
  ){
    if(!missing(param_be)      & !missing(canton) & ! missing(esg) & !missing(tab_flux)  & !missing(tab_be)){
      .Object@param_be <- param_be
      .Object@canton <- canton
      .Object@esg   <- esg
      .Object@tab_flux  <- tab_flux
      .Object@tab_be  <- tab_be
      validObject(.Object)
    } else {
      #Traitement du cas vide

      list_flux <- list(nom_produit = character(), prime = numeric(), prestation = numeric(),
                        prestation_fdb = numeric(), frais = numeric(), flux_be = numeric())
      list_be <- list(nom_produit = character(), prime_actu = numeric(), prestation_actu = numeric(),
                        prestation_fdb_actu = numeric(), frais_actu = numeric(), be = numeric())

      .Object@param_be = new("ParamBe")
      .Object@canton = new("Canton")
      .Object@esg = new("ESG")
      .Object@tab_flux = list_flux
      .Object@tab_be = list_be
    }
    return(.Object)
  }
)
