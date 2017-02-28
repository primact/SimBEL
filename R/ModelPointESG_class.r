#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ModelPointESG
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe ModelPointESG
##'
##' Classe pour une extraction de l'ESG.
##'
##' @name ModelPointESG
##' @slot annee est une valeur \code{integer} correspondant a l'annee de projection.
##' @slot num_traj est une valeur \code{integer} correspondant au numero de simulation de l'ESG.
##' @slot indice_action
##' @slot indice_immo
##' @slot indice_inflation
##' @slot yield_curve
##' @slot deflateur
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export

setClass(
  Class =  "ModelPointESG",
  representation = representation(
    annee            = "integer",
    num_traj         = "integer",
    indice_action    = "data.frame",
    indice_immo      = "data.frame",
    indice_inflation = "numeric",
    yield_curve      = "numeric",
    deflateur        = "numeric")
)

setValidity(
  Class = "ModelPointESG",
  function (object){
            retval <- NULL
            # Mettre le validity sous forme d'un lapply qui parcourt la liste des object@indice_action et object@indice_immo
            # if(nrow(object@indice_action)>1) {retval <- "[ModelPointESG] : Tentative d'affectation de plusieurs models points action dans l'objet ModelPointESG"}
            # if(nrow(object@indice_immo)>1)   {retval <- "[ModelPointESG] : Tentative d'affectation de plusieurs models points immo dans l'objet ModelPointESG"}

            # Validation des types
            if(!is.integer(object@annee))            {retval <- "[ModelPointESG] : Annee de projection non entiere"}
            if(!is.integer(object@num_traj))         {retval <- "[ModelPointESG] : Num de trajectoire selectionnee non entier"}
            if(!is.numeric(object@indice_inflation)) {retval <- "[ModelPointESG] : Courbe d'inflation non reelle"}
            if(!is.numeric(object@yield_curve))      {retval <- "[ModelPointESG] : Courbe des taux non reelle"}
            if(!is.numeric(object@deflateur))        {retval <- "[ModelPointESG] : Courbe des deflateurs non reelle"}

            if (is.null(retval)) return (TRUE)
            else return (retval)
        }
  )


setMethod(
  f = "initialize",
  signature = "ModelPointESG",
  definition = function(.Object, annee = integer(), num_traj = integer(),
                        indice_action = data.frame(), indice_immo = data.frame(),
                        indice_inflation = numeric(), yield_curve  = numeric(), deflateur = numeric()){

    if(!missing(annee) & !missing(num_traj) & !missing(indice_action) & !missing(indice_immo) & !missing(indice_inflation) & !missing(yield_curve) & !missing(deflateur)){
      .Object@annee             <- annee
      .Object@num_traj          <- num_traj
      .Object@indice_action     <- indice_action
      .Object@indice_immo       <- indice_immo
      .Object@indice_inflation  <- indice_inflation
      .Object@yield_curve       <- yield_curve
      .Object@deflateur         <- deflateur
      validObject(.Object)
    } else if(missing(annee) & missing(num_traj) & missing(indice_action) & missing(indice_immo) & missing(indice_inflation) & missing(yield_curve) & missing(deflateur)){
      #Traitement du cas vide
      .Object@annee             <- integer()
      .Object@num_traj          <- integer()
      .Object@indice_action     <- data.frame()
      .Object@indice_immo       <- data.frame()
      .Object@indice_inflation  <- numeric()
      .Object@yield_curve       <- numeric()
      .Object@deflateur         <- numeric()
    } else {stop("[ModelPointESG : initialize] : Constructeur non integralement renseigne")}
    return(.Object)
  }
)

