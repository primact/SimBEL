
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ModelPointESG}.
##'
##' Une classe pour une extraction de l'ESG pour une annee et une simulation particuliere.
##'
##' @name ModelPointESG
##' @slot annee une valeur \code{integer} correspondant a l'annee de projection.
##' @slot num_traj une valeur \code{integer} correspondant au numero de simulation de l'ESG.
##' @slot indice_action un \code{data.frame} contenant les valeurs prises par les indices actions pour l'annee et la
##' simulation selectionnees.
##' @slot indice_immo un \code{data.frame} contenant les valeurs prises par les indices immobiliers pour l'annee et la
##' simulation selectionnees.
##' @slot indice_inflation une valeur \code{numeric} correspondant a la valeur prise par l'indice inflation
##'  pour l'annee et la simulation selectionnees.
##' @slot yield_curve un vecteur \code{numeric} contenant la structure par terme des taux d'interets spots
##' pour l'annee et la simulation selectionnees. La courbe representee correspond aux valeurs des
##' R(k, k+i) ou i va de 1 au \code{nb_annee_proj}.
##' @slot deflateur une valeur \code{numeric} correspondant a la valeur prise par le deflateur stochastique
##'  pour l'annee et la simulation selectionnees.
##' @docType class
##' @author Prim'Act
##' @seealso Les methodes de chargement d'un ESG \code{\link{chargement_ESG}} et d'extraction d'un model point \code{\link{extract_ESG}}.
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

