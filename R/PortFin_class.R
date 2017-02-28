#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe PortFin
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe PortFin
##'
##' Classe pour le portefeuille global d'actif
##'
##' @name PortFin
##' @slot ptf_action est un objet de type Action, qui represente le portefeuille d'action d'un canton.
##' @slot ptf_immo est un objet de type Immo, qui represente le portefeuille immobilier d'un canton.
##' @slot ptf_oblig est un objet de type Oblig, qui represente le portefeuille obligataire d'un canton.
##' @slot ptf_treso est un objet de type Treso, qui represente le portefeuille monetaire d'un canton.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
setClass(
  Class = "PortFin",
  representation = representation(
    annee       = "integer",
    ptf_action  = "Action",
    ptf_immo    = "Immo",
    ptf_oblig   = "Oblig",
    ptf_treso   = "Treso",
    pre         = "PRE",
    rc          = "RC",
    frais_fin   = "FraisFin",
    pvl_action  = "numeric",
    pvl_immo    = "numeric",
    pvl_oblig   = "numeric",
    mvl_action  = "numeric",
    mvl_immo    = "numeric",
    mvl_oblig   = "numeric",
    vm_vnc_precedent = "list")
)
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "PortFin",
            function (object){
              retval <- NULL

              if(!validObject(object@ptf_action)) {retval <- "[PortFin] : Objet Action non valide"}
              if(!validObject(object@ptf_immo))   {retval <- "[PortFin] : Objet Immo non valide"}
              if(!validObject(object@ptf_oblig))  {retval <- "[PortFin] : Objet Oblig non valide"}
              if(!validObject(object@ptf_treso))  {retval <- "[PortFin] : Objet Treso non valide"}
              if(!validObject(object@pre))  {retval <- "[PortFin] : Objet Pre non valide"}
              if(!validObject(object@rc))  {retval <- "[PortFin] : Objet RC non valide"}
              if(!validObject(object@frais_fin))  {retval <- "[PortFin] : Objet FraisFin non valide"}


              if(length(object@annee) > 1 | length(object@pvl_action) > 1 | length(object@mvl_action) > 1 | length(object@pvl_immo) > 1 | length(object@mvl_immo) > 1 | length(object@pvl_oblig) > 1 | length(object@mvl_oblig) > 1 )
                {retval <- "[PortFin] : Les PMVL et l'annee doivent être des reels, pas de composantes vectorielles autorisees."}

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
  signature = "PortFin",
  definition = function(.Object, annee      = integer(),
                                 ptf_action = new("Action"),
                                 ptf_immo   = new("Immo"),
                                 ptf_oblig  = new("Oblig"),
                                 ptf_treso  = new("Treso"),
                                 pre  = new("PRE"),
                                 rc  = new("RC"),
                                 frais_fin = new("FraisFin"),
                                 pvl_action = numeric(),
                                 pvl_immo   = numeric(),
                                 pvl_oblig  = numeric(),
                                 mvl_action = numeric(),
                                 mvl_immo   = numeric(),
                                 mvl_oblig  = numeric(),
                                 vm_vnc_precedent = list()){
    if(!missing(annee)      & !missing(ptf_action) & ! missing(ptf_immo) & !missing(ptf_oblig)  & !missing(ptf_treso) &
       !missing(pre)      & !missing(rc) & ! missing(frais_fin) &
       !missing(pvl_action) & !missing(pvl_immo)  & !missing(pvl_oblig)  & !missing(mvl_action) & !missing(mvl_immo)  &
       !missing(mvl_oblig)  & !missing(vm_vnc_precedent)){
      .Object@annee      <- annee
      .Object@ptf_action <- ptf_action
      .Object@ptf_immo   <- ptf_immo
      .Object@ptf_oblig  <- ptf_oblig
      .Object@ptf_treso  <- ptf_treso
      .Object@pre  <- pre
      .Object@rc  <- rc
      .Object@frais_fin  <- frais_fin
      .Object@pvl_action <- pvl_action
      .Object@pvl_immo   <- pvl_immo
      .Object@pvl_oblig  <- pvl_oblig
      .Object@mvl_action <- mvl_action
      .Object@mvl_immo   <- mvl_immo
      .Object@mvl_oblig  <- mvl_oblig
      .Object@vm_vnc_precedent <- vm_vnc_precedent
      validObject(.Object)
    } else if(missing(annee)      & missing(ptf_action) & missing(ptf_immo)  & missing(ptf_oblig)  & missing(ptf_treso) &
              missing(pre)        & missing(rc)         & missing(frais_fin) &
              missing(pvl_action) & missing(pvl_immo)   & missing(pvl_oblig) & missing(mvl_action) & missing(mvl_immo)  &
              missing(mvl_oblig)  & missing(vm_vnc_precedent)){
      #Traitement du cas vide
      .Object@annee      <- as.integer(0)
      .Object@ptf_action <- new("Action")
      .Object@ptf_immo   <- new("Immo")
      .Object@ptf_oblig  <- new("Oblig")
      .Object@ptf_treso  <- new("Treso")
      .Object@pre        <- new("PRE")
      .Object@rc         <- new("RC")
      .Object@frais_fin  <- new("FraisFin")
      .Object@pvl_action <- numeric()
      .Object@pvl_immo   <- numeric()
      .Object@pvl_oblig  <- numeric()
      .Object@mvl_action <- numeric()
      .Object@mvl_immo   <- numeric()
      .Object@mvl_oblig  <- numeric()
      .Object@vm_vnc_precedent <- vm_vnc_precedent
    } else{ stop("[PortFin] : La creation d'un nouvel objet PortFin necessite de renseigner l'ensemble des champs constituant un tel objet. \n")}
    return(.Object)
  }
)

