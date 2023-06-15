#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{PortFin}.
##'
##' Une Classe pour le portefeuille financier.
##'
##' @name PortFin
##' @slot annee est un objet de type \code{integer}correspondant a l'annee courante.
##' @slot ptf_action est un objet de type \code{\link{Action}}, qui represente le portefeuille d'action d'un canton.
##' @slot ptf_immo est un objet de type \code{\link{Immo}}, qui represente le portefeuille immobilier d'un canton.
##' @slot ptf_oblig est un objet de type \code{\link{Oblig}}, qui represente le portefeuille obligataire d'un canton.
##' @slot ptf_treso est un objet de type \code{\link{Treso}}, qui represente le portefeuille monetaire d'un canton.
##' @slot pre est un objet de type \code{\link{PRE}}, qui represente la PRE d'un canton.
##' @slot rc est un objet de type \code{\link{RC}}, qui represente la RC d'un canton.
##' @slot frais_fin est un objet de type \code{\link{FraisFin}}, qui represente les frais financiers d'un canton.
##' @slot pvl_action est un \code{numeric}, qui correspond a la somme des plus values latentes des actifs Actions qui sont en situation de
##' plus values latentes.
##' @slot pvl_immo est un \code{numeric}, qui correspond a la somme des plus values latentes des actifs Immo qui sont en situation de
##' plus values latentes.
##' @slot pvl_oblig est un \code{numeric}, qui correspond a la somme des plus values latentes des actifs Obligs qui sont en situation de
##' plus values latentes.
##' @slot mvl_action est un \code{numeric}, qui correspond a la somme des moins values latentes des actifs Actions qui sont en situation de
##' moins values latentes.
##' @slot mvl_immo est un \code{numeric}, qui correspond a la somme des moins values latentes des actifs Immos qui sont en situation de
##' moins values latentes.
##' @slot mvl_oblig est un \code{numeric}, qui correspond a la somme des moins values latentes des actifs Obligs qui sont en situation de
##' moins values latentes.
##' @slot vm_vnc_precedent est une liste composee de deux elements : la \code{vm_precedente} et la
##'  \code{vnc_precedente}, correspondant respectivement a la valeur de marche
##' et a la valeur nette comptable en debut d'annee de l'objet PortFin.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul des rendements : \code{\link{calc_rdt}}.
##' Le calcul des plus ou moins-values latentes : \code{\link{calc_pmvl}}.
##' Le chargement des donneees initiales : \code{\link{chargement_PortFin}}, \code{\link{chargement_PortFin_reference}}.
##' Les methodes de mise a jour : \code{\link{update_PortFin}}, \code{\link{update_PortFin_reference}}, \code{\link{do_update_pmvl}},
##' \code{\link{do_update_vm_vnc_precedent}}.
##' L'allocation d'actifs : \code{\link{print_alloc}}.
##' @keywords classes
##' @export
##' @include Action_class.R Immo_class.R FraisFin_class.R Oblig_class.R PRE_class.R RC_class.R Treso_class.R

setClass(
    Class = "PortFin",
    representation = representation(
        annee = "integer",
        ptf_action = "Action",
        ptf_immo = "Immo",
        ptf_oblig = "Oblig",
        ptf_treso = "Treso",
        pre = "PRE",
        rc = "RC",
        frais_fin = "FraisFin",
        pvl_action = "numeric",
        pvl_immo = "numeric",
        pvl_oblig = "numeric",
        mvl_action = "numeric",
        mvl_immo = "numeric",
        mvl_oblig = "numeric",
        vm_vnc_precedent = "list"
    )
)
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(
    Class = "PortFin",
    function(object) {
        retval <- NULL

        if (!validObject(object@ptf_action)) retval <- "[PortFin] : Objet Action non valide"
        if (!validObject(object@ptf_immo)) retval <- "[PortFin] : Objet Immo non valide"
        if (!validObject(object@ptf_oblig)) retval <- "[PortFin] : Objet Oblig non valide"
        if (!validObject(object@ptf_treso)) retval <- "[PortFin] : Objet Treso non valide"
        if (!validObject(object@pre)) retval <- "[PortFin] : Objet Pre non valide"
        if (!validObject(object@rc)) retval <- "[PortFin] : Objet RC non valide"
        if (!validObject(object@frais_fin)) retval <- "[PortFin] : Objet FraisFin non valide"


        if (length(object@annee) > 1L | length(object@pvl_action) > 1L | length(object@mvl_action) > 1L |
            length(object@pvl_immo) > 1L | length(object@mvl_immo) > 1L | length(object@pvl_oblig) > 1L |
            length(object@mvl_oblig) > 1L) {
            retval <- "[PortFin] : Les PMVL et l'annee doivent etre des reels, pas de composantes vectorielles autorisees."
        }

        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "PortFin",
    definition = function(.Object, annee = integer(),
                          ptf_action = new("Action"),
                          ptf_immo = new("Immo"),
                          ptf_oblig = new("Oblig"),
                          ptf_treso = new("Treso"),
                          pre = new("PRE"),
                          rc = new("RC"),
                          frais_fin = new("FraisFin"),
                          pvl_action = numeric(),
                          pvl_immo = numeric(),
                          pvl_oblig = numeric(),
                          mvl_action = numeric(),
                          mvl_immo = numeric(),
                          mvl_oblig = numeric(),
                          vm_vnc_precedent = list()) {
        if (!missing(annee) & !missing(ptf_action) & !missing(ptf_immo) & !missing(ptf_oblig) & !missing(ptf_treso) &
            !missing(pre) & !missing(rc) & !missing(frais_fin) &
            !missing(pvl_action) & !missing(pvl_immo) & !missing(pvl_oblig) & !missing(mvl_action) & !missing(mvl_immo) &
            !missing(mvl_oblig) & !missing(vm_vnc_precedent)) {
            .Object@annee <- annee
            .Object@ptf_action <- ptf_action
            .Object@ptf_immo <- ptf_immo
            .Object@ptf_oblig <- ptf_oblig
            .Object@ptf_treso <- ptf_treso
            .Object@pre <- pre
            .Object@rc <- rc
            .Object@frais_fin <- frais_fin
            .Object@pvl_action <- pvl_action
            .Object@pvl_immo <- pvl_immo
            .Object@pvl_oblig <- pvl_oblig
            .Object@mvl_action <- mvl_action
            .Object@mvl_immo <- mvl_immo
            .Object@mvl_oblig <- mvl_oblig
            .Object@vm_vnc_precedent <- vm_vnc_precedent
            validObject(.Object)
        } else if (missing(annee) & missing(ptf_action) & missing(ptf_immo) & missing(ptf_oblig) & missing(ptf_treso) &
            missing(pre) & missing(rc) & missing(frais_fin) &
            missing(pvl_action) & missing(pvl_immo) & missing(pvl_oblig) & missing(mvl_action) & missing(mvl_immo) &
            missing(mvl_oblig) & missing(vm_vnc_precedent)) {
            # Traitement du cas vide
            .Object@annee <- as.integer(0)
            .Object@ptf_action <- new("Action")
            .Object@ptf_immo <- new("Immo")
            .Object@ptf_oblig <- new("Oblig")
            .Object@ptf_treso <- new("Treso")
            .Object@pre <- new("PRE")
            .Object@rc <- new("RC")
            .Object@frais_fin <- new("FraisFin")
            .Object@pvl_action <- numeric()
            .Object@pvl_immo <- numeric()
            .Object@pvl_oblig <- numeric()
            .Object@mvl_action <- numeric()
            .Object@mvl_immo <- numeric()
            .Object@mvl_oblig <- numeric()
            .Object@vm_vnc_precedent <- vm_vnc_precedent
        } else {
            stop("[PortFin] : La creation d'un nouvel objet PortFin necessite de renseigner l'ensemble des champs constituant un tel objet. \n")
        }
        return(.Object)
    }
)
