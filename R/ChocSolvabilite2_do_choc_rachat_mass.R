
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_rachat_mass
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque rachat massif.
##'
##' \code{do_choc_rachat_mass} est une methode permettant d'appliquer le choc rachat massif de la formule standard
##'  Solvabilite 2 a un canton.
##' @name do_choc_rachat_mass
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central) de l'assureur.
##' @param autres_passifs_choc est un objet de la classe \code{\link{AutresPassifs}}, il correspond au chargement des autres passifs choques.
##' Ces derniers ont ete renseignes par l'utilisateur en donnees.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant a la mise a jour du choc rachat massif.
##' au sens de la formule standard Solvabilite 2.
##' @note La parametrisation des chocs de rachat massif est effectuee dans les fichiers d'inputs utilisateurs.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R AutresPassifs-class.R

setGeneric(name = "do_choc_rachat_mass", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_rachat_mass")})
setMethod(
    f = "do_choc_rachat_mass",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){

      # Application du choc lapse mass
      choc   <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_massif"])
      canton@ptf_passif@choc_lapse_mass <- choc

      # Chargement des autres passifs
      canton@ptf_passif@autres_passifs <- autres_passifs_choc

      return(canton)
    }
)
