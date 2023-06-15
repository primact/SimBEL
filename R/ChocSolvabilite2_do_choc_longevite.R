#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_longevite
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque longevite.
##'
##' \code{do_choc_longevite} est une methode permettant d'appliquer le choc longevite de la formule standard
##'  Solvabilite 2 a un canton.
##' @name do_choc_longevite
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central) de l'assureur.
##' @param autres_passifs_choc est un objet de la classe \code{\link{AutresPassifs}}, il correspond au chargement des autres passifs
##' choques en longevite.
##' Ces derniers ont ete renseignes par l'utilisateur en donnees.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario choque longevite
##' au sens de la formule standard Solvabilite 2.
##' @note La parametrisation des chocs de longevite est effectuee dans les fichiers d'inputs utilisateurs.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R AutresPassifs-class.R

setGeneric(name = "do_choc_longevite", def = function(x, canton, autres_passifs_choc) {
    standardGeneric("do_choc_longevite")
})
setMethod(
    f = "do_choc_longevite",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc) {
        choc_longevite <- as.numeric(x@param_choc_sousc["mp"]["choc_longevite"])
        ptf_passif <- canton@ptf_passif

        ptf_passif["ht"] <- get_choc_table(ptf_passif["ht"], choc_longevite)

        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc

        canton@ptf_passif <- ptf_passif
        return(canton)
    }
)
