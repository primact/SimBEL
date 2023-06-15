#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_rachat_up
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton dont les taux de rachat sont choques a la hausse.
##'
##' \code{do_choc_rachat_up} est une methode permettant d'appliquer le choc a la hausse des taux de rachat
##'  de la formule standard Solvabilite 2 a un canton.
##' @name do_choc_rachat_up
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central)
##'  de l'assureur.
##' @param autres_passifs_choc est un objet de la classe \code{\link{AutresPassifs}}, il correspond au chargement
##' des autres passifs choques en rachat a la hausse.
##' Ces derniers ont ete renseignes par l'utilisateur en donnees.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario de choc a la hausse
##'  des taux de rachats au sens de la formule standard Solvabilite 2.
##' @note La parametrisation des chocs a la hausse des taux de rachat est effectuee dans les fichiers d'inputs
##'  utilisateurs.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R AutresPassifs-class.R

setGeneric(name = "do_choc_rachat_up", def = function(x, canton, autres_passifs_choc) {
    standardGeneric("do_choc_rachat_up")
})
setMethod(
    f = "do_choc_rachat_up",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc) {
        choc_rachat_up <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_up"])
        choc_rachat_up_lim <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_up_lim"])
        ptf_passif <- canton@ptf_passif

        ptf_passif["ht"] <- get_choc_rach(ptf_passif["ht"], "up", choc_rachat_up, choc_rachat_up_lim)

        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc

        canton@ptf_passif <- ptf_passif
        return(canton)
    }
)
