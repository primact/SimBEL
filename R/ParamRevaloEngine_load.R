#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des hyp des hypotheses de revalorisation
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Chargement des attributs d'un objet \code{ParamRevaloEngine} a partir des donnees utilisateurs.
##'
##' \code{param_revalo_load} est la methode de chargement des attributs d'un objet \code{\link{ParamRevaloEngine}}
##' a partir des donnees de l'environnement utilisateur.
##' @name param_revalo_load
##' @docType methods
##' @param file_revalo_address un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur.
##' @return L'objet de la classe \code{\link{ParamRevaloEngine}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @include ParamRevaloEngine_class.R PortFin_class.R

setGeneric(name = "param_revalo_load", def = function(file_revalo_address) {
    standardGeneric("param_revalo_load")
})
setMethod(
    f = "param_revalo_load",
    signature = "character",
    definition = function(file_revalo_address) {
        # Lecture du fichier
        temp <- read.csv2(file_revalo_address)

        # Tests
        if (!all(!is.na(temp))) {
            stop("[ParamRevalo - load] : Presence de NA dans le fichier d'input")
        }

        # Creation de l'objet
        param_revalo <- new("ParamRevaloEngine",
            taux_pb_fi   = temp[, "taux_pb_fi"],
            taux_pb_tech = temp[, "taux_pb_tech"],
            tx_marge_min = temp[, "tx_marge_min"]
        )

        # Output
        return(param_revalo)
    }
)
