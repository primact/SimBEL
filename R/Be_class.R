#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe Be
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{Be}.
##'
##' Une classe pour le calcul du best estimate d'un assureur.
##' @name Be
##' @slot param_be un objet \code{\link{ParamBe}} qui regroupe les parametres de base du calcul d'un best estimate.
##' @slot canton un objet de type \code{\link{Canton}} correspond au canton parametre en date initiale.
##' @slot esg un objet de type \code{\link{ESG}}.
##' @slot base un objet de type \code{\link{DataBase}}.
##' @slot tab_flux une liste qui contient les flux moyens de best estimate et de ses composantes.
##' @slot tab_be est une liste qui contient la valeur du best estimate et de ses composantes.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul d'un best estimate : \code{\link{run_be}}.
##' Le calcul d'une simulation de best estimate : \code{\link{run_be_simu}}.
##' L'initialisation d'un best estimate dans les situations centrales et choquees : \code{\link{init_scenario}}.
##' La sortie des resultats au format ".csv" : \code{\link{write_be_results}}.
##' La classe \code{\link{Canton}}.
##' La classe \code{\link{ESG}}.
##' La classe \code{\link{ParamBe}}.
##' @keywords classes
##' @export
##' @include Canton_class.R  ParamBe_class.R ESG_class.R DataBase_class.R
setClass(
    Class = "Be",
    representation = representation(
        param_be = "ParamBe",
        canton = "Canton",
        esg = "ESG",
        base = "DataBase",
        tab_flux = "list",
        tab_be = "list"
    ),
    validity = function (object){
        retval <- NULL

        if(!validObject(object@param_be)) retval <- c(retval, "[Be] : Objet ParamBe non valide")
        if(!validObject(object@canton))   retval <- c(retval, "[Be] : Objet Canton non valide")
        if(!validObject(object@esg))      retval <- c(retval, "[Be] : Objet ESG non valide")
        if(!validObject(object@base))     retval <- c(retval, "[Be] : Objet Base non valide")
        if(!is.list(object@tab_flux))     retval <- c(retval, "[Be] : Objet tab_flux non valide")
        if(!is.list(object@tab_be))       retval <- c(retval, "[Be] : Objet tab_be non valide")

        # Nom de la liste qui permettent d'alimenter les tableaux de flux
        nom_flux <- c("nom_produit", "prime", "prestation", "prestation_fdb", "frais", "flux_be")
        # Nom de la liste qui permettent d'alimenter les tableaux de flux
        nom_be <- c("nom_produit", "prime_actu", "prestation_actu", "prestation_fdb_actu", "frais_actu", "be")

        # Test sur la longueur des listes de resultats
        if(length(object@tab_flux) != length(nom_flux))
            retval <- c(retval, paste("[Be] : Objet tab_flux doit etre de longueur ", length(nom_flux),".", sep = ""))
        if(length(object@tab_be) != length(nom_be))
            retval <- c(retval, paste("[Be] : Objet tab_be doit etre de longueur ", length(nom_be),".", sep = ""))

        # Test sur les noms des listes de resultats
        if(! prod(names(object@tab_flux) == nom_flux))  retval <- c(retval, "[Be] : Nom de l'objet tab_flux non valide")
        if(! prod(names(object@tab_be) == nom_be))      retval <- c(retval, "[Be] : Nom de l'objet tab_be non valide")

        if (is.null(retval)) return (TRUE)
        else return (retval)
    }
)



# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "Be",
    definition = function(.Object,
                          param_be = new("ParamBe"),
                          canton = new("Canton"),
                          esg = new("ESG"),
                          base = "DataBase",
                          tab_flux = list(),
                          tab_be = list()
    ){
        if(!missing(param_be)      & !missing(canton) & ! missing(esg) & !missing(base) & !missing(tab_flux)  & !missing(tab_be)){
            .Object@param_be <- param_be
            .Object@canton <- canton
            .Object@esg   <- esg
            .Object@base   <- base
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
            # .Object@base = new("DataBase")
            .Object@tab_flux = list_flux
            .Object@tab_be = list_be
        }
        return(.Object)
    }
)
