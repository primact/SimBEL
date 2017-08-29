#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des donnnes et des hypotheses de la TauxPB
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger les valeurs des hypotheses et des donnees de TauXPB
##'
##' \code{tauxpb_load} est une methode permettant de charger les parametres associees a un objet de classe \code{\link{TauxPB}}.
##' @name tauxpb_load
##' @docType methods
##' @param file_tauxpb_address est un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur permettant de renseigner un objet \code{\link{TauxPB}}.
##' @return L'objet de la classe \code{\link{TauxPB}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l'input.
##' @export
##' @include HypCanton_class.R
setGeneric(name = "tauxpb_load", def = function(file_tauxpb_address){standardGeneric("tauxpb_load")})
setMethod(
    f = "tauxpb_load",
    signature = c(file_tauxpb_address = "character"),
    definition = function(file_tauxpb_address){
        
        # Lecture du fichier
        temp <- read.csv2(file_tauxpb_address, header = TRUE)
        
        # Tests
        if (! all(! is.na(temp)))
            stop("[TauxPB - load] : Presence de NA dans le fichier d'input")
        
        # Creation de l'objet
        taux_pb <- new(Class = "TauxPB", temp)
        
        # Output
        return(taux_pb)
    }
)
