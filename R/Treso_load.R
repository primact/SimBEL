#----------------------------------------------------------------------------------------------------------------------------------------------------
#           load_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger le portefeuille treso initital dans un objet de type \code{\link{Treso}}.
##'
##' \code{load_treso} est une methode permettant de charger le portefeuille treso
##' @name load_treso
##' @docType methods
##' @param file_treso_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur permettant de renseigner le portefeuille.
##' @return L'objet de la classe \code{\link{Treso}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @seealso La classe \code{\link{PortFin}} et sa methode \code{\link{chargement_PortFin}}.
##' @include Action_class.R
setGeneric(name = "load_treso", def = function(file_treso_address){standardGeneric("load_treso")})
setMethod(
    f = "load_treso",
    signature = "character",
    definition = function(file_treso_address){
        
        # Lecture du fichier
        temp <- read.csv2(file_treso_address)
        
        # Tests
        if (! all(! is.na(temp)))
            stop("[Action - load] : Presence de NA dans le fichier d'input.")
        
        # Creation de l'objet
        ptf_treso <- new("Treso", ptf = data.frame(num_mp     = (temp[,"num_mp"]),
                                                   val_marche = (temp[,"val_marche"]),
                                                   val_nc     = (temp[,"val_nc"])))
        
        # Output
        return(ptf_treso)
    }
)
