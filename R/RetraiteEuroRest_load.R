#----------------------------------------------------------------------------------------------------------------------------------------------------
#           load_reteurorest
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger un portefeuille retraite initital dans un objet de type \code{\link{RetraiteEuroRest}}.
##'
##' \code{load_reteurorest} est une methode permettant de charger un portefeuille retraite en phase de restitution.
##' @name load_reteurorest
##' @docType methods
##' @param file_reteurorest_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur permettant de renseigner le portefeuille.
##' @return L'objet de la classe \code{\link{Action}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @seealso La classe \code{\link{PortPassif}} et sa methode \code{\link{load_pp}}.
##' @include Action_class.R
setGeneric(name = "load_reteurorest", def = function(file_reteurorest_address){standardGeneric("load_reteurorest")})
setMethod(
    f = "load_reteurorest",
    signature = "character",
    definition = function(file_reteurorest_address){
        
        # Lecture du fichier
        temp <- read.csv2(file_reteurorest_address, header = TRUE)
        
        # Tests
        if (! all(! is.na(temp)))
            stop("[RetraiteEuroRest - load] : Presence de NA dans un fichier d'input.")
        
        # Creation de l'objet
        ptf_rer <- new(Class ="RetraiteEuroRest", 
                       mp = temp, 
                       tab = new("TabRetEuroRest"), 
                       tab_proba = new("TabProbaRetEuroRest", temp["num_mp"]))
        
        # Output
        return(ptf_rer)
    }
)
