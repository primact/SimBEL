#----------------------------------------------------------------------------------------------------------------------------------------------------
#           load_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger le portefeuille action initital dans un objet de type \code{\link{Action}}.
##'
##' \code{load_action} est une methode permettant de charger le portefeuille action.
##' @name load_action
##' @docType methods
##' @param file_action_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur permettant de renseigner le portefeuille.
##' @return L'objet de la classe \code{\link{Action}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @seealso La classe \code{\link{PortFin}} et sa methode \code{\link{chargement_PortFin}}.
##' @include Action_class.R
setGeneric(name = "load_action", def = function(file_action_address){standardGeneric("load_action")})
setMethod(
    f = "load_action",
    signature = "character",
    definition = function(file_action_address){

        # Lecture du fichier
        temp <- read.csv2(file_action_address, colClasses = c("integer","numeric","numeric","numeric","logical",
                                                              "logical","numeric","numeric","numeric","integer","numeric","logical",
                                                              "character", "numeric"))

        # Tests
        if (! all(! is.na(temp)))
            stop("[Action - load] : Presence de NA dans le fichier d'input.")
        if(! all(temp[,"nb_unit"] > 0))
            stop("[Action - load] : Le portefeuille Action ne peut contenir des nombres d'unites negatives ou nulles")
        if(! all(temp[,"val_marche"] > 0))
            stop("[Action - load] : Le portefeuille Action ne peut contenir des valeurs de marche negatives ou nulles")
        if(! all(temp[,"fx_rate"] > 0))
          stop("[Action - load] : Le portefeuille Action ne peut contenir des taux de change negatifs ou nuls")

        # Conversion du champs contenant les devises en character
        ptf = data.frame(num_mp     = (temp[,"num_mp"]),
                         val_marche = (temp[,"val_marche"]),
                         val_nc     = (temp[,"val_nc"]),
                         val_achat  = (temp[,"val_achat"]),
                         presence   = (temp[,"presence"]),
                         cessible   = (temp[,"cessible"]),
                         nb_unit    = (temp[,"nb_unit"]),
                         dur_det    = (temp[,"dur_det"]),
                         pdd        = (temp[,"pdd"]),
                         num_index  = (temp[,"num_index"]),
                         div        = (temp[,"div"]),
                         ind_invest = (temp[,"ind_invest"]),
                         currency   = (temp[,"currency"]),
                         fx_rate   =  (temp[,"fx_rate"])
        )
        ptf$currency <- as.character(ptf$currency)

        # Creation de l'objet
        ptf_action <- new("Action", ptf = ptf)

        # Output
        return(ptf_action)
    }
)
