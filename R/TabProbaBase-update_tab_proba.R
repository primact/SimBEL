#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de mise a jour des tableaux de probas avec insertion de nouvelles colonnes
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Mise a jour des tables de probas pour les contrats d'epargne et de retraite.
##' Insertion d'une nouvelle colonne avec les proabilites.
##'
##' \code{update_tab_proba} est une methode permettant de calculer les differents taux de sortie
##'   sur une periode.
##' @name update_tab_proba
##' @docType methods
##' @param x un objet de la classe \code{\link{TabProbaEpEuroInd}} ou de la classe \code{\link{TabProbaRetEuroRest}} a mettre a jour.
##' @param an une valeur de type \code{integer} correspondant a l'annee des probabilites.
##' @param y une liste contenant les probabilites a inserer dans la table :
##' \describe{
##' \item{\code{proba_flux} : }{probabilites de mouvement de flux pour les contrats d'epargne en euros et de retraite.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_proba_flux}}}
##' \item{\code{coef_rente} : }{ coefficients actuariels, uniquement pour le cas des retraites.}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @author Prim'Act
##' @seealso Le calcul des probabilites : \code{\link{calc_proba_flux}}.
##' @export
##' @include TabProbaEpEuroInd-class.R TabProbaRetEuroRest-class.R

#--------------------------------------------------------
setGeneric(name = "update_tab_proba", def = function(x, an, y) {standardGeneric("update_tab_proba")})


#--------------------------------------------------------
setMethod(
    f = "update_tab_proba",
    signature("TabProbaEpEuroInd", "integer", "list"),
    def = function(x, an, y){

        # Verification inputs
        if (length(y) != 1L)                                stop("[TabProbaEpEUroInd : update_tab_proba] : L'input y doit correspondre a une liste de longueur 1. \n")
        # Verification des noms des elements de la liste
        if (sum(names(y) == c("proba_flux")) != 1L)  stop("[TabProbaEpEUroInd : update_tab_proba] : L'input y doit correspondre a une liste de longueur 1 de nom : proba_flux\n")

        proba_flux <- .subset2(y, 1L)

        # Verification des types des elements de la liste
        if (! is.list(proba_flux))       stop("[TabProbaEpEUroInd : update_tab_proba] : L'input y doit correspondre a une liste de longueur 1, de nom : proba_flux dont le type est : matrix. \n")


        # Extraction des donnees de la liste
        nom_proba_flux <- names(proba_flux)
        num_rach_tot  <- which(nom_proba_flux == "qx_rach_tot")
        num_rach_part <- which(nom_proba_flux == "qx_rach_part")
        num_qx_dc     <- which(nom_proba_flux == "qx_dc")
        qx_rach_tot <- .subset2(proba_flux, num_rach_tot)
        qx_rach_part<- .subset2(proba_flux, num_rach_part)
        qx_dc       <- .subset2(proba_flux, num_qx_dc)

        # Creation nom de la colonne
        col <- paste("Annee", an, sep = "_")

        # Insertion des donnees
        x@qx_rach_tot[col]  <- qx_rach_tot
        x@qx_rach_part[col] <- qx_rach_part
        x@qx_dc[col]        <- qx_dc

        # Output
        return(x)
    }
)

#-----------------------------------------------------------
setMethod(
    f = "update_tab_proba",
    signature("TabProbaRetEuroRest", "integer", "list"),
    def = function(x, an, y){


        # Verification inputs
        if (length(y) != 2L)    stop("[TabProbaRetEuroRest : update_tab_proba] : L'input y doit correspondre a une liste de longueur 2. \n")

        # Verification des noms des elements de la liste
        if (sum(names(y) == c("proba_flux", "coef_rente")) != 2L)  stop("[TabProbaRetEuroRest : update_tab_proba] : L'input y doit correspondre a une liste de longueur 2 de nom : proba_flux\n")

        # Donnees
        proba_flux  <- .subset2(y, 1L)
        coef_rente  <- .subset2(y, 2L)

        # Verification des types des elements de la liste
        if (! (is.list(proba_flux) | is.null(y[["proba_flux"]])))    stop("[TabProbaRetEuroRest : update_tab_proba] : L'input y doit correspondre a une liste de longueur 2, de nom : proba_flux, coef_rente dont le type est : matrix, numeric. \n")
        if (! (is.numeric(coef_rente)| is.null(y[["coef_rente"]])))    stop("[TabProbaRetEuroRest : update_tab_proba] : L'input y doit correspondre a une liste de longueur 2, de nom : proba_flux, coef_rente dont le type est : matrix, numeric. \n")


        # Extraction des donnees de la liste
        ax                      <- coef_rente[1L, ]
        proba_sortie_retraite   <- proba_flux["proba_sortie_retraite"]
        proba_survie_un_an      <- proba_flux["proba_survie_un_an"]

        # Creation nom de la colonne
        col <- paste("Annee", an, sep = "_")

        # Insertion des donnees
        if (! is.null(ax))
            x@ax[col] <- ax

        if (! is.null(proba_sortie_retraite))
            x@sortie_retraite[col] <- proba_sortie_retraite

        if (! is.null(proba_survie_un_an))
            x@survie_un_an[col]    <- proba_survie_un_an

        # Output
        return(x)
    }
)
