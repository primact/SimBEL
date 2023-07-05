#----------------------------------------------------------------------------------------------------------------------------------------------------
#           extract_ESG : methode permettant d'extraire une situation economique
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' permet de construire et charger les trajectoires simulees par le Generateur de Scenarios Economiques de Prim'Act.
##'
##' \code{extract_ESG} construit l'objet de classe \code{\link{ModelPointESG}} a partir d'un objet
##'  de la classe \code{\link{ESG}}.
##' Le \code{ModelPointESG} ainsi construit correspond a l'extraction de donnees de l'ESG
##'  pour une annee specifique et pour une simulation specifique.
##' @name extract_ESG
##' @docType methods
##' @param x un objet de la classe \code{\link{ESG}}.
##' @param num_trajectoire une valeur de type \code{integer} correspondant a la trajectoire de simulation
##'  dont on souhaite obtenir les valeurs.
##' @param annee une valeur de type \code{integer} correspondant a l'annee d'interet pour le model point
##'  (possibilite de selectionner les annees 0 a \code{nb_annee_proj}).
##' @return \code{x} l'objet de la classe \code{\link{ModelPointESG}} construit.
##' @author Prim'Act
##' @seealso La classe \code{\link{ModelPointESG}}.
##' @export
##' @include ESG_class.R ModelPointESG_class.R
setGeneric(name = "extract_ESG", def = function(x, num_trajectoire, annee) {
    standardGeneric("extract_ESG")
})
setMethod(
    f = "extract_ESG",
    signature = c(x = "ESG", num_trajectoire = "integer", annee = "integer"),
    def = function(x, num_trajectoire, annee) {
        # Extraction de donnees
        ind_action <- x@ind_action
        ind_immo <- x@ind_immo
        len_ind_action <- length(ind_action)
        len_ind_immo <- length(ind_immo)

        # On parcourt la liste des differents indices action puis immobilier
        # Pour chaque indice, on retient la valeur correspondant a num_trajectoire, annee+1
        S_action <- sapply(1:len_ind_action, function(y) {
            return(.subset2(.subset2(ind_action, y), annee + 1)[num_trajectoire])
        })
        S_immo <- sapply(1:len_ind_immo, function(y) {
            return(.subset2(.subset2(ind_immo, y), annee + 1)[num_trajectoire])
        })

        if (annee > 0) {
            S_prev_action <- sapply(1:len_ind_action, function(y) {
                return(.subset2(.subset2(ind_action, y), annee)[num_trajectoire])
            })
            S_prev_immo <- sapply(1:len_ind_immo, function(y) {
                return(.subset2(.subset2(ind_immo, y), annee)[num_trajectoire])
            })
        } else {
            S_prev_action <- S_action
            S_prev_immo <- S_immo
        }

        # Extraction de l'inflation
        indice_inflation <- .subset2(x@ind_inflation, annee + 1L)[num_trajectoire]

        # Extraction des rendements
        yield_curve <- x@yield_curve
        yield_curve <- as.numeric(yield_curve[[paste0("annee", annee)]][num_trajectoire, ])

        # Extraction du deflateur
        deflateur <- .subset2(x@deflateur, annee + 1L)[num_trajectoire]

        # Output
        return(new("ModelPointESG",
            annee = annee,
            num_traj = num_trajectoire,
            indice_action = data.frame(S_action, S_prev_action),
            indice_immo = data.frame(S_immo, S_prev_immo),
            indice_inflation = indice_inflation,
            yield_curve = yield_curve,
            deflateur = deflateur
        ))
    }
)
