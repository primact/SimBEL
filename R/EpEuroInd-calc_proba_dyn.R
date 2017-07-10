#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des probas dynamiques de mouvement de flux d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Calcul les probabilites dynamiques de mouvement de flux pour des contrats epargne en euros et de retraite.
##'
##' \code{calc_proba_dyn} est une methode permettant de calculer les differents taux de sortie dynamique
##'   sur une periode.
##' @name calc_proba_dyn
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param ht un objet de la classe \code{\link{HypTech}} contenant differentes tables de mortalite et differentes
##' lois de rachat.
##' @return Une matrice contenant pour chaque model points en ligne :
##' \describe{
##' \item{\code{qx_rach_tot_dyn} : }{un vecteur contenant les taux de rachats totaux dynamiques}
##' \item{\code{qx_rach_part_dyn} : }{un vecteur contenant les taux de rachats partiels dynamiques}.
##' }
##' @author Prim'Act
##' @seealso La recuperation des taux de rachat dynamique : \code{\link{get_rach_dyn}}.
##' @export
##' @include EpEuroInd-class.R HypTech-class.R

#--------------------------------------------------------
setGeneric(name = "calc_proba_dyn", def = function(x, ht){standardGeneric("calc_proba_dyn")})
# Epargne
# y = list(ht)
# Retraite
# y = list()
#--------------------------------------------------------

setMethod(
    f = "calc_proba_dyn",
    signature = c(x = "EpEuroInd", ht = "HypTech"),
    def = function(x, ht){

        # Model point
        epi_mp <- x@mp

        # Nombre de lignes
        nb_mp <- nrow(epi_mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_epi <- names(epi_mp)
        num_rach_dyn_tot   <- which(nom_epi == "num_rach_dyn_tot")
        num_rach_dyn_part  <- which(nom_epi == "num_rach_dyn_part")
        num_tx_cible_prec  <- which(nom_epi == "tx_cible_prec")
        num_tx_revalo_prec <- which(nom_epi == "tx_revalo_prec")

        # Extraction des donnees
        rach_dyn_tot  <- .subset2(epi_mp, num_rach_dyn_tot)
        rach_dyn_part <- .subset2(epi_mp, num_rach_dyn_part)
        tx_cible_prec  <- .subset2(epi_mp, num_tx_cible_prec)
        tx_revalo_prec <- .subset2(epi_mp, num_tx_revalo_prec)

        # Extraction des differentes methodes
        tab_rach_tot  <- levels(rach_dyn_tot)
        tab_rach_part <- levels(rach_dyn_part)

        # Initialisation du vecteur tx_cible
        qx_rach_tot_dyn  <-vector("numeric", length = nb_mp)
        qx_rach_part_dyn <-vector("numeric", length = nb_mp)

        # Calcul des qx pour les rachats totaux
        for (tab in tab_rach_tot) {
            row <- which(rach_dyn_tot == tab) # lignes associees a la methode en question
            qx_rach_tot_dyn[row] <- get_rach_dyn(ht, tab, tx_cible_prec[row], tx_revalo_prec[row])
        }

        # Calcul des qx pour les rachats partiels
        for (tab in tab_rach_part) {
            row <- which(rach_dyn_part == tab) # lignes associees a la methode en question
            qx_rach_part_dyn[row] <- get_rach_dyn(ht, tab, tx_cible_prec[row], tx_revalo_prec[row])
        }

        # Output
        return(list(qx_rach_tot_dyn = qx_rach_tot_dyn,
                    qx_rach_part_dyn = qx_rach_part_dyn))
    }
)
