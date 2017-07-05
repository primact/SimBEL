#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des taux de sortie d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Calcul des taux de sortie pour des contrats epargne en euros.
##'
##' \code{calc_tx_sortie} est une methode permettant de calculer les differents taux de sortie
##'   sur une periode.
##' @name calc_tx_sortie
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param y une liste contenant les parametres.
##' @param ht un objet de la classe \code{\link{HypTech}} contenant differentes tables de mortalite et differentes
##' lois de rachat.
##' @return Une matrice contenant pour chaque model points en ligne :
##' \describe{
##' \item{\code{qx_rach_tot} : }{un vecteur contenant les taux de rachats totaux}
##' \item{\code{qx_rach_tot_dyn} : }{un vecteur contenant les taux de rachats totaux dynamiques}
##' \item{\code{qx_dc} : }{un vecteur contenant les taux de deces}
##' \item{\code{qx_rach_part} : }{un vecteur contenant les taux de rachats partiels}
##' \item{\code{qx_rach_part_dyn} : }{un vecteur contenant les taux de rachats partiels dynamiques}.
##' }
##' @author Prim'Act
##' @seealso La recuperation des taux de rachat structurel : \code{\link{get_qx_rach}}.
##' La recuperation des taux de rachat dynamique : \code{\link{get_rach_dyn}}.
##' La recuperation des taux de deces : \code{\link{get_qx_mort}}.
##' @export
##' @include EpEuroInd-class.R HypTech-class.R RetraiteEuroRest_class.R

#--------------------------------------------------------
setGeneric(name = "calc_tx_sortie", def = function(x, y){standardGeneric("calc_tx_sortie")})
# Epargne
# y = list(ht)
# Retraite
# y = list()
#--------------------------------------------------------

setMethod(
  f = "calc_tx_sortie",
  signature = c(x = "EpEuroInd", y = "list"),
  def = function(x, y){
    # Verification inputs
    if (length(y) != 1)                        {stop("[EpEuroInd : calc_tx_sortie] : L'input y doit correspondre a une liste de longueur 1. \n")}
    if (sum(names(y) == c("ht")) != length(y)) {stop("[EpEuroInd : calc_tx_sortie] : L'input y doit correspondre a une liste de longueur 1 de nom ht . \n")}
    if (class(y[["ht"]]) != "HypTech")         {stop("[EpEuroInd : calc_tx_sortie] : L'input y doit correspondre a une liste de longueur 1, de nom ht, dont le type est HYpTech. \n")}
    ht = y [["ht"]]

    epi_mp <- x@mp

    # Nombre de lignes
    nb_mp <- nrow(x@mp)

    # Gestion des noms de colonnes du data.frame de donnnees
    nom_epi <- names(epi_mp)
    num_rach_tot <- which(nom_epi == "num_rach_tot")
    age <- which(nom_epi == "age")
    anc <- which(nom_epi == "anc")

    num_rach_dyn_tot <- which(nom_epi == "num_rach_dyn_tot")
    tx_cible_prec <- which(nom_epi == "tx_cible_prec")
    tx_revalo_prec <- which(nom_epi == "tx_revalo_prec")

    num_tab_mort <- which(nom_epi == "num_tab_mort")
    gen <- which(nom_epi == "gen")

    num_rach_part <- which(nom_epi == "num_rach_part")

    num_rach_dyn_part <- which(nom_epi == "num_rach_dyn_part")

    # Fonction d'extraction des taux de sortie
    calc_tx_sortie_mp <- function(i) {

      #Age du model point i
      age_i <- .subset2(epi_mp, age)[i]
      anc_i <- .subset2(epi_mp, anc)[i]
      tx_cible_prec_i <- .subset2(epi_mp, tx_cible_prec)[i]
      tx_revalo_prec_i <- .subset2(epi_mp, tx_revalo_prec)[i]

      return(
      c(qx_rach_tot = get_qx_rach(ht, as.character(.subset2(epi_mp, num_rach_tot)[i]), age_i, anc_i),
        qx_rach_tot_dyn = get_rach_dyn(ht, as.character(.subset2(epi_mp, num_rach_dyn_tot)[i]), tx_cible_prec_i, tx_revalo_prec_i),

        qx_dc = get_qx_mort(ht, as.character(.subset2(epi_mp, num_tab_mort)[i]), age_i, .subset2(epi_mp, gen)[i]),

        qx_rach_part = get_qx_rach(ht, as.character(.subset2(epi_mp, num_rach_part)[i]), age_i, anc_i),

        qx_rach_part_dyn = get_rach_dyn(ht, as.character(.subset2(epi_mp, num_rach_dyn_part)[i]),
                                        tx_cible_prec_i, tx_revalo_prec_i))
    )}

    return(t(sapply(1:nb_mp, calc_tx_sortie_mp)))
  }
)

#-----------------------------------------------------------
setMethod(
    f = "calc_tx_sortie",
    signature = c(x = "RetraiteEuroRest", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 1)                        {stop("[RetraiteEuroRest : calc_tx_sortie] : L'input y doit correspondre a une liste de longueur 1. \n")}
        if (sum(names(y) == c("ht")) != length(y)) {stop("[RetraiteEuroRest : calc_tx_sortie] : L'input y doit correspondre a une liste de longueur 1 de nom ht . \n")}
        if (class(y[["ht"]]) != "HypTech")         {stop("[RetraiteEuroRest : calc_tx_sortie] : L'input y doit correspondre a une liste de longueur 1, de nom ht, dont le type est HYpTech. \n")}
        ht = y [["ht"]]

        epi_mp <- x@mp

        # Nombre de lignes
        nb_mp <- nrow(x@mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_epi <- names(epi_mp)
        num_rach_tot <- which(nom_epi == "num_rach_tot")
        age <- which(nom_epi == "age")
        anc <- which(nom_epi == "anc")

        num_rach_dyn_tot <- which(nom_epi == "num_rach_dyn_tot")
        tx_cible_prec <- which(nom_epi == "tx_cible_prec")
        tx_revalo_prec <- which(nom_epi == "tx_revalo_prec")

        num_tab_mort <- which(nom_epi == "num_tab_mort")
        gen <- which(nom_epi == "gen")

        num_rach_part <- which(nom_epi == "num_rach_part")

        num_rach_dyn_part <- which(nom_epi == "num_rach_dyn_part")

        # Fonction d'extraction des taux de sortie
        calc_tx_sortie_mp <- function(i) {

            #Age du model point i
            age_i <- .subset2(epi_mp, age)[i]
            anc_i <- .subset2(epi_mp, anc)[i]
            tx_cible_prec_i <- .subset2(epi_mp, tx_cible_prec)[i]
            tx_revalo_prec_i <- .subset2(epi_mp, tx_revalo_prec)[i]

            return(
                c(qx_rach_tot = get_qx_rach(ht, as.character(.subset2(epi_mp, num_rach_tot)[i]), age_i, anc_i),
                  qx_rach_tot_dyn = get_rach_dyn(ht, as.character(.subset2(epi_mp, num_rach_dyn_tot)[i]), tx_cible_prec_i, tx_revalo_prec_i),

                  qx_dc = get_qx_mort(ht, as.character(.subset2(epi_mp, num_tab_mort)[i]), age_i, .subset2(epi_mp, gen)[i]),

                  qx_rach_part = get_qx_rach(ht, as.character(.subset2(epi_mp, num_rach_part)[i]), age_i, anc_i),

                  qx_rach_part_dyn = get_rach_dyn(ht, as.character(.subset2(epi_mp, num_rach_dyn_part)[i]),
                                                  tx_cible_prec_i, tx_revalo_prec_i))
            )}

        return(t(sapply(1:nb_mp, calc_tx_sortie_mp)))
    }
)
