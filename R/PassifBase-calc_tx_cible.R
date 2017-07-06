#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd et RetEpEuroRest
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul du taux cible pour des contrats epargne en euros et de retraite en phase de restitution.
##'
##' \code{calc_tx_cible} est une methode permettant d'evaluer le taux de revalorisation cible
##'   de chaque model point.
##' @name calc_tx_cible
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros ou retraite euro en phases de restitution.
##' @param y une liste contenant les parametres.
##' \describe{
##' \item{\code{list_rd} : }{est un vecteur contenant les rendements de reference.}
##' \item{\code{ht} : }{est un objet de la classe \code{\link{HypTech}}.}
##' }
##' Le format de \code{list_rd} est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @return \code{tx_cible_an} : un vecteur contenant les taux cible de l'annee
##' @return \code{tx_cible_se} : un vecteur contenant les taux cible de l'annee sur base semestrielle
##' @note Pour les besoins des calculs a mi-annee, des taux semestriels sont produits.
##' @author Prim'Act
##' @seealso La recuperation des taux cibles calcules : \code{\link{get_comport}}.
##' @export
##' @include EpEuroInd-class.R HypTech-class.R RetraiteEuroRest_class.R

#--------------------------------------------------------
setGeneric(name = "calc_tx_cible", def = function(x, y) {standardGeneric("calc_tx_cible")})
#--------------------------------------------------------

setMethod(
    f = "calc_tx_cible",
    signature = c(x = "EpEuroInd", y = "list" ),
    def = function(x, y){

        # Verification inputs
        if (length(y) != 2L) stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2. \n")
        if (sum(names(y) == c("ht", "list_rd")) != 2L) stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2 de nom : ht, list_rd . \n")

        # Extraction listes
        ht      <- .subset2(y, 1L)
        list_rd <- .subset2(y, 2L)

        if (class(ht) != "HypTech") stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")
        if (! is.numeric(list_rd))  stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")


        # ModelPoint
        mp <- x@mp

        # Nom de ligne
        nb_mp <- nrow(mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_tx_cible <- names(mp)
        num_tx_cible <- which(nom_tx_cible == "tx_cible")
        num_tx_cible_prec <- which(nom_tx_cible == "tx_cible_prec")
        tx_cible <- .subset2(mp, num_tx_cible)
        tx_cible_prec <- .subset2(mp, num_tx_cible_prec)

        # Extraction des differentes methodes
        meth_tx_cible <- as.character(unique(tx_cible))

        # Initialisation du vecteur tx_cible
        tx_cible_an <-vector("numeric", length = nb_mp)

        # Calcul des tx cibles pour chacune des methodes
        for (meth in meth_tx_cible) {
            row <- which(tx_cible == meth) # lignes associees a la methode en question
            tx_cible_an[row] <- get_comport(ht, meth, list_rd, tx_cible_prec[row]) # calcul du taux cible : C++
        }

        # Calcul du taux cible annuel et semestriel
        tx_cible_se <- taux_period(tx_cible_an, "se")

        # Output
        return(list(tx_cible_an = tx_cible_an, tx_cible_se = tx_cible_se))

    }
)

#--------------------------------------------------------
setMethod(
    f = "calc_tx_cible",
    signature = c(x = "RetraiteEuroRest", y = "list" ),
    def = function(x, y){

        # Verification inputs
        if (length(y) != 2L) stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2. \n")
        if (sum(names(y) == c("ht", "list_rd")) != 2L) stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2 de nom : ht, list_rd . \n")

        # Extraction listes
        ht      <- .subset2(y, 1L)
        list_rd <- .subset2(y, 2L)

        if (! is.numeric(list_rd))  stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")
        if (class(ht) != "HypTech") stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")

        # ModelPoint
        mp <- x@mp

        # Nom de ligne
        nb_mp <- nrow(mp)

        # Gestion des noms de colonnes du data.frame de donnnees
        nom_tx_cible <- names(mp)
        num_tx_cible <- which(nom_tx_cible == "tx_cible")
        num_tx_cible_prec <- which(nom_tx_cible == "tx_cible_prec")
        tx_cible <- .subset2(mp, num_tx_cible)
        tx_cible_prec <- .subset2(mp, num_tx_cible_prec)

        # Extraction des differentes methodes
        meth_tx_cible <- as.character(unique(tx_cible))

        # Initialisation du vecteur tx_cible
        tx_cible_an <-vector("numeric", length = nb_mp)

        # Calcul des tx cibles pour chacune des methodes
        for (meth in meth_tx_cible) {
            row <- which(tx_cible == meth) # lignes associees a la methode en question
            tx_cible_an[row] <- get_comport(ht, meth, list_rd, tx_cible_prec[row]) # calcul du taux cible : C++
        }

        # Calcul du taux cible semestriel
        tx_cible_se <- taux_period(tx_cible_an, "se")

        # Output
        return(list(tx_cible_an = tx_cible_an, tx_cible_se = tx_cible_se))
    }
)


