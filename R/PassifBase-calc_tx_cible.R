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
##' \item{\code{list_rd} : } {est liste contenant les rendements de reference.}
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
##' @aliases EpEuroInd
##' @include EpEuroInd-class.R HypTech-class.R RetraiteEuroRest_class.R

#--------------------------------------------------------
setGeneric(name = "calc_tx_cible", def = function(x, y) {standardGeneric("calc_tx_cible")})
#--------------------------------------------------------

setMethod(
  f = "calc_tx_cible",
  signature = c(x = "EpEuroInd", y = "list" ),
  def = function(x, y){
    # Verification inputs
    if (length(y) != 2)                     {stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2. \n")}
    if (sum(names(y) == c("ht", "list_rd")) != length(y)) {stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2 de nom : ht, list_rd . \n")}
    if (class(y[["ht"]])      != "HypTech") {stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")}
    if (! is.list(y[["list_rd"]]))    {stop("[EpEuroInd : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")}
    ht      = y[["ht"]]
    list_rd = y[["list_rd"]]


    # Nom de ligne
    nb_mp <- nrow(x@mp)

    # Gestion des noms de colonnes du data.frame de donnnees
    nom_table = which(names(x@mp) == "tx_cible")
    tx_cible_prec = which(names(x@mp) == "tx_cible_prec")

    # Fonction d'extraction du taux cible
    calc_tx_cible_mp <- function(i) {return(
      get_comport(ht,
        as.character(.subset2(x@mp, nom_table)[i]),
        list_rd,
        .subset2(x@mp, tx_cible_prec)[i])
      )
    }

    # Calcul du taux cible annuel et semestriel
    tx_cible_an <- sapply(1:nb_mp, calc_tx_cible_mp)
    tx_cible_se <- taux_period(tx_cible_an, "se")

  return(list(tx_cible_an = tx_cible_an, tx_cible_se = tx_cible_se))

  }
)

#--------------------------------------------------------
setMethod(
  f = "calc_tx_cible",
  signature = c(x = "RetraiteEuroRest", y = "list" ),
  def = function(x, y){
    # Verification inputs
    if (length(y) != 2)                     {
      stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2. \n")}
    if (sum(names(y) == c("ht", "list_rd")) != length(y)) {
      stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2 de nom : ht, list_rd . \n")}
    if (class(y[["ht"]])      != "HypTech") {
      stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")}
    if (! is.list(y[["list_rd"]]))    {
      stop("[RetraiteEuroRest : calc_tx_cible] : L'input y doit correspondre a une liste de longueur 2, de nom : ht, list_rd, dont le type est : HypTech, list. \n")}
    ht      = y[["ht"]]
    list_rd = y[["list_rd"]]


    # Nom de ligne
    nb_mp <- nrow(x@mp)

    # Gestion des noms de colonnes du data.frame de donnnees
    nom_table = which(names(x@mp) == "tx_cible")
    tx_cible_prec = which(names(x@mp) == "tx_cible_prec")

    # Fonction d'extraction du taux cible
    calc_tx_cible_mp <- function(i) {return(
      get_comport(ht,
                  as.character(.subset2(x@mp, nom_table)[i]),
                  list_rd,
                  .subset2(x@mp, tx_cible_prec)[i])
    )
    }

    # Calcul du taux cible annuel et semestriel
    tx_cible_an <- sapply(1:nb_mp, calc_tx_cible_mp)
    tx_cible_se <- taux_period(tx_cible_an, "se")

    return(list(tx_cible_an = tx_cible_an, tx_cible_se = tx_cible_se))

  }
)


