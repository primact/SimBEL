#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe EpEuroInd
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{EpEuroInd}.
##'
##' Une classe pour les passifs epargne en euros.
##'
##' @name EpEuroInd
##' @slot mp un objet \code{data.frame} au format fige contenant l'ensemble de model points epargne en euros.
##' @slot tab un objet de la classe \code{\link{TabEpEuroInd}} dedie au stockage de variables intermediaires.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul des primes, des prestations et des PM : \code{\link{calc_primes}},
##' \code{\link{calc_prest}}, \code{\link{calc_pm}}.
##' Le calcul des taux de sortie, du taux minimum et des taux cible de revalorisation :
##' \code{\link{calc_tx_sortie}}, \code{\link{calc_tx_min}}, \code{\link{calc_tx_cible}}.
##' La revalorisation des PM apres participation aux benefices : \code{\link{calc_relavo_pm}}.
##' Le vieillissement des model points sur une periode : \code{\link{vieilli_mp}}.
##' @keywords classes
##' @export
##' @include TabEpEuroInd-class.R
setClass(
  Class = "EpEuroInd",
  slots = c(mp = "data.frame", tab = "TabEpEuroInd"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL
    nb_col_attentu <- 29
    # Verification du nombre de colonnes
    if(ncol(object@mp) != nb_col_attentu){
      retval <- c(retval, "[EpEuroInd] : Nombre d'attributs incorrect, un ptf EpEuroInd est compose d'un DF de 29 colonnes\n")
    }

    # Verification du type des colonnes
    if (!is.integer(object@mp[,1]))   {retval <- c(retval, "[EpEuroInd] : num_mp n'est pas entier\n")}
    if (!is.integer(object@mp[,2]))  {retval <- c(retval, "[EpEuroInd] : num_canton n'est pas entier\n")}
    if (!is.integer(object@mp[,3]))   {retval <- c(retval, "[EpEuroInd] : num_prod n'est pas entier\n")}
    if (!is.integer(object@mp[,4]))   {retval <- c(retval, "[EpEuroInd] : age n'est pas entier\n")}
    if (!is.integer(object@mp[,5]))  {retval <- c(retval, "[EpEuroInd] : gen n'est pas entier\n")}
    if (!is.factor(object@mp[,6]))   {retval <- c(retval, "[EpEuroInd] : num_tab_mort n'est pas factor\n")}
    if (!is.numeric(object@mp[,7]))   {retval <- c(retval, "[EpEuroInd] : chgt_enc n'est pas numerique\n")}
    if (!is.logical(object@mp[,8]))   {retval <- c(retval, "[EpEuroInd] : ind_chgt_enc_pos n'est pas logical\n")}
    if (!is.numeric(object@mp[,9]))  {retval <- c(retval, "[EpEuroInd] : pm n'est pas numerique\n")}
    if (!is.numeric(object@mp[,10])) {retval <- c(retval, "[EpEuroInd] :  nb_contr n'est pas numeric\n")}
    if (!is.integer(object@mp[,11]))  {retval <- c(retval, "[EpEuroInd] : anc n'est pas entier\n")}
    if (!is.integer(object@mp[,12]))   {retval <- c(retval, "[EpEuroInd] : terme n'est pas entier\n")}
    if (!is.factor(object@mp[,13]))   {retval <- c(retval, "[EpEuroInd] : type_cot n'est pas factor\n")}
    if (!is.factor(object@mp[,14]))   {retval <- c(retval, "[EpEuroInd] : periode_cot n'est pas factor\n")}
    if (!is.factor(object@mp[,15]))  {retval <- c(retval, "[EpEuroInd] : tx_cible n'est pas factor\n")}
    if (!is.numeric(object@mp[,16]))   {retval <- c(retval, "[EpEuroInd] : chgt_prime n'est pas numeric\n")}
    if (!is.numeric(object@mp[,17]))   {retval <- c(retval, "[EpEuroInd] : prime n'est pas numeric\n")}
    if (!is.numeric(object@mp[,18]))   {retval <- c(retval, "[EpEuroInd] : tx_tech n'est pas numeric\n")}
    if (!is.integer(object@mp[,19])) {retval <- c(retval, "[EpEuroInd] : terme_tx_tech n'est pas integer\n")}
    if (!is.numeric(object@mp[,20]))  {retval <- c(retval, "[EpEuroInd] : tmg n'est pas numeric\n")}
    if (!is.integer(object@mp[,21])) {retval <- c(retval, "[EpEuroInd] : terme_tmg n'est pas integer\n")}
    if (!is.factor(object@mp[,22]))  {retval <- c(retval, "[EpEuroInd] : num_rach_tot n'est pas factor\n")}
    if (!is.factor(object@mp[,23])) {retval <- c(retval, "[EpEuroInd] : num_rach_part n'est pas factor\n")}
    if (!is.factor(object@mp[,24])) {retval <- c(retval, "[EpEuroInd] : num_rach_dyn_tot n'est pas factor\n")}
    if (!is.factor(object@mp[,25])) {retval <- c(retval, "[EpEuroInd] : num_rach_dyn_part n'est pas factor\n")}
    if (!is.numeric(object@mp[,26])) {retval <- c(retval, "[EpEuroInd] : chgt_rach n'est pas numeric\n")}
    if (!is.numeric(object@mp[,27])) {retval <- c(retval, "[EpEuroInd] : pm_gar n'est pas numeric\n")}
    if (!is.numeric(object@mp[,28])) {retval <- c(retval, "[EpEuroInd] : tx_revalo_prec n'est pas numeric\n")}
    if (!is.numeric(object@mp[,29])) {retval <- c(retval, "[EpEuroInd] : tx_cible_prec n'est pas numeric\n")}

    # Verification du nom des colonnes
    if(sum(colnames(object@mp) == c("num_mp","num_canton","num_prod","age","gen","num_tab_mort",
                                               "chgt_enc","ind_chgt_enc_pos","pm","nb_contr","anc","terme","type_cot",
                                               "periode_cot","tx_cible","chgt_prime","prime","tx_tech","terme_tx_tech",
                                               "tmg","terme_tmg","num_rach_tot","num_rach_part","num_rach_dyn_tot","num_rach_dyn_part",
                                               "chgt_rach", "pm_gar", "tx_revalo_prec","tx_cible_prec")) != nb_col_attentu){
      retval <- c(retval, "[EpEuroInd] : Noms de colonne incorrect \n")
    }

    # Verification de la table de resultats
    if(! validObject(object@tab)){retval <- c(retval, "[EpEuroInd] : Objet 'tab' non valide")}

    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (retval)
    }
  }
)

