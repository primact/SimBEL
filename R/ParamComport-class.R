#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamComport
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe de parametres de comportement \code{ParamComport}.
##'
##' Une classe pour les parametres de comportement.
##' @name ParamComport
##' @slot mat_oblig une valeur \code{numeric} correspondant a la maturite du taux de rendement obligataire pris en
##' reference sur le marche.
##' @slot alloc_mar un vecteur \code{numeric} correspondant a l'allocation pris en reference sur le marche.
##' Le format de cette liste est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference.}{}
##' }
##' @slot w_n une valeur \code{numeric} correspondant au poids accorde au rendement de l'annee courante par
##' rapport a l'annee precedente.
##' @slot marge_mar une valeur \code{numeric} correspondant a la marge financiere pris en reference sur le marche.
##' @slot ch_enc_mar une valeur \code{numeric} correspondant au niveau de chargement sur encours
##' pris en reference sur le marche.
##' @slot ind_ref_action une valeur \code{numeric} correspondant au numero de l'indice action
##' pris en reference sur le marche.
##' @slot ind_ref_immo une valeur \code{numeric} correspondant au numero de l'indice immobilier
##' pris en reference sur le marche.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul du taux cible \code{\link{calc_tx_cible_ref_marche}}.
##' @keywords classes
##' @export

setClass(
  Class = "ParamComport",
  slots = c(mat_oblig = "numeric", alloc_mar = "numeric", w_n = "numeric",
            marge_mar = "numeric", ch_enc_mar = "numeric", ind_ref_action = "numeric", ind_ref_immo = "numeric"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL

    # Verification du type des colonnes
    if (!is.numeric(object@mat_oblig))   {retval <- c(retval, "[ParamComport] : mat_oblig n'est pas numeric\n")}
    if (!is.numeric(object@alloc_mar))  {retval <- c(retval, "[ParamComport] : alloc_mar n'est pas numeric\n")}
    if (!is.numeric(object@w_n))   {retval <- c(retval, "[ParamComport] : w_n n'est pas numeric\n")}
    if (!is.numeric(object@marge_mar))  {retval <- c(retval, "[ParamComport] : marge_mar n'est pas numeric\n")}
    if (!is.numeric(object@ch_enc_mar))   {retval <- c(retval, "[ParamComport] : ch_enc_mar n'est pas numeric\n")}
    if (!is.numeric(object@ind_ref_action))  {retval <- c(retval, "[ParamComport] : ind_ref_action n'est pas numeric\n")}
    if (!is.numeric(object@ind_ref_immo))  {retval <- c(retval, "[ParamComport] : ind_ref_immo n'est pas numeric\n")}

    if(object@mat_oblig < 0 | round(object@mat_oblig) != object@mat_oblig ) {retval <- c(retval, "[ParamComport] :  maturite doit etre un entier strictement positif. \n")}


    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (retval)
    }
  }
)
