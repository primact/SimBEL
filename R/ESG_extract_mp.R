#----------------------------------------------------------------------------------------------------------------------------------------------------
#           extract_ESG : methode permettant d'extraire une situation economique
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' permet de construire et charger les trajectoires simulees par le Generateur de Scenarios Economiques de Prim'Act.
##'
##' \code{extract_ESG} construit l'objet de classe \code{\link{ModelPoint_ESG}} a partir d'un objet
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
##' @return \code{x} l'objet de la classe \code{\link{ModelPoint_ESG}} construit.
##' @author Prim'Act
##' @seealso La classe \code{\link{ModelPoint_ESG}}.
##' @export
##' @include ESG_class.R ModelPointESG_class.R
setGeneric(name = "extract_ESG", def = function(x, num_trajectoire,annee){standardGeneric("extract_ESG")})
setMethod(
  f = "extract_ESG",
  signature = c(x = "ESG", num_trajectoire = "integer", annee = "integer"),
  def = function(x,num_trajectoire,annee){
    # Declaration des vecteurs de stockage
    S_action      <- numeric()
    S_immo        <- numeric()
    S_prev_action <- numeric()
    S_prev_immo   <- numeric()

    # On parcourt la liste des differents indices action puis immobilier
    # Pour chaque indice, on retient la valeur correspondant a num_trajectoire, annee+1
    S_action      <- sapply(1:length(x@ind_action), function(y){
      S_action[y] <- .subset2(x@ind_action[[y]], annee + 1)[num_trajectoire]})
    S_immo        <- sapply(1:length(x@ind_immo),  function(y){
      S_immo[y] <- .subset2(x@ind_immo[[y]], annee + 1)[num_trajectoire]})

    if(annee > 0) {
      S_prev_action <- sapply(1:length(x@ind_action), function(y){
        S_action[y] <- .subset2(x@ind_action[[y]], annee)[num_trajectoire]})
      S_prev_immo   <- sapply(1:length(x@ind_immo), function(y){
        S_immo[y] <- .subset2(x@ind_immo[[y]], annee)[num_trajectoire]})
    } else {
      S_prev_action <- S_action
      S_prev_immo   <- S_immo
    }

    indice_inflation <- .subset2(x@ind_inflation, annee + 1)[num_trajectoire]
    yield_curve <- as.numeric(x@yield_curve[[paste("annee", annee, sep = "")]][num_trajectoire, ])
    deflateur   <- .subset2(x@deflateur, annee + 1)[num_trajectoire]

    x <- new("ModelPointESG",
             annee        = annee,
             num_traj     = num_trajectoire,
             indice_action= data.frame(S_action, S_prev_action),
             indice_immo  = data.frame(S_immo,S_prev_immo),
             indice_inflation = indice_inflation,
             yield_curve  = yield_curve,
             deflateur    = deflateur)
    return(x)
  }
)

