#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les methodes permettant d'extraire une situation economique
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           extract_ESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Cette methode construit l'objet de classe \code{ModelPoint_ESG} a partir d'un objet de la classe \code{ESG}. 
##' Le ModelPoint ESG ainsi construit correspond a l'extraction de courbes de l'ESG pour une annee specifique et pour une simulation specifique.
##'
##' \code{extract_ESG} est une methode permettant de construire et charger les trajectoires simulees par le Generateur de
##' Scenarios Economiques de Prim'Act. 
##' @name extract_ESG
##' @docType methods
##' @param x est un objet de la classe \code{ESG}.
##' @param num_trajectoire est une valeur de type \code{integer} correspondant a la trajectoire de simulation dont on souhaite obtenir les valeurs.
##' @param annee est une valeur de type \code{integer} correspondant a l'annee d'interet pour le model point (possibilite de selectionner les annees 0 a nb_annee_proj).
##' @return \code{x} l'objet de la classe \code{ModelPoint_ESG} construit.
##' @author Prim'Act
##' @seealso La classe \code{\link{ModelPoint_ESG}}.
##' @export
##' @aliases ESG
##' @include ESG_class.R


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
    S_action      <- unlist(lapply(1:length(x["ind_action"]),function(y){S_action[y] <- as.data.frame(x["ind_action"][y])[num_trajectoire, annee + 1]}))
    S_immo        <- unlist(lapply(1:length(x["ind_immo"]),function(y){S_immo[y] <- as.data.frame(x["ind_immo"][y])[num_trajectoire, annee + 1]}))

    if(annee > 0) {
      S_prev_action <- unlist(lapply(1:length(x["ind_action"]),function(y){S_action[y] <- as.data.frame(x["ind_action"][y])[num_trajectoire, annee]}))
      S_prev_immo   <- unlist(lapply(1:length(x["ind_immo"]),function(y){S_immo[y] <- as.data.frame(x["ind_immo"][y])[num_trajectoire, annee]}))
    } else {
      S_prev_action <- S_action
      S_prev_immo   <- S_immo
    }

    indice_inflation <- as.numeric(x["ind_inflation"][num_trajectoire, annee + 1])
    yield_curve <- as.numeric(unlist(as.data.frame(x["yield_curve"][paste("annee", annee, sep = "")])[num_trajectoire,]))
    deflateur   <- as.numeric(x["deflateur"][num_trajectoire, annee + 1])

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

