#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe TauxPB
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{TauxPB}.
##'
##' Une classe pour le stockage des parametres de taux de participation contractuelle par produit.
##'
##' @name TauxPB
##' @slot mp un \code{data frame} contenant les parametres des taux de participation contractuelle par produit.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export

setClass(
  Class = "TauxPB",
  slots = c(mp = "data.frame"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL
    nb_col_attentu <- 2
    # Verification du nombre de colonnes
    if(ncol(object@mp) != nb_col_attentu){
      retval <- c(retval, "[TauxPB] : Nombre d'attributs incorrect, un objet TauxPB est compose d'un DF de 2 colonnes\n")
    }

    # Verification du type des colonnes
    if (!is.factor(object@mp[,1]))   {retval <- c(retval, "[TauxPB] : nom_prod n'est pas character\n")}
    if (!is.numeric(object@mp[,2]))  {retval <- c(retval, "[TauxPB] : taux_pb n'est pas numeric\n")}

    if(prod(!object@mp[,2] < 0) * prod(!object@mp[,2] > 1) == 0) {retval <- c(retval, "[TauxPB] : taux_pb n'est pas entre 0 et 1 \n")}

    # Verification du nom des colonnes
    if(sum(colnames(object@mp) == c("nom_prod","taux_pb")) != nb_col_attentu){
      retval <- c(retval, "[TauxPB] : Noms de colonne incorrect \n")
    }

    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (retval)
    }
  }
)
