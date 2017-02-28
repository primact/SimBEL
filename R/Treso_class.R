#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Treso
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe Treso
##'
##' Classe pour les actifs de type Tresorerie
##'
##' @name Treso
##' @slot ptf_treso est un dataframe, chaque ligne represente un actif de tresorerie du portefeuille de monetaire.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
#removeClass("Treso")
setClass(
  Class = "Treso",
  representation = representation(
    ptf_treso = "data.frame"
  ))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "Treso",
            function (object){
              retval <- NULL
              nb_col <- 3
              #Verification du nombre de colonnes
              if(dim(object@ptf_treso)[2] != nb_col) {retval <- c(retval, "[Treso] : Nombre d'attributs incorrect, un ptf Treso est compose d'un DF de 3 colonnes \n")}
              if(nrow(object@ptf_treso) > 1)         {retval <-c(retval, "[Treso] : Un seul compte de tresorerie est tolere \n")}
              # Verification du type des colonnes
              if (!is.integer(object@ptf_treso[,1]))  {retval <- c(retval, "[Treso] : num_mp n'est pas entier\n")}
              if (!is.double(object@ptf_treso[,2]))   {retval <- c(retval, "[Treso] : val_marche n'est pas reel\n")}
              if (!is.double(object@ptf_treso[,3]))   {retval <- c(retval, "[Treso] : val_nc n'est pas reel\n")}

              # Verification du nom des colonnes
              if(sum(colnames(object@ptf_treso) == c("num_mp","val_marche","val_nc")) != nb_col)
              {retval <- c(retval, "[Treso] : Noms de colonne incorrect\n")}

              if (is.null(retval)) return (TRUE)
              else return (retval)})

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "Treso",
  definition = function(.Object, ptf=data.frame()){
    nb_col <- 3
    # Traitement du cas o? tout les ?l?ments sont renseign?s
    if( !missing(ptf)){
      if(ncol(ptf) != nb_col | sum(names(ptf) != c("num_mp", "val_marche", "val_nc")) != 0)
      {stop("[Treso] : Nombre ou nommage des colonnes du dataframe incorrect")}
      else if(!is.integer(ptf[,"num_mp"])   | !is.double(ptf[,"val_marche"]) | !is.double(ptf[,"val_nc"])) {
        stop("[Treso] : Typage incorrect des colonnes du dataframe")
      } else{
        .Object@ptf_treso <- ptf
        validObject(.Object)
      }
    }
    #Traitement du cas vide
    else
    {.Object@ptf_treso  <- data.frame(integer(),double(),double())
    colnames(.Object@ptf_treso) <- c("num_mp","val_marche","val_nc")
    }
    return(.Object)
  }
)
