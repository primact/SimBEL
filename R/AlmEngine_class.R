# Classe ALM Engine : classe vide contient uniquement des methodes ALM (regle achat vente)
# L'attribut journal_achat_vente permet de conserver en mémoire tout au long de la projection les différentes operations d'achat vente effectuees
setClass(Class = "AlmEngine",
         representation = representation(
           journal_achat_vente = "data.frame"
         ))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "AlmEngine",
            function (object){
              retval <- NULL
              nb_col <- 8
              #Verification du nombre de colonnes
              if(dim(object@journal_achat_vente)[2] != nb_col) {retval <- c(retval, "[AlmEngine] : Nombre d'attributs incorrect, le journal des transactions ALMEngine est compose d'un DF de 7 colonnes \n")}
              # Verification du type des colonnes
              if (!is.integer(object@journal_achat_vente[,1]))   {retval <- c(retval, "[AlmEngine] : annee n'est pas entier\n")}
              if (!is.integer(object@journal_achat_vente[,4]))   {retval <- c(retval, "[AlmEngine] : num_mp n'est pas entier\n")}
              if (!is.integer(object@journal_achat_vente[,5]))   {retval <- c(retval, "[AlmEngine] : num_index n'est pas entier\n")}
              if (!is.numeric(object@journal_achat_vente[,6]))   {retval <- c(retval, "[AlmEngine] : montant n'est pas reel\n")}
              if (!is.numeric(object@journal_achat_vente[,7]))   {retval <- c(retval, "[AlmEngine] : nb_unit n'est pas reel\n")}
              # Verification du nom des colonnes
              if(sum(colnames(object@journal_achat_vente) == c("annee","operation","type_actif","num_mp","num_index","montant","nb_unit", "pmvr")) != nb_col)
              {retval <- c(retval, "[Treso] : Noms de colonne incorrect\n")}

              if (is.null(retval)) return (TRUE)
              else return (retval)})

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "AlmEngine",
  definition = function(.Object, journal_achat_vente=data.frame()){
    nb_col <- 8
    # Traitement du cas o tout les ?l?ments sont renseign?s
    if( !missing(journal_achat_vente)){
      if(ncol(journal_achat_vente) != nb_col |
         sum(names(journal_achat_vente) != c("annee", "operation", "type_actif", "num_mp", "num_index", "montant", "nb_unit", "pmvr")) != 0)
      {stop("[AlmEngine] : Nombre ou nommage des colonnes du dataframe journal_achat_vente incorrect")}
      else if(!is.integer(journal_achat_vente[,"annee"])        | !is.character(journal_achat_vente[,"operation"]) |
              !is.character(journal_achat_vente[,"type_actif"]) | !is.integer(journal_achat_vente[,"num_mp"]) |
              !is.integer(journal_achat_vente[,"num_index"])    | !is.numeric(journal_achat_vente[,"montant"]) |
              !is.numeric(journal_achat_vente[,"nb_unit"])      | ! is.numeric(journal_achat_vente[,"pmvr"])){
        stop("[AlmEngine] : Typage incorrect des colonnes du dataframe")
      } else{
        .Object@journal_achat_vente <- journal_achat_vente
        validObject(.Object)
      }
    }
    #Traitement du cas vide
    else
    {.Object@journal_achat_vente  <- data.frame(integer(),character(), character(), integer(), integer(), double(),double(), double())
    colnames(.Object@journal_achat_vente) <- c("annee","operation","type_actif","num_mp","num_index","montant","nb_unit", "pmvr")
    }
    return(.Object)
  }
)
