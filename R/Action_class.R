
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{Action}.
##'
##' Classe pour les actifs de type Action.
##'
##' @name Action
##' @slot ptf_action est un \code{data.frame}, chaque ligne represente un actif action du portefeuille d'action.
##' @docType class
##' @keywords classes
##' @author Prim'Act
##' @export
##' @seealso Les operations d'achat-vente action  \code{\link{buy_action}},
##' \code{\link{sell_action}} et \code{\link{sell_pvl_action}}.

setClass(
    Class = "Action",
    representation = representation(
        ptf_action = "data.frame")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur : permet ? chaque appel de l'objet de v?rifier quelques ?l?ments de base :
setValidity ("Action",
             function (object){

                 retval <- NULL
                 ptf_action <- object@ptf_action

                 #Verification du nombre de colonnes
                 if(ncol(ptf_action) != 14L) retval <- c(retval, "[Action] : Nombre d'attributs incorrect, un ptf Action
                                                         est compose d'un DF de 14 colonnes /n")

                 # Verification du type des colonnes
                 if (!is.integer(.subset2(ptf_action, 1L)))   retval <- c(retval, "[Action] : num_mp n'est pas entier/n")
                 if (!is.numeric(.subset2(ptf_action, 2L)))   retval <- c(retval, "[Action] : val_marche n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_action, 3L)))   retval <- c(retval, "[Action] : val_nc n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_action, 4L)))   retval <- c(retval, "[Action] : val_achat n'est pas reel/n")
                 if (!is.logical(.subset2(ptf_action, 5L)))   retval <- c(retval, "[Action] : presence n'est pas logical/n")
                 if (!is.logical(.subset2(ptf_action, 6L)))   retval <- c(retval, "[Action] : cessible n'est pas logical/n")
                 if (!is.numeric(.subset2(ptf_action, 7L)))   retval <- c(retval, "[Action] : nb_unit n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_action, 8L)))   retval <- c(retval, "[Action] : dur_det n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_action, 9L)))   retval <- c(retval, "[Action] : pdd n'est pas reel/n")
                 if (!is.integer(.subset2(ptf_action, 10L)))  retval <- c(retval, "[Action] : num_index n'est pas integer/n")
                 if (!is.numeric(.subset2(ptf_action, 11L)))  retval <- c(retval, "[Action] : div n'est pas reel/n")
                 if (!is.logical(.subset2(ptf_action, 12L)))  retval <- c(retval, "[Action] : ind_invest n'est pas logical/n")
                 if (!is.character(.subset2(ptf_action, 13L)))  retval <- c(retval, "[Action] : currency n'est pas character/n")
                 if (!is.numeric(.subset2(ptf_action, 14L)))  retval <- c(retval, "[Action] : fx_rate n'est pas reel/n")

                 # Verification du nom des colonnes
                 if(sum(colnames(object@ptf_action)==c("num_mp","val_marche","val_nc","val_achat",
                                                       "presence","cessible","nb_unit","dur_det",
                                                       "pdd","num_index","div","ind_invest", "currency", "fx_rate")) != 14L)
                     retval <- c(retval, "[Action] : Noms de colonne incorrect/n")

                 if (is.null(retval)) return (TRUE)
                 else return (retval)
             })

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "Action",
    definition = function(.Object, ptf=data.frame()){
        # Traitement du cas ou tout les elements sont renseignes
        if( !missing(ptf)){
            nom_table   <- names(ptf)
            num_mp      <- which(nom_table == "num_mp")
            val_marche  <- which(nom_table == "val_marche")
            val_nc      <- which(nom_table == "val_nc")
            val_achat   <- which(nom_table == "val_achat")
            presence    <- which(nom_table == "presence")
            cessible    <- which(nom_table == "cessible")
            nb_unit     <- which(nom_table == "nb_unit")
            dur_det     <- which(nom_table == "dur_det")
            pdd         <- which(nom_table == "pdd")
            num_index   <- which(nom_table == "num_index")
            div         <- which(nom_table == "div")
            ind_invest  <- which(nom_table == "ind_invest")
            currency    <- which(nom_table == "currency")
            fx_rate     <- which(nom_table == "fx_rate")

            if(ncol(ptf) != 14L | sum(names(ptf)!=c("num_mp","val_marche","val_nc","val_achat",
                                                    "presence","cessible","nb_unit","dur_det",
                                                    "pdd","num_index","div","ind_invest",
                                                    "currency", "fx_rate")) != 0L) {
                stop("[Action] : Nombre ou nommage des colonnes du dataframe incorrect")
            } else if(
                !is.integer(.subset2(ptf, num_mp))   | !is.numeric(.subset2(ptf, val_marche)) | !is.numeric(.subset2(ptf, val_nc))  | !is.numeric(.subset2(ptf, val_achat)) |
                !is.logical(.subset2(ptf, presence)) | !is.logical(.subset2(ptf, cessible))   | !is.numeric(.subset2(ptf, nb_unit)) | !is.numeric(.subset2(ptf, dur_det))   |
                !is.numeric(.subset2(ptf, pdd))      | !is.integer(.subset2(ptf, num_index))  | !is.numeric(.subset2(ptf, div))     | !is.logical(.subset2(ptf, ind_invest)) |
                !is.character(.subset2(ptf, currency))| !is.numeric(.subset2(ptf, fx_rate)) ) {
                stop("[Action] : Typage incorrect des colonnes du dataframe")
            } else {
                .Object@ptf_action <- ptf
                validObject(.Object)
            }
        }

        else { #Traitement du cas vide
            .Object@ptf_action  <- data.frame(integer(),numeric(),numeric(),numeric(),logical(),
                                              logical(),numeric(),numeric(),numeric(),integer(),numeric(),logical(),
                                              character(), numeric())
            colnames(.Object@ptf_action) <- c("num_mp","val_marche","val_nc","val_achat","presence",
                                              "cessible","nb_unit","dur_det","pdd","num_index","div","ind_invest",
                                              "currency", "fx_rate")
        }
        return(.Object)
    }
)
