
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Classe pour les actifs de type immobilier.
##'
##' @name Immo
##' @slot ptf_immo est un dataframe, chaque ligne represente un actif immobilier du portefeuille d'immobilier.
##' @docType class
##' @section Lien a creer
##' @keywords classes
##' @author Prim'Act
##' @export
##' @seealso Les operations d'achat vente immo  \code{\link{buy_immo}} et \code{\link{sell_immo}}.
setClass(
    Class = "Immo",
    representation = representation(
        ptf_immo = "data.frame")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity ("Immo",
             function (object){

                 retval <- NULL
                 nb_col_attendu <- 14L

                 # Recuperation du PTF immo
                 ptf_immo <- object@ptf_immo

                 #Verification du nombre de colonnes
                 if(dim(ptf_immo)[2L] != nb_col_attendu) retval <- c(retval, "[Immo] : Nombre d'attributs incorrect, un ptf Immo est compose d'un DF de 14 colonnes /n")


                 # Verification du type des colonnes
                 if (!is.integer(.subset2(ptf_immo, 1L)))       retval <- c(retval, "[Immo] : num_mp n'est pas entier/n")
                 if (!is.numeric(.subset2(ptf_immo, 2L)))       retval <- c(retval, "[Immo] : val_marche n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_immo, 3L)))       retval <- c(retval, "[Immo] : val_nc n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_immo, 4L)))       retval <- c(retval, "[Immo] : val_achat n'est pas reel/n")
                 if (!is.logical(.subset2(ptf_immo, 5L)))       retval <- c(retval, "[Immo] : presence n'est pas logical/n")
                 if (!is.logical(.subset2(ptf_immo, 6L)))       retval <- c(retval, "[Immo] : cessible n'est pas logical/n")
                 if (!is.numeric(.subset2(ptf_immo, 7L)))       retval <- c(retval, "[Immo] : nb_unit n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_immo, 8L)))       retval <- c(retval, "[Immo] : dur_det n'est pas reel/n")
                 if (!is.numeric(.subset2(ptf_immo, 9L)))       retval <- c(retval, "[Immo] : pdd n'est pas reel/n")
                 if (!is.integer(.subset2(ptf_immo, 10L)))      retval <- c(retval, "[Immo] : num_index n'est pas integer/n")
                 if (!is.numeric(.subset2(ptf_immo, 11L)))      retval <- c(retval, "[Immo] : loyer n'est pas reel/n")
                 if (!is.logical(.subset2(ptf_immo, 12L)))      retval <- c(retval, "[Immo] : ind_invest n'est pas logical/n")
                 if (!is.character(.subset2(ptf_immo, 13L)))    retval <- c(retval, "[Immo] : currency n'est pas character/n")
                 if (!is.numeric(.subset2(ptf_immo, 14L)))      retval <- c(retval, "[Immo] : fx_rate n'est pas reel/n")


                 # Verification du nom des colonnes
                 if(sum(colnames(ptf_immo)==c("num_mp","val_marche","val_nc","val_achat", "presence","cessible","nb_unit","dur_det",
                                              "pdd","num_index","loyer","ind_invest",
                                              "currency", "fx_rate")) != nb_col_attendu)
                     retval <- c(retval, "[Immo] : Noms de colonne incorrect/n")

                 if (is.null(retval)) return (TRUE)
                 else return (retval)
             })

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "Immo",
    definition = function(.Object, ptf=data.frame()){
        # Traitement du cas ou tout les elements sont renseignes
        if( ! missing(ptf)){
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
            loyer       <- which(nom_table == "loyer")
            ind_invest  <- which(nom_table == "ind_invest")
            currency    <- which(nom_table == "currency")
            fx_rate     <- which(nom_table == "fx_rate")


            if(ncol(ptf) != 14L | sum(names(ptf)!=c("num_mp","val_marche","val_nc","val_achat", "presence","cessible","nb_unit","dur_det",
                                                    "pdd","num_index","loyer","ind_invest",
                                                    "currency", "fx_rate")) != 0L){
                stop("[Immo] : Nombre ou nommage des colonnes du dataframe incorrect")
            } else if(
                !is.integer(.subset2(ptf, num_mp))   | !is.numeric(.subset2(ptf, val_marche)) | !is.numeric(.subset2(ptf, val_nc))  | !is.numeric(.subset2(ptf,val_achat)) |
                !is.logical(.subset2(ptf, presence)) | !is.logical(.subset2(ptf, cessible))   | !is.numeric(.subset2(ptf, nb_unit)) | !is.numeric(.subset2(ptf, dur_det))   |
                !is.numeric(.subset2(ptf, pdd))      | !is.integer(.subset2(ptf, num_index))  | !is.numeric(.subset2(ptf, loyer))   | !is.logical(.subset2(ptf, ind_invest))|
                !is.character(.subset2(ptf, currency))| !is.numeric(.subset2(ptf, fx_rate)) ) {
                stop("[Immo] : Typage incorrect des colonnes du dataframe")
            }else {
                .Object@ptf_immo <- ptf
                validObject(.Object)
            }
        } else { #Traitement du cas vide
            .Object@ptf_immo  <- data.frame(integer(),numeric(),numeric(),numeric(),logical(),
                                            logical(),numeric(),numeric(),numeric(),integer(),numeric(),logical(),
                                            character(), numeric())
            colnames(.Object@ptf_immo) <- c("num_mp","val_marche","val_nc","val_achat","presence",
                                            "cessible","nb_unit","dur_det","pdd","num_index","loyer","ind_invest",
                                            "currency", "fx_rate")
        }

        return(.Object)
    }
)
