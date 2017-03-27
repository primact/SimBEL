
# Cette fonction renvoie uniquement l'objet mis ajour des maturites residuelles !!! GERER LA SUPPRESSION DES OBLIGS ARRIVEES AUX TERMES
# Verifier que l'appel fait bien ressortir les
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_mat_res
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la maturite residuelle et de la duree de detention de chaque composante d'un portefeuille obligataire.
##'
##' \code{update_mat_res} est une methode permettant de mettre a jour la maturite residuelle et la duree de detention
##' de chaque composante d'un portefeuille obligataire.
##' @name update_mat_res
##' @docType methods
##' @param x objet de la classe Oblig (decrivant le portefeuille obligataire).
##' @return L'objet x dont
##' \describe{
##' \item{\code{mat_res} : }{est diminuee d'une unite (une unite correspond a un an)}
##' \item{\code{dur_det} : }{est augmentee d'une unite (une unite correspond a un an)}
##' }
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "update_mat_res", def = function(x){standardGeneric("update_mat_res")})
setMethod(
    f = "update_mat_res",
    signature = c(x = "Oblig"),
    definition = function(x){
        x["ptf_oblig"][,"mat_res"] <- x["ptf_oblig"][,"mat_res"] - 1
        x["ptf_oblig"][,"dur_det"] <- x["ptf_oblig"][,"dur_det"] + 1

        # Operation de suppression des elements de maturite residuelle negative apres mise a jour
        if(length(which(x["ptf_oblig"][,"mat_res"] <= 0))>0) {
            num_del         <- x["ptf_oblig"][which(x["ptf_oblig"][,"mat_res"] <= 0),"num_mp"]
            x["ptf_oblig"]  <- x["ptf_oblig"][which(!(x["ptf_oblig"][,"num_mp"] %in% num_del)),]
        }
        # Reordonnancement des num_mp et verification que l'on ne vide pas le portefeuille
        if(nrow(x["ptf_oblig"]) == 0) {cat(" [Oblig : update_mat_res] : Attention : portefeuille obligataire vide\n")}
        return(x)
    }
)
