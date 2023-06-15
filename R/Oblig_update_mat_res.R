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
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire).
##' @return L'objet \code{x} dont
##' \describe{
##' \item{\code{mat_res} : }{est diminuee d'une unite (une unite correspond a un an)}
##' \item{\code{dur_det} : }{est augmentee d'une unite (une unite correspond a un an)}
##' }
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "update_mat_res", def = function(x) {
    standardGeneric("update_mat_res")
})
setMethod(
    f = "update_mat_res",
    signature = c(x = "Oblig"),
    definition = function(x) {
        # Donnees
        ptf_oblig <- x@ptf_oblig
        nom_table <- names(ptf_oblig)
        mat_res <- which(nom_table == "mat_res")
        dur_det <- which(nom_table == "dur_det")

        # Mise a jour des nouvelles donnees
        new_mat_res <- .subset2(ptf_oblig, mat_res) - 1L
        ptf_oblig$mat_res <- new_mat_res
        ptf_oblig$dur_det <- .subset2(ptf_oblig, dur_det) + 1L

        # Operation de suppression des elements de maturite residuelle negative apres mise a jour
        num_del <- which(new_mat_res <= 0)
        if (length(num_del) > 0L) {
            ptf_oblig <- ptf_oblig[-num_del, ]
        }

        # Reordonnancement des num_mp et verification que l'on ne vide pas le portefeuille
        if (nrow(ptf_oblig) == 0L) warning("[Oblig : update_mat_res] : Attention : portefeuille obligataire vide")

        # Mise a jour du PTF oblig
        x@ptf_oblig <- ptf_oblig

        # Output
        return(x)
    }
)
