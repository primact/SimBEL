SEUIL_DEL_ACTION <- .001
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille action suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_action} est une methode permettant de mettre a jour chaque composante d'un portefeuille action suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}} (decrivant le portefeuille action en detention).
##' @param num_sold vecteur de type \code{numeric} contenant le numero de model point action du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur de type \code{numeric} contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return \code{action} l'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @return \code{pmvr} le montant des plus ou moins-values realisees.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "sell_action", def = function(x, num_sold, nb_sold) {
    standardGeneric("sell_action")
})
setMethod(
    f = "sell_action",
    signature = c(x = "Action", num_sold = "numeric", nb_sold = "numeric"),
    definition = function(x, num_sold, nb_sold) {
        ptf_action <- x@ptf_action
        nom_table <- names(ptf_action)
        num_mp <- which(nom_table == "num_mp")
        nb_unit <- which(nom_table == "nb_unit")
        val_achat <- which(nom_table == "val_achat")
        val_marche <- which(nom_table == "val_marche")
        num_val_nc <- which(nom_table == "val_nc")
        cessible <- which(nom_table == "cessible")
        presence <- which(nom_table == "presence")

        # Extraction de donnees
        num_mp <- .subset2(ptf_action, num_mp)
        nb_unit <- .subset2(ptf_action, nb_unit)
        val_nc <- .subset2(ptf_action, num_val_nc)
        val_marche <- .subset2(ptf_action, val_marche)
        val_achat <- .subset2(ptf_action, val_achat)
        cessible <- .subset2(ptf_action, cessible)
        presence <- .subset2(ptf_action, presence)

        # Verification des inputs
        if (length(num_sold) != length(nb_sold)) stop("[Action : sell] : Les vecteurs num_sold et nb_sold doivent etre de meme longueur \n")
        if (!all(is.element(num_sold, num_mp))) stop("[Action : sell] : Tentative de vente d'une action non existante \n")

        # Initialisation des pmv
        pmvr_part <- 0
        pmvr_tot <- 0

        # Selection des lignes soumises a une operation de vente
        row_sold <- which(num_mp %in% num_sold)

        # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
        if (!all(nb_sold <= nb_unit[row_sold])) stop("[Action : sell] : Vente d'action entraine une position negative \n")
        if (!all(cessible[row_sold])) stop("[Action : sell] : Tentative de vente d'une action non cessible \n")
        if (!all(presence[row_sold])) stop("[Action : sell] : Tentative de vente d'une action non presente \n")



        ## Premier cas : vente partielle d'une ligne action
        # Lignes a vendre partiellement
        row_sold_part <- row_sold[which(nb_sold < nb_unit[row_sold])]

        # Operations
        if (length(row_sold_part) > 0L) {
            # Donnees
            nb_sold_part <- nb_sold[which(num_sold %in% num_mp[row_sold_part])]
            nb_unit_part <- nb_unit[row_sold_part]
            val_marche_part <- val_marche[row_sold_part]
            val_nc_part <- val_nc[row_sold_part]
            coef_sold <- (1 - nb_sold_part / nb_unit_part)

            # Calcul PMVR
            pmvr_part <- sum((val_marche_part - val_nc_part) * (1 - coef_sold))

            # Mise a jour du PTF
            ptf_action$val_achat[row_sold_part] <- val_achat[row_sold_part] * coef_sold
            ptf_action$val_marche[row_sold_part] <- val_marche_part * coef_sold
            ptf_action$val_nc[row_sold_part] <- val_nc_part * coef_sold
            ptf_action$nb_unit[row_sold_part] <- nb_unit_part - nb_sold_part
        }



        ## Deuxieme cas : vente totale d'une ligne action
        # Lignes a vendre totalement
        row_sold_tot <- row_sold[which(nb_sold == nb_unit[row_sold])]

        # Operations
        if (length(row_sold_tot) > 0L) {
            # Calcul PMVR
            pmvr_tot <- sum(val_marche[row_sold_tot] - val_nc[row_sold_tot])

            # Suppression des lignes
            ptf_action <- ptf_action[-row_sold_tot, ]
        }



        # Teste si le PTF est vide
        if (nrow(ptf_action) == 0L) {
            warning(" [Action : sell] : Attention : portefeuille action vide")
        }

        # Mise a jour du PTF (suppression des VNC < 0.001)
        x@ptf_action <- ptf_action[which(.subset2(ptf_action, num_val_nc) > SEUIL_DEL_ACTION), ]

        # Calcul des PMVR
        pmvr <- pmvr_part + pmvr_tot

        # Output
        return(list(action = x, pmvr = pmvr))
    }
)
