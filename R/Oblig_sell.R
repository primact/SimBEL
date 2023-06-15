#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille obligation suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_oblig} est une methode permettant de mettre a jour chaque composante d'un portefeuille obligation suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligation en detention).
##' @param num_sold vecteur de type \code{numeric} contenant le numero de model point obligation du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur de type \code{numeric} contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return \code{oblig} l'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @return \code{pmvr} le montant des plus ou moins-values realisees.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "sell_oblig", def = function(x, num_sold, nb_sold) {
    standardGeneric("sell_oblig")
})
setMethod(
    f = "sell_oblig",
    signature = c(x = "Oblig", num_sold = "numeric", nb_sold = "numeric"),
    definition = function(x, num_sold, nb_sold) {
        ptf_oblig <- x@ptf_oblig
        nom_table <- names(ptf_oblig)
        num_mp <- which(nom_table == "num_mp")
        nb_unit <- which(nom_table == "nb_unit")
        val_achat <- which(nom_table == "val_achat")
        val_marche <- which(nom_table == "val_marche")
        num_val_nc <- which(nom_table == "val_nc")
        cessible <- which(nom_table == "cessible")
        presence <- which(nom_table == "presence")

        # Extroblig de donnees
        num_mp <- .subset2(ptf_oblig, num_mp)
        nb_unit <- .subset2(ptf_oblig, nb_unit)
        val_nc <- .subset2(ptf_oblig, num_val_nc)
        val_marche <- .subset2(ptf_oblig, val_marche)
        val_achat <- .subset2(ptf_oblig, val_achat)
        cessible <- .subset2(ptf_oblig, cessible)
        presence <- .subset2(ptf_oblig, presence)

        # Verification des inputs
        if (length(num_sold) != length(nb_sold)) stop("[Oblig : sell] : Les vecteurs num_sold et nb_sold doivent etre de meme longueur \n")
        if (!all(is.element(num_sold, num_mp))) stop("[Oblig : sell] : Tentative de vente d'une oblig non existante \n")

        # Initialisation des pmv
        pmvr_part <- 0
        pmvr_tot <- 0

        # Selection des lignes soumises a une operation de vente
        row_sold <- which(num_mp %in% num_sold)

        # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
        if (!all(nb_sold <= nb_unit[row_sold])) stop("[Oblig : sell] : Vente d'oblig entraine une position negative \n")
        if (!all(cessible[row_sold])) stop("[Oblig : sell] : Tentative de vente d'une oblig non cessible \n")
        if (!all(presence[row_sold])) stop("[Oblig : sell] : Tentative de vente d'une oblig non presente \n")



        ## Premier cas : vente partielle d'une ligne oblig
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
            ptf_oblig$val_achat[row_sold_part] <- val_achat[row_sold_part] * coef_sold
            ptf_oblig$val_marche[row_sold_part] <- val_marche_part * coef_sold
            ptf_oblig$val_nc[row_sold_part] <- val_nc_part * coef_sold
            ptf_oblig$nb_unit[row_sold_part] <- nb_unit_part - nb_sold_part
        }



        ## Deuxieme cas : vente totale d'une ligne oblig
        # Lignes a vendre totalement
        row_sold_tot <- row_sold[which(nb_sold == nb_unit[row_sold])]

        # Operations
        if (length(row_sold_tot) > 0L) {
            # Donnees
            nb_sold_tot <- nb_sold[which(num_sold %in% num_mp[row_sold_tot])]

            # Calcul PMVR
            pmvr_tot <- sum(val_marche[row_sold_tot] - val_nc[row_sold_tot])

            # Suppression des lignes
            ptf_oblig <- ptf_oblig[-row_sold_tot, ]
        }



        # Teste si le PTF est vide
        if (nrow(ptf_oblig) == 0L) {
            warning(" [Oblig : sell] : Attention : portefeuille oblig vide")
        }

        # Mise a jour du PTF
        x@ptf_oblig <- ptf_oblig

        # Calcul des PMVR
        pmvr <- pmvr_part + pmvr_tot

        # Output
        return(list(oblig = x, pmvr = pmvr))
    }
)
