SEUIL_DEL_IMMO <- 0.001
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille immobilier suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_immo} est une methode permettant de mettre a jour chaque composante d'un portefeuille immobilier suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille immobilier en detention).
##' @param num_sold vecteur de type \code{numeric} contenant le numero de model point immobilier du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur de type \code{numeric} contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return \code{immo} l'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @return \code{pmvr} le montant des plus ou moins-values realisees.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "sell_immo", def = function(x, num_sold, nb_sold) {
    standardGeneric("sell_immo")
})
setMethod(
    f = "sell_immo",
    signature = c(x = "Immo", num_sold = "numeric", nb_sold = "numeric"),
    definition = function(x, num_sold, nb_sold) {
        ptf_immo <- x@ptf_immo
        nom_table <- names(ptf_immo)
        num_mp <- which(nom_table == "num_mp")
        nb_unit <- which(nom_table == "nb_unit")
        val_achat <- which(nom_table == "val_achat")
        val_marche <- which(nom_table == "val_marche")
        num_val_nc <- which(nom_table == "val_nc")
        cessible <- which(nom_table == "cessible")
        presence <- which(nom_table == "presence")

        # Extraction de donnees
        num_mp <- .subset2(ptf_immo, num_mp)
        nb_unit <- .subset2(ptf_immo, nb_unit)
        val_nc <- .subset2(ptf_immo, num_val_nc)
        val_marche <- .subset2(ptf_immo, val_marche)
        val_achat <- .subset2(ptf_immo, val_achat)
        cessible <- .subset2(ptf_immo, cessible)
        presence <- .subset2(ptf_immo, presence)

        # Verification des inputs
        if (length(num_sold) != length(nb_sold)) stop("[Immo : sell] : Les vecteurs num_sold et nb_sold doivent etre de meme longueur \n")
        if (!all(is.element(num_sold, num_mp))) stop("[Immo : sell] : Tentative de vente d'une Immo non existante \n")

        # Initialisation des pmv
        pmvr_part <- 0
        pmvr_tot <- 0

        # Selection des lignes soumises a une operation de vente
        row_sold <- which(num_mp %in% num_sold)

        # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
        if (!all(nb_sold <= nb_unit[row_sold])) stop("[Immo : sell] : Vente d'Immo entraine une position negative \n")
        if (!all(cessible[row_sold])) stop("[Immo : sell] : Tentative de vente d'une Immo non cessible \n")
        if (!all(presence[row_sold])) stop("[Immo : sell] : Tentative de vente d'une Immo non presente \n")



        ## Premier cas : vente partielle d'une ligne immo
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
            ptf_immo$val_achat[row_sold_part] <- val_achat[row_sold_part] * coef_sold
            ptf_immo$val_marche[row_sold_part] <- val_marche_part * coef_sold
            ptf_immo$val_nc[row_sold_part] <- val_nc_part * coef_sold
            ptf_immo$nb_unit[row_sold_part] <- nb_unit_part - nb_sold_part
        }



        ## Deuxieme cas : vente totale d'une ligne immo
        # Lignes a vendre totalement
        row_sold_tot <- row_sold[which(nb_sold == nb_unit[row_sold])]

        # Operations
        if (length(row_sold_tot) > 0L) {
            # Donnees
            nb_sold_tot <- nb_sold[which(num_sold %in% num_mp[row_sold_tot])]

            # Calcul PMVR
            pmvr_tot <- sum(val_marche[row_sold_tot] - val_nc[row_sold_tot])

            # Suppression des lignes
            ptf_immo <- ptf_immo[-row_sold_tot, ]
        }



        # Teste si le PTF est vide
        if (nrow(ptf_immo) == 0L) {
            warning(" [Immo : sell] : Attention : portefeuille immo vide")
        }

        # Mise a jour du PTF (suppression des VNC < 0.001)
        x@ptf_immo <- ptf_immo[which(.subset2(ptf_immo, num_val_nc) > SEUIL_DEL_IMMO), ]

        # Calcul des PMVR
        pmvr <- pmvr_part + pmvr_tot

        # Output
        return(list(immo = x, pmvr = pmvr))
    }
)
