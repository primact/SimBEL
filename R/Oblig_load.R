#----------------------------------------------------------------------------------------------------------------------------------------------------
#           load_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger le portefeuille oblig initital dans un objet de type \code{\link{Oblig}}.
##'
##' \code{load_oblig} est une methode permettant de charger le portefeuille oblig
##' @name load_oblig
##' @docType methods
##' @param file_oblig_address est un \code{character} contenant l'adresse exacte du fichier d'input utilisateur permettant de renseigner le
##' portefeuille.
##' @return L'objet de la classe \code{\link{Oblig}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @seealso La classe \code{\link{PortFin}} et sa methode \code{\link{chargement_PortFin}}.
##' @include Action_class.R
setGeneric(name = "load_oblig", def = function(file_oblig_address) {
    standardGeneric("load_oblig")
})
setMethod(
    f = "load_oblig",
    signature = "character",
    definition = function(file_oblig_address) {
        # Lecture du fichier
        temp <- read.csv2(file_oblig_address, colClasses = c(
            "integer", "numeric", "numeric", "numeric", "logical", "logical", "numeric",
            "numeric", "numeric", "numeric", "numeric", "numeric", "factor", "integer",
            "numeric", "numeric", "numeric", "numeric", "character", "numeric"
        ))

        # Tests
        if (!all(!is.na(temp))) {
            stop("[Oblig - load] : Presence de NA dans le fichier d'input.")
        }
        if (!all(temp[, "nb_unit"] > 0)) {
            stop("[Oblig - load] : Le portefeuille Oblig ne peut contenir des nombres d'unites negatives ou nulles")
        }
        if (!all(temp[, "val_marche"] > 0)) {
            stop("[Oblig - load] : Le portefeuille Oblig ne peut contenir des valeurs de marche negatives ou nulles")
        }
        if (!all(temp[, "fx_rate"] > 0)) {
            stop("[Oblig - load] : Le portefeuille Oblig ne peut contenir des taux de change negatifs ou nuls")
        }

        # Conversion du champs contenant les devises en character
        ptf <- data.frame(
            num_mp = (temp[, "num_mp"]),
            val_marche = (temp[, "val_marche"]),
            val_nc = (temp[, "val_nc"]),
            val_achat = (temp[, "val_achat"]),
            presence = (temp[, "presence"]),
            cessible = (temp[, "cessible"]),
            nb_unit = (temp[, "nb_unit"]),
            dur_det = (temp[, "dur_det"]),
            nominal = (temp[, "nominal"]),
            tx_coupon = (temp[, "tx_coupon"]),
            par = (temp[, "par"]),
            mat_res = (temp[, "mat_res"]),
            type = (temp[, "type"]),
            rating = (temp[, "rating"]),
            duration = (temp[, "duration"]),
            zspread = (temp[, "zspread"]),
            cc = (temp[, "cc"]),
            sd = (temp[, "sd"]),
            currency = (temp[, "currency"]),
            fx_rate = (temp[, "fx_rate"])
        )
        ptf$currency <- as.character(ptf$currency)

        # Creation de l'objet
        ptf_oblig <- new("Oblig", ptf = ptf)

        # Output
        return(ptf_oblig)
    }
)
