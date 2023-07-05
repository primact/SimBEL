#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_immo_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Effectue le vieillissement du portefeuille immo d'un portefeuille financier.
##'
##' \code{vieillissement_immo_PortFin} est une methode permettant de projeter la composante immobilier d'un portefeuille financier.
##' @name vieillissement_immo_PortFin
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}, correspondant au portefeuille financier
##'  de l'assureur avant l'etape de vieillissement.
##' @param table_rdt est une liste, construite par la fonction \code{\link{calc_rdt}}.
##' Cette table contient les tables d'evolution des cours et rendements sur l'annee consideree de chacune des classes d'actif.
##' Les tables sont constuites a partir des extractions du Generateur de Scenario Economique de Prim'Act.
##' @return code{portFin} le portefeuille financier dont l'attribut \code{ptf_immo} a ete vieilli d'une annee.
##' @return \code{loyer} le montant de loyer percus en milieu d'annee suite au vieillissement du portefeuille immobilier.
##' @author Prim'Act
##' @export
##' @seealso La fonction de calcul des rendements des actifs \code{\link{calc_rdt}}.
##' @include PortFin_class.R

setGeneric(name = "vieillissement_immo_PortFin", def = function(x, table_rdt) {
    standardGeneric("vieillissement_immo_PortFin")
})
setMethod(
    f = "vieillissement_immo_PortFin",
    signature = c(x = "PortFin", table_rdt = "list"),
    definition = function(x, table_rdt) {
        # Extraction PTF
        ptf_immo <- x@ptf_immo

        # Verification input :
        if (nrow(ptf_immo@ptf_immo) > 0) {
            # Extraction de la table des rdt immo
            rdt_immo <- table_rdt[["rdt_immo"]]

            # Calcul des loyers
            loyer <- sum(rdt_immo[, "loyer"])

            # Mise a jour de la VM Immo en fin d'annee
            ptf_immo <- update_vm_immo(ptf_immo, calc_vm_immo(ptf_immo, rdt_immo[, "rdt"]))

            # Mise a jour des durees de detention Action/Immo
            ptf_immo <- update_dur_det_immo(ptf_immo)

            # Mise a jour PTF
            x@ptf_immo <- ptf_immo
        } else {
            loyer <- 0
        }

        # Output
        return(list(portFin = x, loyer = loyer))
    }
)
