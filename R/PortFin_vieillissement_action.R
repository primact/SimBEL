
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_action_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Effectue le vieillissement/la projection du portefeuille action d'un portefeuille financier.
##'
##' \code{vieillissement_action_PortFin} est une methode permettant de projeter la composante action d'un portefeuille financier.
##' suite a un vieillissement.
##' @name vieillissement_action_PortFin
##' @docType methods
##' @param x objet de la classe \code{PortFin}, correspondant au portefeuille financier de l'assureur avant l'etape de vieillissement de son atribut \code{ptf_action} de la classe \code{Action}.
##' @param table_rdt est une \code{liste}, construite par la fonction \code{\link{calc_rdt}}.
##' Cette table contient les tables d'evolution des cours et rendements sur l'annee consideree de chacune des classes d'actif.
##' Les tables sont constuites a partir des extractions du Generateur de Scenario Economique de Prim'Act.
##' @return Le format de la liste renvoyee est :
##' \describe{
##' \item{\code{portFin} : }  {le portefeuille financier dont l'attribut \code{ptf_action} a ete vieilli d'une annee.}
##' \item{\code{dividende} : }{le montant de dividende percus en milieu d'annee suite au vieillissement du portefeuille action.}
##' }
##' @author Prim'Act
##' @export
##' @seealso La fonction de calcul des rendements des actifs \code{\link{calc_rdt}}.
##' @aliases PortFin
##' @include PortFin_class.R

setGeneric(name = "vieillissement_action_PortFin", def = function(x, table_rdt){standardGeneric("vieillissement_action_PortFin")})
setMethod(
    f = "vieillissement_action_PortFin",
    signature = c(x = "PortFin", table_rdt = "list"),
    definition = function(x, table_rdt){
        # Verification input :
        if(nrow(x["ptf_action"]["ptf_action"]) > 0) {

            # Calcul des dividendes
            dividende  <- sum(table_rdt[["rdt_action"]][["div"]])

            # Mise a jour de la VM Action en fin d'annee
            x["ptf_action"] <- update_vm_action(x["ptf_action"], calc_vm_action(x["ptf_action"], table_rdt[["rdt_action"]][["rdt"]] ))
            # Mise a jour des durees de detention Action/Immo
            x["ptf_action"] <- update_dur_det_action(x["ptf_action"])
            return(list(portFin = x, dividende = dividende))
        } else {
            return(list(portFin = x, dividende = 0))
        }
    })
