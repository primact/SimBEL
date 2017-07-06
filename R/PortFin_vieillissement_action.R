
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_action_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Effectue le vieillissement du portefeuille action d'un portefeuille financier.
##'
##' \code{vieillissement_action_PortFin} est une methode permettant de projeter la composante action d'un portefeuille financier.
##' @name vieillissement_action_PortFin
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}, correspondant au portefeuille financier de l'assureur
##'  avant l'etape de vieillissement.
##' @param table_rdt est une liste, construite par la fonction \code{\link{calc_rdt}}.
##' Cette table contient les tables d'evolution des cours et rendements sur l'annee consideree de chacune des
##' classes d'actif. Les tables sont constuites a partir des extractions du Generateur de Scenario Economique
##'  de Prim'Act.
##' @return \code{portFin} le portefeuille financier dont l'attribut \code{ptf_action} a ete vieilli d'une annee.
##' @return \code{dividende} le montant de dividende percus en milieu d'annee suite au vieillissement du portefeuille
##'  action.
##' @author Prim'Act
##' @export
##' @seealso La fonction de calcul des rendements des actifs \code{\link{calc_rdt}}.
##' @include PortFin_class.R

setGeneric(name = "vieillissement_action_PortFin", def = function(x, table_rdt){standardGeneric("vieillissement_action_PortFin")})
setMethod(
    f = "vieillissement_action_PortFin",
    signature = c(x = "PortFin", table_rdt = "list"),
    definition = function(x, table_rdt){
        
        # Extraction du PTF
        ptf_action <- x@ptf_action
        
        # Verification input :
        if(nrow(ptf_action@ptf_action) > 0) {
            
            # Extraction table rdt
            rdt_action <- table_rdt[["rdt_action"]]
            
            # Calcul des dividendes
            dividende  <- sum(rdt_action[, "div"])
            
            # Mise a jour de la VM Action en fin d'annee
            ptf_action <- update_vm_action(ptf_action, calc_vm_action(ptf_action, rdt_action[, "rdt"] ))
            
            # Mise a jour des durees de detention Action/Immo
            ptf_action <- update_dur_det_action(ptf_action)
            
            # Mise a jour du PTF
            x@ptf_action <- ptf_action
            
        } else {
            dividende = 0
        }
        
        # Output
        return(list(portFin = x, dividende = dividende))
    })
