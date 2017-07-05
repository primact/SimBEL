#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de veillissement d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Veillissement d'un an des contrats epargne en euros.
##'
##' \code{vieilli_mp} est une methode permettant de vieillir
##'  les model points epargne en euros d'une peridoe.
##' @name vieilli_mp
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros.
##' @param y une liste contenant les parametres.
##' \describe{
##' \item{\code{pm_fin_ap_pb} : }{un vecteur de type \code{numeric} contenant par model point
##' les montants de PM revalorises apres participation aux benefices.}
##' \item{\code{tx_revalo} : }{un vecteur de type \code{numeric} contenant par model point
##' les taux de revalorisation nets appliques.}
##' }
##' @return l'objet \code{x} vieilli d'une periode.
##' @author Prim'Act
##' @seealso Calcul de la revalorisation des PM \code{\link{calc_revalo_pm}}.
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R

#----------------------------------------------------
setGeneric(name = "vieilli_mp", def = function(x, y) {standardGeneric("vieilli_mp")})
# Epargne
# y = list(pm_fin_ap_pb, tx_revalo)
# Retraite
# y = list(pm_fin_ap_pb)
#----------------------------------------------------
setMethod(
    f = "vieilli_mp",
    signature = c(x = "EpEuroInd", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 2)                          {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longueur 2. \n")}
        if (sum(names(y) == c("pm_fin_ap_pb", "tx_revalo")) != length(y)) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longueur 2 de nom : pm_fin_ap_pb, tx_revalo . \n")}
        if (! is.numeric(y[["pm_fin_ap_pb"]])) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longueur 2, de nom : pm_fin_ap_pb, tx_revalo, dont le type est : numeric, numeric. \n")}
        if (! is.numeric(y[["tx_revalo"]])) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longueur 2, de nom : pm_fin_ap_pb, tx_revalo, dont le type est : numeric, numeric. \n")}
        pm_fin_ap_pb = y[["pm_fin_ap_pb"]]
        tx_revalo    = y[["tx_revalo"]]


        # viellissement de 1 an
        x@mp$age <- x@mp$age + as.integer(1)
        x@mp$anc <- x@mp$anc + as.integer(1)

        # Ajustement du nombre de contrat, de la PM garanti (calcul FDB) et du taux cible.
        x@mp$nb_contr <- x@tab@tab[["nb_contr"]]
        x@mp$pm_gar <- x@tab@tab[["pm_gar"]]
        x@mp$tx_cible_prec <- x@tab@tab[["tx_cible"]]

        # Ajustement de PM de fin
        x@mp$pm <- pm_fin_ap_pb
        x@mp$tx_revalo_prec <- tx_revalo

        # output
        return(x)
    }
)

setMethod(
    f = "vieilli_mp",
    signature = c(x = "RetraiteEuroRest", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 2)                          {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longueur 2. \n")}
        if (sum(names(y) == c("pm_fin_ap_pb", "tx_rev_net")) != length(y)) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longueur 2.")}
        if (! is.numeric(y[["pm_fin_ap_pb"]])) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longeur 2 dont les elements sont de type 'numeric', 'numeric'.")}
        if (! is.numeric(y[["tx_rev_net"]])) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longeur 2 dont les elements sont de type 'numeric', 'numeric'.")}
        if ( length(y[["tx_rev_net"]]) != nrow(x@mp)) {stop("[PassifBase : vieilli_mp] : L'input y doit correspondre a une liste de longeur 2 dont les elements sont de type 'numeric', 'numeric'.")}

        # Ajustement de la rente
        x@mp$rente <- as.numeric(x@mp$rente * ( rep(1, length(y[["tx_rev_net"]])) + y[["tx_rev_net"]]))

        # Ajustement de la PM garantie
        x@mp$pm <- as.numeric(y[["pm_fin_ap_pb"]])

        # Ajustement du taux cible
        x@mp$tx_cible_prec <- x@tab@tab[["tx_cible"]]

        # output
        return(x)
    }
)
