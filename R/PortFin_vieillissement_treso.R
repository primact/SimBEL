#----------------------------------------------------------------------------------------------------------------------------------------------------
#           vieillissement_treso_PortFin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Effectue le vieillissement/la projection du portefeuille tresorerie d'un portefeuille financier.
##'
##' \code{vieillissement_treso_PortFin} est une methode permettant de projeter la composante obligataire d'un portefeuille financier.
##' @name vieillissement_treso_PortFin
##' @docType methods
##' @param x objet de la classe \code{PortFin}, correspondant au portefeuille financier de l'assureur avant l'etape de vieillissement de son atribut \code{ptf_treso} de la classe \code{Treso}.
##' @param flux_milieu est un \code{numeric} correspondant aux revenus percus en milieu d'annee (coupons obligataires, loyers, dividendes).
##' @param flux_fin est un \code{numeric} correspondant aux revenus percus en fin d'annee (tombees d'echeances et revenus de tresorerie).
##' @param table_rdt est une \code{liste}, construite par la fonction \code{\link{calc_rdt}}.
##' Cette table contient les tables d'evolution des cours et rendements sur l'annee consideree de chacune des classes d'actif.
##' Les tables sont constuites a partir des extractions du Generateur de Scenario Economique de Prim'Act.
##' @return L'objet renvoye de la classe \code{PortFin} correspond au portefeuille financier initial dont l'attribut \code{ptf_treso} a ete vieilli d'une annee.
##' @author Prim'Act
##' @export
##' @seealso La fonction de calcul des rendements des actifs \code{\link{calc_rdt}}.
##' @aliases PortFin
##' @include PortFin_class.R

setGeneric(name = "vieillissement_treso_PortFin", def = function(x, flux_milieu, flux_fin, table_rdt){standardGeneric("vieillissement_treso_PortFin")})
setMethod(
  f = "vieillissement_treso_PortFin",
  signature = c(x = "PortFin", flux_milieu = "numeric", flux_fin = "numeric", table_rdt = "list"),
  definition = function(x, flux_milieu, flux_fin, table_rdt){
    # Verification input :
    if(nrow(x["ptf_treso"]["ptf_treso"]) > 0) {
        # Revenu de la treso
        rdt_treso <-  revenu_treso(x["ptf_treso"],table_rdt[["rdt_treso"]],flux_milieu)
        # Mise a jour de la treso en fin d'annee (revalo de la treso  en tenant compte des flux de loyer/div/coupons de milieu d'annee+ prise en comtpe des flux de tombee d'echeance percus en fin d'annee)
        x["ptf_treso"] <- update_treso(x["ptf_treso"], rdt_treso + flux_fin + flux_milieu)
    }
    return(x)
  })
