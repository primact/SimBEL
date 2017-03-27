#----------------------------------------------------------------------------------------------------------------------------------------------------
#           base_prod_fin : Methode permettant de calculer la base de revalorisation du passif.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la base de produits financiers attribuables.
##'
##' \code{base_prod_fin} est une methode permettant de calculer la base de produits financiers attribuables
##'  pour la revalorisation des contrats.
##' @name base_prod_fin
##' @docType methods
##' @param tra est une valeur \code{numeric} donnant le taux de rendement de l'actif.
##' @param pm_moy est un vecteur \code{numeric} comprenant le montant de PM moyenne par produit.
##' @param ppb est un objet de la classe \code{\link{Ppb}} qui renvoie l'etat courant de la PPB.
##' @return La valeur de la base de produit financier par produit et au total pour le portefeuille.
##' @author Prim'Act
##' @seealso \code{\link{Ppb}}.
##' @export
##' @aliases RevaloEngine
##' @include Ppb_class.R

setGeneric(name = "base_prod_fin", def = function(tra, pm_moy, ppb){standardGeneric("base_prod_fin")})
setMethod(
  f = "base_prod_fin",
  signature = c(tra = "numeric", pm_moy = "numeric", ppb = "Ppb"),
  definition = function(tra, pm_moy, ppb){

    # Extrait les valeurs moyennes des PM par produit et au global
    pm_moy_port <- sum(pm_moy)

    # calcul de la base de produit financier au global et par produit
    base_prod_fin_port <- tra * (pm_moy_port + ppb@ppb_debut)

    if(pm_moy_port == 0){
      base_prod_fin <- pm_moy * 0
      base_prod_fin_port <- sum(base_prod_fin)
    }else{
      base_prod_fin <- base_prod_fin_port * pm_moy / pm_moy_port
    }

    # Output
    return(list(base_prod_fin = base_prod_fin, base_prod_fin_port = base_prod_fin_port))
  }
)
