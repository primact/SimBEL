#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le financement de la contrainte legale
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_contrainte_legale
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique la contrainte legale de participation aux benefices
##'
##' \code{finance_contrainte_legale} est une methode permettant de calculer la contrainte legale
##' de participation aux benefices et de l'appliquer si necessaire pour accroitre la revalorisation.
##' @name finance_contrainte_legale
##' @docType methods
##' @param base_fin est un vecteur de type \code{numeric} comprenant par produit la base de produits financiers.
##' @param base_fin_entendu est une valeur \code{numeric} comprenant la base totale de produits financiers,
##' y compris passifs non modelises.
##' @param result_tech est une valeur de type \code{numeric} comprenant le resultat technique.
##' @param it_stock est un vecteur de type \code{numeric} comprenant par produit les interets techniques affectes au stock.
##' @param rev_stock_nette est un vecteur de type \code{numeric} comprenant par produit la revalorisaton nette
##'  affectee au stock.
##' @param rev_prest_nette est un vecteur de type \code{numeric} comprenant par produit a revalorisaton nette
##'  affectee aux prestations.
##' @param dot_ppb est une valeur de type \code{numeric} comprenant la dotation de PPB financant la revalorisation
##' sur stock, hors TMG.
##' @param marge_fin est une valeur de type \code{numeric} comprenant la marge financiere courante de l'assureur.
##' @param ppb est un objet de la classe \code{Ppb} qui renvoie l'etat courant de la PPB.
##' @param param_revalo est un objet de la classe \code{ParamRevaloEngine}
##' comprenant les parametres de revalorisation.
##' @return Une liste avec la revalorisation nette affectee au stock
##' la marge financiere de l'assureur apres prise en compte de la contrainte legale, et la PPB.
##' @author Prim'Act
##' @export
##' @aliases RevaloEngine

setGeneric(name = "finance_contrainte_legale", def = function(base_fin, base_fin_etendu, result_tech, it_stock,
                                                              rev_stock_nette, rev_prest_nette, dot_ppb, marge_fin,
                                                              ppb, param_revalo){
  standardGeneric("finance_contrainte_legale")})

setMethod(
  f = "finance_contrainte_legale",
  signature = c(base_fin = "numeric", base_fin_etendu = "numeric", result_tech = "numeric", it_stock = "numeric",
               rev_stock_nette = "numeric", rev_prest_nette = "numeric", dot_ppb = "numeric", marge_fin = "numeric",
               ppb = "Ppb", param_revalo = "ParamRevaloEngine"),
  definition = function(base_fin, base_fin_etendu, result_tech, it_stock,
                        rev_stock_nette, rev_prest_nette, dot_ppb, marge_fin, ppb,
                        param_revalo){



    # Controle
    if(length(base_fin) != length(it_stock) | length(rev_prest_nette) != length(it_stock) |
       length(rev_stock_nette) != length(it_stock)){
      stop("[RevaloEngine-finance_contrainte_legale] : les vecteurs en entree ne sont pas de meme longueur.")
    }

    if(length(base_fin_etendu) > 1  | length(result_tech) > 1 | length(dot_ppb) > 1 |
       length(marge_fin) > 1){
      stop("[finance_contrainte_legale] : les vecteurs correspondant a l'assiette financiere
           etendue, au resultat technique, a la dotation de PPB
          et a la marge financiere d'assureur doivent etre de taille 1.")
    }


    # Calcul de la revalorisation legale
    ind_result_tech <- (result_tech > 0)
    rev_reg <- max (0, param_revalo@taux_pb_fi * base_fin_etendu  +
                      param_revalo@taux_pb_tech * result_tech * ind_result_tech +
                      result_tech * (1 - ind_result_tech) - sum(it_stock))

    # Montant de PB deja distribue
    tot_rev_assure <- sum(rev_stock_nette + rev_prest_nette) + dot_ppb

    # Reprise additionnelle
    suppl <- max(0,(rev_reg - tot_rev_assure))

    # Marge financier finale
    marge_fin <- marge_fin - suppl

    # Dotation
    op_pbb <- calc_dotation_ppb(ppb, suppl)

    # Mise a jour de la PPB
    ppb <- op_pbb[["ppb"]]
    dotation <- op_pbb[["dotation"]]

    # Revalorisation residuelle du stock
    add_rev_regl <- max(0, suppl - dotation)

    # Calcul de la revalorisation du stock nette apres prise en compte de la contrainte legale
    # L'attribution s'effectue uniquement sur les produits modelises.
    if(sum(base_fin) != 0){
      rev_stock_nette_regl <- rev_stock_nette + add_rev_regl * pmax(0, base_fin) / sum(base_fin)
    } else{ # Repartition au prorara si la base financiere est nulle
      nb_prod <- length(base_fin)
      rev_stock_nette_regl <- rev_stock_nette + add_rev_regl * 1 / nb_prod

    }

    # Output
    return(list(rev_stock_nette = rev_stock_nette_regl, marge_fin = marge_fin, ppb = ppb))
    }
)


