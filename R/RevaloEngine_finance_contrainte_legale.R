#----------------------------------------------------------------------------------------------------------------------------------------------------
#           finance_contrainte_legale : methode permettant de calculer le financement de la contrainte legale
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique la contrainte legale de participation aux benefices.
##'
##' \code{finance_contrainte_legale} est une methode permettant de calculer la contrainte legale
##' de participation aux benefices et de l'appliquer si necessaire pour accroitre la revalorisation.
##' @name finance_contrainte_legale
##' @docType methods
##' @param base_fin un vecteur \code{numeric} comprenant par produit la base de produits financiers.
##' @param base_fin_entendu une valeur \code{numeric} comprenant la base totale de produits financiers
##' (somme des produits modelise et des  passifs non modelises).
##' @param result_tech une valeur \code{numeric} comprenant le resultat technique.
##' @param it_stock un vecteur \code{numeric} comprenant par produit les interets techniques affectes au stock.
##' @param rev_stock_nette un vecteur de type \code{numeric} comprenant par produit la revalorisaton nette
##'  affectee au stock.
##' @param rev_prest_nette un vecteur de type \code{numeric} comprenant par produit a revalorisaton nette
##'  affectee aux prestations.
##' @param dot_ppb une valeur \code{numeric} comprenant la dotation de PPB financant la revalorisation
##' sur stock.
##' @param marge_fin une valeur \code{numeric} comprenant la marge financiere courante de l'assureur.
##' @param ppb un objet de la classe \code{\link{Ppb}} qui renvoie l'etat courant de la PPB.
##' @param param_revalo un objet de la classe \code{\link{ParamRevaloEngine}}.
##' comprenant les parametres de revalorisation.
##' @details Cette methode permet de calculer la contrainte de revalorisation imposee par la reglementation. Si cette
##' contrainte est verifie alors rien n'est fait, hormis la mise a jour eventuelle du solde negatif de PB. Sinon, la
##' revalorisation additionnelle est dote a la PPB, jusqu'au maximum de dotation possible, puis le relicat est alloue
##' entre les produits. La revalorisation additionelle vient diminuer la marge financiere de l'assureur.
##' @return \code{rev_stock_nette} la valeur de la revalorisation nette servie apres application de la contrainte legale.
##' @return \code{marge_fin} le montant de marge de l'assureur apres reduction.
##' @return \code{ppb} l'objet \code{ppb} mis a jour.
##' @return \code{param_revalo} l'objet \code{param_revalo} mis a jour (solde de PB reglementaire negatif).
##' @author Prim'Act
##' @export
##' @include Ppb_class.R ParamRevaloEngine_class.R

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
    solde_pb <- param_revalo@taux_pb_fi * base_fin_etendu  +
      param_revalo@taux_pb_tech * result_tech * ind_result_tech +
      result_tech * (1 - ind_result_tech) - sum(it_stock)
    # Report du solde negatif
    solde_pb <- solde_pb + param_revalo@solde_pb_regl

    # Mise a jour du solde de PB minimale
    if(solde_pb < 0){
      param_revalo@solde_pb_regl <- solde_pb
    } else{
      param_revalo@solde_pb_regl <- 0
    }

    # Calcul de la revalorisation legale
    rev_reg <- max (0, solde_pb)

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
      rev_stock_nette_regl <- rev_stock_nette + add_rev_regl * base_fin / sum(base_fin)
    } else{ # Repartition au prorara si la base financiere est nulle
      nb_prod <- length(base_fin)
      rev_stock_nette_regl <- rev_stock_nette + add_rev_regl * 1 / nb_prod

    }

    # Output
    return(list(rev_stock_nette = rev_stock_nette_regl,
                marge_fin = marge_fin,
                ppb = ppb,
                param_revalo = param_revalo))
    }
)


