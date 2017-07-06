#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

//' Calcule le taux de revalorisation cible.
//'
//' \code{calc_tx_cible_ref_marche} est une methode permettant de calculer le taux de revalorisation cible
//' en evaluant le taux de rendement des assureurs sur le marche.
//' @name calc_tx_cible_ref_marche
//' @docType methods
//' @param param_comport un objet de la classe \code{\link{ParamComport}} contenant les parametres
//'  comportementaux.
//' @param list_rd une liste contenant les rendements de reference. Le format de cette liste est :
//' \describe{
//' \item{le taux de rendement obligataire}{}
//' \item{le taux de rendement de l'indice action de reference}{}
//' \item{le taux de rendement de l'indice immobilier de reference}{}
//' \item{le taux de rendement de l'indice tresorerie de reference}{}
//' }
//' @param tx_cible_prec une valeur \code{numeric} correspondant au taux cible de la periode precedente.
//' @return La valeur du taux cible.
//' @author Prim'Act
//' @export
//' @include ParamComport-class.R

// [[Rcpp::export]]
NumericVector calc_tx_cible_ref_marche(NumericVector rdt, NumericVector alloc_mar, float ch_enc_mar, float marge_mar, float w_n, NumericVector tx_cible_prec) {

    int size_1 = rdt.size(), size_2 = tx_cible_prec.size();
    float tx_mar = 0, tx_cible_an = 0;
    float tp_max = 0;

    NumericVector out(size_2);

    for (int i=0;i<size_1;i++) {
        tx_mar += rdt[i] * alloc_mar[i];
    };
    // tx_mar = sum(rdt * alloc_mar) ;

    tp_max = tx_mar - ch_enc_mar;
    if (tp_max > 0)
        tx_cible_an = tp_max * w_n * (1.0 - marge_mar);


    for(int i=0;i<size_2;i++)
        out(i) = tx_cible_an + (1.0 - w_n) * tx_cible_prec(i);

    //tx_cible_an = (1.0 - w_n) * tx_cible_prec;

    return(out);
}



