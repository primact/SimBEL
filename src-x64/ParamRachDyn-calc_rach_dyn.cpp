#include <Rcpp.h>
using namespace Rcpp;

//' Calcule la composante rachats dynamique.
//'
//' \code{calc_rach_dyn} est une methode permettant de calculer la composante rachat dynamique
//' selon la methodologie transmise dans le ONC de l'ACPR de 2013.
//' @name calc_rach_dyn
//' @docType methods
//' @param p un objet de la classe \code{\link{ParamRachDyn}} contenant les parametres de rachats dynamiques.
//' @param tx_cible une valeur \code{numeric} correspondant au taux de revalorisation cible.
//' @param tx_serv une valeur \code{numeric} correspondant au taux de revalorisation servi.
//' @return La valeur du taux rachat.
//' @author Prim'Act
//' @include ParamRachDyn-class.R
//' @export

// [[Rcpp::export]]
NumericVector calc_rach_dyn(NumericVector vec_param, NumericVector tx_cible, NumericVector tx_serv) {

    int l = tx_cible.size();
    NumericVector out(l);
    float tx_cible_i, tx_serv_i, diff_tx;
    float alpha = vec_param(0), beta = vec_param(1), gamma = vec_param(2);
    float delta = vec_param(3), RCMIN = vec_param(4), RCMAX = vec_param(5);

    for(int i=0 ; i<l ; i++) {

        // Extraction taux
        tx_cible_i = tx_cible(i);
        tx_serv_i = tx_serv(i);

        // Difference entre cible et serv
        diff_tx = tx_serv_i - tx_cible_i;

        // Calcul du taux de rachat
        if(diff_tx <= alpha)
            out(i) = RCMAX;
        else if(diff_tx <= beta)
            out(i) = RCMAX * (diff_tx - beta)/(alpha - beta);
        else if(diff_tx <= gamma)
            out(i) = 0;
        else if(diff_tx <= delta)
            out(i) = RCMIN * (diff_tx - gamma)/(delta - gamma);
        else if(diff_tx > delta)
            out(i) = RCMIN;
        else
            out(i) = 0;
    }

    return out;
}
