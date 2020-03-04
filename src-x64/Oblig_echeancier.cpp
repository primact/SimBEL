#include <Rcpp.h>
#include <string.h>
using namespace Rcpp;

//' Calcule les flux obligataires.
//'
//' \code{echeancier} est une methode permettant de calculer les flux jusqu'a maturite residuelle d'un model point ou
//' d'un ensemble de model points obligataires.
//' @name echeancier
//' @docType methods
//' @param coupon un vecteur contenant les taux de coupons de chaque obligation.
//' @param maturite un vecteur d'entiers contenant les maturites residuelles de chaque obligation.
//' @param zspread un vecteur contenant les zero-spreadsde chaque obligation.
//' @param nominal un vecteur contenant les valeurs nominales de chaque obligation.
//' @param yield un vecteur contenant la courbe de taux consideree (peut-etre vide).
//' @return Une matrice contenant :
//' \describe{
//' \item{\code{grid_flux} : }{la matrice d'ecoulement des flux. Cette matrice a autant de colonnes
//' que le max du vecteur de maturite residuelle, et autant de lignes que les vecteurs d'input
//' \code{coupon, maturite, zspread, nominal}.
//' Chaque ligne decrit les flux annuels a venir pour l'actif obligataire dont les caracteristiques sont
//' renseignees en input.}
//' }
//' @author Prim'Act
//' @export
//' @include Oblig_class.R


// [[Rcpp::export]]
NumericMatrix echeancier(NumericVector coupon, NumericVector maturite, NumericVector zspread, NumericVector nominal, NumericVector yield) {

    int mat_max = max(maturite);
    int n_oblig = coupon.size();
    int len_yield = yield.size();
    bool yield_bool = (len_yield > 0);

    if(mat_max <= 1) mat_max = 2;

    NumericMatrix out(n_oblig, mat_max);


    for (int i=0; i<n_oblig; i++) {
        for (int j=0; j<mat_max; j++) {

            if((j+1) <= maturite(i)) {
                out(i,j) = coupon(i);

                if((j+1) == maturite(i))
                    out(i,j) += nominal(i);
            }
            else {
                out(i,j) = 0; // Initialisation
            }


            if(yield_bool)
                out(i,j) *= pow(1.0 + yield(j) + zspread(i), -(j+1));

        }
    }

    return out;
}

