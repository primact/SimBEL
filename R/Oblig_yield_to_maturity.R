
#----------------------------------------------------------------------------------------------------------------------------------------------------
#          yield_to_maturity
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les yield to maturity de chaque composante d'un portefeuille obligataire.
##'
##' \code{yield_to_maturity} est une methode permettant de calculer les yield to maturity de chaque composante d'un portefeuille obligataire.
##' @name yield_to_maturity
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuile obligataire).
##' @return Un vecteur dont chaque element correspond au yield to maturity des obligations du portefeuille obligataire.
##' Ce vecteur a autant d'elements que le portefeuille obligataire a de lignes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "yield_to_maturity", def = function(x){standardGeneric("yield_to_maturity")})
setMethod(
    f = "yield_to_maturity",
    signature = "Oblig",
    definition = function(x){

        nom_table <- names(x@ptf_oblig)
        mat_res   <- which(nom_table == "mat_res")
        zsp       <- which(nom_table == "zspread")
        val_mar   <- which(nom_table == "val_marche")

        if(nrow(x@ptf_oblig)==0) {stop("[Oblig : yield_to_maturity] : Portefeuille vide")}
        coupon   <- calc_coupon(x)
        nominal  <- calc_nominal(x)
        maturite <- .subset2(x@ptf_oblig, mat_res)
        zspread  <- .subset2(x@ptf_oblig, zsp)
        vm       <- .subset2(x@ptf_oblig, val_mar)
        m        <- max(maturite)
        # Calcul des YtM pour chaque ligne obligataire du portefeuille
        # On resout a chaque fois la fonction VM(YtM)-VA=0
        ytm <- unlist(lapply(1:length(coupon), function(i){
            multiroot(function(y){return(sum(echeancier(coupon[i], maturite[i], zspread[i], nominal[i], rep(y, m))) - vm[i])},
                      start = 0,
                      maxiter = 300)$root}))
        return(ytm)
    }
)
