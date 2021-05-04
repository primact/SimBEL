#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe FraisPassif
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{FraisPassif}
##'
##' Une classe de parametres pour les frais des produits du portefeuille de passif.
##'
##' @name FraisPassif
##' @slot mp un objet \code{data.frame} contenant les parametres de frais au passif par produit.
##' @docType class
##' @author Prim'Act
##' @seealso Le calcul des frais de passif \code{\link{calc_frais}}.
##' @keywords classes
##' @export
setClass(
    Class = "FraisPassif",
    slots = c(mp = "data.frame"),
    validity = function (object){

        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_col_attentu <- 13L

        # ModelPoint
        mp <- object@mp

        # Verification du nombre de colonnes
        if(ncol(mp) != nb_col_attentu)
            retval <- c(retval, "[FraisPassif] : Nombre d'attributs incorrect, un objet FraisPassif est compose d'un DF de 13 colonnes\n")

        # Verification du type des colonnes
        if (!is.factor(mp[,1L]))     retval <- c(retval, "[FraisPassif] : nom_prod n'est pas factor\n")
        if (!is.numeric(mp[,2L]))    retval <- c(retval, "[FraisPassif] : frais_fixe_prime n'est pas numeric\n")
        if (!is.numeric(mp[,3L]))    retval <- c(retval, "[FraisPassif] : frais_var_prime n'est pas numeric\n")
        if (!is.logical(mp[,4L]))    retval <- c(retval, "[FraisPassif] : ind_inf_frais_fixe_prime n'est pas logical\n")
        if (!is.logical(mp[,5L]))    retval <- c(retval, "[FraisPassif] : ind_inf_frais_var_prime n'est pas logical\n")
        if (!is.numeric(mp[,6L]))    retval <- c(retval, "[FraisPassif] : frais_fixe_prest n'est pas numeric\n")
        if (!is.numeric(mp[,7L]))    retval <- c(retval, "[FraisPassif] : frais_var_prest n'est pas numeric\n")
        if (!is.logical(mp[,8L]))    retval <- c(retval, "[FraisPassif] : ind_inf_frais_fixe_prest n'est pas logical\n")
        if (!is.logical(mp[,9L]))    retval <- c(retval, "[FraisPassif] : ind_inf_frais_var_prest n'est pas logical\n")
        if (!is.numeric(mp[,10L]))   retval <- c(retval, "[FraisPassif] :  frais_fixe_enc n'est pas numeric\n")
        if (!is.numeric(mp[,11L]))   retval <- c(retval, "[FraisPassif] : frais_var_enc n'est pas numeric\n")
        if (!is.logical(mp[,12L]))   retval <- c(retval, "[FraisPassif] : ind_inf_frais_fixe_enc n'est pas logical\n")
        if (!is.logical(mp[,13L]))   retval <- c(retval, "[FraisPassif] : ind_inf_frais_var_enc n'est pas logical\n")



        # Verification du nom des colonnes
        if(sum(colnames(mp) == c("nom_prod","frais_fixe_prime","frais_var_prime","ind_inf_frais_fixe_prime",
                                 "ind_inf_frais_var_prime","frais_fixe_prest","frais_var_prest","ind_inf_frais_fixe_prest",
                                 "ind_inf_frais_var_prest","frais_fixe_enc","frais_var_enc","ind_inf_frais_fixe_enc","ind_inf_frais_var_enc")) != nb_col_attentu)
            retval <- c(retval, "[FraisPassif] : Noms de colonne incorrect \n")


        # Resultats du controle
        if (is.null(retval))  return (TRUE)
        else                  return (retval)

    }
)
