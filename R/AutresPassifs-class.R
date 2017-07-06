#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe AutresPassifs
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{AutresPassifs}.
##'
##' Une classe pour la gestion des passifs hors modele.
##' @name AutresPassifs
##' @slot mp un objet \code{data.frame} au format fige contenant les flux des passifs hors modele.
##' @docType class
##' @author Prim'Act
##' @seealso La lecture des flux d'une annee \code{\link{proj_annee_autres_passifs}}.
##' @keywords classes
##' @export
setClass(
    Class = "AutresPassifs",
    slots = c(mp = "data.frame"),
    validity = function (object){
        
        # liste permettant de stocker les erreurs de chargement
        retval <- NULL
        nb_col_attentu <- 7L
        
        # ModelPoint
        mp <- object@mp
        
        # Verification du nombre de colonnes
        if(ncol(mp) != nb_col_attentu) 
            retval <- c(retval, "[AutresPassifs] : Nombre d'attributs incorrect, un ptf AutresPassifs est compose d'un DF de 7 colonnes\n")
        
        # Verification du type des colonnes
        if (!is.numeric(mp[,1L]))     retval <- c(retval, "[AutresPassifs] : annee n'est pas numeric\n")
        if (!is.numeric(mp[,2L]))     retval <- c(retval, "[AutresPassifs] : prime n'est pas numeric\n")
        if (!is.numeric(mp[,3L]))     retval <- c(retval, "[AutresPassifs] : prestation n'est pas numeric\n")
        if (!is.numeric(mp[,4L]))     retval <- c(retval, "[AutresPassifs] : frais n'est pas numeric\n")
        if (!is.numeric(mp[,5L]))     retval <- c(retval, "[AutresPassifs] : pm_deb n'est pas numeric\n")
        if (!is.numeric(mp[,6L]))     retval <- c(retval, "[AutresPassifs] : pm_fin n'est pas numeric\n")
        if (!is.numeric(mp[,7L]))     retval <- c(retval, "[AutresPassifs] : it n'est pas numeric\n")
        
        # Verification du nom des colonnes
        if(sum(colnames(mp) == c("annee","prime","prestation","frais","pm_deb", "pm_fin", "it")) != nb_col_attentu)
            retval <- c(retval, "[AutresPassifs] : Noms de colonne incorrect \n")
        
        # Resultats du controle
        if (is.null(retval))
            return (TRUE)
        else
            return (retval)
    }
)
