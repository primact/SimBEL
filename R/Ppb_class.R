#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe Ppb
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{Ppb}.
##'
##' Classe pour la provision pour participation aux benefices (PPB)
##' @name Ppb
##' @slot hist_ppb est le vecteur contenant les valeurs courantes \code{numeric} prise par la PPB sur les huit dernieres annees.
##' @slot valeur_ppb est la valeur courante \code{numeric} prise par la PPB.
##' @slot ppb_debut est la valeur prise \code{numeric} par la PPB en debut d'annee.
##' @slot seuil_rep est une valeur \code{numeric} correspondant a la proportion de PPB de debut d'annee
##' que l'on peut reprendre sur une periode.
##' @slot seuil_dot est une valeur \code{numeric} correspondant au montant maximal de dotation possible sur la PPB
##' sur une periode, exprimee comme une fraction de la PPB de debut d'annee.
##' @slot compte_rep est une valeur \code{numeric} qui totalise les montants de reprises effectuees sur une periode.
##' @slot compte_dot est une valeur \code{numeric} qui totalise les montants de dotations effectuees sur une periode.
##' @docType class
##' @author Prim'Act
##' @seealso La dotation et la reprise de PPB : \code{\link{calc_dotation_ppb}}, \code{\link{calc_reprise_ppb}}.
##' @keywords classes
##' @export
setClass(
    Class = "Ppb",
    representation = representation(
        hist_ppb = "numeric",
        valeur_ppb = "numeric",
        ppb_debut = "numeric",
        seuil_rep = "numeric",
        seuil_dot = "numeric",
        compte_rep = "numeric",
        compte_dot = "numeric"))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "Ppb",
            function (object){
                retval <- NULL

                # Test sur les types
                if (!is.numeric(object@hist_ppb))  {retval <- c(retval, "[Ppb] : hist_ppb n'est pas numeric/n")}
                if (!is.numeric(object@valeur_ppb))  {retval <- c(retval, "[Ppb] : valeur_ppb n'est pas numeric/n")}
                if (!is.numeric(object@ppb_debut))  {retval <- c(retval, "[Ppb] : ppb_debut n'est pas numeric/n")}
                if (!is.numeric(object@seuil_rep))  {retval <- c(retval, "[Ppb] : seuil_rep n'est pas numeric/n")}
                if (!is.numeric(object@seuil_dot))  {retval <- c(retval, "[Ppb] : seuil_dot n'est pas numeric/n")}
                if (!is.numeric(object@compte_rep))  {retval <- c(retval, "[Ppb] : compte_rep n'est pas numeric/n")}
                if (!is.numeric(object@compte_dot))  {retval <- c(retval, "[Ppb] : compte_dot n'est pas numeric/n")}

                # Test sur la longueur
                if (length(object@hist_ppb) != 8)  {retval <- c(retval, "[Ppb] : hist_ppb doit etre de longueur 8/n")}
                if (length(object@valeur_ppb) != 1)  {retval <- c(retval, "[Ppb] : valeur_ppb doit etre de longueur 1/n")}
                if (length(object@ppb_debut) != 1)  {retval <- c(retval, "[Ppb] : ppb_debut doit etre de longueur 1/n")}
                if (length(object@seuil_rep) != 1)  {retval <- c(retval, "[Ppb] : seuil_rep doit etre de longueur 1/n")}
                if (length(object@seuil_dot) != 1)  {retval <- c(retval, "[Ppb] : seuil_dot doit etre de longueur 1/n")}
                if (length(object@compte_rep) != 1)  {retval <- c(retval, "[Ppb] : compte_rep doit etre de longueur 1/n")}
                if (length(object@compte_dot) != 1)  {retval <- c(retval, "[Ppb] : compte_dot doit etre de longueur 1/n")}

                if (object@seuil_rep > 1 | object@seuil_rep < 0)  {retval <- c(retval, "[Ppb] : seuil_rep doit etre compris
                                                                             entre 0 et 1/n")}
                if (object@seuil_dot < 0)  {retval <- c(retval, "[Ppb] : seuil_dot doit etre compris
                                                                             positif/n")}


                if (is.null(retval)) return (TRUE)
                else return (retval)
            }
)

