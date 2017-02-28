#----------------------------------------------------------
# Ce script est la definition de la classe AutresPassifs dediee aux model points des autres passifs
# Il s'agit d'une classe ajoutee specificaquement pour le besoin MGP
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe AutresReserves
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe AutreReserves
##'
##' Classe permettant de gerer le stock de provision globale de gestion (PGG) et de
##' provision pour sinistres a payer (PSAP)
##'
##' @name AutresReserves
##' @slot pgg_debut la valeur de la PGG en debut de periode
##' @slot psap_debut la valeur de la PSAP en debut de periode
##' @slot pgg_valeur la valeur courant de la PGG.
##' @slot psap_valeur la valeur courant de la PSAP.
##' @slot tx_pgg_ep le taux de PGG applique sur l'epargne.
##' @slot tx_pgg_autres le taux de PGG applique sur les autres passifs.
##' @slot tx_psap_ep le taux de PGG applique sur l'epargne.
##' @slot tx_psap_autres le taux de PGG applique sur les autres passifs.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
setClass(
  Class = "AutresReserves",
  representation = representation(
    pgg_debut = "numeric",
    psap_debut = "numeric",
    pgg_valeur = "numeric",
    psap_valeur = "numeric",
    tx_pgg_ep = "numeric",
    tx_pgg_autres = "numeric",
    tx_psap_ep = "numeric",
    tx_psap_autres = "numeric"
    ))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "AutresReserves",
            function (object){
              retval <- NULL

              # Test sur les types
              if (!is.numeric(object@pgg_debut))  {retval <- c(retval, "[AutresReserves] : pgg_debut n'est pas numeric/n")}
              if (!is.numeric(object@psap_debut))  {retval <- c(retval, "[AutresReserves] : psap_debut n'est pas numeric/n")}
              if (!is.numeric(object@pgg_valeur))  {retval <- c(retval, "[AutresReserves] : pgg_valeur n'est pas numeric/n")}
              if (!is.numeric(object@psap_valeur))  {retval <- c(retval, "[AutresReserves] : psap_valeur n'est pas numeric/n")}

              if (!is.numeric(object@tx_pgg_ep))  {retval <- c(retval, "[AutresReserves] : tx_pgg_ep n'est pas numeric/n")}
              if (!is.numeric(object@tx_pgg_autres))  {retval <- c(retval, "[AutresReserves] : tx_pgg_autres n'est pas numeric/n")}
              if (!is.numeric(object@tx_psap_ep))  {retval <- c(retval, "[AutresReserves] : tx_psap_ep n'est pas numeric/n")}
              if (!is.numeric(object@tx_psap_autres))  {retval <- c(retval, "[AutresReserves] : tx_psap_autres n'est pas numeric/n")}

              # # Test sur la longueur
              # if (length(object@tx_pgg_ep) != 1)  {retval <- c(retval, "[AutresReserves] : tx_pgg_ep doit etre de longueur 1/n")}
              # if (length(object@tx_pgg_autres) != 1)  {retval <- c(retval, "[AutresReserves] : tx_pgg_autres doit etre de longueur 1/n")}
              # if (length(object@tx_psap_ep) != 1)  {retval <- c(retval, "[AutresReserves] : tx_psap_ep doit etre de longueur 1/n")}
              # if (length(object@tx_psap_autres) != 1)  {retval <- c(retval, "[AutresReserves] : tx_psap_autres doit etre de longueur 1/n")}

              if (is.null(retval)) return (TRUE)
              else return (retval)
              }
              )

