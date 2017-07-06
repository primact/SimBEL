#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des taux de revalorisation minimum d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Calcul le taux de revalorisation contractuel minimum pour des contrats epargne en euros.
##'
##' \code{calc_tx_min} est une methode permettant de calculer les taux de revalorisation minimum
##'   sur une periode. La revalorisation minimum est le maximum entre le taux technique et
##'   le taux minimim garanti (TMG) du contrat.
##' @name calc_tx_min
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param an un \code{numeric} representant l'annee de projection courante.
##' @return \code{tx_tech_an} : un vecteur contenant les taux de technique de l'annee
##' @return \code{tx_tech_se} : un vecteur contenant les taux de technique de l'annee sur base semestrielle
##' @return \code{tx_an} : un vecteur contenant les taux de revalorisation minimum de l'annee
##' @return \code{x_se} : un vecteur contenant les taux de revalorisation minimum de l'annee exprimes en semestriel.
##' @note Pour les besoins des calculs a mi-annee, des taux semestriels sont produits.
##' @author Prim'Act
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R

#--------------------------------------------------------
setGeneric(name = "calc_tx_min", def = function(x, an){standardGeneric("calc_tx_min")})


#--------------------------------------------------------
setMethod(
    f = "calc_tx_min",
    signature = c(x = "EpEuroInd", an ="numeric"),
    def = function(x, an){

        # Data.frame ModelPoint
        mp      <- x@mp
        nb_mp   <- nrow(mp)
        nom_mp  <- names(mp)
        num_terme_tx_tech   <- which(nom_mp == "terme_tx_tech")
        num_terme_tmg       <- which(nom_mp == "terme_tmg")
        num_tx_tech         <- which(nom_mp == "tx_tech")
        num_tmg             <- which(nom_mp == "tmg")

        # Calcul des indicatrice de versement du taux technique et du tmg
        ind_tx_tech <- (an <= .subset2(mp, num_terme_tx_tech)) # indicatrice taux technique pour l'annee en cours
        ind_tmg     <- (an <= .subset2(mp, num_terme_tmg)) # indicatrice taux technique pour l'annee en cours

        # Calul du produit du TxTech_MP et de l'incatrice
        tx_tech_mp <- .subset2(mp, num_tx_tech) * ind_tx_tech

        # calcul du taux technique
        tx_tech <- pmax(0, tx_tech_mp) # taux technique
        tx_tech_se <- taux_period(tx_tech, period = "se") # taux semestriel

        # Calcul du taux minimum
        tx_min <- pmax(0, tx_tech_mp, .subset2(mp, num_tmg) * ind_tmg) # taux annuel minimum
        tx_min_se <- taux_period(tx_min, period = "se") # taux semestriel

        # Output
        return(list(
            tx_tech_an = tx_tech,
            tx_tech_se = tx_tech_se,
            tx_an = tx_min,
            tx_se = tx_min_se))

    }
)

#--------------------------------------------------------
setMethod(
    f = "calc_tx_min",
    signature = c(x = "RetraiteEuroRest"),
    def = function(x){

        # Output zero
        out_zero <- rep(0, nrow(x@mp))

        # Output
        return(list(
            tx_tech_an = out_zero,
            tx_tech_se = out_zero,
            tx_an = out_zero,
            tx_se = out_zero))

    }
)
