##' Fonction permettant de calculer le taux d'interet sur une periode.
##'
##' \code{taux_period} permet de calculer le taux d'interet sur une periode
##' @name taux_period
##' @docType methods
##' @param x un vecteur de la classe \code{numeric} representant le taux annuel.
##' @param period un \code{character} representant la peridocite souhaitee. Cette variable prend pour valeur :
##' \describe{
##' \item{\code{an} : }{annuel ;}
##' \item{\code{se} : }{semestriel ;}
##' \item{\code{trim} : }{trimestriel ;}
##' \item{\code{mens} : }{mensuel.}}
##' @return Le taux periodique.
##' @author Prim'Act

setGeneric(name = "taux_period", def = function(x, period) {
    standardGeneric("taux_period")
})
setMethod(
    f = "taux_period",
    signature("numeric", "character"),
    definition = function(x, period) {
        # Taux
        switch(period,
            "an" = {
                valeur_period <- 1
            },
            "se" = {
                valeur_period <- 0.5
            },
            "tri" = {
                valeur_period <- 0.25
            },
            "mens" = {
                valeur_period <- 1 / 12
            },
            stop("Le nom de la 'period' doit etre controle")
        )


        # Output
        return((1 + x)^valeur_period - 1)
    }
)



##' Fonction permettant de calculer le taux de chargement sur une periode.
##'
##' \code{chgt_period} permet de calculer le taux de chargement sur une periode.
##' @name chgt_period
##' @docType methods
##' @param x un vecteur de la classe \code{numeric} representant le taux annuel.
##' @param period un \code{character} representant la peridocite souhaitee. Cette variable prend pour valeur :
##' \describe{
##' \item{\code{an} : }{annuel ;}
##' \item{\code{se} : }{semestriel ;}
##' \item{\code{trim} : }{trimestriel ;}
##' \item{\code{mens} : }{mensuel.}}
##' @return Le taux periodique.
##' @author Prim'Act
##'
#----------------------------------------------------------------------------------------------------------------------------------------------------
setGeneric(name = "chgt_period", def = function(x, period) {
    standardGeneric("chgt_period")
})
setMethod(
    f = "chgt_period",
    signature("numeric", "character"),
    definition = function(x, period) {
        # Taux
        switch(period,
            "an" = {
                valeur_period <- 1
            },
            "se" = {
                valeur_period <- 0.5
            },
            "tri" = {
                valeur_period <- 0.25
            },
            "mens" = {
                valeur_period <- 1 / 12
            },
            stop("Le nom de la 'period' doit etre controle")
        )


        # Output
        return(1 - (1 - x)^valeur_period)
    }
)
