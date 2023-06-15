#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_ax : Methode de calcul des ax
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la valeur d'un coefficient actuariel d'une rente sur une tete.
##'
##' \code{calc_ax} est une methode permettant de calculer les coefficients actuariels
##' pour des rentes versant potentiellement plusieurs flux dans l'annee.
##' @name calc_ax
##' @docType methods
##' @param tx_tech un \code{numeric} correspondant au taux technique utilise pour l'actualisation.
##' @param freq_rente un \code{integer} correspondant au nombre de versements dans l'annee.
##' @param echu un \code{logical} valant \code{TRUE} si la rente est a terme echue et \code{FALSE}
##' si la rente est a terme echoir.
##' @param table_mort un objet de la classe \code{ParamTableMort} designant la table de mortalite de la tete principale.
##' @param age la valeur \code{integer} de l'age de la tete principale.
##' @param gen la valeur \code{integer} de la generation de la tete principale.
##' @return Le coefficient actuariel calcule.
##' @author Prim'Act
##' @seealso Le calcul des probabilites de survies \code{\link{calc_proba_survie}}.
##' @export
##' @include ParamTableMort-class.R

setGeneric("calc_ax", function(tx_tech, freq_rente, echu, table_mort, age, gen) {
    standardGeneric("calc_ax")
})
setMethod(
    f = "calc_ax",
    signature = c(
        tx_tech = "numeric", freq_rente = "integer", echu = "logical",
        table_mort = "ParamTableMort", age = "integer", gen = "integer"
    ),
    def = function(tx_tech, freq_rente, echu, table_mort, age, gen) {
        # Calcul de la borne pour la somme des termes actualises
        nPeriodes <- max(table_mort@age_max - age, 1L)

        # Calcul de l'ax echu
        ax <- sum(calc_proba_survie(table_mort, age, gen, nPeriodes) * (1 + tx_tech)^-(1:nPeriodes)) +
            (freq_rente - 1) / (2 * freq_rente)

        # Ajustement si terme echoir
        ax <- ax + (1 - echu) * 1 / freq_rente

        # Output
        return(ax)
    }
)


#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_axy : Methode de calcul des axy
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule la valeur d'un coefficient actuariel d'une rente avec reversion.
##'
##' \code{calc_axy} est une methode permettant de calculer les coefficients actuariels
##' pour des rentes avec reversion versant potentiellement plusieurs flux dans l'annee.
##' @name calc_axy
##' @docType methods
##' @param tx_tech un \code{numeric} correspondant au taux technique utilise pour l'actualisation.
##' @param tx_rvs un \code{numeric} correspondant au taux de reversion de la rente.
##' @param freq_rente un \code{integer} correspondant au nombre de versements dans l'annee.
##' @param echu un \code{logical} valant \code{TRUE} si la rente est a terme echue et \code{FALSE}
##' si la rente est a terme echoir.
##' @param table_mort_1 un objet de la classe \code{ParamTableMort} designant la table de mortalite de la tete principale.
##' @param age_1 la valeur \code{integer} de l'age de la tete principale.
##' @param gen_1 la valeur \code{integer} de la generation de la tete principale.
##' @param table_mort_2 un objet de la classe \code{ParamTableMort} designant la table de mortalite de la tete secondaire.
##' @param age_2 la valeur \code{integer} de l'age de la tete secondaire.
##' @param gen_2 la valeur \code{integer} de la generation de la tete secondaire.
##' @return Le coefficient actuariel calcule.
##' @author Prim'Act
##' @seealso Le calcul des probabilites de survies \code{\link{calc_proba_survie}}.
##' @export
##' @include ParamTableMort-class.R
##'
setGeneric("calc_axy", function(tx_tech, tx_rvs, freq_rente, echu, table_mort_1, age_1, gen_1,
                                table_mort_2, age_2, gen_2) {
    standardGeneric("calc_axy")
})
setMethod(
    f = "calc_axy",
    signature = c(
        tx_tech = "numeric", tx_rvs = "numeric", freq_rente = "integer", echu = "logical",
        table_mort_1 = "ParamTableMort", age_1 = "integer", gen_1 = "integer",
        table_mort_2 = "ParamTableMort", age_2 = "integer", gen_2 = "integer"
    ),
    def = function(tx_tech, tx_rvs, freq_rente, echu, table_mort_1, age_1, gen_1,
                   table_mort_2, age_2, gen_2) {
        # Bornes de la sommation
        nPeriodes1 <- max(table_mort_1@age_max - age_1, 1L) # Premiere tete
        nPeriodes2 <- max(table_mort_2@age_max - age_2, 1L) # Deuxieme tete
        nPeriodes3 <- max(nPeriodes1, nPeriodes2) # Max pour la combinaison des tetes


        # Calcul axy
        axy <- calc_ax(tx_tech, freq_rente, echu, table_mort_1, age_1, gen_1) # = ax
        axy <- axy + tx_rvs * calc_ax(tx_tech, freq_rente, echu, table_mort_2, age_2, gen_2) # = ax + ay
        axy <- axy - tx_rvs * (sum(calc_proba_survie(table_mort_1, age_1, gen_1, nPeriodes3) *
            calc_proba_survie(table_mort_2, age_2, gen_2, nPeriodes3) *
            (1 + tx_tech)^-(1:(nPeriodes3))) +
            (freq_rente - 1) / (2 * freq_rente) +
            (1 - echu) * 1 / freq_rente)

        # Output
        return(axy)
    }
)
