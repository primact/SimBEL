#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_rdt()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les rendements action, immobilier et de la tresorerie.
##'
##' \code{calc_rdt} est une methode permettant de calculer les rendements des portfeuilles \code{\link{Action}} et
##' \code{\link{Immo}} d'un objet \code{\link{PortFin}}. Le rendement de la \code{\link{Treso}} est egalement fourni.
##' @name calc_rdt
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}.
##' @param mp_ESG objet de la classe \code{\link{ModelPointESG}} decrivant les conditions de l'annee n
##' (ainsi que l'annee (n-1) pour les indices Actions et Immobilier).
##' @return \code{rdt_action} un \code{data.frame} compose de deux colonnes et autant de lignes que le
##' portefeuille Action.
##' @return \code{rdt_immo} un \code{data.frame} compose de deux colonnes et autant de lignes que le
##' portefeuille Immobilier.
##' @return \code{rdt_treso} une valeur \code{numeric} correspondant au taux de rendement de la tresorerie..
##' @author Prim'Act
##' @export
##' @include PortFin_class.R ModelPointESG_class.R

setGeneric(name = "calc_rdt", def = function(x, mp_ESG){standardGeneric("calc_rdt")})
setMethod(
    f = "calc_rdt",
    signature = c(x = "PortFin", mp_ESG = "ModelPointESG"),
    definition = function(x, mp_ESG){
        
        # Recuperation des PTF
        ptf_action <- x@ptf_action@ptf_action
        ptf_immo   <- x@ptf_immo@ptf_immo
        
        # Gestion des noms de colonnes du data.frame de donnnees
        nom_table_action <- names(ptf_action)
        num_index_action <- which(nom_table_action == "num_index")
        
        nom_table_immo <- names(ptf_immo)
        num_index_immo <- which(nom_table_immo == "num_index")
        
        
        # Creation du vecteur action/actionprev selon les indices
        index_action <- .subset2(ptf_action, num_index_action)
        index_immo <- .subset2(ptf_immo, num_index_immo)
        
        # Afin de tenir compte des differents indices possibles de chaque ligne action,
        # on est oblige de raisonner de la sorte pour etablir les bons cours courants et precedents
        
        # Gestion des noms de colonnes du data.frame de donnnees
        nom_table_mp_action <- names(mp_ESG@indice_action)
        ncol_S_action <- which(nom_table_mp_action == "S_action")
        ncol_S_prev_action <- which(nom_table_mp_action == "S_prev_action")
        
        # Rendement action
        S_action <- .subset2(mp_ESG@indice_action, ncol_S_action)[index_action]
        S_prev_action <- .subset2(mp_ESG@indice_action, ncol_S_prev_action)[index_action]
        
        
        # Gestion des noms de colonnes du data.frame de donnnees
        mp_ESG_indice_immo <- mp_ESG@indice_immo
        nom_table_mp_immo <- names(mp_ESG_indice_immo)
        ncol_S_immo <- which(nom_table_mp_immo == "S_immo")
        ncol_S_prev_immo <- which(nom_table_mp_immo == "S_prev_immo")
        
        # Rendement immo
        S_immo <- .subset2(mp_ESG_indice_immo, ncol_S_immo)[index_immo]
        S_prev_immo <- .subset2(mp_ESG_indice_immo, ncol_S_prev_immo)[index_immo]
        
        
        # Appel des fonctions actions et immobilier
        rdt_action <- revalo_action(x@ptf_action, S_action, S_prev_action)
        rdt_immo   <- revalo_immo(x@ptf_immo, S_immo, S_prev_immo)
        rdt_treso  <- revalo_treso(mp_ESG@yield_curve[1L], 0) # specificite de la yield curve des ModelPointESG : elle demarre a R(t,t+1), le taux precedent est donc R(t,t) = 0
        
        # Output
        return(list(rdt_action = rdt_action, rdt_immo = rdt_immo, rdt_treso = rdt_treso))
    }
)



