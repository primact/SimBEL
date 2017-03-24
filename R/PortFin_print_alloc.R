
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           print_alloc()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le poids de chaque composante du portefeuille action.
##'
##' \code{pint_alloc} est une methode permettant de calculer l'allocation absolue et relative du portefeuille.
##' @name print_alloc
##' @docType methods
##' @param x objet de la classe PortFin.
##' @return Un data frame compose de quatre colonnes et cinq lignes.
##' La colonne \describe{
##' \item{\code{alloc_valeur} : }{decrit le montant alloue en valeur de marche par poche d'actif.}
##' \item{\code{alloc_proportion} : }{decrit la proportion allouee en valeur de marche par poche d'actif.}
##' \item{\code{alloc_valeur_vnc} : }{decrit le montant alloue en valeur nette comptable par poche d'actif.}
##' \item{\code{alloc_proportion_vnc} : }{decrit la proportion allouee en valeur nette comptable par poche d'actif.}
##' }
##' Les lignes correspondent aux classes d'actifs : (Action / Immobilier / Obligation / Tresorerie / Actifs cumules)
##' @author Prim'Act
##' @export
##' @aliases PortFin
##' @include PortFin_class.R

setGeneric(name = "print_alloc", def = function(x){standardGeneric("print_alloc")})
setMethod(
    f = "print_alloc",
    signature = "PortFin",
    definition = function(x){

        # Allocation en valeur de marche
        alloc_action <- sum(x["ptf_action"]["ptf_action"][,"val_marche"]) # Action
        alloc_immo   <- sum(x["ptf_immo"]["ptf_immo"][,"val_marche"]) # Immo
        alloc_oblig  <- sum(x["ptf_oblig"]["ptf_oblig"][,"val_marche"]) # Oblig
        alloc_treso  <- sum(x["ptf_treso"]["ptf_treso"][,"val_marche"]) # Treso
        alloc_total <- alloc_action + alloc_immo + alloc_oblig + alloc_treso # Total

        # Allocation en valeur nette comptable
        alloc_action_nc <- sum(x["ptf_action"]["ptf_action"][,"val_nc"]) # Action
        alloc_immo_nc   <- sum(x["ptf_immo"]["ptf_immo"][,"val_nc"]) # Immo
        alloc_oblig_nc  <- sum(x["ptf_oblig"]["ptf_oblig"][,"val_nc"]) # Oblig
        alloc_treso_nc  <- sum(x["ptf_treso"]["ptf_treso"][,"val_nc"]) # Treso
        alloc_total_nc <- alloc_action_nc + alloc_immo_nc + alloc_oblig_nc + alloc_treso_nc # Total

        alloc <- data.frame(alloc_valeur = c(alloc_action, alloc_immo, alloc_oblig, alloc_treso, alloc_total),
                            alloc_proportion = c(alloc_action/alloc_total, alloc_immo/alloc_total,
                                                 alloc_oblig/alloc_total, alloc_treso/alloc_total, alloc_total/alloc_total),
                            alloc_valeur_nc = c(alloc_action_nc, alloc_immo_nc, alloc_oblig_nc, alloc_treso_nc, alloc_total_nc),
                            alloc_proportion_nc = c(alloc_action_nc/alloc_total_nc, alloc_immo_nc/alloc_total_nc,
                                                    alloc_oblig_nc/alloc_total_nc, alloc_treso_nc/alloc_total_nc, alloc_total_nc/alloc_total_nc),
                            row.names = c("alloc_action", "alloc_immo", "alloc_oblig", "alloc_treso","alloc_total"))

        return(alloc)
    }
)
