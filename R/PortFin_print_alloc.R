
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           print_alloc()
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le poids de chaque composante du portefeuille.
##'
##' \code{pint_alloc} est une methode permettant de calculer l'allocation absolue et relative du portefeuille.
##' @name print_alloc
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}.
##' @return Un \code{data.frame} compose de quatre colonnes et cinq lignes.
##' La colonne \describe{
##' \item{\code{alloc_valeur} : }{decrit le montant alloue en valeur de marche par poche d'actif.}
##' \item{\code{alloc_proportion} : }{decrit la proportion allouee en valeur de marche par poche d'actif.}
##' \item{\code{alloc_valeur_vnc} : }{decrit le montant alloue en valeur nette comptable par poche d'actif.}
##' \item{\code{alloc_proportion_vnc} : }{decrit la proportion allouee en valeur nette comptable par poche d'actif.}
##' }
##' Les lignes correspondent aux classes d'actifs : (Action / Immobilier / Obligation / Tresorerie / Actifs cumules)
##' @author Prim'Act
##' @export
##' @include PortFin_class.R

setGeneric(name = "print_alloc", def = function(x){standardGeneric("print_alloc")})
setMethod(
    f = "print_alloc",
    signature = "PortFin",
    definition = function(x){

      # Gestion des noms de colonnes du data.frame de donnnees
      nom_table_action <- names(x@ptf_action@ptf_action)
      nom_table_immo <- names(x@ptf_immo@ptf_immo)
      nom_table_oblig <- names(x@ptf_oblig@ptf_oblig)
      nom_table_treso <- names(x@ptf_treso@ptf_treso)

      val_marche_action <- which(nom_table_action == "val_marche")
      val_nc_action <- which(nom_table_action == "val_nc")
      val_marche_immo <- which(nom_table_immo == "val_marche")
      val_nc_immo <- which(nom_table_immo == "val_nc")
      val_marche_oblig <- which(nom_table_oblig == "val_marche")
      val_nc_oblig <- which(nom_table_oblig == "val_nc")
      val_marche_treso <- which(nom_table_treso == "val_marche")
      val_nc_treso <- which(nom_table_treso == "val_nc")


      # Allocation en valeur de marche
      alloc_action <- sum(.subset2(x@ptf_action@ptf_action, val_marche_action)) # Action
      alloc_immo <- sum(.subset2(x@ptf_immo@ptf_immo, val_marche_immo)) # Immo
      alloc_oblig <- sum(.subset2(x@ptf_oblig@ptf_oblig, val_marche_oblig)) # oblig
      alloc_treso <- sum(.subset2(x@ptf_treso@ptf_treso, val_marche_treso)) # treso
      alloc_total <- alloc_action + alloc_immo + alloc_oblig + alloc_treso # Total

      # Allocation en valeur nette comptable
      alloc_action_nc <- sum(.subset2(x@ptf_action@ptf_action, val_nc_action)) # Action
      alloc_immo_nc <- sum(.subset2(x@ptf_immo@ptf_immo, val_nc_immo)) # immo
      alloc_oblig_nc <- sum(.subset2(x@ptf_oblig@ptf_oblig, val_nc_oblig)) # oblig
      alloc_treso_nc <- sum(.subset2(x@ptf_treso@ptf_treso, val_nc_treso)) # treso
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
