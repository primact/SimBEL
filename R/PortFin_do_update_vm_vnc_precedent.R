
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_update_vm_vnc_precedent
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue et met a jour l'attribut \code{vm_vnc_precedent} d'un portefeuille financier.
##'
##' \code{do_update_vm_vnc_precedent} est une methode permettant de calculer et mettre a jour l'attribut \code{vm_vnc_precedent}
##' d'un portefeuille financier.
##' @name do_update_vm_vnc_precedent
##' @docType methods
##' @param x objet de la classe \code{\link{PortFin}}, correspondant au portefeuille financier de l'assureur
##'  avant mise a jour de l'attribut \code{vm_vnc_precedent}.
##' @return L'objet \code{x} de la classe \code{\link{PortFin}} renvoye correspond au portefeuille financier
##' de l'assureur dont l'attribut \code{vm_vnc_precedent} a ete mis a jour.
##' @author Prim'Act
##' @seealso \code{\link{print_alloc}}.
##' @export
##' @include PortFin_class.R

setGeneric(name = "do_update_vm_vnc_precedent", def = function(x){standardGeneric("do_update_vm_vnc_precedent")})
setMethod(
    f = "do_update_vm_vnc_precedent",
    signature = c(x = "PortFin"),
    definition = function(x){

        # Calcul des VM et VNC courantes
        table_alloc <- print_alloc(x)

        # Operation de mise a jour, on affecte a l'attribut VM/VNC precedente les valeurs de VM/VNC actuelle :
        x@vm_vnc_precedent <- list(vm = list(action = table_alloc["alloc_action", "alloc_valeur"],
                                                immo   = table_alloc["alloc_immo",   "alloc_valeur"],
                                                oblig  = table_alloc["alloc_oblig",  "alloc_valeur"],
                                             treso = table_alloc["alloc_treso",  "alloc_valeur"]),
                                      vnc = list(action = table_alloc["alloc_action", "alloc_valeur_nc"],
                                                 immo   = table_alloc["alloc_immo",   "alloc_valeur_nc"],
                                                 oblig  = table_alloc["alloc_oblig",  "alloc_valeur_nc"],
                                                 treso = table_alloc["alloc_treso",  "alloc_valeur_nc"]))

        # Output
        return(x)
    }
)
