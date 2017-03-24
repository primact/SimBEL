# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_dur_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des duration d'un portefeuille obligataire.
##'
##' \code{update_dur_oblig} est une methode permettant de mettre a jour la duration des composantes d'un portefeuille obligataire.
##' @name update_dur_oblig
##' @docType methods
##' @param x objet de la classe \code{Oblig} (decrivant le portefeuille obligataire en detention).
##' @param duration un vecteur de \code{numeric} a assigner a l'objet \code{Obligation}. 
##' @return L'objet \code{x} dont les durations ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "update_dur_oblig", def = function(x,duration){standardGeneric("update_dur_oblig")})
setMethod(
    f = "update_dur_oblig",
    signature = c(x = "Oblig", duration = "numeric"),
    definition = function(x, duration){
        # Verification des inputs
        if (nrow(x["ptf_oblig"]) != length(duration)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
        if(sum(duration < 0) > 0) { stop("[Oblig : update_dur_oblig] :  Le vecteur de duration initialement entre ne peut contenir de valeurs negatives. \n")}
        x["ptf_oblig"][,"duration"] <- duration
        return(x)
    }
)