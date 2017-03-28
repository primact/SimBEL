
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_spread_unitaire
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique le choc spread de la formule standard Solvabilite 2 a une ligne obligataire.
##'
##' \code{do_choc_spread_unitaire} Permet a partir d'une table contenant les elements du choc de spread obligataire
##'  Solvabilite 2 et d'une ligne obligataire d'un element \code{\link{Oblig}} d'un portefeuille financier
##'  \code{\link{PortFin}}
##' d'appliquer le choc de spread a cette ligne obligataire.
##' @name do_choc_spread_unitaire
##' @docType methods
##' @param table_choc_spread un \code{data.frame} contenant la table de parametres avec les chocs de spreads.
##' @param ligne_oblig  un \code{data.frame}. Il correspond a une ligne obligataire d'un portefeuille \code{\link{Oblig}}
##'  d'un assureur.
##' @return \code{vm_choquee} une valeur \code{numeric} correspondant a la valeur de marche de la ligne obligataire
##'  suite a l'application du choc de spread a cette ligne.
##' @author Prim'Act
##' @seealso La classe \code{\link{PortFin}}.
##' @export
##' @include ChocSolvabilite2_class.R


setGeneric(name = "do_choc_spread_unitaire", def = function(table_choc_spread, ligne_oblig){standardGeneric("do_choc_spread_unitaire")})
setMethod(
    f = "do_choc_spread_unitaire",
    signature = c("data.frame", "data.frame"),
    definition = function(table_choc_spread, ligne_oblig){
        # Vecteur permettant d'effectuer un tri selon le rating de la ligne obligataire etudiee
        # necessaire pour lire l'input confere forme speciale du fichier
        tri_rating <- which(table_choc_spread$rating == ligne_oblig$rating)
        duration_consideree = max(1, ligne_oblig$duration)

        if (duration_consideree <= 5) {
            # Application du tri selon les bornes de duration
            tri_duration <- which(table_choc_spread$duration == "0-5ans")
            # Selection de la ligne dans la table de parametre
            line_select <- tri_rating[tri_rating %in% tri_duration]
            # Calcul de la vm choquee
            coef        <- table_choc_spread$param_B[line_select] * duration_consideree
            vm_choquee  <- (1 - coef) * ligne_oblig$val_marche
        }
        else if (duration_consideree <= 10 & duration_consideree > 5) {
            # Application du tri selon les bornes de duration
            tri_duration <- which(table_choc_spread$duration == "5-10ans")
            # Selection de la ligne dans la table de parametre
            line_select <- tri_rating[tri_rating %in% tri_duration]
            # Calcul de la vm choquee
            coef        <- table_choc_spread$param_A[line_select] + table_choc_spread$param_B[line_select] * (duration_consideree - 5)
            vm_choquee  <- (1 - coef) * ligne_oblig$val_marche
        }
        else if (duration_consideree <= 15 & duration_consideree > 10) {
            # Application du tri selon les bornes de duration
            tri_duration <- which(table_choc_spread$duration == "10-15ans")
            # Selection de la ligne dans la table de parametre
            line_select <- tri_rating[tri_rating %in% tri_duration]
            # Calcul de la vm choquee
            coef        <- table_choc_spread$param_A[line_select] + table_choc_spread$param_B[line_select] * (duration_consideree - 10)
            vm_choquee  <- (1 - coef)  * ligne_oblig$val_marche
        }
        else if (duration_consideree <= 20 & duration_consideree > 15) {
            # Application du tri selon les bornes de duration
            tri_duration <- which(table_choc_spread$duration == "15-20ans")
            # Selection de la ligne dans la table de parametre
            line_select <- tri_rating[tri_rating %in% tri_duration]
            # Calcul de la vm choquee
            coef        <- table_choc_spread$param_A[line_select] + table_choc_spread$param_B[line_select] * (duration_consideree - 15)
            vm_choquee  <- (1 - coef)  * ligne_oblig$val_marche
        }
        else if (duration_consideree > 20) {# Application du tri selon les bornes de duration
            tri_duration <- which(table_choc_spread$duration == "20ans")
            # Selection de la ligne dans la table de parametre
            line_select <- tri_rating[tri_rating %in% tri_duration]
            # Calcul de la vm choquee
            coef        <- min(1,table_choc_spread$param_A[line_select] + table_choc_spread$param_B[line_select] * (duration_consideree - 20))
            vm_choquee  <- (1 - coef) * ligne_oblig$val_marche
        }
        return(vm_choquee)
    }
)
