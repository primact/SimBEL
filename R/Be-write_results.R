#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script execute enregistre les resultats.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           write_be_results
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Enregistre les resultats d'une evaluation best estimate
##'
##' \code{write_be_results} est une methode permettant d'enregistrer en \code{.cvs} les resultats
##' d'une evaluation best estimate.
##' @name write_be_results
##' @docType methods
##' @param nom_run est un objet de type \code{character} utilise pour nommer le fichier de resultats.
##' @param path est un objet de type \code{character} utilise pour indiquer le chemin d'enregistrement des resultats.
##' @param x est un objet de type \code{Be}.
##' @author Prim'Act
##' @export
##' @aliases Be
##'
setGeneric(name = "write_be_results", def = function(nom_run, path, x){standardGeneric("write_be_results")})
setMethod(
  f = "write_be_results",
  signature = c(nom_run = "character", path = "character", x = "Be"),
  definition = function(nom_run, path, x){

    # Liste des fichiers de sorties
    nom_flux <- names(x@tab_flux)
    nom_be <- names(x@tab_be)

    # Boucle sur les flux de BE
    for(i in nom_flux[- which(nom_flux == "nom_produit")]){
      m <- x@tab_flux[[i]] # Matrice de flux
      colnames(m) <- nom_flux[["nom_produit"]] # Nom des produits en colonne

      # Sauvegarde des resultats
      write.csv2(m, file = paste(path, nom_run, "_", i,  ".csv", sep = ""))
    }

    # Boucle sur les elements de BE
    for(i in nom_be[- which(nom_be == "nom_produit")]){
      vec <- x@tab_be[[i]] # vecteur de be
      names(vec) <- nom_be[["nom_produit"]] # Nom des produits en colonne

      # Sauvegarde des resultats
      write.csv2(vec, file = paste(path, nom_run, "_", i, ".csv", sep = ""))
    }

  }
)

