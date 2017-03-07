#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script execute les operations de calcul d'un BE
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           run_be
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul d'un BE
##'
##' \code{run_be} est une methode permettant de calcul un best estimate pour un canton.
##' @name run_be
##' @docType methods
##' @param x est un objet de type \code{Be}.
##' @param pre_on est une valeur \code{logical} qui lorsqu'elle vaut \code{TRUE} prend en compte la variation
##' de PRE dans le resultat technique utilisee pour le calcul de la participation aux benefices reglementaires.
##' @return L'objet BE avec les tables de resultats mise a jour.
##' @author Prim'Act
##' @export
##' @aliases Be
##'
setGeneric(name = "run_be", def = function(x, pre_on){standardGeneric("run_be")})
setMethod(
  f = "run_be",
  signature = c(x = "Be", pre_on = "logical"),
  definition = function(x, pre_on){

    # Affiche l'heure de demarrage de la simulation
    start_time <- Sys.time()
    print(paste("Date de lancement de l'evaluation : ", start_time, sep = ""))


    # Nombre de simulations
    nb_simu <- x@esg@nb_simu

    # Barre de progression
    pb <- txtProgressBar(min = 0, max = nb_simu, style = 3)

    # Boucle sur les simulations
    result_simu <- lapply(1:nb_simu,function(i){

      # Calcul du BE pour la simulation
      res <- run_be_simu(x, i, pre_on)
      # Mise a jour barre de progression
      setTxtProgressBar(pb, i)

      return(res)})

    # Ferme la barre de progression
    close(pb)

    # Alimentation des tableaux de resultats
    print("Alimentation des tableaux de resultats")
    # Nom de la liste qui permettent d'alimenter les tableaux de flux
    nom_flux <- c("nom_produit","prime", "prestation", "prestation_fdb", "frais", "flux_be")
    # Nom de la liste qui permettent d'alimenter les tableaux de flux
    nom_be <- c("nom_produit", "prime_actu", "prestation_actu", "prestation_fdb_actu", "frais_actu", "be")

    # Initialisation des calculs de moyenne
    res_flux <- lapply(nom_flux[-1], function(x){
      result_simu[[1]][[x]] / nb_simu
    })

    res_be <- lapply(nom_be[-1], function(x){
      result_simu[[1]][[x]] / nb_simu
    })

    # Nom des flux et des postes de BE
    names(res_flux) <- nom_flux[-1]
    names(res_be) <- nom_be[-1]

    # Boucle de remplissage et de calcul des moyennes
    for(i in 2:nb_simu){
      # Boucle pour alimenter le tableau des flux
      for(j in nom_flux[-1]){
        res_flux[[j]] <- res_flux[[j]] + result_simu[[i]][[j]] / nb_simu # Moyenne sur les simulations
      }
      for(j in nom_be[-1]){
        res_be[[j]] <- res_be[[j]] + result_simu[[i]][[j]] / nb_simu # Moyenne sur les simulations
      }
    }

    # Ajout des noms de produits
    res_flux[["nom_produit"]] <- result_simu[[1]][["nom_produit"]]
    res_be[["nom_produit"]] <- result_simu[[1]][["nom_produit"]]

    # Stockage des resultats
    for(j in names(res_flux)){
      x["tab_flux"][[j]] <- res_flux[[j]]
    }
    for(j in names(res_be)){
      x["tab_be"][[j]] <- res_be[[j]]
    }

    # Messages de fin
    print("Fin de l'evaluation")
    # Affiche l'heure de fin de la simulation
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    print(paste("Date de fin de l'evaluation : ", end_time, sep = ""))
    print(paste("Temps necessaire a l'evaluation : ", as.numeric(time_taken, units = "mins"), " minutes", sep = ""))
    # Output
    return(x)
    }
)
