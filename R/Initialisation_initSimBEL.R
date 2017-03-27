##' Initialisation d'un workspace.
##'
##' \code{init_SimBEL} est la methode d'initialisation d'un workspace.
##' @name init_SimBEL
##' @docType methods
##' @param x un objet de la classe \code{\link{Initialisation}}.
##' @return Pas de sortie.
##' @note  Cette methode permet de creer l'objet \code{\link{Canton}} initial et de le sauvegarder dans le repertoire adequat de l'architecture.
##' @author Prim'Act
##' @export
##' @aliases Initialisation
##' @include Initialisation_class.R

setGeneric(name = "init_SimBEL", def = function(x){standardGeneric("init_SimBEL")})
setMethod(
    f = "init_SimBEL",
    signature = "Initialisation",
    definition = function(x){
        # Verification des inputs
        if(length(x@address) == 0) {stop("[Initialisation : init_SimBEL] : Veuillez faire tourner la fonction set_architecture sur l'objet Initialisation avant de lancer le processus d'initialisation. \n")}

        # Message
      message("Chargement des donnees et parametres du canton initial")
        # Actif
            # ESG et ModelPoint_ESG
            table_ESG   <- chargement_ESG(x@address[["param"]][["ESG"]], x@nb_simu, x@nb_annee_proj)
            mp_ESG_init <- extract_ESG(table_ESG, x@nb_simu, annee = as.integer(0))
            # Portfeuille financier de l'assureur
            Ptf_Fin     <- chargement_PortFin(x@address[["data"]][["actif"]], mp_ESG_init)
        # Passif
            Ptf_Passif <- load_pp(x)

        # Chargement d'un canton initial
            canton_init <- new("Canton")
            canton_init@annee        <- as.integer(0)
            canton_init@ptf_fin      <- Ptf_Fin
            canton_init@ptf_passif   <- Ptf_Passif
            canton_init@mp_esg       <- mp_ESG_init
            canton_init@ppb          <- ppb_load(paste(x@address[["param"]][["ppb"]], "param_ppb.csv", sep = "/"))
            canton_init@hyp_canton   <- hyp_canton_load(paste(x@address[["param"]][["hyp_canton"]], "param_hyp_canton.csv", sep = "/"))
            canton_init@param_alm    <- param_alm_engine_load(paste(x@address[["param"]][["alm"]], "param_alm.csv", sep = "/"), new("PortFin"))
            canton_init@param_revalo <- param_revalo_load(paste(x@address[["param"]][["revalo"]], "param_revalo.csv", sep = "/"))


        # Sauvegarde au format .RData du canton initial
        save(canton_init, file = paste(x@address[["save_folder"]][["init"]], "canton_init.RData", sep = "/"))

        # Output
        return(message("Fin du chargement des donnees du canton initial"))
    }
)
