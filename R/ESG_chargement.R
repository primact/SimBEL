#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur general
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Cette methode charge les tables de simulations d'un ESG.
##'
##' \code{chargement_ESG} est une methode permettant de charger les trajectoires simulees par le generateur de
##' scenarios economiques (ESG) de Prim'Act et d'alimenter un objet \code{\link{ESG}}.
##' @name chargement_ESG
##' @docType methods
##' @param folder_ESG_address est un \code{character}. Il correspond au chemin de reference du dossier contenant
##' les extractions de l'ESG Prim'Act.
##' @param nb_simu est une valeur de type \code{integer} correspondant au nombre de trajectoire
##'  simulees par l'ESG Prim'Act.
##' @param nb_annee_proj est une valeur de type \code{integer} correspondant au nombre d'annees de projection
##'  des sorties de l'ESG Prim'Act.
##' @details Les differentes adresses potentielles pour les differents ESG employes (central, hausse de taux, baisse de taux)
##' sont construites par la fonction \code{\link{set_architecture}} de la classe \code{\link{Initialisation}}.
##' @return \code{x} l'objet de la classe \code{\link{ESG}} construit.
##' @author Prim'Act
##' @export
##' @include ESG_class.R ESG_internal.R

setGeneric(name = "chargement_ESG", function(folder_ESG_address, nb_simu, nb_annee_proj){standardGeneric("chargement_ESG")})
setMethod(
    f = "chargement_ESG",
    signature = c(folder_ESG_address = "character", nb_simu = "integer", nb_annee_proj = "integer"),
    definition = function(folder_ESG_address, nb_simu, nb_annee_proj){

        # Valeur en dur du terme qui est utilise en temps normal. Utiliser pour message d'erreur
        TERME <- 36L

        # Nom des fichiers et arborescence
        # La colonne 1 contient le nom de l'indice,
        # La colonne 2 contient le nom du fichier,
        # la colonne 3 contient le type (Action / Immo / Inflation / Deflateur)
        # On veut pouvoir avoir plusieurs indices actions/immo
        file_name           <- read.csv2(paste(folder_ESG_address, "noms_liens.csv", sep = "/"), header = T)[1:6,1:4]
        folder_Indices      <- paste(folder_ESG_address, "Simulation_Indices", sep = "/")
        folder_yield_curve  <- paste(folder_ESG_address, "Simulation_CourbeDesTaux", sep = "/")
        folder_deflateur    <- paste(folder_ESG_address, "Simulation_Deflateurs", sep = "/")

        # Tables d'identifiant des differents indices actions et immo
        id_indices_action   <- file_name[which(file_name[,3] == "Action"),]
        id_indices_immo     <- file_name[which(file_name[,3] == "Immo"),]
        # Verificateur des numeros d'indices
        if (length(id_indices_action$num_index) != length(unique(id_indices_action$num_index)) | length(id_indices_action$num_index) != nrow(id_indices_action)) stop("[ESG chargement indices actions] : Veuillez indiquer un numero d'index unique par index action. \n")
        if (!is.integer(id_indices_action$num_index) | sum(sort(id_indices_action$num_index) != c(1:length(id_indices_action$num_index))) > 0 ) stop("[ESG chargement] : Les numeros d'index action doivent etre des entiers, l'indicage doit demarrer a 1 et etre continu par pas de 1.")
        if (length(id_indices_immo$num_index) != length(unique(id_indices_immo$num_index)) | length(id_indices_immo$num_index) != nrow(id_indices_immo)) stop("[ESG chargement indices immo] : Veuillez indiquer un numero d'index unique par index immo. \n")
        if (!is.integer(id_indices_immo$num_index) | sum(sort(id_indices_immo$num_index) != c(1:length(id_indices_immo$num_index))) > 0 ) stop("[ESG chargement] : Les numeros d'index immo doivent etre des entiers, l'indicage doit demarrer a 1 et etre continu par pas de 1.")

        # Creation de listes d'indices actions et immos
        if(nrow(id_indices_action) < 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner au moins un indice de type
                                              Action")}
        indice_action       <- list()
        indice_action       <- lapply(id_indices_action$num_index, function(y){indice_action[[y]] <- read.csv2(paste(folder_Indices, id_indices_action[y,2], sep = "/"))})
        names(indice_action)<- id_indices_action[,1]

        if(nrow(id_indices_immo) < 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner au moins un indice de type
                                            Immo")}
        indice_immo         <- list()
        indice_immo         <- lapply(id_indices_immo$num_index, function(y){indice_immo[[y]] <- read.csv2(paste(folder_Indices, id_indices_immo[y,2], sep = "/"))})
        names(indice_immo)  <- id_indices_immo[,1]

        # Chargement de l'indice inflation
        if(length(which(file_name[,3] == "Inflation")) > 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner
                                                                  exactement un indice de type Inflation")}
        indice_inflation    <- read.csv2(paste(folder_Indices, file_name[which(file_name[,3] == "Inflation"),2], sep = "/"))

        # Chargement des courbe de taux
        if(length(which(file_name[,3] == "yield_curve")) != 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner
                                                                     exactement un lien de type yield_curve")}
        yield_curve <- list()
        yield_curve <- lapply(0:nb_annee_proj,function(y){
            temp_name <- gsub(paste("dans_0_an", sep = ""), paste("dans_", y, "_an", sep = ""), file_name[which(file_name[,3] == "yield_curve"),2])
            temp_curve <- read.csv2(paste(folder_yield_curve, temp_name, sep = "/"))
            if(ncol(temp_curve) != TERME ) {
                warning(paste("[ESG chargement : yield_curve] : Les courbes de taux spot forward ", y," ans, ne
                              contiennent pas exactement ", TERME," maturites."))
            } else {
                if(! all(colnames(temp_curve) != paste("X", c(0.0833333, 0.25, 0.5, 0.75, 1:30, 40, 50))))
                  warning(paste("[ESG chargement : yield_curve] : Les colonnes des courbes de taux spot forward ",
                                y, "ans, ne sont pas renseignees selon la convention retenue pour les maturites de 1 a ",
                                TERME," (le nommage des colonnes doit etre 0.0833333, 0.25, 0.5, 0.75, 1, 2, ..., ", TERME,").", sep = ""))
            }
            yield_curve[[y+1]] <- temp_curve})
        names(yield_curve) <- paste("annee", 0:nb_annee_proj, sep = "")

        # Chargement des courbe de deflateur
        if(length(which(file_name[,3] == "deflateur")) != 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner exactement un lien de type deflateur")}
        deflateur          <- read.csv2(paste(folder_deflateur, file_name[which(file_name[,3] == "deflateur"),2], sep = "/"))

        # Affectation des listes a l'objet ESG

        x <- new("ESG")
        x["nb_simu"]       <- nb_simu
        x["ind_action"]    <- indice_action
        x["ind_immo"]      <- indice_immo
        x["ind_inflation"] <- indice_inflation
        x["yield_curve"]   <- yield_curve
        x["deflateur"]     <- deflateur

        return(x)
    }
)



