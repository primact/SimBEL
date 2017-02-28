#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les methodes permettant d'extraire une situation economique
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           extract_ESG
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe ESG
##'
##' Classe pour les
##'
##' @name extract_ESG

# Fonction d'extraction d'un model point d'ESG
# ATTENTION dans les parametres annee va de 0 a 50 (i.e. le code le transforme en de 1 a 51)
# Renvoi un objet de la classe ModelPointESG
setGeneric(name = "extract_ESG", def = function(x, num_trajectoire,annee){standardGeneric("extract_ESG")})
setMethod(
  f = "extract_ESG",
  signature = c(x = "ESG", num_trajectoire = "integer", annee = "integer"),
  def = function(x,num_trajectoire,annee){
    # Declaration des vecteurs de stockage
    S_action      <- numeric()
    S_immo        <- numeric()
    S_prev_action <- numeric()
    S_prev_immo   <- numeric()

    # On parcourt la liste des differents indices action puis immobilier
    # Pour chaque indice, on retient la valeur correspondant a num_trajectoire, annee+1
    S_action      <- unlist(lapply(1:length(x["ind_action"]),function(y){S_action[y] <- as.data.frame(x["ind_action"][y])[num_trajectoire, annee + 1]}))
    S_immo        <- unlist(lapply(1:length(x["ind_immo"]),function(y){S_immo[y] <- as.data.frame(x["ind_immo"][y])[num_trajectoire, annee + 1]}))

    if(annee > 0) {
      S_prev_action <- unlist(lapply(1:length(x["ind_action"]),function(y){S_action[y] <- as.data.frame(x["ind_action"][y])[num_trajectoire, annee]}))
      S_prev_immo   <- unlist(lapply(1:length(x["ind_immo"]),function(y){S_immo[y] <- as.data.frame(x["ind_immo"][y])[num_trajectoire, annee]}))
    } else {
      S_prev_action <- S_action
      S_prev_immo   <- S_immo
    }

    indice_inflation <- as.numeric(x["ind_inflation"][num_trajectoire,])
    yield_curve <- as.numeric(unlist(as.data.frame(x["yield_curve"][paste("annee", annee, sep = "")])[num_trajectoire,]))
    deflateur   <- as.numeric(x["deflateur"][num_trajectoire,annee + 1])

    x <- new("ModelPointESG",
             annee        = annee,
             num_traj     = num_trajectoire,
             indice_action= data.frame(S_action, S_prev_action),
             indice_immo  = data.frame(S_immo,S_prev_immo),
             indice_inflation = indice_inflation,
             yield_curve  = yield_curve,
             deflateur    = deflateur)
    return(x)
  }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Constructeur general
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Fonction de chargement general des elements de l'ESG
setGeneric(name = "chargement_ESG",function(folder_ESG_address, nb_simu, nb_annee_proj){standardGeneric("chargement_ESG")})
setMethod(
    f = "chargement_ESG",
    signature = c(folder_ESG_address = "character", nb_simu = "integer", nb_annee_proj = "integer"),
    definition = function(folder_ESG_address, nb_simu, nb_annee_proj){

        # Nom des fichiers et arborescence
        # La colonne 1 contient le nom de l'indice,
        # La colonne 2 contient le nom du fichier,
        # la colonne 3 contient le type (Action / Immo / Inflation / Deflateur)
        # On veut pouvoir avoir plusieurs indices actions/immo
        file_name           <- read.csv2(paste(folder_ESG_address, "/noms_liens.csv", sep = ""), header = T)[1:6,1:3]
        folder_Indices      <- paste(folder_ESG_address, "/Simulation_Indices", sep = "")
        folder_yield_curve  <- paste(folder_ESG_address, "/Simulation_CourbeDesTaux", sep = "")
        folder_deflateur    <- paste(folder_ESG_address, "/Simulation_Deflateurs", sep = "")

        # Tables d'identifiant des differents indices actions et immo
        id_indices_action   <- file_name[which(file_name[,3] == "Action"),]
        id_indices_immo     <- file_name[which(file_name[,3] == "Immo"),]

        # Creation de listes d'indices actions et immos
        if(nrow(id_indices_action) < 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner au moins un indice de type Action")}
        indice_action       <- list()
        indice_action       <- lapply(1:nrow(id_indices_action), function(y){indice_action[[y]] <- read.csv2(paste(folder_Indices, id_indices_action[y,2], sep = "/"))})
        names(indice_action)<- id_indices_action[,1]

        if(nrow(id_indices_immo) < 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner au moins un indice de type Immo")}
        indice_immo         <- list()
        indice_immo         <- lapply(1:nrow(id_indices_immo), function(y){indice_immo[[y]] <- read.csv2(paste(folder_Indices, id_indices_immo[y,2], sep = "/"))})
        names(indice_immo)  <- id_indices_immo[,1]

        # Chargement de l'indice inflation
        if(length(which(file_name[,3] == "Inflation")) > 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner exactement un indice de type Inflation")}
        indice_inflation    <- read.csv2(paste(folder_Indices, file_name[which(file_name[,3] == "Inflation"),2], sep = "/"))

        # Chargement des courbe de taux
        if(length(which(file_name[,3] == "yield_curve")) != 1) {stop("[ESG : chargement_ESG] : Veuillez renseigner exactement un lien de type yield_curve")}
        yield_curve <- list()
        yield_curve <- lapply(0:nb_annee_proj,function(y){
            temp_name <- gsub(paste("dans_0_an", sep = ""), paste("dans_", y, "_an", sep = ""), file_name[which(file_name[,3] == "yield_curve"),2])
            yield_curve[[y+1]] <- read.csv2(paste(folder_yield_curve, temp_name, sep = "/"))})
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



