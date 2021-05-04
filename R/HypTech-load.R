#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des parametres technique
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Methode permettant de charger la valeur des parametres techniques.
##'
##' \code{load_ht} est une methode permettant de charger les parametres associees a un
##' objet de classe \code{\link{HypTech}}.
##' @name load_ht
##' @docType methods
##' @param x est un objet de la classe \code{\link{Initialisation}} qui est utilise pour renseigner le chemin
##' d'acces de tous les parametres techniques.
##' @return L'objet de la classe \code{\link{HypTech}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @seealso La classe \code{\link{Initialisation}} et sa methode \code{\link{set_architecture}}
##'  pour renseigner l'input.
##' @export
##' @include HypTech-class.R Initialisation_load.R
##'
setGeneric(name = "load_ht", def = function(x){standardGeneric("load_ht")})
setMethod(
    f = "load_ht",
    signature = "Initialisation",
    def = function(x){

        # Recuperation de l'attribut 'address'
        address <- x@address

        input_morta      <- read.csv2(paste(address[["param"]][["tables"]], "empl_tables_morta.csv", sep="/"), header = TRUE, colClasses = c("character", "character", "character"))
        input_rach       <- read.csv2(paste(address[["param"]][["tables"]], "empl_tables_rachats.csv", sep="/"), header = TRUE, colClasses = c("character", "character", "character"))
        input_param_rach <- read.csv2(paste(address[["param"]][["tables"]], "empl_param_rachats_conj.csv", sep="/"), header = TRUE, colClasses = c("character", "character"))

        list_morta       <- list()
        list_rach        <- list()
        list_param_rach  <- list()

        ## 1 - Tables de mortalite
        # Chargement et instanciation des tables de moratlite
        for (i in 1:nrow(input_morta["nom_table_R"])) {

            # Lecture de la table
            temp_csv     <- read.csv2(paste(address[["param"]][["tables"]], input_morta[i,"nom_table_csv"], sep = "/"), header = TRUE, colClasses = c("integer", "integer", "numeric"))

            # Tests
            if (! all(! is.na(temp_csv)))
                stop("[HypTech - load] : Presence de NA dans l'une des tables de mortalite.")

            # Conversion de la table en fonction du type
            if (input_morta[i,"type"] == "lx")
                table <- convert_table(temp_csv[order(temp_csv["gen"], temp_csv["age"]),], type = "lx")
            else if (input_morta[i,"type"] == "qx")
                table <- convert_table(temp_csv[order(temp_csv["gen"], temp_csv["age"]),], type = "qx")
            else
                stop("HypTech-load : Le type des tables de mortalite doit correspondre a l'un des deux types suivants : qx ou lx.")

            # Creation de l'objet et ajout dans la liste
            list_morta[[i]] <- new(Class = "ParamTableMort", table)

        }
        # Nommage des tables
        names(list_morta) <- input_morta[,"nom_table_R"]


        ## 2 - Tables de rachats
        # Chargement et instanciation des tables de rachat
        for (i in 1:nrow(input_rach["nom_table_R"])) {

            # Lecture du fichier
            temp_csv        <- read.csv2(paste(address[["param"]][["tables"]], input_rach[i,"nom_table_csv"], sep = "/"), header = TRUE, colClasses = c("integer", "integer", "numeric"))

            # Tests
            if (! all(! is.na(temp_csv)))
                stop("[HypTech - load] : Presence de NA dans l'une des tables de rachats.")

            # Creation de l'objet et ajout dans la liste
            list_rach[[i]]  <- new(Class ="ParamTableRach", temp_csv)

        }
        # Nommage des tables
        names(list_rach) <- input_rach[,"nom_table_R"]


        ## 3 - Parametres rachats dynamiques
        # Chargement et instanciation des tables de parametres de rachats dynamique
        for (i in 1L:nrow(input_param_rach["nom_table_R"])) {

            # Lecture du fichier
            temp_csv              <- read.csv2(paste(address[["param"]][["tables"]], input_param_rach[i,"nom_table_csv"], sep = "/"), header = TRUE,
                                               colClasses = rep("numeric", 6))

            # Tests
            if (! all(! is.na(temp_csv)))
                stop("[HypTech - load] : Presence de NA dans l'une des tables de rachats dynamiques.")

            # Creation de l'objet et ajout dans la liste
            list_param_rach[[i]]  <- new(Class ="ParamRachDyn", temp_csv)

        }
        # Nommage des tables
        names(list_param_rach) <- input_param_rach[,"nom_table_R"]

        ## 4 - Parametres de comportement
        # Chargement et instanciation des parametres de comportement
        param_comport_csv   <- read.csv2(paste(address[["param"]][["tables"]], "param_comport.csv", sep = "/"), header = TRUE,
                                         colClasses = c("character", rep("numeric", 10)))
        nom_param_comport   <- as.character(param_comport_csv[,"nom_param_comport"])
        mat_oblig           <- as.numeric(param_comport_csv["mat_oblig"])
        alloc_mar           <- as.numeric((c(param_comport_csv["alloc_mar_action"],
                                             param_comport_csv["alloc_mar_immo"],
                                             param_comport_csv["alloc_mar_tres"],
                                             param_comport_csv["alloc_mar_oblig"])))
        w_n                 <- as.numeric(param_comport_csv["w_n"])
        marge_mar           <- as.numeric(param_comport_csv["marge_mar"])
        ch_enc_mar          <- as.numeric(param_comport_csv["ch_enc_mar"])
        ind_ref_action      <- as.numeric(param_comport_csv["ind_ref_action"])
        ind_ref_immo        <- as.numeric(param_comport_csv["ind_ref_immo"])

        # instanciation de la classe ParamComport
        param_comport       <- new(Class = "ParamComport",
                                   mat_oblig,
                                   alloc_mar,
                                   w_n,
                                   marge_mar,
                                   ch_enc_mar,
                                   ind_ref_action,
                                   ind_ref_immo )

        list_param_comport  = list(param_comport)
        names(list_param_comport) <- nom_param_comport

        # instanciation de la classe HypTech
        return(new(Class="HypTech",
                   list_morta,
                   list_rach,
                   list_param_rach,
                   list_param_comport))
    }
)
