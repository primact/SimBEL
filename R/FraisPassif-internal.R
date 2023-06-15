#----------------------------------------------------------
# Ce script comprend les methodes internes de la classe FraisPassif
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 06/02/2017. Fait par MT : initialisation
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction d'initialisation d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet renseigne.
#           - Erreur autrement
setMethod(
    f = "initialize",
    signature = "FraisPassif",
    definition = function(.Object, mp = data.frame()) {
        if (!missing(mp)) {
            .Object@mp <- mp

            # Validation du format
            validObject(.Object)
        } else {
            # Traitement du cas vide
            .Object@mp <- data.frame(
                nom_prod = factor(),
                frais_fixe_prime = numeric(),
                frais_var_prime = numeric(),
                ind_inf_frais_fixe_prime = logical(),
                ind_inf_frais_var_prime = logical(),
                frais_fixe_prest = numeric(),
                frais_var_prest = numeric(),
                ind_inf_frais_fixe_prest = logical(),
                ind_inf_frais_var_prest = logical(),
                frais_fixe_enc = numeric(),
                frais_var_enc = numeric(),
                ind_inf_frais_fixe_enc = logical(),
                ind_inf_frais_var_enc = logical()
            )
        }
        # Output
        return(.Object)
    }
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
    f = "[",
    signature = "FraisPassif",
    definition = function(x, i) {
        switch(EXPR = i,
            "mp" = {
                return(x@mp)
            },
            stop("Cet attribut n'existe pas!")
        )
    }
)


# Setteur
setReplaceMethod(
    f = "[",
    signature = "FraisPassif",
    definition = function(x, i, value) {
        switch(EXPR = i,
            "mp" = {
                x@mp <- value
            },
            stop("Cet attribut n'existe pas!")
        )
        validObject(x)
        return(x)
    }
)
