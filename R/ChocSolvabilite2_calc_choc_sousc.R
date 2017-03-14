# GK 06/03/2017

setGeneric(name = "do_choc_frais", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_frais")})
setMethod(
    f = "do_choc_frais",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){   
    
        #----------------------------------------------------------------------------------------------------------------------------------------------------
        #           A ajouter l'inflation +1 % : GDK
        #----------------------------------------------------------------------------------------------------------------------------------------------------
        # SOUS ENTENDU L OBJET Choc_Solvabilite2 contient un attribut param_choc_sousc qui est de la forme $mp$choc_frais_assiette
        tx_relatif_choc_frais <- as.numeric(x@param_choc_sousc["mp"]["choc_frais_assiette"])
        ptf_passif <- canton@ptf_passif
        
        #prime
        ptf_passif["fp"]["mp"]["frais_fixe_prime"] <- ptf_passif["fp"]["mp"]["frais_fixe_prime"] * (1 + tx_relatif_choc_frais)
        ptf_passif["fp"]["mp"]["frais_var_prime"]  <- ptf_passif["fp"]["mp"]["frais_var_prime"]  * (1 + tx_relatif_choc_frais)
        #prest
        ptf_passif["fp"]["mp"]["frais_fixe_prest"] <- ptf_passif["fp"]["mp"]["frais_fixe_prest"] * (1 + tx_relatif_choc_frais)
        ptf_passif["fp"]["mp"]["frais_var_prest"]  <- ptf_passif["fp"]["mp"]["frais_var_prest"]  * (1 + tx_relatif_choc_frais)
        #enc
        ptf_passif["fp"]["mp"]["frais_fixe_enc"]   <- ptf_passif["fp"]["mp"]["frais_fixe_enc"]   * (1 + tx_relatif_choc_frais)
        ptf_passif["fp"]["mp"]["frais_var_enc"]    <- ptf_passif["fp"]["mp"]["frais_var_enc"]    * (1 + tx_relatif_choc_frais)
        
        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc
          
        canton@ptf_passif <- ptf_passif 
        
        return(canton)       
    }
)
    
setGeneric(name = "do_choc_mortalite", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_mortalite")})
setMethod(
    f = "do_choc_mortalite",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){   
        
        choc_mortalite   <- as.numeric(x@param_choc_sousc["mp"]["choc_mortalite"])
        ptf_passif <- canton@ptf_passif
        
        ptf_passif["ht"] <- get_choc_table(ptf_passif["ht"], choc_mortalite)
        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc
        
        canton@ptf_passif <- ptf_passif
        return(canton)
    }
)
    
setGeneric(name = "do_choc_longevite", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_longevite")})
setMethod(
    f = "do_choc_longevite",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){   
        
        choc_longevite   <- as.numeric(x@param_choc_sousc["mp"]["choc_longevite"])
        ptf_passif <- canton@ptf_passif
        
        ptf_passif["ht"] <- get_choc_table(ptf_passif["ht"], choc_longevite)
        
        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc
        
        canton@ptf_passif <- ptf_passif
        return(canton)
    }
)
    
setGeneric(name = "do_choc_rachat_up", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_rachat_up")})
setMethod(
    f = "do_choc_rachat_up",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){   
        
        choc_rachat_up     <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_up"])
        choc_rachat_up_lim <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_up_lim"])
        ptf_passif <- canton@ptf_passif
        
        ptf_passif["ht"]   <- get_choc_rach(ptf_passif["ht"], "up", choc_rachat_up, choc_rachat_up_lim)
        
        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc
        
        canton@ptf_passif <- ptf_passif
        return(canton)
    }
)

    
setGeneric(name = "do_choc_rachat_down", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_rachat_down")})
setMethod(
    f = "do_choc_rachat_down",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){   
        
        choc_rachat_down     <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_down"])
        choc_rachat_down_lim <- as.numeric(x@param_choc_sousc["mp"]["choc_rachat_down_lim"])
        ptf_passif <- canton@ptf_passif
        
        ptf_passif["ht"]<- get_choc_rach(ptf_passif["ht"], "down", choc_rachat_down, choc_rachat_down_lim)
        
        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc
        
        canton@ptf_passif <- ptf_passif
        return(canton)
    }
)
