

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           A remonter dans classe plus generale
#----------------------------------------------------------------------------------------------------------------------------------------------------
taux_period <- function(y, period = c("an")){
  #--------------------------------------------------------
  # Fonction permettant de calculer le taux d interet sur une periode
  #
  # Input
  # y : vecteur de taux annuel
  # period : peridocite souhaitee. Prend pour valeur :
  #          - 'an' : annuel
  #          - 'se' : semestriel
  #          - 'trim' : trimestriel
  #          - 'mens' : mensuel
  #
  # output
  # taux periodique
  #---------------------------------------------------------
  nom_period <- c("an", "se", "trim", "mens")
  valeur_period <- c(1, 0.5, 0.25, 1 / 12)

  # Test
  if(which(period %in% nom_period)){
    y_period <- (1 + y)^{valeur_period[which(period == nom_period)]} - 1
  }else{
    stop("Le nom de la 'period' doit etre controle")
  }

  # Output
  return(y_period)
}

#----------------------------------------------------------------------------------------------------------------------------------------------------
chgt_period <- function(y, period = c("an")){
  #--------------------------------------------------------
  # Fonction permettant de calculer le taux de chargement sur une periode
  #
  # Input
  # y : vecteur de taux de chargement sur base annuelle
  # period : peridocite souhaitee. Prend pour valeur :
  #          - 'an' : annuel
  #          - 'se' : semestriel
  #          - 'trim' : trimestriel
  #          - 'mens' : mensuel
  #
  # output
  # taux periodique
  #---------------------------------------------------------
  nom_period <- c("an", "se", "trim", "mens")
  valeur_period <- c(1, 0.5, 0.25, 1 / 12)

  # Test
  if(which(period %in% nom_period)){
    y_period <- 1 - (1 - y)^{valeur_period[which(period == nom_period)]}
  }else{
    stop("Le nom de la 'period' doit etre controle")
  }

  # Output
  return(y_period)
}
