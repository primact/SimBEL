
# Creer une fonction qui puisse appliquer le choc spread a un objet Oblig selon la maturite et le rating
# table _choc_action est l'element d'un objet : ChocSolvabilite2@param_choc_mket$table_choc_action
setGeneric(name = "do_choc_action_type1", def = function(x, canton){standardGeneric("do_choc_action_type1")})
setMethod(
    f = "do_choc_action_type1",
    signature = c("ChocSolvabilite2", "Canton"),
    definition = function(x, canton){
    # Verification des inputs
    if (nrow(canton@ptf_fin@ptf_action@ptf_action) == 0) { stop("[choc Mket : Action] : tentative de calcul du choc action avec un objet Action vide impossible. \n")}
    table_choc_action <- x@param_choc_mket@table_choc_action_type1
    ptf_action <- canton@ptf_fin@ptf_action

    # Selection des indices qui interessent
    index <- unique(ptf_action@ptf_action$num_index[which(ptf_action@ptf_action$num_index %in% table_choc_action$num_index)])

    # Trier par numero d'index
    # Dans l'exemple initial les num_mp sont ordonnes et les num_index aussi neanmoins au cours de la projection il va y avoir des permutations
    # Pour acceler l'algo de selection on prefere trier initialement le df par num_index croissante
    temp <- ptf_action@ptf_action[order(ptf_action@ptf_action$num_index),]

    # Spliter le dataframe action par numero d'index
    split_ptf_action <- list()
    split_ptf_action <- lapply(index, function(x){split_ptf_action[[x]] <- temp[which(temp$num_index == index[x]),]})

    # Appliquer le choc par bloc de numero d'index
    split_ptf_action <- lapply(1:length(index), function(x){
        split_ptf_action[[x]]$val_marche <- (1-table_choc_action[which(table_choc_action$num_index == index[x]),"choc_action"]) * split_ptf_action[[x]]$val_marche
        return(split_ptf_action[[x]])})

    # Refusionner le data.frame
    merged_ptf_action <- do.call("rbind", split_ptf_action)

    # Resortir un objet action, dont l'attribut ptf_action est trie comme initialement i.e. par ordre croissant du num_mp
    ptf_action_mod    <- new("Action", merged_ptf_action[order(merged_ptf_action$num_mp),])
    canton@ptf_fin@ptf_action <- ptf_action_mod
    return(canton)
    }
)



setGeneric(name = "do_choc_action_type2", def = function(x, canton){standardGeneric("do_choc_action_type2")})
setMethod(
  f = "do_choc_action_type2",
  signature = c("ChocSolvabilite2", "Canton"),
  definition = function(x, canton){
    # Verification des inputs
    if (nrow(canton@ptf_fin@ptf_action@ptf_action) == 0) { stop("[choc Mket : Action] : tentative de calcul du choc action avec un objet Action vide impossible. \n")}
    table_choc_action <- x@param_choc_mket@table_choc_action_type2
    ptf_action <- canton@ptf_fin@ptf_action

    # Selection des indices qui interessent
    index <- unique(ptf_action@ptf_action$num_index[which(ptf_action@ptf_action$num_index %in% table_choc_action$num_index)])

    # Trier par numero d'index
    # Dans l'exemple initial les num_mp sont ordonnes et les num_index aussi neanmoins au cours de la projection il va y avoir des permutations
    # Pour acceler l'algo de selection on prefere trier initialement le df par num_index croissante
    temp <- ptf_action@ptf_action[order(ptf_action@ptf_action$num_index),]

    # Spliter le dataframe action par numero d'index
    split_ptf_action <- list()
    split_ptf_action <- lapply(index, function(x){split_ptf_action[[x]] <- temp[which(temp$num_index == index[x]),]})

    # Appliquer le choc par bloc de numero d'index
    split_ptf_action <- lapply(1:length(index), function(x){
      split_ptf_action[[x]]$val_marche <- (1-table_choc_action[which(table_choc_action$num_index == index[x]),"choc_action"]) * split_ptf_action[[x]]$val_marche
      return(split_ptf_action[[x]])})

    # Refusionner le data.frame
    merged_ptf_action <- do.call("rbind", split_ptf_action)

    # Resortir un objet action, dont l'attribut ptf_action est trie comme initialement i.e. par ordre croissant du num_mp
    ptf_action_mod    <- new("Action", merged_ptf_action[order(merged_ptf_action$num_mp),])
    canton@ptf_fin@ptf_action <- ptf_action_mod
    return(canton)
  }
)



setGeneric(name = "do_choc_immo", def = function(x, canton){standardGeneric("do_choc_immo")})
setMethod(
    f = "do_choc_immo",
    signature = c("ChocSolvabilite2", "Canton"),
    definition = function(x, canton){
    # Verification des inputs
    if (nrow(canton@ptf_fin@ptf_immo@ptf_immo) == 0) { stop("[choc Mket : Immo] : tentative de calcul du choc immo avec un objet Immo vide impossible. \n")}

    ptf_immo <- canton@ptf_fin@ptf_immo
    table_choc_immo <- x@param_choc_mket@table_choc_immo

    # Selection des indices qui interessent
    index <- unique(ptf_immo@ptf_immo$num_index[which(ptf_immo@ptf_immo$num_index %in% table_choc_immo$num_index)])

    # Trier par numero d'index
    # Dans l'exemple initial les num_mp sont ordonnes et les num_index aussi neanmoins au cours de la projection il va y avoir des permutations
    # Pour acceler l'algo de selection on prefere trier initialement le df par num_index croissante
    temp <- ptf_immo@ptf_immo[order(ptf_immo@ptf_immo$num_index),]

    # Spliter le dataframe immo par numero d'index
    split_ptf_immo <- list()
    split_ptf_immo <- lapply(1:length(index), function(x){split_ptf_immo[[x]] <- temp[which(temp$num_index == index[x]),]})

    # Appliquer le choc par bloc de numero d'index
    split_ptf_immo <- lapply(1:length(index), function(x){
        split_ptf_immo[[x]]$val_marche <- (1-table_choc_immo[which(table_choc_immo$num_index == index[x]),"choc_immo"]) * split_ptf_immo[[x]]$val_marche
        return(split_ptf_immo[[x]])})

    # Refusionner le data.frame
    merged_ptf_immo <- do.call("rbind", split_ptf_immo)

    # Resortir un objet immo, dont l'attribut ptf_immo est trie comme initialement i.e. par ordre croissant du num_mp
    ptf_immo_mod    <- new("Immo", merged_ptf_immo[order(merged_ptf_immo$num_mp),])
    canton@ptf_fin@ptf_immo <- ptf_immo_mod

    return(canton)
    }
)

setGeneric(name = "do_choc_spread", def = function(x, canton){standardGeneric("do_choc_spread")})
setMethod(
    f = "do_choc_spread",
    signature = c("ChocSolvabilite2", "Canton"),
    definition = function(x, canton){
    # Verification des inputs
    if(nrow(canton@ptf_fin@ptf_oblig@ptf_oblig) == 0) {stop("[choc Mket : Spread] : tentative de calcul du choc spread avec un objet Oblig vide impossible. \n")}
    temp <- canton@ptf_fin@ptf_oblig@ptf_oblig

    # Methode bourrine
    table_choc_spread <- x@param_choc_mket@table_choc_spread
    temp$val_marche <- unlist(lapply(1:nrow(temp), function(x){do_choc_spread_unitaire(table_choc_spread, temp[x,])}))
    canton@ptf_fin@ptf_oblig <- new("Oblig", temp)

    return(canton)
    }
)

setGeneric(name = "do_choc_taux", def = function(canton){standardGeneric("do_choc_taux")})
setMethod(
  f = "do_choc_taux",
  signature = c("Canton"),
  definition = function(canton){
    # Verification des inputs
    if(nrow(canton@ptf_fin@ptf_oblig@ptf_oblig) == 0) {stop("[choc Mket : Taux] :
                                                            tentative de calcul du choc taux avec un objet Oblig vide
                                                            impossible. \n")}
    # Mise a jour des valeurs de marche du portefeuille
    canton@ptf_fin["ptf_oblig"]$val_marche <- calc_vm_oblig(canton@ptf_fin@ptf_oblig,
                                                            canton@mp_esg@yield_curve)
    # Mise a jour des valeurs de marche du portefeuille de reference
    canton@param_alm@ptf_reference["ptf_oblig"]$val_marche <- calc_vm_oblig(canton@param_alm@ptf_reference@ptf_oblig,
                                                                            canton@mp_esg@yield_curve)
    return(canton)
  }
)


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
