
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_action_type2
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque action.
##'
##' \code{do_choc_action_type2} est une  methode permettant d'appliquer le choc action type 2 de la formule
##'  standard Solvabilite 2 a un canton.
##' @name do_choc_action_type2
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central) de l'assureur.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario choque action
##' au sens de la formule standard Solvabilite 2.
##' @note Il est possible d'appliquer des chocs actions distincts a chaque action selon l'index.
##' Cette parametrisation est effectuee dans les fichiers d'inputs utilisateurs.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R

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


        # Mise a jour des PMVL Action/Immo/Oblig
        canton@ptf_fin <- do_update_pmvl(canton@ptf_fin)

        # Convention : on ne remet pas a jour la valeur de la PRE

        # Mise a jour des montant totaux de VM et de VNC des actifs
        canton@ptf_fin <- do_update_vm_vnc_precedent(canton@ptf_fin)

        # APPLICATION SIMPLIFIEE DU CHOC AU PORTEFEUILLE DE REFERENCE :
        if(nrow(canton@param_alm@ptf_reference@ptf_action@ptf_action) == 0) {stop("[choc Mket : Action type 2] : Portefeuille de reference Action vide - application du choc impossible. \n")}
        temp <- canton@param_alm@ptf_reference@ptf_action@ptf_action
        # Methode bourrine : le portefeuille de reference est cense etre de petite taille --> il ne necessite pas les etapes de decoupages effectues pour le portefeuille immo standard qui peut devenir gros
        # Creation du vecteur de choc selon les index constituants le portefeuille de reference
        index <- canton@param_alm@ptf_reference@ptf_action@ptf_action$num_index
        vecteur_choc_action_ref <- numeric()
        vecteur_choc_action_ref <- unlist(lapply(1:length(index), function(x){vecteur_choc_action_ref <- c(vecteur_choc_action_ref, table_choc_action[which(table_choc_action$num_index == index[x]),"choc_action"])}))
        # Application du choc sur le portefeuille de reference
        canton@param_alm@ptf_reference@ptf_action@ptf_action$val_marche <- (1 - vecteur_choc_action_ref) * canton@param_alm@ptf_reference@ptf_action@ptf_action$val_marche
        canton@param_alm@ptf_reference@ptf_action@ptf_action$val_achat  <- canton@param_alm@ptf_reference@ptf_action@ptf_action$val_marche
        canton@param_alm@ptf_reference@ptf_action@ptf_action$val_nc     <- canton@param_alm@ptf_reference@ptf_action@ptf_action$val_marche

        return(canton)
    }
)
