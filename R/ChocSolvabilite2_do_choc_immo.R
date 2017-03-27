#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque immobilier.
##'
##' \code{do_choc_immo} est une methode permettant d'appliquer le choc immobilier de la formule standard Solvabilite 2
##'  a un canton.
##' @name do_choc_immo
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central) 
##' de l'assureur.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario choque immobilier au sens de la formule standard Solvabilite 2.
##' @note Il est possible d'appliquer des chocs immobiliers distincts a chaque ligne immobilier present en portefeuille
##'  selon l'index.
##' Cette parametrisation est effectuee dans les fichiers d'inputs utilisateurs. 
##' @author Prim'Act
##' @export
##' @aliases ChocSolvabilite2
##' @include ChocSolvabilite2_class.R Canton_class.R

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
        
        # Mise a jour des PMVL Action/Immo/Oblig
        canton@ptf_fin <- do_update_pmvl(canton@ptf_fin)
        # Convention : on ne remet pas a jour la valeur de la PRE
        # Mise a jour des montant totaux de VM et de VNC des actifs
        canton@ptf_fin <- do_update_vm_vnc_precedent(canton@ptf_fin)
        
        # APPLICATION SIMPLIFIEE DU CHOC AU PORTEFEUILLE DE REFERENCE :
        if(nrow(canton@param_alm@ptf_reference@ptf_immo@ptf_immo) == 0) {stop("[choc Mket : Immo] : Portefeuille de reference Immo vide - application du choc impossible. \n")}
        temp <- canton@param_alm@ptf_reference@ptf_immo@ptf_immo
        # Methode bourrine : le portefeuille de reference est cense etre de petite taille --> il ne necessite pas les etapes de decoupages effectues pour le portefeuille immo standard qui peut devenir gros
        # Creation du vecteur de choc selon les index constituants le portefeuille de reference
        index <- canton@param_alm@ptf_reference@ptf_immo@ptf_immo$num_index
        vecteur_choc_immo_ref <- numeric()
        vecteur_choc_immo_ref <- unlist(lapply(1:length(index), function(x){vecteur_choc_immo_ref <- c(vecteur_choc_immo_ref, table_choc_immo[which(table_choc_immo$num_index == index[x]),"choc_immo"])}))
        # Application du choc sur le portefeuille de reference
        canton@param_alm@ptf_reference@ptf_immo@ptf_immo$val_marche <- (1 - vecteur_choc_immo_ref) * canton@param_alm@ptf_reference@ptf_immo@ptf_immo$val_marche
        canton@param_alm@ptf_reference@ptf_immo@ptf_immo$val_achat  <- canton@param_alm@ptf_reference@ptf_immo@ptf_immo$val_marche
        canton@param_alm@ptf_reference@ptf_immo@ptf_immo$val_nc     <- canton@param_alm@ptf_reference@ptf_immo@ptf_immo$val_marche
        
        return(canton)
    }
)