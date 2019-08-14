
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul de la revalo des PM d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule et applique la revalorisation pour des PM pour des contrats epargne en euros et des retraites en phase de restitution.
##'
##' \code{calc_revalo_pm} est une methode permettant de calculer la revallorisation des PM sur une annee.
##' @name calc_revalo_pm
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros.
##' @param y une liste contenant les parametres.
##' \describe{
##' \item{\code{rev_net_alloue} : }{est une valeur \code{numeric} correspondant au montant de revalorisation a allouer.}
##' \item{\code{rev_brute_alloue_gar} : }{est une valeur \code{numeric} correspondant au montant de revalorisation a allouer a la PM garantie.}
##' \item{\code{tx_soc} : }{est une valeur \code{numeric} correspondant au taux de prelevement sociaux.
##' A remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}}.}
##' }
##' @details En epargne, cette methode permet de calculer les montants de PM de fin d'annee avec une revalorisation
##' minimale et une revalorisation additionnelle au titre de la participation aux benefices de l'annee.
##' Les chargements sur encours sont egalement calcules et preleves.
##' Cette methode permet de gerer les contrats a taux de revalorisation net negatif. En retraite, elle permet de
##' revaloriser le montant des rentes.
##' @return Une liste contenant :
##' \describe{
##' \item{\code{flux} : }{une liste comprenant les flux de l'annee}
##' \item{\code{stock} : }{une liste comprenant les nombres de sorties}
##' \item{\code{tx_rev_net} : }{un vecteur correspondant au taux de revalorisation net appliques
##'  a chaque model point.}
##' }
##' @return Le format de la liste \code{flux} est :
##' \describe{
##' \item{\code{rev_stock_brut_ap_pb} : }{un vecteur contenant la revalorisation
##' brute de l'annee appliquee au PM}
##' \item{\code{rev_stock_nette_ap_pb} : }{un vecteur contenant la revalorisation
##' nette de l'annee appliquee au PM. Elle peut etre negative pour des contrats a taux negatif.}
##' \item{\code{enc_charg_stock_ap_pb} : }{un vecteur contenant les montants de chargement sur encours
##' de l'annee calcules pour le stock de PM}
##' \item{\code{soc_stock_ap_pb} : }{un vecteur contenant les prelevements sociaux de l'annee}
##' }
##' @return Le format de la liste \code{stock} est :
##' s\describe{
##' \item{\code{pm_fin_ap_pb : }}{un vecteur contenant le montant de PM en fin d'annee}
##' }
##' @author Prim'Act
##' @seealso Le calcul des PM avec revalorisation minimale uniquement \code{\link{calc_pm}}.
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R
##'

#--------------------------------------------------------
setGeneric(name = "calc_revalo_pm", def = function(x, y) {standardGeneric("calc_revalo_pm")})
# Epargne
# y = list(rev_net_alloue, tx_soc)
# Retraite
# y = list()
#--------------------------------------------------------

setMethod(
    f = "calc_revalo_pm",
    signature = c(x = "EpEuroInd", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 3)                            stop("[EpEuroInd : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 3. \n")
        # Verification des noms des elements de la liste
        if (sum(names(y) == c("rev_net_alloue", "rev_brute_alloue_gar","tx_soc")) != length(y)) stop("[EpEuroInd : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 3 de nom :
                                                                                                     rev_net_alloue, rev_brute_alloue_gar, tx_soc . \n")

        # affectation pour eviter la modification des fonctions
        rev_net_alloue <- .subset2(y, 1)
        rev_brute_alloue_gar <- .subset2(y, 2)
        tx_soc         <- .subset2(y, 3)

        # Verification des types des elements de la liste
        if (! is.numeric(rev_net_alloue)) stop("[EpEuroInd : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 3, de nom : rev_net_alloue, rev_brute_alloue_gar, tx_soc, dont le type est : numeric, numeric. \n")
        if (! is.numeric(rev_brute_alloue_gar)) stop("[EpEuroInd : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 3, de nom : rev_net_alloue, rev_brute_alloue_gar,  tx_soc, dont le type est : numeric, numeric. \n")
        if (! is.numeric(tx_soc))         stop("[EpEuroInd : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 3, de nom : rev_net_alloue, rev_brute_alloue_gar, tx_soc dont le type est : numeric, numeric. \n")


        # Seuil pour gerer les problemes d'arrondi
        SEUIL_ARRONDI <- 0.00001

        # Table ModelPoint
        mp <- x@mp
        nom_mp <- names(mp)
        num_chgt_enc <- which(nom_mp == "chgt_enc")
        num_ind_chgt_enc_pos <- which(nom_mp == "ind_chgt_enc_pos")
        ind_chgt_enc_pos <- .subset2(mp, num_ind_chgt_enc_pos)

        # Tab
        tab <- x@tab@tab
        nom_tab <- names(tab)
        num_enc_charg_rmin_th <- which(nom_tab == "enc_charg_rmin_th")
        num_enc_charg_base_th <- which(nom_tab == "enc_charg_base_th")
        num_rev_stock_brut <- which(nom_tab == "rev_stock_brut")
        num_bes_tx_cible <- which(nom_tab == "bes_tx_cible")
        num_pm_deb <- which(nom_tab == "pm_deb")
        num_prest <- which(nom_tab == "prest")
        num_pri_net <- which(nom_tab == "pri_net")
        num_pm_gar <- which(nom_tab == "pm_gar")
        rev_stock_brut <- .subset2(tab, num_rev_stock_brut)
        bes_tx_cible <- .subset2(tab, num_bes_tx_cible)
        pm_deb <- .subset2(tab, num_pm_deb)
        prest <- .subset2(tab, num_prest)
        pri_net <- .subset2(tab, num_pri_net)
        pm_gar <- .subset2(tab, num_pm_gar)

        # Nombre de model points
        nb_mp <- nrow(x@mp)

        # Calcul des chargements
        chgt_enc_an <- .subset2(mp, num_chgt_enc)

        # Chargements theoriques avant pb
        chgt_enc_stock_th_av_pb <- .subset2(tab, num_enc_charg_rmin_th) + .subset2(tab, num_enc_charg_base_th)

        # Revalorisation nette avant pb
        rev_stock_nette_av_pb <- rev_stock_brut - chgt_enc_stock_th_av_pb
        # Application de la contrainte de taux negatif.
        rev_stock_nette_av_pb <- pmax(0, rev_stock_nette_av_pb) * ind_chgt_enc_pos +
            rev_stock_nette_av_pb * (1 - ind_chgt_enc_pos)

        # Calucl des chargements et de la revalorisation nette
        if(rev_net_alloue == 0){

            # Chargements reels
            chgt_enc_stock <- rev_stock_brut * ind_chgt_enc_pos + chgt_enc_stock_th_av_pb * (1 - ind_chgt_enc_pos)

            # Revalorisation nette
            rev_stock_nette <- rev_stock_nette_av_pb
        } else {

            # Allocation de la revalorisation additionnelle selon le taux cible.
            if(sum(bes_tx_cible) != 0)
                rev_net_alloue_mp <- rev_net_alloue * (bes_tx_cible / sum(bes_tx_cible))
            else # Attribution proportionnelle
                rev_net_alloue_mp <- rev_net_alloue * (1 / nb_mp)

            # Revalorisation nette
            rev_stock_nette <- rev_stock_nette_av_pb * (rev_stock_nette_av_pb > 0)  + rev_net_alloue_mp

            # Chargements reels
            chgt_enc_stock <- chgt_enc_stock_th_av_pb + rev_net_alloue_mp / (1 - chgt_enc_an) * chgt_enc_an


            # Revalorisation brute
            rev_stock_brut <- rev_stock_brut * (rev_stock_nette_av_pb > 0) +
                chgt_enc_stock_th_av_pb * (rev_stock_nette_av_pb <= 0) + rev_net_alloue_mp / (1 - chgt_enc_an)

        }

        # Attribution de la revalorisation garantie
        if(rev_brute_alloue_gar != 0){
          # Allocation de la revalorisation additionnelle selon le taux cible.
          if(sum(bes_tx_cible) != 0)
            rev_brute_alloue_gar_mp <- rev_brute_alloue_gar * (bes_tx_cible / sum(bes_tx_cible))
          else # Attribution proportionnelle
            rev_brute_alloue_gar_mp <- rev_brute_alloue_gar * (1 / nb_mp)
        } else{
          rev_brute_alloue_gar_mp <- 0
        }


        #Calcul du taux de revalorisation net
        tx_rev_net <- rev_stock_nette / (pm_deb - prest + 0.5 * pri_net)
        # Controle des denominateurs negatifs
        tx_rev_net[which((pm_deb - prest + 0.5 * pri_net) == 0)] <- 0

        # Prelevements sociaux
        soc_stock <- pmax(0, rev_stock_nette) * tx_soc # prelevements sociaux

        # Evaluation des provisions mathematiques avant PB
        pm_fin_ap_pb <- pm_deb - prest + pri_net + rev_stock_nette - soc_stock

        # PM garantie
        pm_gar_ap_pb <- pm_gar + rev_brute_alloue_gar_mp * (1 - chgt_enc_an) * (1 - tx_soc)

        # Application d'un seuil pour eviter les problemes d'arrondi
        pm_fin_ap_pb[which(abs(pm_fin_ap_pb) < SEUIL_ARRONDI)] <- 0

        # Message d'erreur si PM negative
        if(! all(pm_fin_ap_pb >= 0))
            stop("[EpEuro-calc_revalo_pm] : la valeur des PM ne peut pas etre negative.")

        # output
        return(list(stock = list(
            pm_fin_ap_pb = pm_fin_ap_pb,
            pm_gar_ap_pb = pm_gar_ap_pb
        ),
        flux = list(
            rev_stock_brut_ap_pb = rev_stock_brut,
            rev_stock_nette_ap_pb = rev_stock_nette,
            enc_charg_stock_ap_pb = chgt_enc_stock,
            soc_stock_ap_pb = soc_stock,
            supp_brut_gar_ap_pb = rev_brute_alloue_gar_mp,
            supp_nette_gar_ap_pb = rev_brute_alloue_gar_mp * (1 - chgt_enc_an)
        ),
        tx_rev_net = tx_rev_net
        ))
    }
)

#--------------------------------------------------------
setMethod(
    f = "calc_revalo_pm",
    signature = c(x = "RetraiteEuroRest", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 2L)  stop("[RetraiteEuroRest : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 2. \n")

        # affectation pour eviter la modification des fonctions
        rev_net_alloue <- .subset2(y, 1L)
        rev_brute_alloue_gar <- .subset2(y, 2)

        # Verification des types des elements de la liste
        if (! is.numeric(rev_net_alloue)) stop("[RetraiteEuroRest : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 2, de nom : rev_net_alloue, rev_brute_alloue_gar dont le type est : numeric, numeric. \n")
        if (! is.numeric(rev_brute_alloue_gar)) stop("[RetraiteEuroRest : calc_revalo_pm] : L'input y doit correspondre a une liste de longueur 2, de nom : rev_net_alloue, rev_brute_alloue_gar, dont le type est : numeric, numeric. \n")


        # Seuil pour gerer les problemes d'arrondi
        SEUIL_ARRONDI <- 0.00001


        # Nombre de model points
        nb_mp <- nrow(x@mp)

        # Tab
        tab <- x@tab@tab
        nom_tab <- names(tab)
        num_bes_tx_cible <- which(nom_tab == "bes_tx_cible")
        num_pm_fin <- which(nom_tab == "pm_fin")
        num_pm_gar <- which(nom_tab == "pm_gar")
        bes_tx_cible <- .subset2(tab, num_bes_tx_cible)
        pm_fin <- .subset2(tab, num_pm_fin)
        pm_gar <- .subset2(tab, num_pm_gar)


        # Calcul de la somme des besoins de tx cibles (pour eviter de le faire 2 fois)
        som <- sum(bes_tx_cible)

        # Allocation de la revalorisation additionnelle et le surplus garanti (en brut) selon le taux cible
        if (som != 0){
          rev_net_alloue_mp <- rev_net_alloue * (bes_tx_cible / som)
          rev_brute_alloue_gar_mp <- rev_brute_alloue_gar * (bes_tx_cible / som)
        }

        else{
          # Attribution proportionnelle
          rev_net_alloue_mp <- rev_net_alloue * (rep(1, nb_mp) / nb_mp)
          rev_brute_alloue_gar_mp <- rev_brute_alloue_gar * (rep(1, nb_mp) / nb_mp)
        }


        # Revalorisation brute
        rev_stock_brut <- rev_net_alloue_mp
        rev_stock_brut_gar <- rev_brute_alloue_gar_mp

        # Revalorisation nette
        rev_stock_nette <- rev_net_alloue_mp
        rev_stock_nette_gar <- rev_brute_alloue_gar_mp


        # Evaluation des provisions mathematiques avant PB
        pm_fin_ap_pb <- pm_fin + rev_stock_nette
        pm_gar_ap_pb <- pm_gar + rev_stock_nette_gar

        # Application d'un seuil pour eviter les problemes d'arrondi
        pm_fin_ap_pb [which(abs(pm_fin_ap_pb) < SEUIL_ARRONDI)] <- 0

        #Calcul du taux de revalorisation net
        tx_rev_net <- rev_net_alloue_mp / pm_fin_ap_pb
        # Controle des denominateurs negatifs
        tx_rev_net[which(pm_fin == 0)] <- 0


        # Message d'erreur si PM negative
        if(! all(pm_fin_ap_pb >= 0))
            stop("[RetraiteEuroRest-calc_revalo_pm] : la valeur des PM ne peut pas etre negative.")

        # output
        out_zero <- rep(0, nb_mp) # OutPut zero
        return(list(stock = list(
            pm_fin_ap_pb = pm_fin_ap_pb,
            pm_gar_ap_pb = pm_gar_ap_pb
        ),
        flux = list(
            rev_stock_brut_ap_pb = rev_stock_brut,
            rev_stock_nette_ap_pb = rev_stock_nette,
            supp_brut_gar__ap_pb = rev_stock_brut_gar, # Supplement brut de revalorisation sur le PM gar
            supp_nette_gar_ap_pb = rev_stock_nette_gar,  # Supplement net de revalorisation sur le PM gar
            enc_charg_stock_ap_pb = out_zero, # Pas de chargement sur encours pour les retraites
            soc_stock_ap_pb = out_zero # Pas de pr?l?vements sociaux pour les retraites
        ),
        tx_rev_net = tx_rev_net
        ))
    }
)
