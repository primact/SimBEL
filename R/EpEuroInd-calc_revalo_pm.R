#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 02/02/2017. Fait par QG : initialisation
#----------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul de la revalo des pm d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul la revalo pour des PM d'un model point epargne en euros.
##'
##' \code{calc_revalo_pm} est une methode permettant de calculer les PM sur une annee.
##' @name calc_revalo_pm
##' @docType methods
##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
##' @param rev_net_alloue une valeur de type \code{numeric} correspondant au montant de revalorisation a allouer
##' @param rev_net_alloue une valeur de type \code{numeric} correspondant au montant de revalorisation a allouer
##' @param tx_soc le taux de charges sociales
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd
##'
setGeneric(name = "calc_revalo_pm", def = function(x, rev_net_alloue, tx_soc)
  {standardGeneric("calc_revalo_pm")})
#--------------------------------------------------------

setMethod(
  f = "calc_revalo_pm",
  signature = c(x = "EpEuroInd", rev_net_alloue = "numeric", tx_soc = "numeric"),
  def = function(x, rev_net_alloue, tx_soc){

    # Nombre de model points
    nb_mp <- nrow(x@mp)

    #Calcul des chargements
    chgt_enc_an <- x@mp$chgt_enc

    # Allocation de la revalorisation additionnelle selon le taux cible.
    if(sum (x@tab@tab[["bes_tx_cible"]]) != 0){
      rev_net_alloue_mp <- rev_net_alloue * (x@tab@tab[["bes_tx_cible"]] / sum (x@tab@tab[["bes_tx_cible"]]))
    }else{ # Attribution proportionnelle
      rev_net_alloue_mp <- rev_net_alloue * (rep(1, nb_mp) / nb_mp)
      }


    # Indicatrice non possibilite de taux negatif
    ind1 <- x@mp$ind_chgt_enc_pos == 0
    # Cumul avec un test sur les chargements
    ind2 <- ind1 | ((x@tab@tab[["enc_charg_rmin_th"]] +
                       rev_net_alloue_mp >= x@tab@tab[["enc_charg_base_th"]]) *  (! ind1))

    # option 1 : Prelevement possible des chargements
    # Calcul des chargements
    chgt_enc_stock1 <- x@tab@tab[["enc_charg_rmin_th"]] + x@tab@tab[["enc_charg_base_th"]] +
      rev_net_alloue_mp / (1 - chgt_enc_an) * chgt_enc_an

    # Calcul de la revalorisation brute et nette
    revalo_stock_brute1 <- x@tab@tab[["rev_stock_brut"]] + rev_net_alloue_mp / (1 - chgt_enc_an)

    revalo_stock_nette1 <- revalo_stock_brute1 - chgt_enc_stock1

    # option 2 : pas de prelevement possible des chargements au dela de la revalorisaton
    chgt_enc_stock2 <-  x@tab@tab[["rev_stock_brut"]] + rev_net_alloue_mp
    revalo_stock_brute2 <- chgt_enc_stock2
    revalo_stock_nette2 <- 0

    # Application de la revalorisation par model point selon l'option 1 ou 2
    enc_charg_stock_ap_pb      <- ind2 * chgt_enc_stock1  + (1 - ind2) * chgt_enc_stock2
    rev_stock_brut_ap_pb  <- ind2 * revalo_stock_brute1  + (1 - ind2) * revalo_stock_brute2
    rev_stock_nette_ap_pb  <- ind2 * revalo_stock_nette1  + (1 - ind2) * revalo_stock_nette2

    #Calcul du taux de revalorisation net
    tx_rev_net <- rev_stock_nette_ap_pb / (x@tab@tab[["pm_deb"]] - x@tab@tab[["prest"]]
                                           + 0.5 * x@tab@tab[["pri_net"]])
    # Controle des denominateurs negatifs
    tx_rev_net[which((x@tab@tab[["pm_deb"]] - x@tab@tab[["prest"]]
                + 0.5 * x@tab@tab[["pri_net"]]) == 0)] <- 0


    # Prelevements sociaux
    soc_stock_ap_pb <- pmax(0, rev_stock_nette_ap_pb) * tx_soc # prelevements sociaux

    # Evaluation des provisions mathematiques avant PB
    pm_fin_ap_pb <- x@tab@tab[["pm_deb"]] - x@tab@tab[["prest"]] + x@tab@tab[["pri_net"]] +
      rev_stock_nette_ap_pb -  soc_stock_ap_pb

    # Message d'erreur si PM negative
    if( ! prod(pm_fin_ap_pb >= 0)){
      stop("[EpEuro-calc_revalo_pm] : la valeur des PM ne peut pas etre negative.")
    }


    # output
    return(list(stock = list(
                  pm_fin_ap_pb = pm_fin_ap_pb
                ),
                flux = list(
                  rev_stock_brut_ap_pb = rev_stock_brut_ap_pb,
                  rev_stock_nette_ap_pb = rev_stock_nette_ap_pb,
                  enc_charg_stock_ap_pb = enc_charg_stock_ap_pb,
                  soc_stock_ap_pb = soc_stock_ap_pb
                ),
              tx_rev_net = tx_rev_net
    ))
  }
)
