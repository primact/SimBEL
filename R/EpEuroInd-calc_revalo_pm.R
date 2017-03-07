#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 02/02/2017. Fait par QG : initialisation
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul de la revalo des pm d'un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul la revalorisation pour des PM d'un model point epargne en euros.
##'
##' \code{calc_revalo_pm} est une methode permettant de calculer la revallorisation des PM sur une annee.
##' @name calc_revalo_pm
##' @docType methods
##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
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

    # Seuil pour gerer les problemes d'arrondi
    SEUIL_ARRONDI <- 0.00001


    # Nombre de model points
    nb_mp <- nrow(x@mp)

    #Calcul des chargements
    chgt_enc_an <- x@mp$chgt_enc

    # Chargements theoriques avant pb
    chgt_enc_stock_th_av_pb <- x@tab@tab[["enc_charg_rmin_th"]] + x@tab@tab[["enc_charg_base_th"]]

    # Revalorisation nette avant pb
    rev_stock_nette_av_pb <- x@tab@tab[["rev_stock_brut"]] - chgt_enc_stock_th_av_pb
    # Application de la contraite de taux negatif.
    rev_stock_nette_av_pb <- pmax(0, rev_stock_nette_av_pb) * x@mp$ind_chgt_enc_pos +
      rev_stock_nette_av_pb * (1 - x@mp$ind_chgt_enc_pos)

    if(rev_net_alloue == 0){
      # Revalorisation brute
      rev_stock_brut <- x@tab@tab[["rev_stock_brut"]]

      # Chargements reels
      chgt_enc_stock <- rev_stock_brut * x@mp$ind_chgt_enc_pos +
        chgt_enc_stock_th_av_pb * (1 - x@mp$ind_chgt_enc_pos)

      # Revalorisation nette
      rev_stock_nette <- rev_stock_nette_av_pb
    } else{

      # Allocation de la revalorisation additionnelle selon le taux cible.
      if(sum (x@tab@tab[["bes_tx_cible"]]) != 0){
        rev_net_alloue_mp <- rev_net_alloue * (x@tab@tab[["bes_tx_cible"]] / sum (x@tab@tab[["bes_tx_cible"]]))
      }else{ # Attribution proportionnelle
        rev_net_alloue_mp <- rev_net_alloue * (rep(1, nb_mp) / nb_mp)
      }

      # Revalorisation nette
      rev_stock_nette <- rev_stock_nette_av_pb * (rev_stock_nette_av_pb > 0)  + rev_net_alloue_mp

      # Chargements reels
      chgt_enc_stock <- chgt_enc_stock_th_av_pb + rev_net_alloue_mp / (1 - chgt_enc_an) * chgt_enc_an


      # Revalorisation brute
      rev_stock_brut <- x@tab@tab[["rev_stock_brut"]] * (rev_stock_nette_av_pb > 0) +
        chgt_enc_stock_th_av_pb * (rev_stock_nette_av_pb <= 0) + rev_net_alloue_mp / (1 - chgt_enc_an)

    }

    #Calcul du taux de revalorisation net
    tx_rev_net <- rev_stock_nette / (x@tab@tab[["pm_deb"]] - x@tab@tab[["prest"]]
                                           + 0.5 * x@tab@tab[["pri_net"]])
    # Controle des denominateurs negatifs
    tx_rev_net[which((x@tab@tab[["pm_deb"]] - x@tab@tab[["prest"]]
                + 0.5 * x@tab@tab[["pri_net"]]) == 0)] <- 0

    # Prelevements sociaux
    soc_stock <- pmax(0, rev_stock_nette) * tx_soc # prelevements sociaux

    # Evaluation des provisions mathematiques avant PB
    pm_fin_ap_pb <- x@tab@tab[["pm_deb"]] - x@tab@tab[["prest"]] + x@tab@tab[["pri_net"]] +
      rev_stock_nette -  soc_stock

    # Application d'un seuil pour eviter les problemes d'arrondi
    pm_fin_ap_pb [which(abs(pm_fin_ap_pb) < SEUIL_ARRONDI)] <- 0

    # Message d'erreur si PM negative
    if( ! prod(pm_fin_ap_pb >= 0)){
      stop("[EpEuro-calc_revalo_pm] : la valeur des PM ne peut pas etre negative.")
    }

    # output
    return(list(stock = list(
                  pm_fin_ap_pb = pm_fin_ap_pb
                ),
                flux = list(
                  rev_stock_brut_ap_pb = rev_stock_brut,
                  rev_stock_nette_ap_pb = rev_stock_nette,
                  enc_charg_stock_ap_pb = chgt_enc_stock,
                  soc_stock_ap_pb = soc_stock
                ),
              tx_rev_net = tx_rev_net
    ))
  }
)
