#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par QG : initialisation
#----------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de pm d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les PM d'un model point epargne en euros.
##'
##' \code{calc_prest} est une methode permettant de calculer les PM sur une annee.
##' @name calc_prest
##' @docType methods
##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
##' @param tx_sortie une liste contenant les taux de sortie associes a chaque ligne de model points :
##'  \describe{
##'  \item{\code{qx_rach_tot : }}{un vecteur avec les taux de rachat total structurel}
##'  \item{\code{qx_rach_tot_dyn : }}{un vecteur avec les taux de rachat total dynamique}
##'  \item{\code{qx_dc : }}{un vecteur avec les taux de deces}
##'  \item{\code{qx_rach_part : }}{un vecteur avec les taux de rachats partiel structurel}
##'  \item{\code{qx_rach_part_dyn : }}{un vecteur avec les taux de rachats partiel dynamique}
##'  }
##' @param tx_min un data.frame contenant le taux de revalorisation minimum associes a chaque ligne de model points :
##'  \describe{
##'  \item{\code{tx_min_an : }}{un vecteur de taux annuels}
##'  \item{\code{tx_min_se : }}{un vecteur de taux semestriels}
##'  }
##' @param an un numeric represantant l'annee de projection courante
##' @param method un character prenant pour valeur \code{normal} pour le calcul des flux avec application de la revalorisation au titre
##' participation aux benefices, et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).
##' @return Une liste contenant :
##' \describe{
##' \item{\code{method} : }{la valeur de l'argument \code{method}}
##' \item{\code{flux} : }{un data.frame comprenant les flux de l'annee}
##' \item{\code{nb} : }{un data.frame comprenant les nombres de sorties}
##' }
##' Le format de \code{flux} est :
##' \describe{
##' \item{\code{ech} : }{un vecteur contenant les flux de sortie en echeance de l'annee}
##' \item{\code{rach_tot} : }{un vecteur contenant les flux de rachat totaux de l'annee}
##' \item{\code{dc} : }{un vecteur contenant les flux de deces de l'annee}
##' \item{\code{rach_part} : }{un vecteur contenant les flux de rachat partiel de l'annee}
##' \item{\code{prest} : }{un vecteur contenant les flux prestations de l'annee}
##' \item{\code{rev_ech} : }{un vecteur contenant la revalorisation des echeances de l'annee}
##' \item{\code{rev_rach_tot} : }{un vecteur contenant la revalorisation des rachats totaux de l'annee}
##' \item{\code{rev_dc} : }{un vecteur contenant la revalorisation des deces de l'annee}
##' \item{\code{rev_rach_part} : }{un vecteur contenant la revalorisation des rachats partiels de l'annee}
##' \item{\code{rev} : }{un vecteur contenant la revalorisation des prestations de l'annee}
##' }
##' Le format de \code{nb} est :
##' \describe{
##' \item{\code{nb_ech : }}{un vecteur contenant le nombre de sorties en echeance de l'annee}
##' \item{\code{nb_rach_tot : }}{un vecteur contenant le nombre de rachats totaux de l'annee}
##' \item{\code{nb_dc : }}{un vecteur contenant le nombre de deces de l'annee}
##' \item{\code{nb_sortie : }}{un vecteur contenant le nombre de sorties de l'annee}
##' \item{\code{nb_contr_fin : }}{un vecteur contenant le nombre de contrats en cours en fin d'annee}
##' }
##'
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd
##'
setGeneric(name = "calc_pm", def = function(x, tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc){standardGeneric("calc_pm")})
#--------------------------------------------------------

setMethod(
  f = "calc_pm",
  signature = c(x = "EpEuroInd", tab_prime = "list", tab_prest = "list", tx_cible = "list", tx_min = "list",
                an = "numeric", method = "character", tx_soc = "numeric"),
  def = function(x, tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc){

    # Applique la method de calcul
    if(method == "normal"){ # calcul des flux normaux
      pm_deb <- x@mp$pm
    }else if (method == "gar"){ # calcul des flux avec revalorisation garantie uniquement
      pm_deb <- x@mp$pm_gar
    }

    nb_mp <- nrow(x@mp)
    # Extraction des taux de revalorisation minimum. La table tx_rev peut etre une table de taux de revalorisation apres pb ou avant pb
    # tx_min <-  calc_tx_min(x, an)
    tx_min_an <- tx_min[["tx_an"]]
    tx_min_se <- tx_min[["tx_se"]]
    tx_tech_an <- tx_min[["tx_tech_an"]]
    tx_tech_se <- tx_min[["tx_tech_se"]]

    # Calcul du taux de chargement sur encours
    # Applique une limite sur le chargement sur encours selon la valeur de l'indicatrice
    # permettant les taux negatifs.
    chgt_enc_th <- x@mp$chgt_enc # Chargements sur encours theoriques
    chgt_enc <- pmin(chgt_enc_th, tx_min_an /(1 + tx_min_an)) * x@mp$ind_chgt_enc_pos +
      chgt_enc_th * (1 - x@mp$ind_chgt_enc_pos)

    # Calcul de la revalorisation brute
    rev_stock_brut <- (pm_deb - tab_prest[["prest"]]) * (tx_min_an) +
      tab_prime[["pri_net"]] * tx_min_se

    # Chargements : sur encours
    enc_charg_stock <- (pm_deb - tab_prest[["prest"]]) * (1 + tx_min_an) * chgt_enc +
      tab_prime[["pri_net"]] * (1 + tx_min_se) * chgt_period(chgt_enc, period = "se")

    # Chargement sur encours theorique en decomposant la part revalative au passif non revalorises
    # et a la revalorisation
    enc_charg_base_th <- (pm_deb - tab_prest[["prest"]]) * chgt_enc_th +
      tab_prime[["pri_net"]] * chgt_period(chgt_enc_th, period = "se")
    enc_charg_rmin_th <- (pm_deb -tab_prest[["prest"]]) * (tx_min_an)  * chgt_enc_th +
      tab_prime[["pri_net"]] * tx_min_se * chgt_period(chgt_enc_th, period = "se")

    # Base utilise pour appliques le calcul du taux de chargement sur encours
    base_enc_th <- (pm_deb -tab_prest[["prest"]]) * (1 + tx_min_an) +
      tab_prime[["pri_net"]] * (1 + tx_min_se)


    # enc_charg <- enc_charg_stock + tab_prest[["enc_charg_prest"]]

    # Calcul de la revalorisation sur stock
    rev_stock_nette <- rev_stock_brut - enc_charg_stock

    # Revalorisation nette totale
    rev_total_nette <- rev_stock_nette + tab_prest[["rev_prest_nette"]]

    # Prelevement sociaux
    soc_stock <- pmax(0, rev_stock_nette) * tx_soc
    # soc <- soc_stock + tab_prest[["soc_prest"]]

    # Evaluation des provisions mathematiques avant PB
    pm_fin <- pm_deb - tab_prest[["prest"]] + tab_prime[["pri_net"]] + rev_stock_nette -  soc_stock

    # Message d'erreur si PM negative
    if( ! prod(pm_fin >= 0)){
      stop("[EpEuro-calc_pm] : la valeur des PM ne peut pas etre negative.")
    }

    # PM moyenne et taux de chargement
    pm_moy <- (pm_deb + pm_fin) / 2


    # Evaluation des interets techniques
    it_tech_stock <- (pm_deb - tab_prest[["prest"]]) * tx_tech_an + tab_prime[["pri_net"]] * tx_tech_se
    it_tech <- it_tech_stock + tab_prest[["it_tech_prest"]]

    # EValuation du besoin de taux cible
    bes_tx_cible <- pmax(0,
                         tx_cible[["tx_cible_an"]]*(pm_deb - tab_prest[["prest"]]) +
                           tx_cible[["tx_cible_se"]] * tab_prime[["pri_net"]])

    # output
    return(list(method = method,
                stock = list(
                  pm_deb = pm_deb,
                  pm_fin = pm_fin,
                  pm_moy = pm_moy
                ),
                flux = list(
                  rev_stock_brut = rev_stock_brut,
                  rev_stock_nette = rev_stock_nette,
                  # rev_total_nette = rev_total_nette,
                  enc_charg_stock = enc_charg_stock,
                  enc_charg_base_th = enc_charg_base_th,
                  enc_charg_rmin_th = enc_charg_rmin_th,
                  base_enc_th = base_enc_th,
                  soc_stock = soc_stock,
                  # enc_charg = enc_charg,
                  # rach_charg = rach_charg,
                  # pri_chgt = pri_chgt,
                  # soc_stock = soc_stock,
                  it_tech_stock = it_tech_stock,
                  it_tech = it_tech,
                  bes_tx_cible = bes_tx_cible
                  )
              ))
  }
)
