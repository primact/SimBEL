
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de PM d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les PM pour des contrats epargne en euros.
##'
##' \code{calc_pm} est une methode permettant de calculer les provisions mathematiques (PM)
##'  de fin de periode avant application de la revalorisation au titre de la participation aux benefices.
##' @name calc_pm
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param tab_prime une liste contenant les flux de primes pour chaque ligne de model points.
##' Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_primes}}.
##' @param tab_prest est une liste contenant les flux de prestations pour chaque ligne de model points.
##' Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_prest}}.
##' @param tx_cible est une liste conteant les taux cible annuel et semestriel par model points.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_cible}}.
##' @param tx_min une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_min}}.
##' @param an une valeur \code{numeric} represantant l'annee de projection courante.
##' @param method un \code{character} prenant pour valeur \code{normal} pour le calcul
##'  des flux avec application de la revalorisation au titre de la participation aux benefices,
##'   et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).
##' @param tx_soc est une valeur \code{numeric} correspondant au taux de prelevements sociaux.
##' @details Cette methode permet de calculer les montants de PM de fin d'annee avec une revalorisation
##' minimale. Les chargements sur encours sont egalement preleves. Cette methode permet de gerer les contrats a taux de
##' revalorisation net negatif. Cette methode permet egalement de calculer le besoin de financement necessaire
##' pour atteindre les exigences de revalorisation des assures.
##' @return Une liste contenant :
##' \describe{
##' \item{\code{method} : }{la valeur de l'argument \code{method}}
##' \item{\code{flux} : }{une liste comprenant les flux de l'annee}
##' \item{\code{stock} : }{une liste comprenant les nombres de sorties}
##' }
##' @return Le format de la liste \code{flux} est :
##' \describe{
##' \item{\code{rev_stock_brut} : }{un vecteur contenant la revalorisation minimale
##' brute de l'annee appliquee au PM}
##' \item{\code{rev_stock_nette} : }{un vecteur contenant la revalorisation minimale
##' nette de l'annee appliquee au PM}
##' \item{\code{enc_charg_stock} : }{un vecteur contenant les chargement sur encours de l'annee,
##' calcules en prenant en compte la revalorisation minimale}
##' \item{\code{enc_charg_base_th} : }{un vecteur contenant les chargements sur encours theoriques
##' de l'annee, evalues sur la base de la PM non revalorisees}
##' \item{\code{enc_charg_rmin_th} : }{un vecteur contenant les chargements sur encours theoriques
##'  de l'annee, evalues sur la seule base de la revalorisation minimale des PM}
##' \item{\code{base_enc_th} : }{un vecteur contenant l'assiette de calcul des chargements sur encours de l'annee}
##' \item{\code{soc_stock} : }{un vecteur contenant le prelevements sociaux de l'annee}
##' \item{\code{it_tech_stock} : }{un vecteur contenant les interets techniques sur stock de l'annee}
##' \item{\code{it_tech} : }{un vecteur contenant les interets techniques sur stock et
##' sur prestations de l'annee}
##' \item{\code{bes_tx_cible} : }{un vecteur contenant le besoin de financement de l'annee pour
##' atteindre le taux cible de chaque assure.}
##' }
##' @return Le format de la liste \code{stock} est :
##' \describe{
##' \item{\code{pm_deb : }}{un vecteur contenant le montant de PM en debut d'annee}
##' \item{\code{pm_fin : }}{un vecteur contenant le montant de PM en fin d'annee, avec
##' revalorisation au taux minimum}
##' \item{\code{pm_moy : }}{un vecteur contenant le montant de PM moyenne sur l'annee.}
##' }
##' @author Prim'Act
##' @seealso \code{\link{calc_primes}}, \code{\link{calc_prest}}, \code{\link{calc_tx_cible}},
##' \code{\link{calc_tx_min}}.
##' @export
##' @aliases EpEuroInd
##' @include EpEuroInd-class.R
##'
setGeneric(name = "calc_pm", def = function(x, tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc){standardGeneric("calc_pm")})
#--------------------------------------------------------

setMethod(
  f = "calc_pm",
  signature = c(x = "EpEuroInd", tab_prime = "list", tab_prest = "list", tx_cible = "list", tx_min = "list",
                an = "numeric", method = "character", tx_soc = "numeric"),
  def = function(x, tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc){


    # Seuil pour gerer les problemes d'arrondi
    SEUIL_ARRONDI <- 0.00001

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

    # Application d'un seuil pour eviter les problemes d'arrondi
    pm_fin[which(abs(pm_fin) < SEUIL_ARRONDI)] <- 0

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
