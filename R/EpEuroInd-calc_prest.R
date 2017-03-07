#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 22/01/2017. Fait par QG : initialisation
#----------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de prestations d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux de prestations d'un model point epargne en euros.
##'
##' \code{calc_prest} est une methode permettant de calculer les flux de prestations,
##' les chargements sur encours relatifs a ces prestations et les nombres de sorties sur une annee.
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
##' \item{\code{rev_prest} : }{un vecteur contenant la revalorisation des prestations de l'annee}
##' \item{\code{rev_prest_nette} : }{un vecteur contenant la revalorisation des prestations nette de l'annee}
##' \item{\code{enc_charg} : }{un vecteur contenant les chargements sur l'encours de l'annee}
##' \item{\code{rach_charg} : }{un vecteur contenant les chargements sur les rachats de l'annee}
##' \item{\code{soc_prest} : }{un vecteur contenant les prelevements sociaux sur prestations de l'annee}
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
setGeneric(name = "calc_prest", def = function(x, tx_sortie, tx_min, an, method, tx_soc){standardGeneric("calc_prest")})
#--------------------------------------------------------

setMethod(
  f = "calc_prest",
  signature = c(x = "EpEuroInd", tx_sortie = "matrix", tx_min = "list", an = "numeric", method = "character", tx_soc = "numeric"),
  def = function(x, tx_sortie, tx_min, an, method, tx_soc){

    # Calcul des taux de sortie
    # tx_cible <- calc_tx_cible(x, list_rd, list_param_mar)
    # tx_sortie <- calc_tx_sortie(x, ht)

    # Calcul du taux minimum
    # tx_min <-  calc_tx_min(x, an)

    # Applique la method de calcul
    if(method == "normal"){ # calcul des flux normaux
      pm_deb <- x@mp$pm
    }else if (method == "gar"){ # calcul des flux avec revalorisation garantie uniquement
      pm_deb <- x@mp$pm_gar
    }

    nb_mp <- nrow(x@mp)
    #Indicatrice de sortie en echeance
    ind_ech <- rep(0, nb_mp)
    ind_ech[which(an <= x@mp$terme)] <- 1

    # Extraction des taux de revalorisation minimum et des taux technique
    tx_tech_an <- tx_min[["tx_tech_an"]]
    tx_tech_se <- tx_min[["tx_tech_se"]]
    tx_min_an <- tx_min[["tx_an"]]
    tx_min_se <- tx_min[["tx_se"]]


    # Calcul des echeances et de la revalorisation
    ech <- pm_deb * (1 - ind_ech)  # echeance
    rev_ech <- ech * tx_min_se # revalorisation au taux minimum
    nb_ech <- x@mp$nb_contr * (1 - ind_ech) # nombre de contrats en echeance


    # Calcul des flux  rachats totaux
    # Taux de rachat incluant les rachats structurels et conjoncturels
    qx_rach_tot_glob <- pmax(0, pmin(1, tx_sortie[, "qx_rach_tot"] + tx_sortie[, "qx_rach_tot_dyn"]))
    rach_tot <- pm_deb * qx_rach_tot_glob * ind_ech # Flux de rachats totaux
    rev_rach_tot <- rach_tot * tx_min_se # revalorisation au taux minimum
    nb_rach_tot <- x@mp$nb_contr * qx_rach_tot_glob * ind_ech # nombre de contrats en rachat total

    # Calcul des flux de deces
    # Taux de deces sur la population des non rachetes
    qx_dc <- tx_sortie[, "qx_dc"] * (1 - qx_rach_tot_glob)
    dc <- pm_deb * qx_dc * ind_ech # Flux de rachats totaux
    rev_dc <- dc * tx_min_se # revalorisation au taux minimum
    nb_dc <- x@mp$nb_contr * qx_dc * ind_ech # nombre de contrats en deces

    # Calcul des flux rachats partiels
    # Taux de rachat incluant les rachats structurels et conjoncturels sur la population des non rachetes et vivants
    qx_rach_part_glob <- (1 - qx_rach_tot_glob) * (1 - tx_sortie[, "qx_dc"]) *
      pmax(0, pmin(1, tx_sortie[, "qx_rach_part"] + tx_sortie[ ,"qx_rach_part_dyn"]))
    rach_part <- pm_deb * qx_rach_part_glob * ind_ech # Flux de rachats partiels
    rev_rach_part <- rach_part * tx_min_se # revalorisation au taux minimum

    # Total des prestations
    prest <- ech + rach_tot + dc + rach_part # total prestations
    rev_prest <- rev_ech + rev_rach_tot + rev_dc + rev_rach_part # total revalorisation des prestations
    nb_sortie <- nb_ech + nb_dc + nb_rach_tot # nombre de sorties
    nb_contr_debut <- x@mp$nb_contr
    nb_contr_fin <- nb_contr_debut - nb_sortie # nombre de contrats en cours en fin d'annee
    nb_contr_moy <- (nb_contr_debut + nb_contr_fin) / 2  # nombre de contrats moyen

    # Calcul du taux de chargement sur encours
    # Applique une limite sur le chargement sur encours selon la valeur de l'indicatrice
    # permettant les taux négatifs.
    chgt_enc <- pmin(x@mp$chgt_enc, tx_min_an /(1 + tx_min_an)) * x@mp$ind_chgt_enc_pos +
      x@mp$chgt_enc * (1 - x@mp$ind_chgt_enc_pos)

    # Calcul des chargements sur encours
    enc_charg <- (prest + rev_prest) * chgt_period(chgt_enc, period = "se")

    # Calcul de la revalorisation nette des prestations avec capitalisation sur un semestre
    rev_prest_nette <- rev_prest - enc_charg

    # Calcul des autres chargements et des prelevements sociaux
    rach_charg <- (rach_tot + rach_part + rev_rach_tot + rev_rach_part) * x@mp$chgt_rach
    soc_prest <- pmax(0, rev_prest_nette) * tx_soc # prelevements sociaux

    # Calcul des interets techniques sur prestations
    it_tech_prest <- prest * tx_tech_se

    # output
    return(list(method = method,
                flux = list(
                  ech = ech,
                  rach_tot = rach_tot,
                  dc = dc,
                  rach_part = rach_part,
                  prest = prest,
                  rev_ech = rev_ech,
                  rev_rach_tot = rev_rach_tot,
                  rev_dc = rev_dc,
                  rev_rach_part = rev_rach_part,
                  rev_prest = rev_prest,
                  rev_prest_nette = rev_prest_nette,
                  enc_charg_prest = enc_charg,
                  rach_charg = rach_charg,
                  soc_prest = soc_prest,
                  it_tech_prest = it_tech_prest),
                stock = list(
                  nb_ech = nb_ech,
                  nb_rach_tot = nb_rach_tot,
                  nb_dc = nb_dc,
                  nb_debut = nb_contr_debut,
                  nb_sortie = nb_sortie,
                  nb_contr_fin = nb_contr_fin,
                  nb_contr_moy = nb_contr_moy
                )
              ))
  }
)
