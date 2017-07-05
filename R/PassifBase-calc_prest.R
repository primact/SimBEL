
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de prestations d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux de prestations pour des contrats epargne en euros ou retraite euros en phases de restitution.
##'
##' \code{calc_prest} est une methode permettant de calculer les flux de prestations,
##' les chargements sur encours relatifs a ces prestations et les nombres de sorties sur une periode.
##' @name calc_prest
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros,
##' ou les model points retraite euro en phase de restitution.
##' @param y une liste contenant les parametres :
##' \describe{
##' \item{\code{proba_flux} : }{une matrice contenant les taux de sortie associes a chaque ligne de model points.
##' Le format de cette matrice correspond a la sortie de la methode \code{\link{calc_proba_flux}}}
##' \item{\code{tx_min} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_min}}.
##' A remplir uniquement si \code{x} est de type \link{\code{EpEuroInd}}.}
##' \item{\code{an} : }{une valeur \code{numeric} represantant l'annee de projection courante.
##' A remplir uniquement si \code{x} est de type \link{\code{EpEuroInd}}.}
##' \item{\code{method} : }{ un \code{character} prenant pour valeur \code{normal} pour le calcul
##'  des flux avec application de la revalorisation au titre de la participation aux benefices,
##'   et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).}
##' \item{\code{tx_soc} : } {est une valeur \code{numeric} correspondant au taux de prelevements sociaux.
##' A remplir uniquement si \code{x} est de type \link{\code{EpEuroInd}}.}
##' \item{\code{ht} : }{est un objet de la classe \link{\code{HypTech}}.}
##' }
##' @details En epargne, cette methode permet de calculer les flux de sortie en echeance, les flux de rachat totaux et partiels et
##' les flux de deces d'un contrat epargne en euros. Ces prestations font l'objet d'une relavorisation
##' au taux minimum contractuel. Les nombres de sortie sont egalement produits.
##' Des chargements sont appliques sur flux de rachats. Des prelevements sur encours sont appliques sur les
##' prestations revalorises au taux minimum contractuel. Cette methode permet de gerer les contrats a taux de
##' revalorisation net negatif.
##' Pour un contrat de retraite, elle permet de sortir les flux de rente et
##'  les nombres de sorties.
##' @return Une liste contenant :
##' \describe{
##' \item{\code{method} : }{la valeur de l'argument \code{method}}
##' \item{\code{flux} : }{une liste comprenant les flux de l'annee}
##' \item{\code{stock} : }{une liste comprenant les nombres de sorties}
##' }
##' @return Le format de la liste \code{flux} est :
##' \describe{
##' \item{\code{ech} : }{un vecteur contenant les flux de sortie en echeance de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rach_tot} : }{un vecteur contenant les flux de rachat totaux de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{dc} : }{un vecteur contenant les flux de deces de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rach_part} : }{un vecteur contenant les flux de rachat partiel de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rente} : }{le flux annuel de rente par model point : nul si l'objet est de type \link{\code{EpEuroInd}}.}
##' \item{\code{prest} : }{un vecteur contenant les flux prestations de l'annee (renseigne que l'objet x soit de type \link{\code{RetraiteEuroRest}} ou \link{\code{EpEuroInd}}).}
##' \item{\code{rev_ech} : }{un vecteur contenant la revalorisation des echeances de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rev_rach_tot} : }{un vecteur contenant la revalorisation des rachats totaux de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rev_dc} : }{un vecteur contenant la revalorisation des deces de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rev_rach_part} : }{un vecteur contenant la revalorisation des rachats partiels de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rev_prest} : }{un vecteur contenant la revalorisation brute des prestations de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rev_prest_nette} : }{un vecteur contenant la revalorisation des prestations nette de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{enc_charg} : }{un vecteur contenant les chargements sur l'encours de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{rach_charg} : }{un vecteur contenant les chargements sur les rachats de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{soc_prest} : }{un vecteur contenant les prelevements sociaux sur prestations de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{it_tech_prest} : }{un vecteur contenant les interets techniques sur prestations de l'annee. : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' }
##' @return Le format de la liste \code{stock} est :
##' \describe{
##' \item{\code{nb_ech : }}{un vecteur contenant le nombre de sorties en echeance de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{nb_rach_tot : }}{un vecteur contenant le nombre de rachats totaux de l'annee : nul si l'objet est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{nb_dc : }}{un vecteur contenant le nombre de deces de l'annee}
##' \item{\code{nb_sortie : }}{un vecteur contenant le nombre de sorties de l'annee}
##' \item{\code{nb_contr_fin : }}{un vecteur contenant le nombre de contrats en cours en fin d'annee}
##' \item{\code{nb_contr_moy : }}{un vecteur contenant la moyenne du nombre de contrats sur l'annee.}
##' }
##' @author Prim'Act
##' @seealso \code{\link{calc_proba_flux}}, \code{\link{calc_tx_min}}.
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R
##'

#--------------------------------------------------------
setGeneric(name = "calc_prest", def = function(x, y){standardGeneric("calc_prest")})
# Epargne
# y = list(proba_flux, tx_min, an, method, tx_soc)
# Retraite
# y = list(ht)
#--------------------------------------------------------

setMethod(
  f = "calc_prest",
  signature = c(x = "EpEuroInd", y = "list"),
  def = function(x, y){
    # Verification inputs
    if (length(y) != 5)                      {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5. \n")}
    # Verification des noms des elements de la liste
    if (sum(names(y) == c("proba_flux", "tx_min", "an", "method", "tx_soc")) != length(y)) {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5 de nom : proba_flux, tx_min, an, method, tx_soc. \n")}
    # Verification des types des elements de la liste
    if (! is.matrix(y[["proba_flux"]]))       {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5, de nom : proba_flux, tx_min, an, method, tx_soc, dont le type est : matrix, list, numeric, character, numeric. \n")}
    if (! is.list(y[["tx_min"]]))            {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5, de nom : proba_flux, tx_min, an, method, tx_soc, dont le type est : matrix, list, numeric, character, numeric. \n")}
    if (! is.numeric(y[["an"]]))             {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5, de nom : proba_flux, tx_min, an, method, tx_soc, dont le type est : matrix, list, numeric, character, numeric. \n")}
    if (! is.character(y[["method"]]))       {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5, de nom : proba_flux, tx_min, an, method, tx_soc, dont le type est : matrix, list, numeric, character, numeric. \n")}
    if (! is.numeric(y[["tx_soc"]]))         {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 5, de nom : proba_flux, tx_min, an, method, tx_soc, dont le type est : matrix, list, numeric, character, numeric. \n")}
    # affectation pour eviter la modification des fonctions
    proba_flux = y[["proba_flux"]]
    tx_min    = y[["tx_min"]]
    an        = y[["an"]]
    method    = y[["method"]]
    tx_soc    = y[["tx_soc"]]
    # Calcul des taux de sortie
    # tx_cible <- calc_tx_cible(x, list_rd, list_param_mar)
    # proba_flux <- calc_proba_flux(x, ht)

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
    qx_rach_tot_glob <- pmax(0, pmin(1, proba_flux[, "qx_rach_tot"] + proba_flux[, "qx_rach_tot_dyn"]))
    rach_tot <- pm_deb * qx_rach_tot_glob * ind_ech # Flux de rachats totaux
    rev_rach_tot <- rach_tot * tx_min_se # revalorisation au taux minimum
    nb_rach_tot <- x@mp$nb_contr * qx_rach_tot_glob * ind_ech # nombre de contrats en rachat total

    # Calcul des flux de deces
    # Taux de deces sur la population des non rachetes
    qx_dc <- proba_flux[, "qx_dc"] * (1 - qx_rach_tot_glob)
    dc <- pm_deb * qx_dc * ind_ech # Flux de rachats totaux
    rev_dc <- dc * tx_min_se # revalorisation au taux minimum
    nb_dc <- x@mp$nb_contr * qx_dc * ind_ech # nombre de contrats en deces

    # Calcul des flux rachats partiels
    # Taux de rachat incluant les rachats structurels et conjoncturels sur la population des non rachetes et vivants
    qx_rach_part_glob <- (1 - qx_rach_tot_glob) * (1 - proba_flux[, "qx_dc"]) *
      pmax(0, pmin(1, proba_flux[, "qx_rach_part"] + proba_flux[ ,"qx_rach_part_dyn"]))
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
                  rente = rep(0, nb_mp),
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

#--------------------------------------------------------
setMethod(
    f = "calc_prest",
    signature = c(x = "RetraiteEuroRest", y = "list"),
    def = function(x, y){

        # Verification inputs
        if (length(y) != 3){
          stop("[RetraiteEuroRest : calc_prest] : L'input y doit correspondre a une liste de longueur 2. \n")}
        # Verification des noms des elements de la liste
        if (sum(names(y) == c("ht", "proba_flux", "method")) != length(y)) {
          stop("[RetraiteEuroRest : calc_prest] : L'input y doit correspondre a une liste de longueur 3 de nom : ht, method, proba_flux \n")}
        # Verification des types des elements de la liste
        if (! class(y[["ht"]]) == "HypTech")       {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 3, de nom : ht, method dont le type est : HypTech, character, numeric. \n")}
        if (! is.matrix(y[["proba_flux"]]))       {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 3, de nom : ht, method dont le type est : HypTech, character, numeric. \n")}
        if (! is.character(y[["method"]]))       {stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 3, de nom : ht, method dont le type est : HypTech, character, numeric. \n")}

        # affectation pour eviter la modification des fonctions
        ht <- y[["ht"]]
        proba_flux <- y[["proba_flux"]]
        method <- y[["method"]]

        ##########
        # Dans les donnees les montants de 'rente' sont unitaires. Pour calculer les prestations
        # il faut multiplier par le nombre de contrats. Le montant de PM est total et pas unitaire.
        ##########
        nb_mp          <- nrow(x@mp)
        nom_retraite   <- names(x@mp)
        num_mp         <- which(nom_retraite == "num_mp")
        num_rente      <- which(nom_retraite == "rente")
        num_rente_gar  <- which(nom_retraite == "rente_gar")
        num_age_1      <- which(nom_retraite == "age")
        num_gen_1      <- which(nom_retraite == "gen")
        num_statut     <- which(nom_retraite == "statut_rvs")
        num_tx_rvs     <- which(nom_retraite == "tx_rvs")
        num_tab_mort_1 <- which(nom_retraite == "num_tab_mort")
        num_age_2      <- which(nom_retraite == "age_rvs")
        num_gen_2      <- which(nom_retraite == "gen_rvs")
        num_tab_mort_2 <- which(nom_retraite == "num_tab_mort_rvs")
        num_nb_contr   <- which(nom_retraite == "nb_contr")
        num_ch_arr     <- which(nom_retraite == "ch_arr")

        if(method == "normal"){ # calcul des flux normaux
            prestations <- .subset2(x@mp, num_nb_contr) * .subset2(x@mp, num_rente)
        }else if (method == "gar"){ # calcul des flux avec revalorisation garantie uniquement
            prestations <- .subset2(x@mp, num_nb_contr) * .subset2(x@mp, num_rente_gar)
        }

        # Chargements sur arrerage
        prestations <- prestations / (1 + .subset2(x@mp, num_ch_arr))

        # Calcul des rentes
        rente_flux <- proba_flux[, "proba_survie_un_an"] * prestations

        # Total des prestations
        prest <- rente_flux # total prestations

        ## Calcul du nombre de contrats
        # Nombre de contrat en debut de periode
        nb_contr_debut <- .subset2(x@mp, num_nb_contr)

        # Nombre de contrats
        nb_sortie    <-  proba_flux[, "proba_sortie_retraite"] * nb_contr_debut          # Nombre de contrats sortis
        nb_contr_fin <- nb_contr_debut - nb_sortie           # nombre de contrats en cours en fin d'annee
        nb_contr_moy <- (nb_contr_debut + nb_contr_fin) / 2  # nombre de contrats moyen


        # output
        return(list(method = method,
                    flux = list(
                        ech             = rep(0, nb_mp),
                        rach_tot        = rep(0, nb_mp),
                        dc              = rep(0, nb_mp),
                        rach_part       = rep(0, nb_mp),
                        rente           = rente_flux,
                        prest           = rente_flux,
                        rev_ech         = rep(0, nb_mp),
                        rev_rach_tot    = rep(0, nb_mp),
                        rev_dc          = rep(0, nb_mp),
                        rev_rach_part   = rep(0, nb_mp),
                        rev_prest       = rep(0, nb_mp),
                        rev_prest_nette = rep(0, nb_mp),
                        enc_charg_prest = rep(0, nb_mp),
                        rach_charg      = rep(0, nb_mp),
                        soc_prest       = rep(0, nb_mp),
                        it_tech_prest   = rep(0, nb_mp)),
                    stock = list(
                        nb_ech          = rep(0, nb_mp),
                        nb_rach_tot     = rep(0, nb_mp),
                        nb_dc           = nb_sortie,
                        nb_debut        = nb_contr_debut,
                        nb_sortie       = nb_sortie,
                        nb_contr_fin    = nb_contr_fin,
                        nb_contr_moy    = nb_contr_moy
                    )
        ))
    }
)
